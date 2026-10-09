-- | Bounded Across adapter for Ethereum stablecoins to native Arbitrum USDC.
-- Provider JSON is untrusted: approve only the exact input and decode the bridge
-- bindings before exposing a source-wallet transaction. Unknown route shapes fail
-- closed rather than treating a successful simulation as proof of the recipient.
module Plether.Perps.Funding.Across
  ( acrossProvider, acrossSourceAssets, parseAcrossQuote
  , acrossTransferStatus, parseAcrossTransferStatus ) where

import Control.Exception (try)
import Control.Monad (foldM, unless)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString as BS
import Data.Char (isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client
import Network.HTTP.Types (statusCode, renderQuery)
import Numeric (readHex, showHex)
import Plether.Perps.Funding.Types
import System.Environment (lookupEnv)
import System.Timeout (timeout)

ethUsdc, ethUsdt, arbUsdc, spokePool, periphery, multicallHandler, eventEmitter :: Text
ethUsdc = "0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48"
ethUsdt = "0xdac17f958d2ee523a2206206994597c13d831ec7"
arbUsdc = "0xaf88d065e77c8cc2239327c5edb3a432268e5831"
-- Official deployment registry: https://docs.across.to/chains-and-contracts
spokePool = "0x5c7bcd6e7de5423a257d81b442095a1a6ced35c5"
periphery = "0x97ccdbea4632140639ad5ea9b944aa034eb15fd4"
multicallHandler = "0x0f7ae28de1c8532170ad4ee566b5801485c13a0e"
eventEmitter = "0xbf75133b48b0a42ab9374027902e83c5e2949034"

acrossSourceAssets :: Value
acrossSourceAssets = toJSON $ map asset [(ethUsdc,"USDC" :: Text),(ethUsdt,"USDT")]
 where
  asset (token,symbol) = object
    [ "chainId" .= (1 :: Integer), "token" .= token, "symbol" .= symbol
    , "decimals" .= (6 :: Int), "transactionTargets" .= [spokePool,periphery]
    , "approvalSpenders" .= [spokePool,periphery] ]

-- Authentication is resolved once at startup, so the public capability state
-- accurately reports missing configuration without exposing credentials.
acrossProvider :: Manager -> IO FundingProvider
acrossProvider manager = do
  credentials <- acrossCredentials
  pure $ case credentials of
    Just (key,ident) -> FundingProvider "across" Nothing (quote manager key ident)
    Nothing -> FundingProvider "across" (Just "ACROSS_CREDENTIALS_NOT_CONFIGURED")
      (\_ _ _ -> pure $ Left "ACROSS_CREDENTIALS_NOT_CONFIGURED")

acrossCredentials :: IO (Maybe (Text,Text))
acrossCredentials = do
  apiKey <- fmap (T.strip . T.pack) <$> lookupEnv "ACROSS_API_KEY"
  integrator <- fmap (T.toLower . T.strip . T.pack) <$> lookupEnv "ACROSS_INTEGRATOR_ID"
  pure $ case (apiKey,integrator) of
    (Just key,Just ident) | not (T.null key), T.length key <= 4096
      , T.all (\c -> c >= '!' && c <= '~') key, validIntegrator ident -> Just (key,ident)
    _ -> Nothing

-- | Advisory upstream bridge status, never evidence of a clearinghouse credit.
-- The caller owns per-intent polling/backoff. A `filled` response cannot confirm
-- our deposit; only canonical clearinghouse logs may advance that state.
-- https://docs.across.to/introduction/tracking-deposits
acrossTransferStatus :: Manager -> Text -> IO (Either Text Text)
acrossTransferStatus manager sourceTransaction = case validateHash sourceTransaction of
  Left _ -> pure $ Left "ACROSS_INVALID_SOURCE_TRANSACTION"
  Right transactionHash -> do
    credentials <- acrossCredentials
    case credentials of
      Nothing -> pure $ Left "ACROSS_CREDENTIALS_NOT_CONFIGURED"
      Just (key,integrator) -> do
        initial <- parseRequest "https://app.across.to/api/deposit/status"
        let request = initial
              { method = "GET", redirectCount = 0
              , responseTimeout = responseTimeoutMicro 8_000_000
              , checkResponse = \_ _ -> pure ()
              , requestHeaders = [("Authorization","Bearer " <> TE.encodeUtf8 key),("Accept","application/json")]
              , queryString = renderQuery True
                [("depositTxnRef",Just $ TE.encodeUtf8 transactionHash),("integratorId",Just $ TE.encodeUtf8 integrator)] }
        result <- timeout 10_000_000 $ try @HttpException $ withResponse request manager $ \response ->
          if statusCode (responseStatus response) /= 200
            then pure $ Left "ACROSS_STATUS_UNAVAILABLE"
            else readBoundedLimit 16384 (responseBody response)
        pure $ case result of
          Nothing -> Left "ACROSS_STATUS_TIMEOUT"
          Just (Left _) -> Left "ACROSS_STATUS_UNAVAILABLE"
          Just (Right (Left err)) -> Left err
          Just (Right (Right body)) -> case eitherDecodeStrict' body of
            Left _ -> Left "ACROSS_INVALID_STATUS_JSON"
            Right payload -> parseAcrossTransferStatus payload

parseAcrossTransferStatus :: Value -> Either Text Text
parseAcrossTransferStatus = either (const $ Left "ACROSS_INVALID_STATUS") Right . parseEither parser
 where
  parser = withObject "Across deposit status" $ \v -> do
    status <- v .: "status"
    ensure (status `elem` ["pending","filled","expired","refunded"]) "unrecognized deposit status"
    pure status

validIntegrator :: Text -> Bool
validIntegrator ident = T.length ident == 6 && T.isPrefixOf "0x" ident
  && T.all isHexDigit (T.drop 2 ident)

quote :: Manager -> Text -> Text -> FundingDeployment -> QuoteRequest -> Text -> IO (Either Text ProviderQuote)
quote manager apiKey integrator deployment request receiver =
  case validateRoute deployment request receiver of
    Left err -> pure $ Left err
    Right () -> do
      initial <- parseRequest "https://app.across.to/api/swap/approval"
      let upstream = initial
            { method = "GET", redirectCount = 0
            , responseTimeout = responseTimeoutMicro 15_000_000
            , checkResponse = \_ _ -> pure ()
            , requestHeaders = [("Authorization", "Bearer " <> TE.encodeUtf8 apiKey),("Accept","application/json")]
            , queryString = renderQuery True $ map (\(k,v) -> (k,Just $ TE.encodeUtf8 v))
              [ ("tradeType","exactInput"), ("strictTradeType","true")
              , ("originChainId","1"), ("destinationChainId","42161")
              , ("inputToken",T.toLower $ qrSourceToken request), ("outputToken",arbUsdc)
              , ("amount",qrSourceAmount request), ("depositor",qrSourceOwner request)
              , ("recipient",receiver), ("refundAddress",qrSourceOwner request)
              , ("refundOnOrigin","true"), ("skipOriginTxEstimation","false")
              , ("slippage","0.005"), ("integratorId",integrator) ] }
      -- Bound total wall time as well as response inactivity and total decoded bytes.
      result <- timeout 20_000_000 $ try @HttpException $ withResponse upstream manager $ \response ->
        if statusCode (responseStatus response) /= 200
          then pure $ Left "ACROSS_UPSTREAM_UNAVAILABLE"
          else readBounded (responseBody response)
      case result of
        Nothing -> pure $ Left "ACROSS_UPSTREAM_TIMEOUT"
        Just (Left _) -> pure $ Left "ACROSS_UPSTREAM_UNAVAILABLE"
        Just (Right (Left err)) -> pure $ Left err
        Just (Right (Right body)) -> case eitherDecodeStrict' body of
          Left _ -> pure $ Left "ACROSS_INVALID_JSON"
          Right payload -> do
            now <- floor <$> getPOSIXTime
            pure $ parseAcrossQuote now integrator deployment request receiver payload

readBounded :: BodyReader -> IO (Either Text BS.ByteString)
readBounded = readBoundedLimit 262144

readBoundedLimit :: Int -> BodyReader -> IO (Either Text BS.ByteString)
readBoundedLimit limit = go 0 []
 where
  go total chunks reader = do
    chunk <- brRead reader
    if BS.null chunk then pure $ Right $ BS.concat $ reverse chunks
      else if total + BS.length chunk > limit then pure $ Left "ACROSS_RESPONSE_TOO_LARGE"
      else go (total + BS.length chunk) (chunk:chunks) reader

validateRoute :: FundingDeployment -> QuoteRequest -> Text -> Either Text ()
validateRoute deployment request receiver = do
  _ <- validateAddress receiver
  _ <- validateAddress $ qrSourceOwner request
  _ <- validateAddress $ qrBeneficiary request
  _ <- validateAmount $ qrSourceAmount request
  unless (qrSourceChainId request == 1 && fdChainId deployment == 42161
    && T.toLower (fdToken deployment) == arbUsdc
    && T.toLower (qrSourceToken request) `elem` [ethUsdc,ethUsdt]) $ Left "ACROSS_UNSUPPORTED_ROUTE"

-- | Parse an independently captured API response, using the current time and the
-- configured integrator ID. Exact ABI layouts are deliberate compatibility gates.
parseAcrossQuote :: Integer -> Text -> FundingDeployment -> QuoteRequest -> Text -> Value -> Either Text ProviderQuote
parseAcrossQuote now integrator deployment request receiver payload = do
  validateRoute deployment request receiver
  unless (validIntegrator integrator) $ Left "ACROSS_INVALID_INTEGRATOR"
  either (Left . ("ACROSS_QUOTE_REJECTED: " <>) . T.pack) Right $ parseEither parseQuote payload
 where
  token = T.toLower $ qrSourceToken request
  owner = T.toLower $ qrSourceOwner request
  recipient = T.toLower receiver
  parseQuote = withObject "Across quote" $ \v -> do
    exactText v "amountType" "exactInput"
    let direct = token == ethUsdc
        target = if direct then spokePool else periphery
    exactText v "crossSwapType" (if direct then "bridgeableToBridgeable" else "anyToBridgeable")
    v .: "inputToken" >>= checkToken 1 token
    v .: "outputToken" >>= checkToken 42161 arbUsdc
    -- Refunds must be native USDC on Ethereum, even when the input was USDT.
    v .: "refundToken" >>= checkToken 1 ethUsdc
    amount <- amountField v "inputAmount"
    maximumInput <- amountField v "maxInputAmount"
    requested <- positive $ qrSourceAmount request
    ensure (amount == requested && maximumInput == requested) "input amount mismatch"
    expected <- amountField v "expectedOutputAmount"
    minimumOutput <- amountField v "minOutputAmount"
    ensure (not direct || expected == minimumOutput) "direct bridge estimate mismatch"
    ensure (expected >= minimumOutput && minimumOutput * 100 >= amount * 95
      && expected * 100 <= amount * 105) "output bounds exceed stablecoin route policy"
    checks <- v .: "checks"
    allowance <- checks .: "allowance"
    addressField allowance "token" >>= equals token "allowance token mismatch"
    addressField allowance "spender" >>= equals target "allowance spender mismatch"
    amountField allowance "expected" >>= equals amount "allowance amount mismatch"
    available <- allowance .: "actual" >>= nonnegative
    balance <- checks .: "balance"
    addressField balance "token" >>= equals token "balance token mismatch"
    amountField balance "expected" >>= equals amount "balance amount mismatch"
    availableBalance <- balance .: "actual" >>= nonnegative
    ensure (availableBalance >= amount) "insufficient source balance"
    approvals <- v .:? "approvalTxns" .!= []
    ensure (length approvals <= 2) "too many approvals"
    mapM_ (checkApproval target) approvals
    tx <- v .: "swapTx"
    simulation <- tx .: "simulationSuccess"
    ensure simulation "source simulation failed"
    tx .: "chainId" >>= equals (1 :: Integer) "source transaction chain mismatch"
    addressField tx "to" >>= equals target "source target mismatch"
    value <- tx .:? "value" .!= String "0"
    ensure (value == String "0" || value == String "0x0" || value == Number 0) "unexpected native value"
    _ <- tx .: "gas" >>= positive
    calldata <- tx .: "data"
    encoded <- hexData calldata
    apiExpiry <- v .: "quoteExpiryTimestamp"
    ensure (apiExpiry > now && apiExpiry <= now + 7200) "expired or unbounded API quote"
    expiry <- if direct then checkDeposit amount minimumOutput encoded
      else checkSwap amount minimumOutput encoded
    let expiresAt = minimum [apiExpiry,expiry,now+180]
    ensure (expiresAt > now + 30) "quote expires too soon"
    reference <- v .: "id"
    ensure (not (T.null reference) && T.length reference <= 256 && T.all (\c -> c >= '!' && c <= '~') reference)
      "invalid quote reference"
    let exactApproval = SourceTransaction 1 "approval" token (approvalData target amount) "0"
        resetApproval = SourceTransaction 1 "approval" token (approvalData target 0) "0"
        sourceApprovals
          | available >= amount = []
          | token == ethUsdt && available > 0 = [resetApproval,exactApproval]
          | otherwise = [exactApproval]
    pure $ ProviderQuote expiresAt (decimal expected) (decimal minimumOutput)
      (sourceApprovals <> [SourceTransaction 1 "bridge" target (T.toLower calldata) "0"]) reference

  checkApproval target = withObject "approval" $ \approval -> do
    approval .: "chainId" >>= equals (1 :: Integer) "approval chain mismatch"
    addressField approval "to" >>= equals token "approval target mismatch"
    val <- approval .:? "value" .!= String "0"
    ensure (val == String "0" || val == String "0x0" || val == Number 0) "approval native value"
    encoded <- approval .: "data" >>= hexData
    ensure (T.length encoded == 136 && T.take 8 encoded == "095ea7b3") "unsupported approval method"
    wordAddress (T.drop 8 encoded) 0 >>= equals target "approval spender mismatch"
    -- Never use the provider's allowance amount. Rebuild an exact approval below.
    _ <- wordNumber (T.drop 8 encoded) 32
    pure ()

  checkDeposit amount minimumOutput encoded = do
    ensure (T.take 8 encoded == "ad5425c6") "unsupported SpokePool method"
    let args = T.drop 8 encoded
    wordAddress args 0 >>= equals owner "refund owner mismatch"
    wordAddress args 32 >>= equals recipient "destination recipient mismatch"
    wordAddress args 64 >>= equals ethUsdc "bridge input token mismatch"
    wordAddress args 96 >>= equals arbUsdc "bridge output token mismatch"
    wordNumber args 128 >>= equals amount "bridge input amount mismatch"
    wordNumber args 160 >>= equals minimumOutput "bridge output amount mismatch"
    wordNumber args 192 >>= equals (42161 :: Integer) "destination chain mismatch"
    expiry <- checkTimes args 224
    wordNumber args 352 >>= equals (384 :: Integer) "noncanonical message offset"
    wordNumber args 384 >>= equals (0 :: Integer) "destination calls unsupported"
    checkTrailer args 416
    pure expiry

  checkSwap amount minimumOutput encoded = do
    ensure (T.take 8 encoded == "110560ad") "unsupported periphery method"
    let args = T.drop 8 encoded
    wordNumber args 0 >>= equals (32 :: Integer) "noncanonical swap tuple"
    let tuple = T.drop 64 args
    wordNumber tuple 0 >>= equals (0 :: Integer) "submission fee unsupported"
    wordNumber tuple 32 >>= equals (0 :: Integer) "submission recipient unsupported"
    wordNumber tuple 64 >>= equals (384 :: Integer) "noncanonical deposit tuple"
    wordAddress tuple 96 >>= equals token "swap token mismatch"
    exchange <- wordAddress tuple 128
    -- Router addresses: docs.0x.org/docs/core-concepts/contracts and
    -- github.com/Uniswap/universal-router/blob/main/deploy-addresses/mainnet.json.
    -- A swap may only use these public Ethereum routers. The Across swap proxy
    -- limits input to swapTokenAmount and enforces minimum returned USDC before
    -- depositing. Unrecognized exchanges are never forwarded to the wallet.
    ensure (exchange `elem`
      [ "0x0000000000001ff3684f28c67538d4d072c22734"
      , "0x66a9893cc07d91d95644aedd05d03f95e1dba8af" ]) "unsupported source exchange"
    transferType <- wordNumber tuple 160
    ensure (transferType == 0 || transferType == 1) "unsupported swap transfer method"
    wordNumber tuple 192 >>= equals amount "swap input amount mismatch"
    minBridgeInput <- wordNumber tuple 224
    ensure (minBridgeInput >= minimumOutput && minBridgeInput <= amount * 105 `div` 100)
      "swap output bound mismatch"
    routerOffset <- wordNumber tuple 256
    wordNumber tuple 288 >>= equals (1 :: Integer) "proportional swap output required"
    wordAddress tuple 320 >>= equals spokePool "swap SpokePool mismatch"
    wordNumber tuple 352 >>= equals (0 :: Integer) "unsupported swap nonce"
    let deposit = T.drop (384*2) tuple
    wordAddress deposit 0 >>= equals ethUsdc "swap bridge input token mismatch"
    wordAddress deposit 32 >>= equals arbUsdc "swap bridge output token mismatch"
    wordNumber deposit 64 >>= equals minimumOutput "swap bridge output amount mismatch"
    wordAddress deposit 96 >>= equals owner "swap refund owner mismatch"
    destination <- wordAddress deposit 128
    wordNumber deposit 160 >>= equals (42161 :: Integer) "swap destination chain mismatch"
    expiry <- checkTimes deposit 192
    wordNumber deposit 320 >>= equals (352 :: Integer) "noncanonical swap message offset"
    (message,messageEnd) <- bytesAt deposit 352
    if T.null message then ensure (destination == recipient) "swap recipient mismatch"
      else do
        ensure (destination == multicallHandler) "unsupported destination handler"
        checkDestinationMessage message
    ensure (routerOffset == fromIntegral (384+messageEnd)) "noncanonical router offset"
    (routerCalldata,routerEnd) <- bytesAt tuple (fromInteger routerOffset)
    ensure (T.length routerCalldata >= 8 &&
      if exchange == "0x0000000000001ff3684f28c67538d4d072c22734"
        then T.take 8 routerCalldata == "2213bc0b"
        else T.take 8 routerCalldata `elem` ["24856bc3","3593564c"])
      "unsupported source router method"
    checkTrailer tuple routerEnd
    pure expiry

  -- The published handler executes arbitrary calls, so permit only this exact
  -- ABI-canonical transfer-and-log recipe. Its runtime and the immutable emitter
  -- runtime must also be pinned by release/chain validation before quoting.
  checkDestinationMessage message = do
    wordNumber message 0 >>= equals (32 :: Integer) "noncanonical instructions tuple"
    wordNumber message 32 >>= equals (64 :: Integer) "noncanonical calls offset"
    wordNumber message 64 >>= equals (0 :: Integer) "destination fallback unsupported"
    wordNumber message 96 >>= equals (4 :: Integer) "unsupported destination call count"
    let calls = T.drop (128*2) message
    end <- foldM (checkCall calls) 128 [0 :: Int,1,2,3]
    ensure (T.length message == (128+end)*2) "unexpected destination data"
   where
    checkCall calls expectedOffset index = do
      actualOffset <- wordNumber calls (index*32)
      ensure (actualOffset == fromIntegral expectedOffset) "noncanonical destination call offset"
      let call = T.drop (expectedOffset*2) calls
      target <- wordAddress call 0
      wordNumber call 32 >>= equals (96 :: Integer) "noncanonical destination calldata offset"
      wordNumber call 64 >>= equals (0 :: Integer) "unexpected destination native value"
      (callData,end) <- bytesAt call 96
      if index < 2 then do
        ensure (target == multicallHandler) "unexpected destination transfer target"
        ensure (callData == "ef8738d3" <> T.replicate 24 "0" <> T.drop 2 arbUsdc
          <> T.replicate 24 "0" <> T.drop 2 recipient) "destination token or recipient mismatch"
       else do
        ensure (target == eventEmitter && T.take 8 callData == "d836083e") "unsupported destination metadata target"
        let logArgs = T.drop 8 callData
        wordNumber logArgs 0 >>= equals (32 :: Integer) "noncanonical event calldata"
        (metadata,logEnd) <- bytesAt logArgs 32
        ensure (not (T.null metadata) && T.length logArgs == logEnd*2) "invalid event metadata"
      pure $ expectedOffset+end

  -- Absolute timestamps only: relative deadlines require separate policy review.
  -- The expiry returned to the browser is shorter than both fill and quote limits.
  checkTimes args base = do
    _ <- wordAt args base -- exclusive relayer is part of the signed transaction
    quoteTimestamp <- wordNumber args (base+32)
    fillDeadline <- wordNumber args (base+64)
    exclusivity <- wordNumber args (base+96)
    ensure (quoteTimestamp >= now-600 && quoteTimestamp <= now+30) "stale quote timestamp"
    ensure (fillDeadline > now+60 && fillDeadline <= now+86400 && fillDeadline < 2^(32 :: Int)) "invalid fill deadline"
    ensure (exclusivity < 2^(32 :: Int) && (exclusivity <= 31536000 || exclusivity <= fillDeadline))
      "invalid exclusivity deadline"
    pure $ min (quoteTimestamp+900) (fillDeadline-30)

  checkTrailer args end = do
    let suffix = T.drop (end*2) args
    ensure (T.length args >= end*2 && suffix `elem`
      ["", "73c0de", "1dc0de" <> T.drop 2 (T.toLower integrator) <> "73c0de"])
      "unexpected calldata suffix"

checkToken :: Integer -> Text -> Value -> Parser ()
checkToken chain token = withObject "token" $ \v -> do
  addressField v "address" >>= equals token "token address mismatch"
  v .: "chainId" >>= equals chain "token chain mismatch"
  v .: "decimals" >>= equals (6 :: Int) "token decimal mismatch"

ensure :: Bool -> String -> Parser ()
ensure predicate message = unless predicate $ fail message

equals :: Eq a => a -> String -> a -> Parser ()
equals expected message actual = ensure (actual == expected) message

exactText :: Object -> Key -> Text -> Parser ()
exactText fields key expected = fields .: key >>= equals expected "quote type mismatch"

addressField :: Object -> Key -> Parser Text
addressField fields key = fields .: key >>= either (fail . T.unpack) pure . validateAddress

amountField :: Object -> Key -> Parser Integer
amountField fields key = fields .: key >>= positive

positive :: Text -> Parser Integer
positive = either (fail . T.unpack) pure . validateAmount

nonnegative :: Text -> Parser Integer
nonnegative "0" = pure 0
nonnegative value = positive value

hexData :: Text -> Parser Text
hexData value = do
  ensure (T.isPrefixOf "0x" value && even (T.length value) && T.length value >= 10
    && T.length value <= 131074 && T.all isHexDigit (T.drop 2 value)) "invalid calldata"
  pure $ T.toLower $ T.drop 2 value

wordAt :: Text -> Int -> Parser Text
wordAt input byteOffset = do
  let result = T.take 64 $ T.drop (byteOffset*2) input
  ensure (byteOffset >= 0 && T.length result == 64) "truncated ABI word"
  pure result

wordNumber :: Text -> Int -> Parser Integer
wordNumber input byteOffset = do
  value <- wordAt input byteOffset
  case readHex $ T.unpack value of
    [(number,"")] -> pure number
    _ -> fail "invalid ABI uint256"

-- Decode canonical dynamic bytes with bounded allocation and zero padding.
bytesAt :: Text -> Int -> Parser (Text,Int)
bytesAt input offset = do
  count <- wordNumber input offset
  ensure (count <= 32768) "dynamic calldata too large"
  let size = fromInteger count
      padded = ((size+31) `div` 32)*32
      end = offset+32+padded
      contents = T.take (size*2) $ T.drop ((offset+32)*2) input
      padding = T.take ((padded-size)*2) $ T.drop ((offset+32+size)*2) input
  ensure (T.length input >= end*2 && T.all (== '0') padding) "truncated or noncanonical dynamic bytes"
  pure (contents,end)

wordAddress :: Text -> Int -> Parser Text
wordAddress input byteOffset = do
  value <- wordAt input byteOffset
  ensure (T.take 24 value == T.replicate 24 "0") "invalid ABI address padding"
  either (fail . T.unpack) pure $ validateAddress $ "0x" <> T.drop 24 value

approvalData :: Text -> Integer -> Text
approvalData spender amount = "0x095ea7b3" <> T.replicate 24 "0" <> T.drop 2 spender <> uintWord amount

uintWord :: Integer -> Text
uintWord value = let encoded = T.pack $ showHex value "" in T.replicate (64 - T.length encoded) "0" <> encoded

decimal :: Integer -> Text
decimal = T.pack . show
