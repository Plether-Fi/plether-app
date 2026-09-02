module Plether.Protocol.Release
  ( ProtocolOperationalWalletEvidence (..)
  , ProtocolOperationalWallet (..)
  , ProtocolRelease (..)
  , knownProtocolReleases
  , currentProtocolRelease
  , protocolReleaseById
  , protocolReleaseId
  , protocolReleaseToJson
  ) where

import Data.Aeson (Value, object, (.=))
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Plether.Config (Config (..))

-- | Public, non-secret operational identities published with a release.
-- Private signing configuration is deliberately not a source for this
-- registry: an address appears here only after the release owner elects to
-- make that operational identity public.
data ProtocolOperationalWallet = ProtocolOperationalWallet
  { powRole :: Text
  , powAddress :: Text
  , powDescription :: Text
  , powRepresentativeEvidence :: ProtocolOperationalWalletEvidence
  }
  deriving stock (Show, Eq)

data ProtocolOperationalWalletEvidence = ProtocolOperationalWalletEvidence
  { poweSelector :: Text
  , poweTransactionHash :: Text
  , poweBlockNumber :: Integer
  }
  deriving stock (Show, Eq)

data ProtocolRelease = ProtocolRelease
  { prId :: Text
  , prName :: Text
  , prChainId :: Integer
  , prDeploymentBlock :: Integer
  , prCalculationVersion :: Text
  , prUsdc :: Text
  , prOrderRouter :: Text
  , prOrderLifecycleBook :: Maybe Text
  , prOrderRouterAdmin :: Text
  , prCfdEngine :: Text
  , prCfdEngineLens :: Text
  , prCfdEngineSettlementSidecar :: Text
  , prCfdEngineAdmin :: Text
  , prMarginClearinghouse :: Text
  , prPublicLens :: Text
  , prAccountLens :: Text
  , prHousePool :: Text
  , prSeniorVault :: Text
  , prJuniorVault :: Text
  , prPletherOracle :: Text
  , prOperationalWallets :: [ProtocolOperationalWallet]
  }
  deriving stock (Show, Eq)

protocolReleaseId :: Integer -> Text
protocolReleaseId chainId
  | chainId == 421614 = "arbitrum-sepolia-2026-08-v1.2.0"
  | otherwise = "chain-" <> T.pack (show chainId) <> "-current"

-- Keep this startup-safe compiled table in parity with
-- config/protocol-releases.json. ReleaseSpec verifies the complete address
-- record and the analytics ABI coverage consumed by the explorer.
knownProtocolReleases :: [ProtocolRelease]
knownProtocolReleases =
  [ ProtocolRelease
      { -- Keep the existing network/date prefix and append the protocol label
        -- so another deployment in the same month cannot alias this release.
        prId = "arbitrum-sepolia-2026-08-v1.2.0"
      , prName = "Plether Perps — August 2026 (v1.2.0)"
      , prChainId = 421614
      , prDeploymentBlock = 302257125
      , prCalculationVersion = "protocol-transparency-v1"
      , prUsdc = "0x1647e41f49ED6D688936092B5a291c4B28106343"
      , prOrderRouter = "0x97A901dE2B267c307E264FD5F71403F8072F73e7"
      , prOrderLifecycleBook = Just "0xa210928a7E0AE27626B8d0E67Bbd82305438aB9E"
      , prOrderRouterAdmin = "0x3d0e430D670D74988C1B3e76b6ef018e79ab1E37"
      , prCfdEngine = "0x3dc9C0A1f9C745A4B08BD5C2E6c7aE613561c20D"
      , prCfdEngineLens = "0x140067daAdd28bE4b04e649EEaCf6F5ECbEe8C79"
      , prCfdEngineSettlementSidecar = "0x288F70eC7cF0e16ae4FE4b91B5c266B047c83aFF"
      , prCfdEngineAdmin = "0xda1240c36f3a4ddcAB3028F66B15Dfe91702dE2A"
      , prMarginClearinghouse = "0x2f98787F6dCC3b1f2E4a2AFa5acf410159b9F211"
      , prPublicLens = "0xC41e92F541cCF19FA203a96CecF3Ae4D2Ed7F60A"
      , prAccountLens = "0x429DA61a7a616DeDD84d2a51eB6Dc1bD72427dC1"
      , prHousePool = "0x86939a377A78EDe8EEe5445765ac77c9016E35E2"
      , prSeniorVault = "0xB5A9a9d634197B8F0EA7c4042CF8d5701767D710"
      , prJuniorVault = "0xdf306B52eaC722D5994E2cc93D2818F391d68Adb"
      , prPletherOracle = "0xC69ec16EfB71F62984E9b2688396F34062277FdC"
      , prOperationalWallets = []
      }
  , ProtocolRelease
      { prId = "arbitrum-sepolia-2026-07"
      , prName = "Plether Perps — July 2026"
      , prChainId = 421614
      , prDeploymentBlock = 288439939
      , prCalculationVersion = "protocol-transparency-v1"
      , prUsdc = "0xB15503d70B0eAa644dc6650d2A248762F7c5bCE3"
      , prOrderRouter = "0x04E3103752f623fBcDcD01f588590Af4c53E4c1E"
      , prOrderLifecycleBook = Nothing
      , prOrderRouterAdmin = "0x3073d6D021eC20b95a8b7C780f5c30c07036ff6C"
      , prCfdEngine = "0x6A25eA1015b5f032d8a2D95d57AEfcB99219bF0a"
      , prCfdEngineLens = "0xa9aa4097874e9622eaabee68f65ff5e3757728c5"
      , prCfdEngineSettlementSidecar = "0x0b652c4d4610234e221403076c116292f935b424"
      , prCfdEngineAdmin = "0xb256d4E88d649b2A149aA8B8caa3159260eFBc39"
      , prMarginClearinghouse = "0x19c2f60f6312EAF9acDE4C2b04551a05cA9bE76e"
      , prPublicLens = "0x4E202C06e2C378d1a85577ac631e592AB66f23FB"
      , prAccountLens = "0xC4C886A6F1D7CB22C833AC1b29f29Da43AfbcCd1"
      , prHousePool = "0xFA654f4c548130F09C3Fb962AbD4bE32c0357C18"
      , prSeniorVault = "0x4bAb5448C1BD9A48B978ABcb014F1a8F80F100A8"
      , prJuniorVault = "0x7258d6E91fbEFB8a16751575adbe9bBB3086D458"
      , prPletherOracle = "0xADfEd3bf768D810309B97b4dF9F9E77Eaa3a401c"
      , prOperationalWallets =
          [ ProtocolOperationalWallet
              { powRole = "oracle_updater"
              , powAddress = "0x1329A7Fa975F2F8aA2cAc86A833f03655201d34f"
              , powDescription =
                  "Publicly observed sender of permissionless updateMarkPrice(bytes[]) transactions; this is release metadata, not an onchain privilege."
              , powRepresentativeEvidence =
                  ProtocolOperationalWalletEvidence
                    { poweSelector = "0x2efdaf14"
                    , poweTransactionHash =
                        "0x10b4a7e44530e7b97ab0e907f00000f0e69de58f3875bce46a7923fb1bf14e18"
                    , poweBlockNumber = 292710937
                    }
              }
          , ProtocolOperationalWallet
              { powRole = "order_keeper"
              , powAddress = "0x5a71a4094Ec81165Ada48AA4c27dA48ec27E0d6B"
              , powDescription =
                  "Publicly observed sender of permissionless executeOrder(uint64,bytes[]) transactions; this is release metadata, not an onchain privilege."
              , powRepresentativeEvidence =
                  ProtocolOperationalWalletEvidence
                    { poweSelector = "0xc700abdc"
                    , poweTransactionHash =
                        "0x70c860c00071635d9d57e18b430584e78acc54ff71e57470fbbbbe3c729fc67e"
                    , poweBlockNumber = 292710334
                    }
              }
          ]
      }
  ]

currentProtocolRelease :: Config -> ProtocolRelease
currentProtocolRelease cfg =
  fromMaybe configuredRelease $
    find (`matchesConfiguredRelease` cfg) knownProtocolReleases
  where
    configuredRelease =
      ProtocolRelease
        { prId =
            "chain-"
              <> T.pack (show $ cfgPerpsChainId cfg)
              <> "-block-"
              <> T.pack (show $ cfgPerpsIndexerStartBlock cfg)
        , prName = "Configured Plether Perps release"
        , prChainId = cfgPerpsChainId cfg
        , prDeploymentBlock = cfgPerpsIndexerStartBlock cfg
        , prCalculationVersion = "protocol-transparency-v1"
        , prUsdc = cfgPerpsUsdc cfg
        , prOrderRouter = cfgPerpsOrderRouter cfg
        , prOrderLifecycleBook = cfgPerpsOrderLifecycleBook cfg
        , prOrderRouterAdmin = cfgPerpsOrderRouterAdmin cfg
        , prCfdEngine = cfgPerpsCfdEngine cfg
        , prCfdEngineLens = cfgPerpsCfdEngineLens cfg
        , prCfdEngineSettlementSidecar = cfgPerpsCfdEngineSettlementSidecar cfg
        , prCfdEngineAdmin = cfgPerpsCfdEngineAdmin cfg
        , prMarginClearinghouse = cfgPerpsMarginClearinghouse cfg
        , prPublicLens = cfgPerpsPublicLens cfg
        , prAccountLens = cfgPerpsAccountLens cfg
        , prHousePool = cfgPerpsHousePool cfg
        , prSeniorVault = cfgPerpsSeniorVault cfg
        , prJuniorVault = cfgPerpsJuniorVault cfg
        , prPletherOracle = cfgPerpsPletherOracle cfg
        , prOperationalWallets = []
        }

-- | Resolve the configured release plus any checked-in historical releases
-- served by the same configured chain. This keeps old release deep links
-- usable after a same-chain deployment cutover without ever sending a state
-- read to an RPC endpoint bound to another chain.
protocolReleaseById :: Config -> Text -> Maybe ProtocolRelease
protocolReleaseById cfg requested =
  find ((== requested) . prId) servedReleases
  where
    current = currentProtocolRelease cfg
    servedReleases =
      current
        : filter
          (\release ->
            prChainId release == prChainId current
              && prId release /= prId current
          )
          knownProtocolReleases

matchesConfiguredRelease :: ProtocolRelease -> Config -> Bool
matchesConfiguredRelease release cfg =
  prChainId release == cfgPerpsChainId cfg
    && and
      [ sameAddress (prUsdc release) (cfgPerpsUsdc cfg)
      , sameAddress (prOrderRouter release) (cfgPerpsOrderRouter cfg)
      , sameOptionalAddress
          (prOrderLifecycleBook release)
          (cfgPerpsOrderLifecycleBook cfg)
      , sameAddress (prOrderRouterAdmin release) (cfgPerpsOrderRouterAdmin cfg)
      , sameAddress (prCfdEngine release) (cfgPerpsCfdEngine cfg)
      , sameAddress (prCfdEngineLens release) (cfgPerpsCfdEngineLens cfg)
      , sameAddress
          (prCfdEngineSettlementSidecar release)
          (cfgPerpsCfdEngineSettlementSidecar cfg)
      , sameAddress (prCfdEngineAdmin release) (cfgPerpsCfdEngineAdmin cfg)
      , sameAddress (prMarginClearinghouse release) (cfgPerpsMarginClearinghouse cfg)
      , sameAddress (prPublicLens release) (cfgPerpsPublicLens cfg)
      , sameAddress (prAccountLens release) (cfgPerpsAccountLens cfg)
      , sameAddress (prHousePool release) (cfgPerpsHousePool cfg)
      , sameAddress (prSeniorVault release) (cfgPerpsSeniorVault cfg)
      , sameAddress (prJuniorVault release) (cfgPerpsJuniorVault cfg)
      , sameAddress (prPletherOracle release) (cfgPerpsPletherOracle cfg)
      ]
 where
  sameAddress left right = T.toCaseFold left == T.toCaseFold right
  sameOptionalAddress Nothing Nothing = True
  sameOptionalAddress (Just left) (Just right) = sameAddress left right
  sameOptionalAddress _ _ = False

protocolReleaseToJson :: ProtocolRelease -> Value
protocolReleaseToJson ProtocolRelease {..} =
  object
    [ "releaseId" .= prId
    , "name" .= prName
    , "chainId" .= prChainId
    , "deploymentBlock" .= show prDeploymentBlock
    , "calculationVersion" .= prCalculationVersion
    , "contracts" .= object
        [ "usdc" .= prUsdc
        , "orderRouter" .= prOrderRouter
        , "orderLifecycleBook" .= prOrderLifecycleBook
        , "orderRouterAdmin" .= prOrderRouterAdmin
        , "cfdEngine" .= prCfdEngine
        , "cfdEngineLens" .= prCfdEngineLens
        , "cfdEngineSettlementSidecar" .= prCfdEngineSettlementSidecar
        , "cfdEngineAdmin" .= prCfdEngineAdmin
        , "marginClearinghouse" .= prMarginClearinghouse
        , "publicLens" .= prPublicLens
        , "accountLens" .= prAccountLens
        , "housePool" .= prHousePool
        , "seniorVault" .= prSeniorVault
        , "juniorVault" .= prJuniorVault
        , "pletherOracle" .= prPletherOracle
        ]
    , "operationalWallets" .=
        [ object
            [ "role" .= powRole wallet
            , "address" .= powAddress wallet
            , "description" .= powDescription wallet
            , "representativeEvidence" .=
                object
                  [ "selector" .=
                      poweSelector (powRepresentativeEvidence wallet)
                  , "transactionHash" .=
                      poweTransactionHash (powRepresentativeEvidence wallet)
                  , "blockNumber" .=
                      show
                        (poweBlockNumber $ powRepresentativeEvidence wallet)
                  ]
            ]
        | wallet <- prOperationalWallets
        ]
    ]
