import { encodeFunctionData, getAddress } from "viem";
import { cfdEngineTraderAbi, eip3009ReceiveWithAuthorizationAbi, erc20ApproveAbi, erc20TransferAbi, marginClearinghouseTraderAbi, orderRouterTraderAbi, } from "./abis.js";
import { splitEip3009Signature, validateAuthorization, } from "./eip3009.js";
import { InvalidPerpsActionError, UnsupportedPerpsActionError, } from "./errors.js";
function address(value, label) {
    try {
        return getAddress(value);
    }
    catch (cause) {
        throw new InvalidPerpsActionError(`${label} is not a valid address.`, cause);
    }
}
function positive(value, label) {
    if (value <= 0n) {
        throw new InvalidPerpsActionError(`${label} must be greater than zero.`);
    }
}
function nonNegative(value, label) {
    if (value < 0n) {
        throw new InvalidPerpsActionError(`${label} cannot be negative.`);
    }
}
function call(to, data) {
    return Object.freeze({ to, value: 0n, data });
}
function plan(kind, account, calls) {
    return Object.freeze({ kind, account: address(account, "Smart account"), calls });
}
/**
 * Creates one atomic smart-account batch:
 * USDC.receiveWithAuthorization(owner -> account), approve, depositMargin.
 */
export function buildAuthorizedDepositAction(input) {
    const account = address(input.account, "Smart account");
    const usdc = address(input.usdc, "USDC");
    const clearinghouse = address(input.clearinghouse, "Clearinghouse");
    validateAuthorization(input.authorization);
    if (address(input.authorization.to, "Authorization recipient") !== account) {
        throw new InvalidPerpsActionError("EIP-3009 authorization recipient must be the canonical smart account.");
    }
    const { v, r, s } = splitEip3009Signature(input.authorizationSignature);
    const amount = input.authorization.value;
    return plan("deposit", account, [
        call(usdc, encodeFunctionData({
            abi: eip3009ReceiveWithAuthorizationAbi,
            functionName: "receiveWithAuthorization",
            args: [
                address(input.authorization.from, "Authorization owner"),
                account,
                amount,
                input.authorization.validAfter,
                input.authorization.validBefore,
                input.authorization.nonce,
                v,
                r,
                s,
            ],
        })),
        call(usdc, encodeFunctionData({
            abi: erc20ApproveAbi,
            functionName: "approve",
            args: [clearinghouse, amount],
        })),
        call(clearinghouse, encodeFunctionData({
            abi: marginClearinghouseTraderAbi,
            functionName: "depositMargin",
            args: [amount],
        })),
    ]);
}
/** Deposit USDC already held by the smart account. */
export function buildSmartAccountBalanceDepositAction(input) {
    positive(input.amount, "Deposit amount");
    const account = address(input.account, "Smart account");
    const usdc = address(input.usdc, "USDC");
    const clearinghouse = address(input.clearinghouse, "Clearinghouse");
    return plan("deposit", account, [
        call(usdc, encodeFunctionData({
            abi: erc20ApproveAbi,
            functionName: "approve",
            args: [clearinghouse, input.amount],
        })),
        call(clearinghouse, encodeFunctionData({
            abi: marginClearinghouseTraderAbi,
            functionName: "depositMargin",
            args: [input.amount],
        })),
    ]);
}
export function buildPlaceOrderAction(input) {
    positive(input.sizeDelta, "Order size");
    nonNegative(input.marginDelta, "Order margin");
    nonNegative(input.targetPrice, "Target price");
    if (input.isClose && input.marginDelta !== 0n) {
        throw new InvalidPerpsActionError("Close orders must use zero margin delta.");
    }
    const account = address(input.account, "Smart account");
    const orderRouter = address(input.orderRouter, "Order router");
    const side = input.side === "BULL" ? 0 : input.side === "BEAR" ? 1 : undefined;
    if (side === undefined) {
        throw new InvalidPerpsActionError("Order side must be BULL or BEAR.");
    }
    return plan("place-order", account, [
        call(orderRouter, encodeFunctionData({
            abi: orderRouterTraderAbi,
            functionName: "commitOrder",
            args: [side, input.sizeDelta, input.marginDelta, input.targetPrice, input.isClose],
        })),
    ]);
}
/**
 * The current delayed-order protocol intentionally exposes no trader cancellation.
 * Keep this function so product code cannot accidentally invent a direct-call path.
 */
export function buildCancelOrderAction(_input) {
    throw new UnsupportedPerpsActionError("Committed perps orders are binding and cannot be cancelled by traders. Wait for keeper finalization.");
}
export function buildAddMarginAction(input) {
    positive(input.amount, "Margin amount");
    const account = address(input.account, "Smart account");
    const cfdEngine = address(input.cfdEngine, "CFD engine");
    return plan("add-margin", account, [
        call(cfdEngine, encodeFunctionData({
            abi: cfdEngineTraderAbi,
            functionName: "addMargin",
            args: [account, input.amount],
        })),
    ]);
}
export function buildWithdrawAction(input) {
    positive(input.amount, "Withdrawal amount");
    const account = address(input.account, "Smart account");
    const clearinghouse = address(input.clearinghouse, "Clearinghouse");
    return plan("withdraw", account, [
        call(clearinghouse, encodeFunctionData({
            abi: marginClearinghouseTraderAbi,
            functionName: "withdrawMargin",
            args: [input.amount],
        })),
    ]);
}
/**
 * Atomically withdraws from the clearinghouse to the smart account, then sends
 * that exact USDC amount to the account's externally owned wallet.
 *
 * The sponsor backend must independently prove `owner` is the registered owner
 * of `account`; accepting an arbitrary recipient would create a sponsored token
 * transfer primitive.
 */
export function buildWithdrawToOwnerAction(input) {
    positive(input.amount, "Withdrawal amount");
    const account = address(input.account, "Smart account");
    const owner = address(input.owner, "Smart-account owner");
    const usdc = address(input.usdc, "USDC");
    const clearinghouse = address(input.clearinghouse, "Clearinghouse");
    if (account === owner) {
        throw new InvalidPerpsActionError("Owner and account share one address; use buildWithdrawAction for EIP-7702 same-address mode.");
    }
    return plan("withdraw-to-owner", account, [
        call(clearinghouse, encodeFunctionData({
            abi: marginClearinghouseTraderAbi,
            functionName: "withdrawMargin",
            args: [input.amount],
        })),
        call(usdc, encodeFunctionData({
            abi: erc20TransferAbi,
            functionName: "transfer",
            args: [owner, input.amount],
        })),
    ]);
}
export function buildSettleTraderClaimAction(input) {
    const account = address(input.account, "Smart account");
    const cfdEngine = address(input.cfdEngine, "CFD engine");
    return plan("settle-claim", account, [
        call(cfdEngine, encodeFunctionData({
            abi: cfdEngineTraderAbi,
            functionName: "settleTraderClaim",
            args: [account],
        })),
    ]);
}
//# sourceMappingURL=actions.js.map