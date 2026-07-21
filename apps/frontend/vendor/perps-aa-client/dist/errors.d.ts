export type PerpsClientErrorCode = "INVALID_ACTION" | "ACTION_UNSUPPORTED" | "ACCOUNT_MISMATCH" | "USER_REJECTED" | "WALLET_UNSUPPORTED" | "SPONSOR_POLICY_DENIED" | "PAYMASTER_REJECTED" | "INSUFFICIENT_MARGIN" | "NO_OPEN_POSITION" | "NO_TRADER_CLAIM" | "TOO_MANY_PENDING_ORDERS" | "ORDER_INVALID" | "AUTHORIZATION_INVALID" | "BUNDLER_REJECTED" | "NETWORK_ERROR" | "UNKNOWN";
export declare class PerpsClientError extends Error {
    readonly code: PerpsClientErrorCode;
    readonly userMessage: string;
    readonly retryable: boolean;
    readonly cause: unknown;
    constructor(input: {
        code: PerpsClientErrorCode;
        message: string;
        userMessage: string;
        retryable?: boolean;
        cause?: unknown;
    });
}
export declare class InvalidPerpsActionError extends PerpsClientError {
    constructor(message: string, cause?: unknown);
}
export declare class UnsupportedPerpsActionError extends PerpsClientError {
    constructor(message: string);
}
/** Converts wallet, bundler, paymaster, and protocol failures into UI-safe errors. */
export declare function mapPerpsExecutionError(error: unknown): PerpsClientError;
//# sourceMappingURL=errors.d.ts.map