import { type Hex } from "viem";
import type { Eip7677PaymasterResponse, ParsedPaymasterEnvelope, PletherPaymasterProfile } from "./types.js";
export declare const PAYMASTER_HEADER_BYTES = 52;
export declare const PLETHER_PAYMASTER_DATA_BYTES = 157;
export declare const PLETHER_PAYMASTER_AND_DATA_BYTES = 209;
export declare const DEFAULT_MAX_VALIDITY_WINDOW_SECONDS = 600n;
/** Accepts either an EIP-7677 split response or a fully packed v0.8 response. */
export declare function normalizePaymasterResponse(response: Eip7677PaymasterResponse, fallback?: ParsedPaymasterEnvelope): ParsedPaymasterEnvelope;
/**
 * Parses the fixed v0.8 envelope:
 * paymaster(20) | verificationGas(16) | postOpGas(16) | validUntil(6) |
 * validAfter(6) | maxCost(16) | policyId(32) | accountCodeHash(32) |
 * signature(65).
 */
export declare function parsePaymasterAndData(paymasterAndData: Hex): ParsedPaymasterEnvelope;
/** Applies the chain-manifest profile after structurally parsing a Plether envelope. */
export declare function validatePletherPaymasterEnvelope(envelope: ParsedPaymasterEnvelope, profile: PletherPaymasterProfile): ParsedPaymasterEnvelope;
//# sourceMappingURL=paymaster.d.ts.map