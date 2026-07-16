export function derivePerpsCapabilities(input) {
    const sponsorship = (action) => input.sponsor.available && input.sponsor.sponsoredActions.has(action);
    const batch = input.smartAccount.canExecuteBatch;
    const firstDepositSupported = batch && input.wallet.canSignTypedData && input.token.supportsReceiveWithAuthorization;
    const normal = (action) => ({
        supported: true,
        sponsored: sponsorship(action),
        ...(!sponsorship(action) && {
            reason: "Gas sponsorship is currently unavailable for this action.",
        }),
    });
    return {
        ownerWalletRemainsConnected: true,
        smartAccountIsCanonicalTrader: true,
        firstDeposit: {
            supported: firstDepositSupported,
            sponsored: firstDepositSupported && sponsorship("deposit"),
            ...(!firstDepositSupported && {
                reason: !batch
                    ? "The selected smart account cannot atomically batch the deposit."
                    : !input.wallet.canSignTypedData
                        ? "The connected wallet cannot sign the USDC EIP-3009 authorization."
                        : "Configured USDC does not support receiveWithAuthorization.",
            }),
            ...(firstDepositSupported && !sponsorship("deposit") && {
                reason: "Gas sponsorship is currently unavailable for deposits.",
            }),
        },
        placeOrder: normal("place-order"),
        cancelOrder: {
            supported: false,
            sponsored: false,
            reason: "Committed orders are binding and have no trader cancellation endpoint.",
        },
        addMargin: normal("add-margin"),
        withdraw: normal("withdraw"),
        withdrawToOwner: normal("withdraw-to-owner"),
        settleClaim: normal("settle-claim"),
    };
}
//# sourceMappingURL=capabilities.js.map