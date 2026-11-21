FROM ghcr.io/pimlicolabs/alto:v1.2.7@sha256:28cee87ea6b58ba10a37273e58602b50321516c36a81d0c35d50526d1f06995d

COPY src/cli/customTransport.ts /app/src/cli/customTransport.ts
COPY src/cli/deploySimulationsContract.ts /app/src/cli/deploySimulationsContract.ts
COPY src/cli/handler.ts /app/src/cli/handler.ts
COPY src/rpc/validation/BundlerCollectorTracerV07.ts /app/src/rpc/validation/BundlerCollectorTracerV07.ts
COPY src/rpc/validation/SafeValidator.ts /app/src/rpc/validation/SafeValidator.ts
RUN pnpm build
