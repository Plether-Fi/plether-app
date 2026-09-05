FROM debian:bookworm-slim AS builder

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl gcc g++ make libffi-dev libgmp-dev zlib1g-dev \
    libpq-dev libsecp256k1-dev pkg-config ca-certificates && \
    rm -rf /var/lib/apt/lists/*

ENV GHCUP_INSTALL_BASE_PREFIX=/opt
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_GHC_VERSION=9.4.8 \
    BOOTSTRAP_HASKELL_CABAL_VERSION=3.10.3.0 \
    BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1 \
    sh
ENV PATH="/opt/.ghcup/bin:${PATH}"

ARG AWS_RDS_CA_BUNDLE_URL=https://truststore.pki.rds.amazonaws.com/global/global-bundle.pem
ARG AWS_RDS_CA_BUNDLE_SHA256=e5bb2084ccf45087bda1c9bffdea0eb15ee67f0b91646106e466714f9de3c7e3
RUN curl --proto '=https' --tlsv1.2 --fail --silent --show-error --retry 3 \
      --output /tmp/aws-rds-global-bundle.pem "$AWS_RDS_CA_BUNDLE_URL" && \
    echo "$AWS_RDS_CA_BUNDLE_SHA256  /tmp/aws-rds-global-bundle.pem" | sha256sum --check --strict

WORKDIR /build

COPY apps/backend/plether-api.cabal apps/backend/cabal.project apps/backend/cabal.project.freeze ./
RUN cabal update && cabal build --only-dependencies

COPY apps/backend/app/ app/
COPY apps/backend/src/ src/
COPY config/perps/arbitrum-sepolia-v2.json config/perps/arbitrum-sepolia-v2.json
COPY apps/backend/test/ test/
RUN cabal build exe:plether-api exe:plether-basket-worker exe:plether-keeper exe:plether-liquidation-worker exe:plether-perps-indexer exe:plether-provider-preflight exe:plether-insights-worker exe:plether-insights-admin exe:plether-candle-admin && \
    cp "$(cabal list-bin plether-api)" /build/plether-api-bin && \
    cp "$(cabal list-bin plether-basket-worker)" /build/plether-basket-worker-bin && \
    cp "$(cabal list-bin plether-keeper)" /build/plether-keeper-bin && \
    cp "$(cabal list-bin plether-liquidation-worker)" /build/plether-liquidation-worker-bin && \
    cp "$(cabal list-bin plether-perps-indexer)" /build/plether-perps-indexer-bin && \
    cp "$(cabal list-bin plether-provider-preflight)" /build/plether-provider-preflight-bin && \
    cp "$(cabal list-bin plether-insights-worker)" /build/plether-insights-worker-bin && \
    cp "$(cabal list-bin plether-insights-admin)" /build/plether-insights-admin-bin && \
    cp "$(cabal list-bin plether-candle-admin)" /build/plether-candle-admin-bin

FROM node:22-bookworm-slim AS oracle-worker

WORKDIR /oracle
COPY apps/backend/oracle-worker/package.json apps/backend/oracle-worker/package-lock.json ./
RUN npm ci --omit=dev --ignore-scripts --no-audit --no-fund
COPY apps/frontend/scripts/perps-oracle-worker.mjs scripts/perps-oracle-worker.mjs

FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq5 libsecp256k1-1 libstdc++6 ca-certificates postgresql-client && \
    rm -rf /var/lib/apt/lists/*

COPY --from=builder /build/plether-api-bin /usr/local/bin/plether-api
COPY --from=builder /build/plether-basket-worker-bin /usr/local/bin/plether-basket-worker
COPY --from=builder /build/plether-keeper-bin /usr/local/bin/plether-keeper
COPY --from=builder /build/plether-liquidation-worker-bin /usr/local/bin/plether-liquidation-worker
COPY --from=builder /build/plether-perps-indexer-bin /usr/local/bin/plether-perps-indexer
COPY --from=builder /build/plether-provider-preflight-bin /usr/local/bin/plether-provider-preflight
COPY --from=builder /build/plether-insights-worker-bin /usr/local/bin/plether-insights-worker
COPY --from=builder /build/plether-insights-admin-bin /usr/local/bin/plether-insights-admin
COPY --from=builder /build/plether-candle-admin-bin /usr/local/bin/plether-candle-admin
COPY --from=builder --chmod=0644 /tmp/aws-rds-global-bundle.pem /etc/ssl/certs/aws-rds-global-bundle.pem
COPY --from=oracle-worker /usr/local/bin/node /usr/local/bin/node
COPY --from=oracle-worker /oracle /app/oracle
COPY apps/frontend/src/contracts/addresses.sepolia.json /app/config/addresses.sepolia.json
COPY apps/frontend/src/contracts/addresses.arbitrum-sepolia.json /app/config/addresses.arbitrum-sepolia.json
COPY apps/frontend/src/contracts/addresses.mainnet.json /app/config/addresses.mainnet.json
COPY apps/backend/schema.sql /app/schema.sql

WORKDIR /app
EXPOSE 3001

CMD ["plether-api", "+RTS", "-N", "-RTS"]
