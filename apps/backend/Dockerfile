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

WORKDIR /build

COPY apps/backend/plether-api.cabal apps/backend/cabal.project apps/backend/cabal.project.freeze ./
RUN cabal update && cabal build --only-dependencies

COPY apps/backend/app/ app/
COPY apps/backend/src/ src/
COPY apps/backend/test/ test/
RUN cabal build exe:plether-api exe:plether-basket-worker exe:plether-keeper exe:plether-perps-indexer && \
    cp "$(cabal list-bin plether-api)" /build/plether-api-bin && \
    cp "$(cabal list-bin plether-basket-worker)" /build/plether-basket-worker-bin && \
    cp "$(cabal list-bin plether-keeper)" /build/plether-keeper-bin && \
    cp "$(cabal list-bin plether-perps-indexer)" /build/plether-perps-indexer-bin

FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq5 libsecp256k1-1 ca-certificates && \
    rm -rf /var/lib/apt/lists/*

COPY --from=builder /build/plether-api-bin /usr/local/bin/plether-api
COPY --from=builder /build/plether-basket-worker-bin /usr/local/bin/plether-basket-worker
COPY --from=builder /build/plether-keeper-bin /usr/local/bin/plether-keeper
COPY --from=builder /build/plether-perps-indexer-bin /usr/local/bin/plether-perps-indexer
COPY apps/frontend/src/contracts/addresses.sepolia.json /app/config/addresses.sepolia.json
COPY apps/frontend/src/contracts/addresses.mainnet.json /app/config/addresses.mainnet.json

WORKDIR /app
EXPOSE 3001

CMD ["plether-api", "+RTS", "-N", "-RTS"]
