# Preserve the reviewed Alto application and Node runtime. The candidate image
# still requires its own complete current scan before deployment.
FROM ghcr.io/pimlicolabs/alto:v1.2.7@sha256:28cee87ea6b58ba10a37273e58602b50321516c36a81d0c35d50526d1f06995d
COPY --chown=1000:1000 rpc-auth.mjs /app/plether-rpc-auth.mjs
