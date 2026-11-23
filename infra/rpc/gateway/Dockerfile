FROM node:26.13.1-alpine@sha256:6bd9ab43c0ec0190ca7b2d193906cb8b7edf6e7f4afa43ddeccd6452710bffb5
WORKDIR /app
COPY --chown=node:node infra/rpc/gateway/server.mjs ./server.mjs
COPY --chown=node:node config/rpc-log-range-policy.json /config/rpc-log-range-policy.json
USER node
EXPOSE 3000
CMD ["node", "server.mjs"]
