FROM node:22-alpine@sha256:1314fb30ec37592d5845e5a160d522c0e680972aa534549940b8d73bde88481a
RUN apk upgrade --no-cache
WORKDIR /app
COPY --chown=node:node infra/rpc/gateway/server.mjs ./server.mjs
COPY --chown=node:node config/rpc-log-range-policy.json /config/rpc-log-range-policy.json
USER node
EXPOSE 3000
CMD ["node", "server.mjs"]
