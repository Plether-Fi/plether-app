FROM docker.io/amazon/aws-for-fluent-bit:3.5.9@sha256:459e631c3bb563c55ec4e36b5e0ada9757ccd1e549a51d41122f7693281b076e

COPY apps/backend/otel-log-router/otel-enrichment.conf /fluent-bit/etc/otel-enrichment.conf
COPY apps/backend/otel-log-router/plether-parsers.conf /fluent-bit/etc/plether-parsers.conf
COPY apps/backend/otel-log-router/posthog-projection.lua /fluent-bit/etc/posthog-projection.lua
COPY apps/backend/otel-log-router/severity.lua /fluent-bit/etc/severity.lua
COPY apps/backend/otel-log-router/entrypoint.sh /entrypoint.sh

RUN chmod 0755 /entrypoint.sh
