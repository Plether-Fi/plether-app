FROM docker.io/amazon/aws-for-fluent-bit:3.5.10@sha256:b1491753f6bf4c2d0c302abd33911ed2869f2275151d05e0549264938211f2c4

COPY apps/backend/otel-log-router/otel-enrichment.conf /fluent-bit/etc/otel-enrichment.conf
COPY apps/backend/otel-log-router/plether-parsers.conf /fluent-bit/etc/plether-parsers.conf
COPY apps/backend/otel-log-router/posthog-projection.lua /fluent-bit/etc/posthog-projection.lua
COPY apps/backend/otel-log-router/severity.lua /fluent-bit/etc/severity.lua
COPY apps/backend/otel-log-router/entrypoint.sh /entrypoint.sh

RUN chmod 0755 /entrypoint.sh
