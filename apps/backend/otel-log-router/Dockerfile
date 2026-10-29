FROM docker.io/amazon/aws-for-fluent-bit:3.0.0@sha256:8d37b4b024fab89cffa497c06d23119044578127c46fae9d37ca3b640d444cdb

COPY apps/backend/otel-log-router/otel-enrichment.conf /fluent-bit/etc/otel-enrichment.conf
COPY apps/backend/otel-log-router/plether-parsers.conf /fluent-bit/etc/plether-parsers.conf
COPY apps/backend/otel-log-router/posthog-projection.lua /fluent-bit/etc/posthog-projection.lua
COPY apps/backend/otel-log-router/severity.lua /fluent-bit/etc/severity.lua
COPY apps/backend/otel-log-router/entrypoint.sh /entrypoint.sh

RUN chmod 0755 /entrypoint.sh
