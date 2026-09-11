# ECS OTLP log router

This image extends AWS for Fluent Bit for use as the ECS FireLens container.
ECS supplies each application record with `container_name`; the custom filters
turn that into OpenTelemetry resource attributes, including `service.name`,
`service.version`, and `deployment.environment.name`.

First-party services emit one JSON object per line. The parser promotes
`message`, `event`, explicit severity, and typed context fields into the OTLP
record. Plaintext lines remain supported and receive a best-effort severity,
which keeps runtime/library diagnostics visible without weakening the
structured schema used by the application services.

The ECS-generated `opentelemetry` output sends OTLP/HTTP protobuf batches to
PostHog. The custom CloudWatch output preserves the existing operational copy.
Authentication is injected through an ECS log-driver `secretOptions` entry and
must never be written into this image or configuration file.

## Sponsored-trading privacy boundary

`rewrite_tag` copies each ECS record to `cloudwatch.*` before applying
`posthog-projection.lua` to the FireLens/PostHog tag. CloudWatch retains the
operational fields used by existing alarms. PostHog receives only the explicit
projection: stable categories, bounded counters/durations, an optional random
attempt UUID and selected service/deployment resource attributes. The body is
the developer-owned event name, never the original message. Alto/Pino raw
messages, signatures, calldata, addresses, amounts, hashes and exceptions are
not exported. Unknown log formats retain severity but no original body.

Projection exceptions emit a minimal fixed diagnostic, rather than allowing
Fluent Bit's protected Lua fallback to forward the original record. Run
`lua posthog-projection.test.lua` here. A real FireLens routing/exporter-outage
qualification remains required before deploying this image.
