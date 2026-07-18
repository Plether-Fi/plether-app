# ECS OTLP log router

This image extends AWS for Fluent Bit for use as the ECS FireLens container.
ECS supplies each application record with `container_name`; the custom filters
turn that into OpenTelemetry resource attributes, including `service.name`,
`service.version`, and `deployment.environment.name`.

The ECS-generated `opentelemetry` output sends OTLP/HTTP protobuf batches to
PostHog. The custom CloudWatch output preserves the existing operational copy.
Authentication is injected through an ECS log-driver `secretOptions` entry and
must never be written into this image or configuration file.
