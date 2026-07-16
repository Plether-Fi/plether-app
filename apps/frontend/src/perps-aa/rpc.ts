export interface JsonRpcErrorData {
  reason?: string
  retryable?: boolean
  callIndex?: number
  status?: string
  replacementUserOperationHash?: string
  [key: string]: unknown
}

interface JsonRpcSuccess<T> {
  jsonrpc: '2.0'
  id: number
  result: T
}

interface JsonRpcFailure {
  jsonrpc: '2.0'
  id: number
  error: {
    code: number
    message: string
    data?: unknown
  }
}

export class JsonRpcRequestError extends Error {
  readonly method: string
  readonly rpcCode: number
  readonly data?: JsonRpcErrorData

  constructor(input: {
    method: string
    rpcCode: number
    message: string
    data?: JsonRpcErrorData
  }) {
    super(input.message)
    this.name = 'JsonRpcRequestError'
    this.method = input.method
    this.rpcCode = input.rpcCode
    this.data = input.data
  }
}

export class JsonRpcTransportError extends Error {
  readonly method: string
  readonly retryable = true
  override readonly cause: unknown

  constructor(method: string, message: string, cause?: unknown) {
    super(message)
    this.name = 'JsonRpcTransportError'
    this.method = method
    this.cause = cause
  }
}

export class JsonRpcTimeoutError extends JsonRpcTransportError {
  constructor(method: string, timeoutMs: number) {
    super(method, `${method} timed out after ${timeoutMs.toString()}ms`)
    this.name = 'JsonRpcTimeoutError'
  }
}

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null
}

function asErrorData(value: unknown): JsonRpcErrorData | undefined {
  return isRecord(value) ? value as JsonRpcErrorData : undefined
}

function createRequestSignal(
  method: string,
  timeoutMs: number,
  externalSignal?: AbortSignal
): {
  signal: AbortSignal
  cleanup: () => void
  didTimeout: () => boolean
} {
  const controller = new AbortController()
  let timedOut = false

  const timeout = globalThis.setTimeout(() => {
    timedOut = true
    controller.abort(new JsonRpcTimeoutError(method, timeoutMs))
  }, timeoutMs)

  const abortFromExternal = () => {
    controller.abort(externalSignal?.reason)
  }
  if (externalSignal?.aborted) {
    abortFromExternal()
  } else {
    externalSignal?.addEventListener('abort', abortFromExternal, { once: true })
  }

  return {
    signal: controller.signal,
    cleanup: () => {
      globalThis.clearTimeout(timeout)
      externalSignal?.removeEventListener('abort', abortFromExternal)
    },
    didTimeout: () => timedOut,
  }
}

let nextRequestId = 1

export async function jsonRpcRequest<T>(input: {
  url: string
  method: string
  params: readonly unknown[]
  timeoutMs?: number
  signal?: AbortSignal
  fetcher?: typeof fetch
}): Promise<T> {
  const timeoutMs = input.timeoutMs ?? 15_000
  const fetcher = input.fetcher ?? globalThis.fetch
  const requestId = nextRequestId
  nextRequestId += 1
  const requestSignal = createRequestSignal(input.method, timeoutMs, input.signal)

  try {
    const response = await fetcher(input.url, {
      method: 'POST',
      headers: {
        'content-type': 'application/json',
      },
      body: JSON.stringify({
        jsonrpc: '2.0',
        id: requestId,
        method: input.method,
        params: input.params,
      }),
      cache: 'no-store',
      signal: requestSignal.signal,
    })

    if (!response.ok) {
      throw new JsonRpcTransportError(
        input.method,
        `${input.method} returned HTTP ${response.status.toString()}`
      )
    }

    const payload = await response.json() as JsonRpcSuccess<T> | JsonRpcFailure
    if ('error' in payload) {
      throw new JsonRpcRequestError({
        method: input.method,
        rpcCode: payload.error.code,
        message: payload.error.message,
        data: asErrorData(payload.error.data),
      })
    }

    return payload.result
  } catch (error) {
    if (requestSignal.didTimeout()) {
      throw new JsonRpcTimeoutError(input.method, timeoutMs)
    }
    if (
      error instanceof JsonRpcRequestError ||
      error instanceof JsonRpcTransportError
    ) {
      throw error
    }
    if (input.signal?.aborted) {
      throw error
    }
    throw new JsonRpcTransportError(
      input.method,
      `${input.method} network request failed`,
      error
    )
  } finally {
    requestSignal.cleanup()
  }
}
