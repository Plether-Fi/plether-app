// Operator-only Chrome DevTools Snippet. Install before the transaction quote,
// then run copy(window.__pletherKernelGasCapture.take('eth'|'sponsored')) in Console.
// It returns the original fetch promise unchanged and never retains the raw reply.
;(() => {
  const name = '__pletherKernelGasCapture'
  const gatewayPath = '/api/perps/v1/aa/kernel/rpc'
  if (Object.prototype.hasOwnProperty.call(window, name)) {
    throw new Error('Restore the existing Kernel gas observer before installing another')
  }
  const originalFetch = window.fetch
  const captured = new Map()
  const operationFields = [
    'sender', 'nonce', 'factory', 'factoryData', 'callData', 'callGasLimit',
    'verificationGasLimit', 'preVerificationGas', 'maxFeePerGas',
    'maxPriorityFeePerGas', 'paymaster', 'paymasterVerificationGasLimit',
    'paymasterPostOpGasLimit', 'paymasterData',
  ]

  function sanitized(raw) {
    if (!raw || typeof raw !== 'object' || !['eth', 'sponsored'].includes(raw.mode) ||
        typeof raw.entryPoint !== 'string' || typeof raw.maxNetworkGasWei !== 'string' ||
        !Number.isSafeInteger(raw.expiresAt) ||
        !raw.operation || typeof raw.operation !== 'object' ||
        (raw.operation.signature !== undefined && raw.operation.signature !== '0x')) return null
    const operation = {}
    for (const key of operationFields) {
      if (Object.prototype.hasOwnProperty.call(raw.operation, key)) {
        if (typeof raw.operation[key] !== 'string') return null
        operation[key] = raw.operation[key]
      }
    }
    operation.signature = '0x'
    return JSON.stringify({ mode: raw.mode, entryPoint: raw.entryPoint, operation,
      maxNetworkGasWei: raw.maxNetworkGasWei, expiresAt: raw.expiresAt })
  }

  const observedFetch = function (...args) {
    const pending = Reflect.apply(originalFetch, window, args)
    try {
      const [input, init] = args
      if (input !== gatewayPath || typeof init?.body !== 'string' ||
          JSON.parse(init.body).method !== 'plether_prepareUserOperation') return pending
      void pending.then(response => {
        if (!response.ok) return
        return response.clone().json().then(body => {
          const safe = sanitized(body?.result)
          if (!safe) return
          const { mode, expiresAt } = JSON.parse(safe)
          captured.set(mode, safe)
          console.info('Kernel gas preparation captured:', mode, 'expires at', expiresAt)
        })
      }).catch(() => {})
    } catch { /* Preserve the page's original request and response. */ }
    return pending
  }
  window.fetch = observedFetch
  window[name] = Object.freeze({
    take(mode) {
      if (mode !== 'eth' && mode !== 'sponsored') throw new Error('Choose eth or sponsored')
      const safe = captured.get(mode)
      if (!safe) throw new Error(`No ${mode} preparation captured; get a fresh quote`)
      captured.delete(mode)
      return safe
    },
    restore() {
      if (window.fetch === observedFetch) window.fetch = originalFetch
      captured.clear()
      delete window[name]
    },
  })
  console.info('Kernel gas preparation observer armed')
})()
