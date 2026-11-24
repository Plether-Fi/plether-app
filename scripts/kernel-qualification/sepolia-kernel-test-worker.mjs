const host = 'app.sepolia.plether.com'
const mount = '/kernel-test/'
const rpcPath = '/api/perps/v1/aa/kernel/rpc'
const upstream = `https://plether-kernel-canary.pages.dev${rpcPath}`

async function proxyKernelRpc(request, origin) {
  if (request.method !== 'POST') return new Response(null, { status: 405 })
  if (request.headers.get('Origin') !== origin) return new Response(null, { status: 403 })

  // The isolated Pages Worker authenticates to the gateway. This Worker has
  // no origin credential and forwards only the Kernel JSON-RPC payload.
  const result = await fetch(upstream, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: request.body,
    redirect: 'manual',
  })
  if (result.status >= 300 && result.status < 400) return new Response(null, { status: 502 })
  const headers = new Headers(result.headers)
  headers.delete('Set-Cookie')
  headers.set('Cache-Control', 'no-store')
  return new Response(result.body, { status: result.status, headers })
}

export default {
  async fetch(request, env) {
    const url = new URL(request.url)
    if (url.protocol !== 'https:' || url.host !== host || !url.pathname.startsWith(mount)) {
      return new Response(null, { status: 404 })
    }
    url.pathname = url.pathname.slice(mount.length - 1)
    if (url.pathname === rpcPath) return proxyKernelRpc(request, url.origin)
    if (url.pathname.startsWith('/api/')) return new Response(null, { status: 404 })
    if (request.method !== 'GET' && request.method !== 'HEAD') return new Response(null, { status: 405 })

    const asset = await env.ASSETS.fetch(new Request(url, request))
    if (asset.status !== 404 || !request.headers.get('Accept')?.includes('text/html')) return asset
    return env.ASSETS.fetch(new Request(new URL('/', url), request))
  },
}
