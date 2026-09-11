import test from 'node:test'
import assert from 'node:assert/strict'
import { mkdtempSync, writeFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { EventEmitter } from 'node:events'
import { createServer as createHttpServer, request as httpRequest } from 'node:http'
import { createServer as createViteServer, loadConfigFromFile } from '../apps/frontend/node_modules/vite/dist/node/index.js'
import { outputs } from './aa-frankfurt-fixture.mjs'

test('real Vite config selects native localhost manifest and authenticates only AA requests', async () => {
  const directory = mkdtempSync(join(tmpdir(), 'plether-frankfurt-vite-'))
  const original = { ...process.env }
  try {
    const file = join(directory, 'outputs.json')
    writeFileSync(file, JSON.stringify(outputs))
    Object.assign(process.env, {
      AA_FRANKFURT_OUTPUTS_FILE: file,
      AA_PROXY_ORIGIN_TOKEN: '0123456789abcdef'.repeat(4),
      AA_FRANKFURT_PAYMASTER_ADDRESS: `0x${'12'.repeat(20)}`,
      VITE_API_URL: '', VITE_API_PROXY_TARGET: '', VITE_API_PROXY_PRESERVE_PATH: '',
    })
    const configPath = new URL('./aa-frankfurt.vite.config.mts', import.meta.url).pathname
    const loaded = await loadConfigFromFile({ command: 'serve', mode: 'test' }, configPath)
    assert(loaded)
    const config = loaded.config
    assert.equal(config.server.host, '127.0.0.1')
    assert.equal(config.server.port, 5173)
    assert.equal(config.server.strictPort, true)
    assert.equal(config.define['import.meta.env.VITE_PERPS_AA_MANIFEST_URL'], '"/perps-aa-manifest.frankfurt.json"')
    assert(!JSON.stringify(config.define).includes(process.env.AA_PROXY_ORIGIN_TOKEN))
    const proxy = config.server.proxy['/api/perps/v1']
    assert.equal(proxy.target, 'http://127.0.0.1:18081')
    assert.equal(proxy.rewrite('/api/perps/v1/aa/rpc'), '/api/aa/rpc')
    const events = new EventEmitter()
    proxy.configure(events)
    for (const url of ['/api/aa/rpc', '/api/aa/rpc?check=1', '/api/health']) {
      const headers = new Map([['X-Plether-AA-Proxy-Token', 'browser-spoof']])
      const request = { setHeader: (key, value) => headers.set(key, value), removeHeader: key => headers.delete(key) }
      events.emit('proxyReq', request, { url, socket: { remoteAddress: '127.0.0.1' } })
      assert.equal(headers.get('X-Plether-AA-Proxy-Token'), url.startsWith('/api/aa/rpc') ? process.env.AA_PROXY_ORIGIN_TOKEN : undefined)
    }
    const plugin = config.plugins.find(plugin => plugin?.name === 'frankfurt-local-only')
    assert.throws(() => plugin.configResolved({ server: { host: '0.0.0.0', port: 5173, strictPort: true } }))
    let middleware
    plugin.configureServer({ middlewares: { use: value => { middleware = value } } })
    const response = { statusCode: 200, end() {}, setHeader() {} }
    middleware({ headers: { host: '127.0.0.1:5173', origin: 'https://evil.invalid' } }, response, () => assert.fail('foreign origin forwarded'))
    assert.equal(response.statusCode, 403)

    // Actual HTTP requests through Vite's proxy to the fixed tunnel port. No AWS,
    // real credentials, wallet, public endpoint or live frontend is contacted.
    const received = []
    const upstream = createHttpServer((req, res) => {
      received.push({ url: req.url, token: req.headers['x-plether-aa-proxy-token'] })
      res.setHeader('Content-Type', 'application/json')
      res.end('{"ok":true}')
    })
    let vite, local
    const listen = (server, port) => new Promise((resolve, reject) => {
      server.once('error', reject)
      server.listen(port, '127.0.0.1', resolve)
    })
    const close = server => new Promise(resolve => server?.listening ? server.close(resolve) : resolve())
    try {
      await listen(upstream, 18081)
      vite = await createViteServer({
        configFile: false, root: directory, plugins: [plugin], logLevel: 'silent',
        server: { ...config.server, middlewareMode: true, hmr: false },
      })
      local = createHttpServer(vite.middlewares)
      await listen(local, 0)
      const get = (path, headers = {}) => new Promise((resolve, reject) => {
        const req = httpRequest({ host: '127.0.0.1', port: local.address().port, path,
          headers: { host: '127.0.0.1:5173', ...headers } }, res => {
          let body = ''
          res.on('data', chunk => { body += chunk })
          res.on('end', () => resolve({ status: res.statusCode, body }))
        })
        req.on('error', reject)
        req.end()
      })
      assert.equal((await get('/api/perps/v1/aa/rpc', { 'X-Plether-AA-Proxy-Token': 'spoof' })).status, 200)
      assert.deepEqual(received.at(-1), { url: '/api/aa/rpc', token: process.env.AA_PROXY_ORIGIN_TOKEN })
      assert.equal((await get('/api/perps/v1/health', { 'X-Plether-AA-Proxy-Token': 'spoof' })).status, 200)
      assert.equal(received.at(-1).token, undefined)
      const count = received.length
      assert.equal((await get('/api/perps/v1/aa/rpc', { origin: 'https://evil.invalid' })).status, 403)
      assert.equal((await get('/api/perps/v1/aa/rpc', { host: 'evil.invalid' })).status, 403)
      assert.equal(received.length, count)
      const manifestResponse = await get('/perps-aa-manifest.frankfurt.json')
      assert.equal(manifestResponse.status, 200)
      assert(!manifestResponse.body.includes(process.env.AA_PROXY_ORIGIN_TOKEN))
      await close(upstream)
      assert.equal((await get('/api/perps/v1/aa/rpc')).status, 500, 'Closed tunnel must fail, not fall back')

      // Before the paymaster exists, localhost must still offer API reads but
      // reject AA and mutation requests without requiring an origin credential.
      process.env.AA_FRANKFURT_MODE = 'api-readonly'
      delete process.env.AA_PROXY_ORIGIN_TOKEN
      delete process.env.AA_FRANKFURT_PAYMASTER_ADDRESS
      const readOnly = await loadConfigFromFile({ command: 'serve', mode: 'test' }, configPath)
      const readOnlyPlugin = readOnly.config.plugins.find(p => p?.name === 'frankfurt-local-only')
      let readOnlyMiddleware
      readOnlyPlugin.configureServer({ middlewares: { use: fn => { readOnlyMiddleware = fn } } })
      for (const [method, url, blocked] of [['GET','/api/perps/v1/health',false],
        ['GET','/api/perps/v1/aa/pimlico',true], ['POST','/api/perps/v1/aa/rpc',true],
        ['POST','/api/perps/v1/anything',true]]) {
        let forwarded = false
        const response = {statusCode:200,end(){},setHeader(){}}
        readOnlyMiddleware({method,url,headers:{host:'127.0.0.1:5173'}}, response, () => {forwarded=true})
        assert.equal(forwarded,!blocked)
        if(blocked) assert.equal(response.statusCode,403)
      }
    } finally {
      await close(local)
      await vite?.close()
      await close(upstream)
    }
  } finally {
    for (const key of Object.keys(process.env)) if (!(key in original)) delete process.env[key]
    Object.assign(process.env, original)
    rmSync(directory, { recursive: true, force: true })
  }
})
