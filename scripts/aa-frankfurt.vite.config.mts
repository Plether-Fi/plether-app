import { defineConfig, loadEnv, mergeConfig, type ResolvedConfig, type ViteDevServer } from '../apps/frontend/node_modules/vite/dist/node/index.js'
import fs from 'node:fs'
import { fileURLToPath } from 'node:url'
import baseConfig from '../apps/frontend/vite.config'
import { frankfurtLocalProfile, localManifestPath } from './aa-frankfurt-local'

// Separate ESM entry point: normal development/builds and the published manifest
// are unchanged. Never use this config for a build, preview or public server.
export default defineConfig(async context => {
  if (context.command !== 'serve' || context.isPreview) throw new Error('Frankfurt config is local development only')
  const frontendRoot = fileURLToPath(new URL('../apps/frontend/', import.meta.url))
  const env = { ...loadEnv(context.mode, frontendRoot, ''), ...process.env }
  const outputPath = env.AA_FRANKFURT_OUTPUTS_FILE
  if (!outputPath) throw new Error('Set AA_FRANKFURT_OUTPUTS_FILE to reviewed terraform output -json')
  const outputs = JSON.parse(fs.readFileSync(outputPath, 'utf8'))
  const source = JSON.parse(fs.readFileSync(new URL('../apps/frontend/public/perps-aa-manifest.json', import.meta.url), 'utf8'))
  const profile = frankfurtLocalProfile(env, outputs, source)
  // Base config reads this server-side value; it is not an origin credential.
  process.env.VITE_API_PROXY_TARGET = profile.target
  const base = typeof baseConfig === 'function' ? await baseConfig(context) : await baseConfig
  return mergeConfig(base, {
    root: frontendRoot,
    define: {
      'import.meta.env.VITE_API_URL': 'undefined',
      'import.meta.env.VITE_PERPS_AA_MANIFEST_URL': JSON.stringify(localManifestPath),
      'import.meta.env.VITE_DEFAULT_CHAIN_ID': JSON.stringify('421614'),
    },
    plugins: [{
      name: 'frankfurt-local-only',
      configResolved(config: ResolvedConfig) {
        if (config.server.host !== '127.0.0.1' || config.server.port !== 5173
          || !config.server.strictPort || config.server.https) {
          throw new Error('Frankfurt must bind only http://127.0.0.1:5173; no --host/--port overrides')
        }
      },
      configureServer(server: ViteDevServer) {
        server.middlewares.use((request, response, next) => {
          const allowed = ['http://127.0.0.1:5173', 'http://localhost:5173']
          if (!['127.0.0.1:5173', 'localhost:5173'].includes(request.headers.host ?? '')
            || (request.headers.origin && !allowed.includes(request.headers.origin))) {
            response.statusCode = 403
            response.end('Local Frankfurt origin required')
            return
          }
          if (request.url?.split('?')[0] === localManifestPath) {
            response.setHeader('Content-Type', 'application/json')
            response.setHeader('Cache-Control', 'no-store')
            response.end(JSON.stringify(profile.manifest))
            return
          }
          if (profile.apiOnly && (!['GET', 'HEAD'].includes(request.method ?? 'GET')
            || /^\/api\/(?:perps\/v1\/)?aa(?:\/|\?|$)/.test(request.url ?? ''))) {
            response.statusCode = 403
            response.end('API-only mode: mutations and AA requests are disabled')
            return
          }
          next()
        })
      },
    }],
    server: { host: '127.0.0.1', port: 5173, strictPort: true, cors: false },
  })
})
