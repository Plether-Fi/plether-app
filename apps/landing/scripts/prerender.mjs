import { mkdir, readFile, rm, writeFile } from 'node:fs/promises'
import path from 'node:path'
import { fileURLToPath, pathToFileURL } from 'node:url'

const __dirname = path.dirname(fileURLToPath(import.meta.url))
const appRoot = path.resolve(__dirname, '..')
const distDir = path.join(appRoot, 'dist')
const ssrDir = path.join(appRoot, 'dist-ssr')

const ROUTES = [
  { pathname: '/', outputFile: 'index.html' },
  { pathname: '/manifesto', outputFile: path.join('manifesto', 'index.html') },
  { pathname: '/media-kit', outputFile: path.join('media-kit', 'index.html') },
  { pathname: '/404', outputFile: '404.html' },
]

function escapeHtml(value) {
  return value
    .replaceAll('&', '&amp;')
    .replaceAll('<', '&lt;')
    .replaceAll('>', '&gt;')
    .replaceAll('"', '&quot;')
}

function applyRouteMeta(template, meta) {
  const escapedTitle = escapeHtml(meta.title)
  const escapedDescription = escapeHtml(meta.description)
  const escapedCanonical = escapeHtml(meta.canonicalUrl)

  let html = template
    .replace(/<title>[^<]*<\/title>/, `<title>${escapedTitle}</title>`)
    .replace(
      /<meta name="description" content="[^"]*"\s*\/?>/,
      `<meta name="description" content="${escapedDescription}" />`,
    )
    .replace(
      /<link rel="canonical" href="[^"]*"\s*\/?>/,
      `<link rel="canonical" href="${escapedCanonical}" />`,
    )
    .replace(
      /<meta property="og:url" content="[^"]*"\s*\/?>/,
      `<meta property="og:url" content="${escapedCanonical}" />`,
    )
    .replace(
      /<meta property="og:title" content="[^"]*"\s*\/?>/,
      `<meta property="og:title" content="${escapedTitle}" />`,
    )
    .replace(
      /<meta property="og:description" content="[^"]*"\s*\/?>/,
      `<meta property="og:description" content="${escapedDescription}" />`,
    )
    .replace(
      /<meta name="twitter:title" content="[^"]*"\s*\/?>/,
      `<meta name="twitter:title" content="${escapedTitle}" />`,
    )
    .replace(
      /<meta name="twitter:description" content="[^"]*"\s*\/?>/,
      `<meta name="twitter:description" content="${escapedDescription}" />`,
    )

  if (meta.noindex) {
    html = html.replace('</head>', '    <meta name="robots" content="noindex" />\n  </head>')
  }

  return html
}

async function main() {
  const templatePath = path.join(distDir, 'index.html')
  const template = await readFile(templatePath, 'utf8')
  const serverEntryUrl = pathToFileURL(path.join(ssrDir, 'entry-server.js')).href
  const { render } = await import(serverEntryUrl)

  for (const route of ROUTES) {
    const { html: appHtml, meta } = render(route.pathname)
    const rendered = applyRouteMeta(template, meta).replace(
      '<div id="root"></div>',
      `<div id="root">${appHtml}</div>`,
    )

    const targetPath = path.join(distDir, route.outputFile)
    await mkdir(path.dirname(targetPath), { recursive: true })
    await writeFile(targetPath, rendered, 'utf8')
    console.log(`Prerendered ${route.pathname} -> dist/${route.outputFile}`)
  }

  await rm(ssrDir, { recursive: true, force: true })
}

main().catch((error) => {
  console.error(error)
  process.exitCode = 1
})
