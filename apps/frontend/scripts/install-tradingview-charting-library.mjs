import { access, cp, mkdir, rm } from 'node:fs/promises'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

const scriptDirectory = path.dirname(fileURLToPath(import.meta.url))
const frontendDirectory = path.resolve(scriptDirectory, '..')
const targetDirectory = path.join(frontendDirectory, 'public', 'charting_library')
const sourceArgument = process.argv[2] ?? process.env.TRADINGVIEW_CHARTING_LIBRARY_PATH

if (!sourceArgument) {
  console.error(
    'Usage: npm run tradingview:install -- /absolute/path/to/charting_library/repository'
  )
  process.exit(1)
}

const sourceRoot = path.resolve(sourceArgument)
const runtimeEntries = [
  'charting_library.standalone.js',
  'bundles',
]

async function containsRuntimeAssets(directory) {
  try {
    await Promise.all(runtimeEntries.map((entry) => access(path.join(directory, entry))))
    return true
  } catch {
    return false
  }
}

const sourceDirectory = await containsRuntimeAssets(sourceRoot)
  ? sourceRoot
  : path.join(sourceRoot, 'charting_library')

for (const entry of runtimeEntries) {
  try {
    await access(path.join(sourceDirectory, entry))
  } catch {
    console.error(`TradingView runtime entry is missing: ${path.join(sourceDirectory, entry)}`)
    process.exit(1)
  }
}

if (!targetDirectory.startsWith(`${frontendDirectory}${path.sep}`)) {
  throw new Error('Refusing to replace TradingView assets outside the frontend directory')
}
if (sourceDirectory === targetDirectory) {
  throw new Error('TradingView source and destination directories must be different')
}

await rm(targetDirectory, { recursive: true, force: true })
await mkdir(targetDirectory, { recursive: true })
for (const entry of runtimeEntries) {
  await cp(path.join(sourceDirectory, entry), path.join(targetDirectory, entry), {
    recursive: true,
    force: true,
  })
}

console.log(`Installed TradingView Advanced Charts assets in ${targetDirectory}`)
console.log(`Copied runtime entries only: ${runtimeEntries.join(', ')}`)
console.log('The destination is ignored by Git and must never be committed.')
