import { access, cp, rm } from 'node:fs/promises'
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
const requiredFiles = [
  'charting_library.standalone.js',
  'charting_library.d.ts',
  'bundles',
]

async function containsRuntimeAssets(directory) {
  try {
    await Promise.all(requiredFiles.map((requiredFile) => access(path.join(directory, requiredFile))))
    return true
  } catch {
    return false
  }
}

const sourceDirectory = await containsRuntimeAssets(sourceRoot)
  ? sourceRoot
  : path.join(sourceRoot, 'charting_library')

for (const requiredFile of requiredFiles) {
  try {
    await access(path.join(sourceDirectory, requiredFile))
  } catch {
    console.error(`TradingView library file is missing: ${path.join(sourceDirectory, requiredFile)}`)
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
await cp(sourceDirectory, targetDirectory, { recursive: true, force: true })

console.log(`Installed TradingView Advanced Charts assets in ${targetDirectory}`)
console.log('The destination is ignored by Git and must never be committed.')
