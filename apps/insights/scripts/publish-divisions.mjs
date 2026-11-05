import { mkdir, readFile, rename, writeFile } from 'node:fs/promises'
import { resolve, join } from 'node:path'
import { publishWeek } from '../src/divisions/engine.ts'
import { isPublication, isWeeklyInput } from '../src/divisions/schema.ts'

const [inputPath, outputDirectory, previousPath, ...extra] = process.argv.slice(2)
if (!inputPath || !outputDirectory || extra.length) {
  throw new Error('Usage: node scripts/publish-divisions.mjs INPUT.json OUTPUT_DIRECTORY [PREVIOUS_PUBLICATION.json]. Requires Node 22.18+.')
}

async function optionalRead(path) {
  try { return await readFile(path, 'utf8') } catch (error) {
    if (error.code === 'ENOENT') return null
    throw error
  }
}

const input = JSON.parse(await readFile(resolve(inputPath), 'utf8'))
if (!isWeeklyInput(input)) throw new Error('Invalid weekly snapshot input')
if (Date.parse(input.publishedAt) > Date.now()) throw new Error('Cannot publish a future result')
const previous = previousPath ? JSON.parse(await readFile(resolve(previousPath), 'utf8')) : null
if (previous && !isPublication(previous)) throw new Error('Invalid previous publication')
const publication = publishWeek(input, previous)
if (!isPublication(publication)) throw new Error('Invalid generated publication')
const output = resolve(outputDirectory)
await mkdir(join(output, 'weeks'), { recursive: true })
const serialized = JSON.stringify(publication, null, 2) + '\n'
const archivePath = join(output, 'weeks', publication.week.startsAt.slice(0, 10) + '.json')
const existing = await optionalRead(archivePath)
if (existing !== null && existing !== serialized) throw new Error('This week is already published with different results; refusing to rewrite history')
const currentPath = join(output, 'current.json')
const currentText = await optionalRead(currentPath)
if (currentText !== null && currentText !== serialized) {
  const current = JSON.parse(currentText)
  if (!isPublication(current) || !previous || JSON.stringify(current) !== JSON.stringify(previous)) {
    throw new Error('Current publication does not match the supplied previous week; refusing to overwrite')
  }
}
// Exclusive archive creation makes retries safe and prevents replacing history.
if (existing === null) await writeFile(archivePath, serialized, { flag: 'wx' })
const temporaryPath = join(output, `current.${process.pid}.tmp`)
await writeFile(temporaryPath, serialized, { flag: 'wx' })
await rename(temporaryPath, currentPath)
console.log(`Published ${publication.week.startsAt}: ${publication.standings.length} traders to ${currentPath}`)
