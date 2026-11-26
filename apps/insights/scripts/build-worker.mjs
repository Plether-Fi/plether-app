import { copyFileSync, mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { inflateSync } from 'node:zlib';
import { Resvg, initWasm } from '@resvg/resvg-wasm';
import { rolldown } from 'rolldown';
import { buildDefaultOgSvg } from '../src/og/ogCardSvg.js';

const __dirname = dirname(fileURLToPath(import.meta.url));
const rootDir = resolve(__dirname, '..');
const distDir = resolve(rootDir, 'dist');
const ogDir = resolve(distDir, 'og');

/**
 * Decode a WOFF 1.0 font buffer into a standard SFNT (.ttf/.otf) buffer so
 * resvg can parse Plether's bundled brand fonts without external font files.
 */
export function decodeWoff1ToSfnt(woff) {
  if (woff.length < 44 || woff.readUInt32BE(0) !== 0x774f4646) {
    throw new Error('Expected a valid WOFF 1.0 font buffer');
  }
  const flavor = woff.readUInt32BE(4);
  const numTables = woff.readUInt16BE(12);
  const totalSfntSize = woff.readUInt32BE(16);

  const tables = [];
  for (let i = 0; i < numTables; i += 1) {
    const entryOffset = 44 + i * 20;
    const tag = woff.subarray(entryOffset, entryOffset + 4);
    const offset = woff.readUInt32BE(entryOffset + 4);
    const compLength = woff.readUInt32BE(entryOffset + 8);
    const origLength = woff.readUInt32BE(entryOffset + 12);
    const origChecksum = woff.readUInt32BE(entryOffset + 16);
    const rawSlice = woff.subarray(offset, offset + compLength);
    const data = compLength < origLength ? inflateSync(rawSlice) : Buffer.from(rawSlice);
    if (data.length !== origLength) {
      throw new Error(`WOFF table length mismatch: expected ${origLength}, got ${data.length}`);
    }
    tables.push({ tag, data, origLength, origChecksum });
  }

  let searchRange = 16;
  let entrySelector = 0;
  while (searchRange * 2 <= numTables * 16) {
    searchRange *= 2;
    entrySelector += 1;
  }
  const rangeShift = numTables * 16 - searchRange;

  const out = Buffer.alloc(totalSfntSize);
  out.writeUInt32BE(flavor, 0);
  out.writeUInt16BE(numTables, 4);
  out.writeUInt16BE(searchRange, 6);
  out.writeUInt16BE(entrySelector, 8);
  out.writeUInt16BE(rangeShift, 10);

  let dataOffset = 12 + numTables * 16;
  for (let i = 0; i < tables.length; i += 1) {
    const t = tables[i];
    const dirOffset = 12 + i * 16;
    t.tag.copy(out, dirOffset);
    out.writeUInt32BE(t.origChecksum, dirOffset + 4);
    out.writeUInt32BE(dataOffset, dirOffset + 8);
    out.writeUInt32BE(t.origLength, dirOffset + 12);
    t.data.copy(out, dataOffset);
    dataOffset += (t.origLength + 3) & ~3;
  }

  return new Uint8Array(out.buffer, out.byteOffset, out.byteLength);
}

async function main() {
  mkdirSync(ogDir, { recursive: true });

  const fontSources = [
    {
      src: resolve(rootDir, 'node_modules/@fontsource/uncut-sans/files/uncut-sans-latin-500-normal.woff'),
      dest: resolve(ogDir, 'uncut-sans-500.ttf'),
    },
    {
      src: resolve(rootDir, 'node_modules/@fontsource/uncut-sans/files/uncut-sans-latin-700-normal.woff'),
      dest: resolve(ogDir, 'uncut-sans-700.ttf'),
    },
    {
      src: resolve(rootDir, 'node_modules/@fontsource/jetbrains-mono/files/jetbrains-mono-latin-700-normal.woff'),
      dest: resolve(ogDir, 'jetbrains-mono-700.ttf'),
    },
  ];

  const fontBuffers = [];
  for (const { src, dest } of fontSources) {
    const ttf = decodeWoff1ToSfnt(readFileSync(src));
    writeFileSync(dest, ttf);
    fontBuffers.push(ttf);
  }

  const wasmPath = resolve(rootDir, 'node_modules/@resvg/resvg-wasm/index_bg.wasm');
  await initWasm(readFileSync(wasmPath));

  const defaultSvg = buildDefaultOgSvg();
  const resvg = new Resvg(defaultSvg, {
    fitTo: { mode: 'width', value: 1200 },
    font: {
      fontBuffers,
      loadSystemFonts: false,
      defaultFontFamily: 'Uncut Sans',
      sansSerifFamily: 'Uncut Sans',
      monospaceFamily: 'JetBrains Mono',
    },
  });
  writeFileSync(resolve(ogDir, 'default.png'), resvg.render().asPng());

  // Bundle the Pages Advanced Mode worker so _worker.js is self-contained with
  // a single static ES import for ./resvg.wasm (compiled by Wrangler at deploy).
  const bundle = await rolldown({
    input: resolve(__dirname, 'worker-entry.mjs'),
    external: ['./resvg.wasm'],
    platform: 'neutral',
    resolve: {
      conditionNames: ['worker', 'browser', 'import', 'default'],
    },
  });
  await bundle.write({
    file: resolve(distDir, '_worker.js'),
    format: 'es',
    minify: true,
  });
  await bundle.close();

  copyFileSync(wasmPath, resolve(distDir, 'resvg.wasm'));
  writeFileSync(resolve(distDir, '.assetsignore'), '_worker.js\nresvg.wasm\n');
  console.log('Built dist/_worker.js, dist/resvg.wasm, and dist/og/default.png');
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  await main();
}
