import assert from 'node:assert/strict'
import { existsSync, mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { dirname, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'
import { test } from 'node:test'
import { patchPrivyWalletDialog, preparePrivyDialogSource } from './patch-privy-wallet-dialog.mjs'

const root = resolve(dirname(fileURLToPath(import.meta.url)), '..')
const installed = ['frontend', 'insights'].map(app => resolve(root, `apps/${app}/node_modules/@privy-io/react-auth`))
  .find(path => existsSync(resolve(path, 'package.json')))
assert.ok(installed, 'Install frontend or Insights dependencies before testing the pinned Privy UI extension.')
const filenames = ['dist/esm/ui.mjs', 'dist/cjs/ui.js', 'dist/dts/ui.d.ts', 'dist/dts/ui.d.mts']
const source = filename => readFileSync(resolve(installed, filename), 'utf8')

function fixture(t) {
  const directory = mkdtempSync(resolve(tmpdir(), 'plether-privy-ui-'))
  t.after(() => { rmSync(directory, { recursive: true, force: true }) })
  writeFileSync(resolve(directory, 'package.json'), JSON.stringify({ name: '@privy-io/react-auth', version: '3.51.2' }))
  for (const filename of filenames) {
    mkdirSync(dirname(resolve(directory, filename)), { recursive: true })
    writeFileSync(resolve(directory, filename), source(filename))
  }
  return directory
}

test('extends the actual pinned ESM/CJS native picker and declarations with matching UI-only APIs', () => {
  const esm = preparePrivyDialogSource('dist/esm/ui.mjs', source('dist/esm/ui.mjs'))
  assert.match(esm, /customDescription:privyConnectionDescription\?\.\(u\)/)
  assert.match(esm, /function usePletherWalletPickerClose\(\)/)
  assert.match(esm, /await close\(\{shouldCallAuthOnSuccess:!1\}\)/)
  assert.match(esm, /accountActions:privyAccountActions/)
  assert.match(esm, /networks:h\}\),privyAccountActions\]/)
  const cjs = preparePrivyDialogSource('dist/cjs/ui.js', source('dist/cjs/ui.js'))
  assert.match(cjs, /customDescription:privyConnectionDescription\?\.\(x\)/)
  assert.match(cjs, /exports.usePletherWalletPickerClose=/)
  assert.match(cjs, /await close\(\{shouldCallAuthOnSuccess:!1\}\)/)
  assert.match(cjs, /accountActions:privyAccountActions/)
  assert.match(cjs, /networks:p\}\),privyAccountActions\]/)
  for (const filename of filenames.slice(2)) {
    const types = preparePrivyDialogSource(filename, source(filename))
    assert.match(types, /connectionDescription\?: \(close: \(\) => void\) => React.ReactNode/)
    assert.match(types, /declare const usePletherWalletPickerClose: \(\) => \(\) => Promise<void>/)
    assert.match(types, /accountActions\?: React.ReactNode/)
  }
})

test('upgrades the previous complete picker-only extension', () => {
  for (const filename of filenames) {
    const patched = preparePrivyDialogSource(filename, source(filename))
    const previous = patched
      .replace(',accountActions:privyAccountActions', '')
      .replace('t("div",{children:[e(Lr,{onClose:u,onLinkNewWallet:()=>c(!0),assets:p,networks:h}),privyAccountActions]})', 'e(Lr,{onClose:u,onLinkNewWallet:()=>c(!0),assets:p,networks:h})')
      .replace('e.jsxs("div",{children:[e.jsx(zt,{onClose:x,onLinkNewWallet:()=>h(!0),assets:u,networks:p}),privyAccountActions]})', 'e.jsx(zt,{onClose:x,onLinkNewWallet:()=>h(!0),assets:u,networks:p})')
      .replace('    /** Plether: settings inside the native connected-wallet dialog. */\n    accountActions?: React.ReactNode;\n', '')
      .replace(', accountActions }: WalletsDialogProps)', ' }: WalletsDialogProps)')
    assert.equal(preparePrivyDialogSource(filename, previous), patched)
  }
})

test('rejects a partially applied account-actions extension', () => {
  const patched = preparePrivyDialogSource('dist/esm/ui.mjs', source('dist/esm/ui.mjs'))
  assert.throws(() => preparePrivyDialogSource('dist/esm/ui.mjs', patched.replace(',accountActions:privyAccountActions', '')), /patch is incomplete/)
})

test('is idempotent across every runtime and declaration format', t => {
  const directory = fixture(t)
  patchPrivyWalletDialog(directory)
  const patched = filenames.map(filename => readFileSync(resolve(directory, filename), 'utf8'))
  assert.deepEqual(patchPrivyWalletDialog(directory), { version: '3.51.2', changedFiles: 0 })
  assert.deepEqual(filenames.map(filename => readFileSync(resolve(directory, filename), 'utf8')), patched)
})

test('rejects SDK version drift before changing files', t => {
  const directory = fixture(t)
  writeFileSync(resolve(directory, 'package.json'), JSON.stringify({ name: '@privy-io/react-auth', version: '3.52.0' }))
  assert.throws(() => patchPrivyWalletDialog(directory), /Expected @privy-io\/react-auth 3.51.2/)
  assert.equal(readFileSync(resolve(directory, filenames[0]), 'utf8'), source(filenames[0]))
})

test('rejects unrelated source drift without partially writing earlier files', t => {
  const directory = fixture(t)
  const last = resolve(directory, filenames.at(-1))
  writeFileSync(last, `${readFileSync(last, 'utf8')}\n// Unexpected upstream change\n`)
  const before = filenames.map(filename => readFileSync(resolve(directory, filename), 'utf8'))
  assert.throws(() => patchPrivyWalletDialog(directory), /checksum mismatch/)
  assert.deepEqual(filenames.map(filename => readFileSync(resolve(directory, filename), 'utf8')), before)
})

test('rejects a partially applied extension instead of silently repairing unknown state', () => {
  const patched = preparePrivyDialogSource('dist/esm/ui.mjs', source('dist/esm/ui.mjs'))
  const partial = patched.replace(',connectionDescription:privyConnectionDescription', '')
  assert.throws(() => preparePrivyDialogSource('dist/esm/ui.mjs', partial), /patch is incomplete/)
})
