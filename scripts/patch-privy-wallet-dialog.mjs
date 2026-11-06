import { createHash } from 'node:crypto'
import { readFileSync, writeFileSync } from 'node:fs'
import { resolve } from 'node:path'
import { fileURLToPath } from 'node:url'

// Privy 3.51.2 exposes no slot for its native wallet picker's description or
// public close action. Keep the native UI and add only those two UI surfaces.
// Verify the entire upstream files before writing; an SDK upgrade must update
// this reviewed patch instead of silently dropping the email-wallet action.
const VERSION = '3.51.2'
const closeHookName = 'usePletherWalletPickerClose'
const typePatches = [
  ['type WalletsDialogProps = {\n    networks?', 'type WalletsDialogProps = {\n    /** Plether: email action in the native external-wallet picker. */\n    connectionDescription?: (close: () => void) => React.ReactNode;\n    networks?'],
  ['declare const WalletsDialog: ({ networks, assets }: WalletsDialogProps)', 'declare const WalletsDialog: ({ networks, assets, connectionDescription }: WalletsDialogProps)'],
  ['export { UserPill, WalletsDialog, useUpdateEmail, useUpdatePhone };', `/** Plether: close the connection modal without completing authentication. */\ndeclare const ${closeHookName}: () => () => Promise<void>;\nexport { UserPill, WalletsDialog, useUpdateEmail, useUpdatePhone, ${closeHookName} };`],
]
const plans = {
  'dist/esm/ui.mjs': {
    sha256: '920daa38c49b14884813d304042176f01d946fbeddaebc0af6ae20d1ce5821be',
    patches: [
      ['const Ar=({networks:r,assets:i})=>', 'const Ar=({networks:r,assets:i,connectionDescription:privyConnectionDescription})=>'],
      ['e(_e,{onConnect:u,onBack:()=>c(!1),onClose:u})', 'e(_e,{onConnect:u,onBack:()=>c(!1),onClose:u,customDescription:privyConnectionDescription?.(u)})'],
      ['export{sr as UserPill,Ar as WalletsDialog};', `function ${closeHookName}(){const{closePrivyModal:close}=ne();return n.useCallback(async()=>{await close({shouldCallAuthOnSuccess:!1})},[close])}export{sr as UserPill,Ar as WalletsDialog,${closeHookName}};`],
    ],
  },
  'dist/cjs/ui.js': {
    sha256: 'af0213e71be2cb766b6e6965886481938f923039496f3cf413432f102d9d987f',
    patches: [
      ['exports.WalletsDialog=({networks:t,assets:n})=>', 'exports.WalletsDialog=({networks:t,assets:n,connectionDescription:privyConnectionDescription})=>'],
      ['e.jsx(k.i,{onConnect:x,onBack:()=>h(!1),onClose:x})', 'e.jsx(k.i,{onConnect:x,onBack:()=>h(!1),onClose:x,customDescription:privyConnectionDescription?.(x)})'],
      ['//# sourceMappingURL=ui.js.map', `exports.${closeHookName}=function ${closeHookName}(){const{closePrivyModal:close}=C.usePrivyInternal();return s.useCallback(async()=>{await close({shouldCallAuthOnSuccess:!1})},[close])};\n//# sourceMappingURL=ui.js.map`],
    ],
  },
  'dist/dts/ui.d.ts': { sha256: 'dedb955f5423bf11a296ead17826b804bd7e16dd5022684a3fdff9141d1a6151', patches: typePatches },
  'dist/dts/ui.d.mts': { sha256: '9f512183125d85207151d64bbea127e11a69edddd5e1c5c3e84eb3ffce7043fa', patches: typePatches },
}

function count(source, fragment) { return source.split(fragment).length - 1 }

export function preparePrivyDialogSource(filename, source) {
  const plan = plans[filename]
  if (!plan) throw new Error(`Unsupported Privy patch file: ${filename}`)
  let original = source
  const applied = []
  for (const [before, after] of plan.patches) {
    if (count(original, after) === 1) {
      original = original.replace(after, before)
      applied.push(true)
    } else if (count(original, before) === 1) {
      applied.push(false)
    } else {
      throw new Error(`Privy ${VERSION} UI patch source mismatch: ${filename}`)
    }
  }
  if (applied.some(Boolean) && !applied.every(Boolean)) {
    throw new Error(`Privy ${VERSION} UI patch is incomplete: ${filename}`)
  }
  if (createHash('sha256').update(original).digest('hex') !== plan.sha256) {
    throw new Error(`Privy ${VERSION} UI patch checksum mismatch: ${filename}`)
  }
  return plan.patches.reduce((result, [before, after]) => result.replace(before, after), original)
}

export function patchPrivyWalletDialog(packageDirectory = resolve(process.cwd(), 'node_modules/@privy-io/react-auth')) {
  const metadata = JSON.parse(readFileSync(resolve(packageDirectory, 'package.json'), 'utf8'))
  if (metadata.name !== '@privy-io/react-auth' || metadata.version !== VERSION) {
    throw new Error(`Expected @privy-io/react-auth ${VERSION}, found ${metadata.name} ${metadata.version}`)
  }
  // Prepare every format and declaration first so drift cannot leave a partial
  // installation. Repeated npm installs produce byte-identical patched files.
  const prepared = Object.keys(plans).map(filename => {
    const path = resolve(packageDirectory, filename)
    const original = readFileSync(path, 'utf8')
    return { path, original, patched: preparePrivyDialogSource(filename, original) }
  })
  const changed = prepared.filter(file => file.original !== file.patched)
  for (const file of changed) writeFileSync(file.path, file.patched)
  return { version: VERSION, changedFiles: changed.length }
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const result = patchPrivyWalletDialog(process.argv[2])
  console.log(`Verified Privy ${result.version} wallet-dialog extension (${result.changedFiles} files updated).`)
}
