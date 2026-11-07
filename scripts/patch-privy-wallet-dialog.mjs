import { createHash } from 'node:crypto'
import { readFileSync, writeFileSync } from 'node:fs'
import { resolve } from 'node:path'
import { fileURLToPath } from 'node:url'

// Privy 3.51.2 exposes no slots for its native wallet picker's description,
// account actions, or public close action. Keep all dialogs SDK-owned.
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

// Apply account actions after the original connection-picker extension. Strip
// them first when verifying an installation, so the previous complete patch
// can also be upgraded without weakening the upstream checksum checks.
const accountActionPatches = {
  'dist/esm/ui.mjs': [
    ['connectionDescription:privyConnectionDescription})=>', 'connectionDescription:privyConnectionDescription,accountActions:privyAccountActions})=>'],
    ['e(Lr,{onClose:u,onLinkNewWallet:()=>c(!0),assets:p,networks:h})', 't("div",{children:[e(Lr,{onClose:u,onLinkNewWallet:()=>c(!0),assets:p,networks:h}),privyAccountActions]})'],
  ],
  'dist/cjs/ui.js': [
    ['connectionDescription:privyConnectionDescription})=>', 'connectionDescription:privyConnectionDescription,accountActions:privyAccountActions})=>'],
    ['e.jsx(zt,{onClose:x,onLinkNewWallet:()=>h(!0),assets:u,networks:p})', 'e.jsxs("div",{children:[e.jsx(zt,{onClose:x,onLinkNewWallet:()=>h(!0),assets:u,networks:p}),privyAccountActions]})'],
  ],
}
const accountActionTypePatches = [
  ['    networks?: {', '    /** Plether: settings inside the native connected-wallet dialog. */\n    accountActions?: React.ReactNode;\n    networks?: {'],
  ['({ networks, assets, connectionDescription }: WalletsDialogProps)', '({ networks, assets, connectionDescription, accountActions }: WalletsDialogProps)'],
]
accountActionPatches['dist/dts/ui.d.ts'] = accountActionTypePatches
accountActionPatches['dist/dts/ui.d.mts'] = accountActionTypePatches

// Give account actions the native dialog's close callback, without replacing
// its wallet management or authentication implementation.
const accountActionClosePatches = {
  'dist/esm/ui.mjs': [['}),privyAccountActions]})', '}),typeof privyAccountActions==="function"?privyAccountActions(u):privyAccountActions]})']],
  'dist/cjs/ui.js': [['}),privyAccountActions]})', '}),typeof privyAccountActions==="function"?privyAccountActions(x):privyAccountActions]})']],
  'dist/dts/ui.d.ts': [['accountActions?: React.ReactNode;', 'accountActions?: React.ReactNode | ((close: () => void) => React.ReactNode);']],
  'dist/dts/ui.d.mts': [['accountActions?: React.ReactNode;', 'accountActions?: React.ReactNode | ((close: () => void) => React.ReactNode);']],
}

const otherWalletLists = {
  'dist/esm/ui.mjs': '[...s,...c].sort(((e,t)=>e.connectedAt-t.connectedAt)).filter((e=>"privy"!==e.walletClientType)).filter((e=>e.address!==h?.address||e.walletClientType!==h.walletClientType)).map(((t,r)=>/*#__PURE__*/e(Cr,{wallet:t,showBalances:!1,networks:a,assets:n,onSelectWallet:()=>L({address:t.address,client:t.walletClientType,appId:o.id})},r)))',
  'dist/cjs/ui.js': '[...d,...p].sort(((e,t)=>e.connectedAt-t.connectedAt)).filter((e=>"privy"!==e.walletClientType)).filter((e=>e.address!==u?.address||e.walletClientType!==u.walletClientType)).map(((t,r)=>/*#__PURE__*/e.jsx(ft,{wallet:t,showBalances:!1,networks:l,assets:s,onSelectWallet:()=>i.setActiveWallet({address:t.address,client:t.walletClientType,appId:o.id})},r)))',
}
const emptyWalletPatches = Object.fromEntries(Object.entries(otherWalletLists).map(([filename, list]) => {
  const jsx = filename.endsWith('.mjs') ? 'e' : 'e.jsx'
  const placeholder = `${jsx}("p",{role:"status",style:{margin:0,padding:"24px 16px",border:"1px dashed var(--privy-color-foreground-4)",color:"var(--privy-color-foreground-3)",fontSize:14,textAlign:"center"},children:"No other wallets connected."})`
  return [filename, [[list, `(privyOtherWallets=>privyOtherWallets.length?privyOtherWallets:[${placeholder}])(${list})`]]]
}))

function count(source, fragment) { return source.split(fragment).length - 1 }

export function preparePrivyDialogSource(filename, source) {
  const plan = plans[filename]
  if (!plan) throw new Error(`Unsupported Privy patch file: ${filename}`)
  let original = source
  const emptyPatches = emptyWalletPatches[filename] ?? []
  for (const [before, after] of emptyPatches) {
    if (count(original, after) === 1) original = original.replace(after, before)
  }
  const closePatches = accountActionClosePatches[filename]
  for (const [before, after] of closePatches) {
    if (count(original, after) === 1) original = original.replace(after, before)
  }
  const accountPatches = accountActionPatches[filename]
  const accountApplied = accountPatches.map(([before, after]) => {
    if (count(original, after) === 1) {
      original = original.replace(after, before)
      return true
    }
    return false
  })
  if (accountApplied.some(Boolean) && !accountApplied.every(Boolean)) {
    throw new Error(`Privy ${VERSION} UI patch is incomplete: ${filename}`)
  }
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
  let patched = plan.patches.reduce((result, [before, after]) => result.replace(before, after), original)
  for (const [before, after] of accountPatches) {
    if (count(patched, before) !== 1) throw new Error(`Privy ${VERSION} account action source mismatch: ${filename}`)
    patched = patched.replace(before, after)
  }
  for (const [before, after] of closePatches) {
    if (count(patched, before) !== 1) throw new Error(`Privy ${VERSION} account close source mismatch: ${filename}`)
    patched = patched.replace(before, after)
  }
  for (const [before, after] of emptyPatches) {
    if (count(patched, before) !== 1) throw new Error(`Privy ${VERSION} empty wallet list source mismatch: ${filename}`)
    patched = patched.replace(before, after)
  }
  return patched
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
