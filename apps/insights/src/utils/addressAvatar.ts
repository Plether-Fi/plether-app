const BAUHAUS_PALETTE = [
  '#FF572D', // Plether flame orange
  '#FFAB96', // Plether peach
  '#F2CF71', // Warm gold
  '#34D399', // Positive emerald
  '#F472B6', // Soft rose
  '#FFF5F9', // Crisp ivory
] as const

const BG_COLOR = '#1B0711'
const MUTED_COLOR = '#4E2538'

type CellKind = 'empty' | 'square' | 'quarter' | 'circle' | 'half' | 'triangle'

interface AvatarCell {
  row: number
  col: number
  kind: CellKind
  rotation: number // 0, 1, 2, 3 (multiples of 90deg)
  color: string
}

export interface AddressAvatarSpec {
  bg: string
  border: string
  primary: string
  secondary: string
  cells: AvatarCell[]
}

function hashSeed(input: string, salt: number): number {
  let h = (0x811c9dc5 ^ salt) >>> 0
  for (let i = 0; i < input.length; i += 1) {
    h ^= input.charCodeAt(i)
    h = Math.imul(h, 0x01000193) >>> 0
  }
  // Murmur3 finalizer avalanche so similar vanity addresses diverge sharply
  h ^= h >>> 16
  h = Math.imul(h, 0x85ebca6b) >>> 0
  h ^= h >>> 13
  h = Math.imul(h, 0xc2b2ae35) >>> 0
  h ^= h >>> 16
  return h >>> 0
}

function nextPrng(state: { s: number }): number {
  state.s = (state.s + 0x6d2b79f5) >>> 0
  let t = state.s
  t = Math.imul(t ^ (t >>> 15), t | 1) >>> 0
  t ^= (t + Math.imul(t ^ (t >>> 7), t | 61)) >>> 0
  return ((t ^ (t >>> 14)) >>> 0)
}

function mirrorRotation(kind: CellKind, rotation: number): number {
  const r = ((rotation % 4) + 4) % 4
  if (kind === 'quarter' || kind === 'triangle') {
    // 0=TL, 1=TR, 2=BR, 3=BL -> mirror horizontally: 0<->1, 2<->3
    return r === 0 ? 1 : r === 1 ? 0 : r === 2 ? 3 : 2
  }
  if (kind === 'half') {
    // 0=top, 1=right, 2=bottom, 3=left -> mirror horizontally: 1<->3
    return r === 1 ? 3 : r === 3 ? 1 : r
  }
  return r
}

const CELL_KINDS: readonly CellKind[] = [
  'square',
  'quarter',
  'quarter',
  'circle',
  'half',
  'triangle',
  'empty',
  'empty',
]

export function deriveAddressAvatar(address: string): AddressAvatarSpec {
  const normalized = address.trim().toLowerCase()
  const seedA = hashSeed(normalized, 0x517cc1b7)
  const seedB = hashSeed(normalized, 0x9e3779b9)
  const rng = { s: (seedA ^ (seedB >>> 1)) >>> 0 }

  const primaryIdx = nextPrng(rng) % BAUHAUS_PALETTE.length
  const secondaryOffset = 1 + (nextPrng(rng) % (BAUHAUS_PALETTE.length - 1))
  const secondaryIdx = (primaryIdx + secondaryOffset) % BAUHAUS_PALETTE.length

  const primary = BAUHAUS_PALETTE[primaryIdx]
  const secondary = BAUHAUS_PALETTE[secondaryIdx]
  const colors = [primary, primary, secondary, secondary, MUTED_COLOR]

  // Build 3x3 horizontally mirrored Bauhaus matrix:
  // Independently sample left column (col=0, mirrored to col=2) and center column (col=1).
  const cells: AvatarCell[] = []
  let filledCount = 0

  for (let row = 0; row < 3; row += 1) {
    const leftKind = CELL_KINDS[nextPrng(rng) % CELL_KINDS.length]
    const leftRot = nextPrng(rng) % 4
    const leftColor = colors[nextPrng(rng) % colors.length]

    const centerKind = CELL_KINDS[nextPrng(rng) % CELL_KINDS.length]
    const centerRot = centerKind === 'quarter' || centerKind === 'triangle'
      ? (nextPrng(rng) % 2) * 2
      : (nextPrng(rng) % 2) * 2 // keep center vertical symmetry for half blocks (top/bottom)
    const centerColor = colors[nextPrng(rng) % colors.length]

    if (leftKind !== 'empty') filledCount += 2
    if (centerKind !== 'empty') filledCount += 1

    cells.push({ row, col: 0, kind: leftKind, rotation: leftRot, color: leftColor })
    cells.push({ row, col: 1, kind: centerKind, rotation: centerRot, color: centerColor })
    cells.push({
      row,
      col: 2,
      kind: leftKind,
      rotation: mirrorRotation(leftKind, leftRot),
      color: leftColor,
    })
  }

  // Guarantee strong visual balance (never sparse or blank)
  if (filledCount < 4) {
    const center = cells.find((c) => c.row === 1 && c.col === 1)
    if (center) {
      center.kind = 'circle'
      center.color = primary
    }
    const topLeft = cells.find((c) => c.row === 0 && c.col === 0)
    const topRight = cells.find((c) => c.row === 0 && c.col === 2)
    if (topLeft && topRight && topLeft.kind === 'empty') {
      topLeft.kind = 'quarter'
      topLeft.rotation = 0
      topLeft.color = secondary
      topRight.kind = 'quarter'
      topRight.rotation = 1
      topRight.color = secondary
    }
  }

  return {
    bg: BG_COLOR,
    border: '#FFAB96',
    primary,
    secondary,
    cells,
  }
}

function renderCellSvg(cell: AvatarCell): string {
  if (cell.kind === 'empty') return ''
  const x = cell.col * 8
  const y = cell.row * 8

  switch (cell.kind) {
    case 'square':
      return `<rect x="${String(x)}" y="${String(y)}" width="8" height="8" fill="${cell.color}" />`
    case 'circle':
      return `<circle cx="${String(x + 4)}" cy="${String(y + 4)}" r="3.2" fill="${cell.color}" />`
    case 'half': {
      const r = cell.rotation % 4
      if (r === 0) return `<rect x="${String(x)}" y="${String(y)}" width="8" height="4" fill="${cell.color}" />`
      if (r === 1) return `<rect x="${String(x + 4)}" y="${String(y)}" width="4" height="8" fill="${cell.color}" />`
      if (r === 2) return `<rect x="${String(x)}" y="${String(y + 4)}" width="8" height="4" fill="${cell.color}" />`
      return `<rect x="${String(x)}" y="${String(y)}" width="4" height="8" fill="${cell.color}" />`
    }
    case 'quarter': {
      // Quarter circle anchored to one corner of the 8x8 cell
      const r = cell.rotation % 4
      if (r === 0) {
        return `<path d="M${String(x)},${String(y)} H${String(x + 8)} A8,8 0 0 1 ${String(x)},${String(y + 8)} Z" fill="${cell.color}" />`
      }
      if (r === 1) {
        return `<path d="M${String(x + 8)},${String(y)} V${String(y + 8)} A8,8 0 0 1 ${String(x)},${String(y)} Z" fill="${cell.color}" />`
      }
      if (r === 2) {
        return `<path d="M${String(x + 8)},${String(y + 8)} H${String(x)} A8,8 0 0 1 ${String(x + 8)},${String(y)} Z" fill="${cell.color}" />`
      }
      return `<path d="M${String(x)},${String(y + 8)} V${String(y)} A8,8 0 0 1 ${String(x + 8)},${String(y + 8)} Z" fill="${cell.color}" />`
    }
    case 'triangle': {
      const r = cell.rotation % 4
      if (r === 0) {
        return `<polygon points="${String(x)},${String(y)} ${String(x + 8)},${String(y)} ${String(x)},${String(y + 8)}" fill="${cell.color}" />`
      }
      if (r === 1) {
        return `<polygon points="${String(x)},${String(y)} ${String(x + 8)},${String(y)} ${String(x + 8)},${String(y + 8)}" fill="${cell.color}" />`
      }
      if (r === 2) {
        return `<polygon points="${String(x + 8)},${String(y)} ${String(x + 8)},${String(y + 8)} ${String(x)},${String(y + 8)}" fill="${cell.color}" />`
      }
      return `<polygon points="${String(x)},${String(y)} ${String(x + 8)},${String(y + 8)} ${String(x)},${String(y + 8)}" fill="${cell.color}" />`
    }
  }
}

export function buildAddressAvatarInnerSvg(address: string): string {
  const spec = deriveAddressAvatar(address)
  const shapes = spec.cells.map(renderCellSvg).filter(Boolean).join('')
  return `<rect width="24" height="24" fill="${spec.bg}" />${shapes}<rect x="0.5" y="0.5" width="23" height="23" fill="none" stroke="${spec.border}" stroke-opacity="0.28" stroke-width="1" />`
}
