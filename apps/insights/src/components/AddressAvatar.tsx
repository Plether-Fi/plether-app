import { useMemo } from 'react'
import { deriveAddressAvatar } from '../utils/addressAvatar'

export function AddressAvatar({
  address,
  size = 20,
  className = '',
}: {
  address: string
  size?: number
  className?: string
}) {
  const spec = useMemo(() => deriveAddressAvatar(address), [address])

  return (
    <svg
      width={size}
      height={size}
      viewBox="0 0 24 24"
      aria-hidden="true"
      className={`inline-block shrink-0 align-middle ${className}`}
    >
      <rect width="24" height="24" fill={spec.bg} />
      {spec.cells.map((cell) => {
        if (cell.kind === 'empty') return null
        const key = `${String(cell.row)}-${String(cell.col)}`
        const x = cell.col * 8
        const y = cell.row * 8

        if (cell.kind === 'square') {
          return <rect key={key} x={x} y={y} width={8} height={8} fill={cell.color} />
        }
        if (cell.kind === 'circle') {
          return <circle key={key} cx={x + 4} cy={y + 4} r={3.2} fill={cell.color} />
        }
        if (cell.kind === 'half') {
          const r = cell.rotation % 4
          if (r === 0) return <rect key={key} x={x} y={y} width={8} height={4} fill={cell.color} />
          if (r === 1) return <rect key={key} x={x + 4} y={y} width={4} height={8} fill={cell.color} />
          if (r === 2) return <rect key={key} x={x} y={y + 4} width={8} height={4} fill={cell.color} />
          return <rect key={key} x={x} y={y} width={4} height={8} fill={cell.color} />
        }
        if (cell.kind === 'quarter') {
          const r = cell.rotation % 4
          const d =
            r === 0
              ? `M${String(x)},${String(y)} H${String(x + 8)} A8,8 0 0 1 ${String(x)},${String(y + 8)} Z`
              : r === 1
                ? `M${String(x + 8)},${String(y)} V${String(y + 8)} A8,8 0 0 1 ${String(x)},${String(y)} Z`
                : r === 2
                  ? `M${String(x + 8)},${String(y + 8)} H${String(x)} A8,8 0 0 1 ${String(x + 8)},${String(y)} Z`
                  : `M${String(x)},${String(y + 8)} V${String(y)} A8,8 0 0 1 ${String(x + 8)},${String(y + 8)} Z`
          return <path key={key} d={d} fill={cell.color} />
        }
        const r = cell.rotation % 4
        const points =
          r === 0
            ? `${String(x)},${String(y)} ${String(x + 8)},${String(y)} ${String(x)},${String(y + 8)}`
            : r === 1
              ? `${String(x)},${String(y)} ${String(x + 8)},${String(y)} ${String(x + 8)},${String(y + 8)}`
              : r === 2
                ? `${String(x + 8)},${String(y)} ${String(x + 8)},${String(y + 8)} ${String(x)},${String(y + 8)}`
                : `${String(x)},${String(y)} ${String(x + 8)},${String(y + 8)} ${String(x)},${String(y + 8)}`
        return <polygon key={key} points={points} fill={cell.color} />
      })}
      <rect
        x="0.5"
        y="0.5"
        width="23"
        height="23"
        fill="none"
        stroke={spec.border}
        strokeOpacity="0.28"
        strokeWidth="1"
      />
    </svg>
  )
}
