export function TrancheMark({ tranche, size = 'lg', showLabel = false }: { tranche: { id: 'senior' | 'junior' }; size?: 'md' | 'lg'; showLabel?: boolean }) {
  const senior = tranche.id === 'senior'
  const markClassName = senior
    ? 'border-brand-peach/60 bg-brand-peach/10 text-brand-peach'
    : 'border-brand-orange/60 bg-brand-orange/10 text-brand-orange'

  return (
    <div
      className={`flex shrink-0 items-center justify-center border ${markClassName} ${
        showLabel ? 'flex-col gap-1 px-3 py-2' : size === 'lg' ? 'h-14 w-14' : 'h-11 w-11'
      }`}
      aria-hidden={showLabel ? undefined : true}
    >
      <svg
        viewBox="0 0 28 28"
        className={size === 'lg' ? 'h-8 w-8' : 'h-7 w-7'}
        fill="none"
        role="presentation"
        aria-hidden="true"
      >
        <rect
          x="6"
          y="4.5"
          width="16"
          height="7"
          fill={senior ? 'currentColor' : 'none'}
          stroke="currentColor"
          strokeWidth="1.5"
          opacity={senior ? 1 : 0.45}
        />
        <rect
          x="3"
          y="16.5"
          width="22"
          height="7"
          fill={senior ? 'none' : 'currentColor'}
          stroke="currentColor"
          strokeWidth="1.5"
          opacity={senior ? 0.45 : 1}
        />
      </svg>
      {showLabel && <span className="text-sm font-medium">{senior ? 'Senior' : 'Junior'}</span>}
    </div>
  )
}

