export function SuccessIcon({ className = '' }: { className?: string }) {
  return (
    <span
      className={`flex h-14 w-14 items-center justify-center rounded-full bg-positive text-app-bg ${className}`}
      aria-hidden="true"
    >
      <svg className="block h-7 w-7" viewBox="0 0 24 24" fill="none">
        <path
          d="m5 12.5 4.25 4.25L19 7"
          stroke="currentColor"
          strokeWidth="2.5"
          strokeLinecap="round"
          strokeLinejoin="round"
        />
      </svg>
    </span>
  )
}
