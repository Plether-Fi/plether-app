export function EmailLoginOption({ onClick }: { onClick: () => void }) {
  return (
    <span className="ph-no-capture ph-no-record" style={{ display: 'block', width: 'min(312px, calc(100vw - 64px))' }}>
      <button
        type="button"
        onClick={onClick}
        style={{
          display: 'flex', alignItems: 'center', gap: 12, width: '100%', minHeight: 56,
          padding: '14px 16px', border: '1px solid rgba(255, 171, 150, 0.3)',
          borderRadius: 'var(--privy-border-radius-md)', background: '#3B212D', color: '#FFF5F9',
          textAlign: 'left', fontSize: 16, fontWeight: 500, cursor: 'pointer',
        }}
      >
        <svg width="22" height="22" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.7" aria-hidden="true">
          <rect x="3" y="5" width="18" height="14" rx="2" />
          <path d="m3 6 9 7 9-7" />
        </svg>
        Log in with Email
      </button>
      <span style={{ display: 'flex', alignItems: 'center', gap: 12, margin: '20px 0 4px', color: '#D8CBD0', fontSize: 11 }}>
        <span style={{ height: 1, flex: 1, background: 'rgba(255, 171, 150, 0.2)' }} />
        OR CONNECT A WALLET
        <span style={{ height: 1, flex: 1, background: 'rgba(255, 171, 150, 0.2)' }} />
      </span>
    </span>
  )
}
