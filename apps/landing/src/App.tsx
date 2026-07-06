import { Fragment, useEffect, useRef, useState, type CSSProperties, type RefObject, type ReactNode } from 'react'
import syntheticSplitterSource from './solidity/SyntheticSplitter.sol?raw'

const DEFAULT_APP_URL = 'https://app.plether.com'
const getAppUrl = () => {
  if (typeof window === 'undefined') {
    return DEFAULT_APP_URL
  }

  const { protocol, hostname, port } = window.location
  const appHostname = hostname.startsWith('app.') ? hostname : `app.${hostname}`

  return `${protocol}//${appHostname}${port ? `:${port}` : ''}`
}

const APP_URL = getAppUrl()
const DOCS_URL = 'https://docs.plether.com'
const X_URL = 'https://x.com/plether_fi'
const CORE_REPO_URL = 'https://github.com/Plether-Fi/plether-core'
const MANIFESTO_URL = '/manifesto'
const MEDIA_KIT_URL = '/media-kit'
const AUDITS_URL = 'https://github.com/Plether-Fi/plether-core/tree/master/audits'
const STABLECOINS_URL = 'https://app.rwa.xyz/stablecoins'
const ECB_STABLECOIN_URL = 'https://x.com/ecb/status/2052644951427805440?s=20'
const PRIMITIVE_SCRUB_DISTANCE = 640
const PRIMITIVE_MOBILE_SCRUB_DISTANCE = 420
const TRUST_SLIDE_STEP_MIN = 420
const TRUST_SLIDE_STEP_MAX = 720
const TRUST_SLIDE_STEP_RATIO = 0.72
const TRUST_SLIDE_ACTIVATION_BIAS = 0.08
const EXPOSURE_REPLAY_IDLE_MS = 1000
const EXPOSURE_REPLAY_TRANSITION_MS = 2880
const EXPOSURE_REPLAY_REVERSE_PAUSE_MS = 1000
const PRIMITIVE_MOBILE_MEDIA = '(max-width: 680px)'
const REDUCED_MOTION_MEDIA = '(prefers-reduced-motion: reduce)'
type HeaderTheme = 'orange' | 'dark' | 'light'
const MOBILE_MENU_ITEMS = [
  { href: APP_URL, label: 'Launch App', opensInNewWindow: true },
  { href: DOCS_URL, label: 'Read Docs' },
  { href: X_URL, label: 'X' },
  { href: CORE_REPO_URL, label: 'Github' },
  { href: MANIFESTO_URL, label: 'Manifesto' },
  { href: AUDITS_URL, label: 'Audit Reports' },
]
const TRUST_ITEMS = [
  {
    title: 'Open Source',
    description: 'All Plether code—contracts, front-end, infrastructure—is open source under the AGPL license. Audit it, fork it, run your own interface.',
  },
  {
    title: 'No VC Funding',
    description: 'Self-funded. No outside investors, no board seats, no insider token allocations waiting to unlock.',
  },
  {
    title: 'Immutable Contracts',
    description: 'No upgradeable proxies. The deployed code cannot be modified by anyone. What you verify is what runs, permanently.',
  },
  {
    title: 'Passes Walkaway Test',
    description: 'No off-chain dependencies, no maintenance. The protocol runs without the team, indefinitely.',
  },
  {
    title: 'No allowlist, no KYC',
    description: 'No identity checks, no approvals, no geofencing at the contract level. Anyone with a wallet can enter and nothing can block your exit.',
  },
]

const MEDIA_ASSETS = [
  { label: 'Logotype', variant: 'logotype' },
  { label: 'Logo Lockup', variant: 'lockup' },
  { label: 'Logomark', variant: 'logomark' },
] as const

const MEDIA_COLORS = [
  {
    label: 'Primary Color',
    colors: [
      { value: 'var(--plether-orange)', code: '#FF512F' },
      { value: 'var(--plether-soft-orange)', code: '#FFAB96' },
    ],
  },
  {
    label: 'Accent Color',
    colors: [
      { value: 'var(--plether-yellow)', code: '#F7D977' },
      { value: '#f7e4b2', code: '#F7E4B2' },
    ],
  },
  {
    label: 'Dark Color',
    colors: [
      { value: 'var(--plether-ink)', code: '#250917' },
      { value: '#9a8890', code: '#9A8890' },
    ],
  },
] as const
const SOLIDITY_KEYWORDS = new Set([
  'address',
  'bool',
  'break',
  'bytes',
  'constant',
  'constructor',
  'contract',
  'delete',
  'else',
  'emit',
  'enum',
  'event',
  'external',
  'false',
  'for',
  'function',
  'if',
  'immutable',
  'import',
  'internal',
  'is',
  'library',
  'mapping',
  'memory',
  'modifier',
  'override',
  'private',
  'public',
  'pure',
  'require',
  'return',
  'returns',
  'solidity',
  'storage',
  'struct',
  'true',
  'uint256',
  'uint64',
  'uint8',
  'using',
  'view',
])
const SOLIDITY_TOKEN_PATTERN = /(\/\/.*|"(?:\\.|[^"\\])*"|'(?:\\.|[^'\\])*'|\b[A-Za-z_][A-Za-z0-9_]*\b|\b\d+(?:_\d+)*\b)/g

function Brand() {
  return (
    <a className="brand" href="/" aria-label="Plether home">
      <img className="brand__mark" src="/logomark.svg" alt="" />
      <img className="brand__type" src="/logotype.svg" alt="plether" />
    </a>
  )
}

function AnimatedFooterLogomark() {
  return (
    <svg
      aria-hidden="true"
      className="footer__mark footer-logomark"
      fill="none"
      viewBox="0 0 211 211"
      xmlns="http://www.w3.org/2000/svg"
    >
      <defs>
        <clipPath id="footer-logomark-center-clip">
          <rect height="64.82" width="64.86" x="73.13" y="72.82" />
        </clipPath>
      </defs>

      <g className="footer-logomark__mark">
        <circle className="footer-logomark__falling-circle footer-logomark__falling-circle--cream" cx="105.56" cy="105.23" r="28.85" />

        <rect className="footer-logomark__center-square" height="64.82" width="64.86" x="73.13" y="72.82" />

        <g clipPath="url(#footer-logomark-center-clip)">
          <circle className="footer-logomark__falling-circle footer-logomark__falling-circle--orange" cx="105.56" cy="105.23" r="28.85" />
        </g>

        <rect className="footer-logomark__square footer-logomark__square--top-left" height="64.82" width="64.85" x="0.28" y="0" />
        <rect className="footer-logomark__square footer-logomark__square--top-center" height="64.82" width="64.85" x="73.13" y="0" />
        <rect className="footer-logomark__square footer-logomark__square--top-right" height="64.82" width="64.86" x="145.98" y="0" />
        <rect className="footer-logomark__square footer-logomark__square--middle-left" height="64.82" width="64.86" x="0.27" y="72.82" />
        <rect className="footer-logomark__square footer-logomark__square--middle-right" height="64.82" width="64.86" x="145.98" y="72.82" />
        <rect className="footer-logomark__square footer-logomark__square--bottom-left" height="64.83" width="65.4" x="0" y="145.64" />
        <rect className="footer-logomark__square footer-logomark__square--bottom-center" height="64.83" width="64.86" x="73.13" y="145.64" />
        <rect className="footer-logomark__square footer-logomark__square--bottom-right" height="64.83" width="64.86" x="145.98" y="145.64" />
        <circle className="footer-logomark__bottom-right-dot" cx="178.41" cy="178.06" r="28.85" />
      </g>
    </svg>
  )
}

function FooterBrand() {
  const brandRef = useRef<HTMLDivElement | null>(null)
  const [hasEnteredViewport, setHasEnteredViewport] = useState(false)

  useEffect(() => {
    if (hasEnteredViewport) {
      return
    }

    const brand = brandRef.current

    if (!brand) {
      return
    }

    if (!('IntersectionObserver' in window)) {
      setHasEnteredViewport(true)
      return
    }

    const observer = new IntersectionObserver(
      (entries) => {
        if (entries.some((entry) => entry.isIntersecting)) {
          setHasEnteredViewport(true)
          observer.disconnect()
        }
      },
      { threshold: 0.2 },
    )

    observer.observe(brand)

    return () => observer.disconnect()
  }, [hasEnteredViewport])

  return (
    <div aria-hidden="true" className={`footer__brand${hasEnteredViewport ? ' footer__brand--visible' : ''}`} ref={brandRef}>
      <AnimatedFooterLogomark />
      <img className="footer__type" src="/logotype.svg" alt="" />
    </div>
  )
}

function DividerDot() {
  return <span className="nav-dot" aria-hidden="true" />
}

function SiteFooter({ footerRef }: { footerRef?: RefObject<HTMLElement | null> }) {
  return (
    <footer ref={footerRef} className="footer">
      <div className="footer__inner">
        <FooterBrand />
        <div className="footer__bottom">
          <p>© Plether 2026</p>
          <nav className="footer__nav" aria-label="Footer navigation">
            <a href={X_URL}>X</a>
            <DividerDot />
            <a href={CORE_REPO_URL}>GitHub</a>
            <DividerDot />
            <a href={DOCS_URL}>Docs</a>
            <DividerDot />
            <a href={MANIFESTO_URL}>Manifesto</a>
            <DividerDot />
            <a href={MEDIA_KIT_URL}>Media Kit</a>
            <DividerDot />
            <a href={AUDITS_URL}>Audit Reports</a>
          </nav>
        </div>
      </div>
    </footer>
  )
}

function SiteHeader({
  theme,
  headerRef,
}: {
  theme: HeaderTheme
  headerRef: RefObject<HTMLElement | null>
}) {
  const [isMenuOpen, setIsMenuOpen] = useState(false)

  useEffect(() => {
    if (!isMenuOpen) {
      return
    }

    const previousOverflow = document.body.style.overflow
    const closeOnEscape = (event: KeyboardEvent) => {
      if (event.key === 'Escape') {
        setIsMenuOpen(false)
      }
    }

    document.body.style.overflow = 'hidden'
    window.addEventListener('keydown', closeOnEscape)

    return () => {
      document.body.style.overflow = previousOverflow
      window.removeEventListener('keydown', closeOnEscape)
    }
  }, [isMenuOpen])

  return (
    <>
      <header ref={headerRef} className={`site-header site-header--${theme}`}>
        <div className="site-header__inner">
          <Brand />
          <nav className="site-nav" aria-label="Primary navigation">
            <a href={X_URL} aria-label="Plether on X" target="_blank" rel="noreferrer">X</a>
            <DividerDot />
            <a href={DOCS_URL} target="_blank" rel="noreferrer">Docs</a>
            <DividerDot />
            <a className="launch-button launch-button--nav" href={APP_URL} target="_blank" rel="noreferrer">
              <span className="button-label">Launch App</span>
            </a>
          </nav>
          <button
            className="mobile-menu-toggle"
            type="button"
            aria-label="Open menu"
            aria-expanded={isMenuOpen}
            aria-controls="mobile-menu"
            onClick={() => setIsMenuOpen(true)}
          >
            <span />
            <span />
            <span />
          </button>
        </div>
      </header>

      {isMenuOpen ? (
        <div className="mobile-menu" id="mobile-menu" role="dialog" aria-modal="true" aria-label="Mobile navigation">
          <div className="mobile-menu__header">
            <Brand />
            <button className="mobile-menu__close" type="button" aria-label="Close menu" onClick={() => setIsMenuOpen(false)}>
              <span />
              <span />
            </button>
          </div>
          <nav className="mobile-menu__nav" aria-label="Mobile navigation">
            {MOBILE_MENU_ITEMS.map((item) => (
              <a
                href={item.href}
                key={item.label}
                onClick={() => setIsMenuOpen(false)}
                target={item.opensInNewWindow ? '_blank' : undefined}
                rel={item.opensInNewWindow ? 'noreferrer' : undefined}
              >
                {item.label}
              </a>
            ))}
          </nav>
          <div className="mobile-menu__bottom">
            <span className="mobile-menu__mark" aria-hidden="true" />
            <p>© Plether 2026</p>
          </div>
        </div>
      ) : null}
    </>
  )
}

function SplitIllustration() {
  return (
    <div className="split-illustration" aria-hidden="true">
      <span className="split-illustration__circle split-illustration__circle--left" />
      <span className="split-illustration__circle split-illustration__circle--right" />
      <span className="split-illustration__side split-illustration__side--left">
        <span className="split-illustration__track">
          <span className="split-illustration__boxed">
            <span />
          </span>
        </span>
      </span>
      <span className="split-illustration__side split-illustration__side--right">
        <span className="split-illustration__track">
          <span className="split-illustration__boxed">
            <span />
          </span>
        </span>
      </span>
    </div>
  )
}

function SolvencyIllustration() {
  return (
    <div className="solvency-illustration" aria-hidden="true">
      {Array.from({ length: 4 }).map((_, index) => (
        <span className={`solvency-illustration__drop-dot solvency-illustration__drop-dot--${index + 1}`} key={`drop-dot-${index}`} />
      ))}
      {Array.from({ length: 9 }).map((_, index) => (
        <span className={`solvency-illustration__cell solvency-illustration__cell--${index + 1}`} key={index} />
      ))}
    </div>
  )
}

function highlightSolidityLine(line: string, lineIndex: number) {
  const nodes: ReactNode[] = []
  let cursor = 0
  let match: RegExpExecArray | null

  SOLIDITY_TOKEN_PATTERN.lastIndex = 0

  while ((match = SOLIDITY_TOKEN_PATTERN.exec(line)) !== null) {
    const [token] = match

    if (match.index > cursor) {
      nodes.push(line.slice(cursor, match.index))
    }

    let tokenClass = ''

    if (token.startsWith('//')) {
      tokenClass = 'token-comment'
    } else if (token.startsWith('"') || token.startsWith("'")) {
      tokenClass = 'token-string'
    } else if (/^\d/.test(token)) {
      tokenClass = 'token-number'
    } else if (SOLIDITY_KEYWORDS.has(token)) {
      tokenClass = 'token-keyword'
    }

    nodes.push(
      tokenClass ? (
        <span className={tokenClass} key={`${lineIndex}-${match.index}`}>
          {token}
        </span>
      ) : (
        token
      ),
    )

    cursor = match.index + token.length

    if (token.startsWith('//')) {
      break
    }
  }

  if (cursor < line.length) {
    nodes.push(line.slice(cursor))
  }

  return nodes
}

function SolidityCodeBlock({ code }: { code: string }) {
  return (
    <pre className="source-code" aria-label="SyntheticSplitter Solidity source">
      <code>
        {code.split('\n').map((line, index) => (
          <Fragment key={index}>
            {highlightSolidityLine(line, index)}
            {'\n'}
          </Fragment>
        ))}
      </code>
    </pre>
  )
}

function CardBreak() {
  return (
    <>
      <br className="feature-card__desktop-break" />
      <span className="feature-card__mobile-space"> </span>
    </>
  )
}

function LandingPage() {
  const headerRef = useRef<HTMLElement | null>(null)
  const primitiveSectionRef = useRef<HTMLElement | null>(null)
  const exposureSectionRef = useRef<HTMLElement | null>(null)
  const builtSectionRef = useRef<HTMLElement | null>(null)
  const trustSectionRef = useRef<HTMLElement | null>(null)
  const sourceSectionRef = useRef<HTMLElement | null>(null)
  const ctaSectionRef = useRef<HTMLElement | null>(null)
  const footerRef = useRef<HTMLElement | null>(null)
  const primitiveMarkProgressRef = useRef(0)
  const activeTrustSlideIndexRef = useRef(0)
  const isTrustSlideInitializedRef = useRef(false)
  const [headerTheme, setHeaderTheme] = useState<HeaderTheme>('orange')
  const [primitiveMarkProgress, setPrimitiveMarkProgress] = useState(0)
  const [isExposureAnimationActive, setIsExposureAnimationActive] = useState(false)
  const [isExposureAnimationReversed, setIsExposureAnimationReversed] = useState(false)
  const [isBuiltAnimationActive, setIsBuiltAnimationActive] = useState(false)
  const [activeTrustSlideIndex, setActiveTrustSlideIndex] = useState(0)
  const [trustMarkerSpinStep, setTrustMarkerSpinStep] = useState(0)

  useEffect(() => {
    if (!isExposureAnimationActive) {
      setIsExposureAnimationReversed(false)
      return
    }

    const reducedMotionQuery = window.matchMedia(REDUCED_MOTION_MEDIA)

    if (reducedMotionQuery.matches) {
      setIsExposureAnimationReversed(false)
      return
    }

    let idleTimer: number | undefined
    let reverseTimer: number | undefined
    let reversePauseTimer: number | undefined
    let forwardTimer: number | undefined
    let isDisposed = false

    const queueReplay = (delayMs = EXPOSURE_REPLAY_IDLE_MS) => {
      idleTimer = window.setTimeout(() => {
        if (isDisposed) {
          return
        }

        setIsExposureAnimationReversed(true)

        reverseTimer = window.setTimeout(() => {
          if (isDisposed) {
            return
          }

          reversePauseTimer = window.setTimeout(() => {
            if (isDisposed) {
              return
            }

            setIsExposureAnimationReversed(false)

            forwardTimer = window.setTimeout(() => {
              if (!isDisposed) {
                queueReplay()
              }
            }, EXPOSURE_REPLAY_TRANSITION_MS)
          }, EXPOSURE_REPLAY_REVERSE_PAUSE_MS)
        }, EXPOSURE_REPLAY_TRANSITION_MS)
      }, delayMs)
    }

    setIsExposureAnimationReversed(false)
    queueReplay(EXPOSURE_REPLAY_TRANSITION_MS + EXPOSURE_REPLAY_IDLE_MS)

    return () => {
      isDisposed = true
      window.clearTimeout(idleTimer)
      window.clearTimeout(reverseTimer)
      window.clearTimeout(reversePauseTimer)
      window.clearTimeout(forwardTimer)
    }
  }, [isExposureAnimationActive])

  useEffect(() => {
    let reducedMotionQuery: MediaQueryList | null = null
    let mobileQuery: MediaQueryList | null = null

    const clampProgress = (value: number) => Math.min(1, Math.max(0, value))
    const getHeaderHeight = () => headerRef.current?.offsetHeight ?? 0
    const getPrimitiveTriggerY = () => {
      const primitiveSection = primitiveSectionRef.current

      if (!primitiveSection) {
        return 0
      }

      return primitiveSection.offsetTop - getHeaderHeight()
    }
    const getScrubDistance = () => mobileQuery?.matches ? PRIMITIVE_MOBILE_SCRUB_DISTANCE : PRIMITIVE_SCRUB_DISTANCE
    const getTrustSlideStep = () => {
      const visibleViewportHeight = window.innerHeight - getHeaderHeight()
      return Math.min(TRUST_SLIDE_STEP_MAX, Math.max(TRUST_SLIDE_STEP_MIN, visibleViewportHeight * TRUST_SLIDE_STEP_RATIO))
    }
    const getTrustScrollDistance = () => (TRUST_ITEMS.length - 1) * getTrustSlideStep()
    const shouldReduceMotion = () => reducedMotionQuery?.matches ?? false
    const setTrustSlide = (nextIndex: number) => {
      const clampedIndex = Math.min(TRUST_ITEMS.length - 1, Math.max(0, nextIndex))

      if (activeTrustSlideIndexRef.current === clampedIndex) {
        isTrustSlideInitializedRef.current = true
        return
      }

      activeTrustSlideIndexRef.current = clampedIndex
      setActiveTrustSlideIndex(clampedIndex)

      if (isTrustSlideInitializedRef.current) {
        setTrustMarkerSpinStep((step) => step + 1)
      } else {
        isTrustSlideInitializedRef.current = true
      }
    }
    const setPrimitiveProgress = (nextProgress: number) => {
      const clampedProgress = clampProgress(nextProgress)

      if (Math.abs(primitiveMarkProgressRef.current - clampedProgress) < 0.001) {
        return
      }

      primitiveMarkProgressRef.current = clampedProgress
      setPrimitiveMarkProgress(clampedProgress)
    }
    const updatePrimitiveLayoutVars = () => {
      document.documentElement.style.setProperty('--site-header-height', `${getHeaderHeight()}px`)
      document.documentElement.style.setProperty('--primitive-scroll-space', shouldReduceMotion() ? '0px' : `${getScrubDistance()}px`)
      document.documentElement.style.setProperty('--trust-scroll-space', shouldReduceMotion() ? '0px' : `${getTrustScrollDistance()}px`)
    }
    const updatePrimitiveProgress = () => {
      const triggerY = getPrimitiveTriggerY()

      if (shouldReduceMotion()) {
        setPrimitiveProgress(window.scrollY >= triggerY ? 1 : 0)
        return
      }

      setPrimitiveProgress((window.scrollY - triggerY) / getScrubDistance())
    }
    const updateHeaderTheme = () => {
      const header = headerRef.current
      const sections = [
        { ref: primitiveSectionRef, theme: 'dark' as const },
        { ref: exposureSectionRef, theme: 'light' as const },
        { ref: builtSectionRef, theme: 'light' as const },
        { ref: trustSectionRef, theme: 'orange' as const },
        { ref: sourceSectionRef, theme: 'light' as const },
        { ref: ctaSectionRef, theme: 'light' as const },
        { ref: footerRef, theme: 'orange' as const },
      ]

      if (!header) {
        return
      }

      updatePrimitiveLayoutVars()
      updatePrimitiveProgress()

      let activeSection: (typeof sections)[number] | undefined

      for (let index = sections.length - 1; index >= 0; index -= 1) {
        const section = sections[index]

        if (section.ref.current && section.ref.current.getBoundingClientRect().top <= header.offsetHeight + 1) {
          activeSection = section
          break
        }
      }

      setHeaderTheme(activeSection?.theme ?? 'orange')
      setIsExposureAnimationActive((exposureSectionRef.current?.getBoundingClientRect().top ?? Number.POSITIVE_INFINITY) <= header.offsetHeight + 1)
      setIsBuiltAnimationActive((builtSectionRef.current?.getBoundingClientRect().top ?? Number.POSITIVE_INFINITY) <= header.offsetHeight + 1)

      const trustSection = trustSectionRef.current

      if (!trustSection) {
        setTrustSlide(0)
        return
      }

      const trustRect = trustSection.getBoundingClientRect()
      const trustStart = trustSection.offsetTop - header.offsetHeight
      const trustScrollDistance = shouldReduceMotion() ? 1 : getTrustScrollDistance()
      const trustEnd = trustStart + trustScrollDistance

      if (window.scrollY < trustStart) {
        setTrustSlide(0)
        return
      }

      if (trustRect.bottom <= header.offsetHeight) {
        setTrustSlide(TRUST_ITEMS.length - 1)
        return
      }

      const trustProgress = clampProgress((window.scrollY - trustStart) / Math.max(1, trustEnd - trustStart))
      const activeTrustIndex = Math.round((trustProgress * (TRUST_ITEMS.length - 1)) + TRUST_SLIDE_ACTIVATION_BIAS)
      setTrustSlide(activeTrustIndex)
    }
    const handleReducedMotionChange = () => {
      updateHeaderTheme()
    }

    reducedMotionQuery = window.matchMedia(REDUCED_MOTION_MEDIA)
    mobileQuery = window.matchMedia(PRIMITIVE_MOBILE_MEDIA)

    updateHeaderTheme()
    window.addEventListener('scroll', updateHeaderTheme, { passive: true })
    window.addEventListener('resize', updateHeaderTheme)
    reducedMotionQuery.addEventListener('change', handleReducedMotionChange)
    mobileQuery.addEventListener('change', updateHeaderTheme)

    return () => {
      window.removeEventListener('scroll', updateHeaderTheme)
      window.removeEventListener('resize', updateHeaderTheme)
      reducedMotionQuery?.removeEventListener('change', handleReducedMotionChange)
      mobileQuery?.removeEventListener('change', updateHeaderTheme)
      document.documentElement.style.removeProperty('--site-header-height')
      document.documentElement.style.removeProperty('--primitive-scroll-space')
      document.documentElement.style.removeProperty('--trust-scroll-space')
    }
  }, [])

  return (
    <main className="landing-page">
      <SiteHeader theme={headerTheme} headerRef={headerRef} />

      <section className="landing-section landing-section--hero">
        <div className="hero" aria-labelledby="hero-title">
          <div className="hero__copy">
            <h1 id="hero-title">Trade &amp; hedge<br />dollar index<br className="mobile-only-break" /> perpetuals.</h1>
          </div>

          <div className="hero__bottom">
            <div className="hero__actions">
              <p>Immutable contracts.<br />No ADL. MEV-resistant.<br />Settled in USDC.</p>
              <div className="button-row">
                <a className="launch-button" href={APP_URL} target="_blank" rel="noreferrer">
                  <span className="button-label">Launch App</span>
                </a>
                <a className="docs-button" href={DOCS_URL} target="_blank" rel="noreferrer">
                  <span className="button-label">Read Docs</span>
                </a>
              </div>
            </div>

            <p className="onchain-word">Onchain.</p>
          </div>
        </div>
      </section>

      <section
        ref={primitiveSectionRef}
        className="landing-section landing-section--primitive"
        aria-labelledby="primitive-title"
      >
        <div className="primitive">
          <div
            className="primitive__mark"
            style={{ '--primitive-mark-progress': primitiveMarkProgress } as CSSProperties}
            aria-hidden="true"
          >
            <span />
          </div>
          <h2 id="primitive-title">The dollar&apos;s missing primitive.</h2>
          <p>
            The dollar is the most-used asset in DeFi and the most-traded asset in global macro, but it has no onchain primitive for taking a position on it. Traders can&apos;t trade it. Holders can&apos;t hedge it. Now you can.
          </p>
        </div>
      </section>

      <section
        ref={exposureSectionRef}
        className={`landing-section landing-section--exposure${isExposureAnimationActive && !isExposureAnimationReversed ? ' landing-section--exposure-active' : ''}`}
        aria-labelledby="exposure-title"
      >
        <div className="exposure">
          <h2 id="exposure-title">Macro exposure that fits your DeFi stack.</h2>
          <div className="feature-grid">
            <article className="feature-card">
              <div className="macro-illustration" aria-hidden="true">
                <img src="/illustration-grid.svg" alt="" />
              </div>
              <h3>Macro exposure<CardBreak />without leaving DeFi</h3>
              <p>Settles in USDC. Self-custodial.<CardBreak />No KYC, no offchain backend.</p>
            </article>

            <article className="feature-card">
              <SplitIllustration />
              <h3>The dollar index,<CardBreak />tradeable onchain</h3>
              <p>Perp DEX for the dollar index.<CardBreak />Trade or hedge the dollar,<CardBreak />onchain.</p>
            </article>

            <article className="feature-card">
              <SolvencyIllustration />
              <h3>Built around solvency,<CardBreak />not extraction.</h3>
              <p>No ADL. No cascading<CardBreak />liquidations. MEV-resistant.<CardBreak />Predictable fees.</p>
            </article>
          </div>
        </div>
      </section>

      <section
        ref={builtSectionRef}
        className={`landing-section landing-section--built${isBuiltAnimationActive ? ' landing-section--built-active' : ''}`}
        aria-labelledby="built-title"
      >
        <div className="built">
          <h2 id="built-title">Plether is built different</h2>
          <div className="principle-grid">
            <article className="principle-card">
              <span className="principle-card__dot" aria-hidden="true"><span /></span>
              <h3>Tailor made for macro markets.</h3>
              <p>The dollar index oracle updates Monday to Friday during FX market hours. No stale prices, no weekend gap risk.</p>
            </article>

            <article className="principle-card">
              <span className="principle-card__dot" aria-hidden="true"><span /></span>
              <h3>Your winning trades stay yours.</h3>
              <p>Maximum payouts are guaranteed at entry. No Auto-Deleverage. No socialized losses.</p>
            </article>

            <article className="principle-card">
              <span className="principle-card__dot" aria-hidden="true"><span /></span>
              <h3>No front-running. Ever.</h3>
              <p>Orders commit without a price. A keeper reveals them at the oracle. Passive value extraction is impossible by design.</p>
            </article>

            <article className="principle-card">
              <span className="principle-card__dot" aria-hidden="true"><span /></span>
              <h3>Hold positions without bleeding fees.</h3>
              <p>Carry fee proportional to leverage, not a variable funding rate. Predictable cost, no surprises.</p>
            </article>
          </div>
        </div>
      </section>

      <section
        ref={trustSectionRef}
        className="landing-section landing-section--trust"
        aria-labelledby="trust-title"
      >
        <div className="trust">
          <span className={`trust__marker${trustMarkerSpinStep === 0 ? '' : ' trust__marker--value-step'}`} aria-hidden="true">
            <span key={`trust-marker-top-${trustMarkerSpinStep}`} />
          </span>
          <h2 id="trust-title" className="sr-only">Plether trust guarantees</h2>
          <div className="trust-slide-stage" role="list">
            {TRUST_ITEMS.map((item, index) => {
              const titleId = `trust-title-${index}`
              const slideOffset = index - activeTrustSlideIndex
              const isActive = activeTrustSlideIndex === index
              const slideState = slideOffset < 0 ? 'previous' : slideOffset > 0 ? 'next' : 'active'

              return (
                <div
                  className={`trust-slide trust-slide--${slideState}`}
                  role="listitem"
                  key={item.title}
                  aria-hidden={!isActive}
                  style={{ '--trust-slide-offset': slideOffset } as CSSProperties}
                >
                  <h3
                    className="trust-slide__title"
                    id={titleId}
                  >
                    {item.title}
                  </h3>
                  <p className="trust-slide__description" aria-labelledby={titleId}>{item.description}</p>
                </div>
              )
            })}
          </div>
          <span
            className={`trust__marker trust__marker--bottom${trustMarkerSpinStep === 0 ? '' : ' trust__marker--value-step'}`}
            aria-hidden="true"
          >
            <span key={`trust-marker-bottom-${trustMarkerSpinStep}`} />
          </span>
        </div>
      </section>

      <section
        ref={sourceSectionRef}
        className="landing-section landing-section--source"
        aria-labelledby="source-title"
      >
        <div className="source">
          <h2 id="source-title">See for yourself.</h2>
          <p>
            Every Plether contract is{' '}
            <a className="text-link" href={CORE_REPO_URL}>
              open source
            </a>
            . Read it line by line.
          </p>
          <div className="source__card">
            <SolidityCodeBlock code={syntheticSplitterSource} />
          </div>
        </div>
      </section>

      <section
        ref={ctaSectionRef}
        className="landing-section landing-section--cta"
        aria-labelledby="cta-title"
      >
        <div className="cta">
          <h2 id="cta-title">
            Onchain macro starts{' '}
            <a className="cta__title-link" href={MANIFESTO_URL}>
              here.
            </a>
          </h2>
          <a className="launch-button cta__button" href={APP_URL} target="_blank" rel="noreferrer">
            <span className="button-label">Launch App</span>
          </a>
        </div>

        <SiteFooter footerRef={footerRef} />
      </section>
    </main>
  )
}

function ManifestoPage() {
  const headerRef = useRef<HTMLElement | null>(null)

  return (
    <div className="manifesto-shell">
      <SiteHeader theme="light" headerRef={headerRef} />
      <main className="manifesto-page" aria-labelledby="manifesto-title">
        <article className="manifesto">
          <h1 id="manifesto-title">Manifesto</h1>
          <div className="manifesto__copy">
            <p>
              We believe USD stablecoins already won.{' '}
              While{' '}
              <a href={STABLECOINS_URL}>99% of stablecoins are already denominated in dollars</a>
              , Christine Lagarde from the European Central Bank has called them{' '}
              <a href={ECB_STABLECOIN_URL}>&quot;not an efficient way to strengthen the international role of the euro.&quot;</a>{' '}
              Stablecoins became the working balance sheet of onchain finance. That balance sheet is denominated in dollars.
            </p>
            <p>That doesn&apos;t make the dollar fair. It makes it dominant.</p>
            <p>We&apos;ve watched what happens when you treat dominant money as safe money. Japanese dollar holders lost half their yen purchasing power after the 1985 Plaza Accord. European dollar holders lost 45% of their euro purchasing power during the 2002-2008 DXY collapse. We lived outside the US in 2025, when the dollar index fell 10% and our USDC stayed flat while our rent went up.</p>
            <p>This isn&apos;t a case against the dollar. The point is that holding any currency you don&apos;t spend is a position, whether you opted in or not.</p>
            <p>We want to give people the tools to hold the dollar without being silently long it. No investors. No roadmap to extract value from users we haven&apos;t earned.</p>
            <p>The permissionless, self-custodial, and open-source layer above dollar stablecoins does not yet exist onchain. We are building it. The first instrument is a dollar index. One position to hedge or trade the dollar, powered by the largest stablecoin liquidity. Onchain macro starts here.</p>
          </div>
        </article>
      </main>
      <SiteFooter />
    </div>
  )
}

function MediaAssetPreview({ variant }: { variant: (typeof MEDIA_ASSETS)[number]['variant'] }) {
  return (
    <div className="media-kit-preview media-kit-preview--brand">
      {variant === 'logotype' ? (
        <img className="media-kit-preview__logotype" src="/logotype.svg" alt="" />
      ) : null}
      {variant === 'lockup' ? (
        <span className="media-kit-preview__lockup">
          <img className="media-kit-preview__logomark" src="/logomark.svg" alt="" />
          <img className="media-kit-preview__lockup-type" src="/logotype.svg" alt="" />
        </span>
      ) : null}
      {variant === 'logomark' ? (
        <img className="media-kit-preview__mark-only" src="/logomark.svg" alt="" />
      ) : null}
    </div>
  )
}

function MediaKitPage() {
  const headerRef = useRef<HTMLElement | null>(null)

  return (
    <div className="media-kit-shell">
      <SiteHeader theme="light" headerRef={headerRef} />
      <main className="media-kit-page" aria-labelledby="media-kit-title">
        <section className="media-kit">
          <h1 id="media-kit-title">Media kit</h1>

          <div className="media-kit__asset-grid">
            {MEDIA_ASSETS.map((asset) => (
              <article className={`media-kit-item media-kit-item--${asset.variant}`} key={asset.label}>
                <MediaAssetPreview variant={asset.variant} />
                <h2>{asset.label}</h2>
              </article>
            ))}
          </div>

          <div className="media-kit__divider" aria-hidden="true" />

          <div className="media-kit__color-grid">
            {MEDIA_COLORS.map((colorGroup) => (
              <article className="media-kit-item media-kit-item--color" key={colorGroup.label}>
                <div
                  className="media-kit-preview media-kit-preview--color"
                  style={{ background: colorGroup.colors[1].value } as CSSProperties}
                  aria-hidden="true"
                >
                  {colorGroup.colors.map((color) => (
                    <span className="media-kit-color-swatch" style={{ background: color.value }} key={color.code}>
                      <span className="media-kit-color-code">{color.code}</span>
                    </span>
                  ))}
                </div>
                <h2>{colorGroup.label}</h2>
              </article>
            ))}
          </div>
        </section>
      </main>
      <SiteFooter />
    </div>
  )
}

export function App() {
  if (window.location.pathname === '/manifesto') {
    return <ManifestoPage />
  }

  if (window.location.pathname === '/media-kit') {
    return <MediaKitPage />
  }

  return <LandingPage />
}
