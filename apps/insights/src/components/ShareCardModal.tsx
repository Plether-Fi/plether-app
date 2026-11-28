import { useEffect, useId, useMemo, useRef, useState } from 'react'
import { createPortal } from 'react-dom'
import {
  buildShareCardSvg,
  buildShareTweetUrl,
  renderShareCardPng,
  shareCardFilename,
  svgToDataUrl,
  type ShareCardData,
} from '../utils/shareCard'

export interface ShareCardOption {
  id: string
  label: string
  data: ShareCardData
}

interface ShareCardModalProps {
  cards: ShareCardOption[]
  initialCardId?: string
  triggerLabel?: string
  compact?: boolean
  shareUrl?: string
}

export function ShareCardModal({
  cards,
  initialCardId,
  triggerLabel = 'Share card',
  compact = false,
  shareUrl,
}: ShareCardModalProps) {
  const dialogRef = useRef<HTMLDialogElement>(null)
  const titleId = useId()
  const [isOpen, setIsOpen] = useState(false)
  const [selectedId, setSelectedId] = useState<string>(initialCardId ?? cards.at(0)?.id ?? '')
  const [busy, setBusy] = useState<'download' | 'copy' | null>(null)
  const [feedback, setFeedback] = useState<string | null>(null)

  const activeCard =
    cards.find((card) => card.id === selectedId) ?? cards.at(0)

  const svg = useMemo(
    () => (isOpen && activeCard ? buildShareCardSvg(activeCard.data) : ''),
    [isOpen, activeCard],
  )
  const previewSrc = useMemo(() => (svg ? svgToDataUrl(svg) : ''), [svg])

  useEffect(() => {
    const dialog = dialogRef.current
    if (isOpen && dialog && !dialog.open) {
      dialog.showModal()
    }
  }, [isOpen])

  if (!activeCard) return null

  const pageOrigin =
    typeof window !== 'undefined' && window.location.origin.startsWith('http')
      ? window.location.origin
      : 'https://insights.plether.com'
  const canonicalCardUrl = (() => {
    const account = activeCard.data.account.toLowerCase()
    if (activeCard.data.kind === 'trade' && activeCard.id.startsWith('trade-')) {
      const tradeId = activeCard.id.slice('trade-'.length)
      return `${pageOrigin}/wallets/${account}?trade=${encodeURIComponent(tradeId)}`
    }
    if (activeCard.data.kind === 'position') {
      return `${pageOrigin}/wallets/${account}?card=position`
    }
    if (activeCard.data.kind === 'division') {
      return `${pageOrigin}/divisions/wallets/${account}`
    }
    return `${pageOrigin}/wallets/${account}`
  })()

  const resolvedShareUrl = shareUrl ?? canonicalCardUrl
  const tweetUrl = buildShareTweetUrl(activeCard.data, resolvedShareUrl)

  function openModal() {
    setSelectedId(initialCardId ?? cards.at(0)?.id ?? '')
    setFeedback(null)
    setIsOpen(true)
  }

  async function handleDownloadPng() {
    if (!svg || !activeCard) return
    setBusy('download')
    setFeedback(null)
    try {
      const blob = await renderShareCardPng(svg)
      const url = URL.createObjectURL(blob)
      const link = document.createElement('a')
      link.href = url
      link.download = shareCardFilename(activeCard.data)
      document.body.appendChild(link)
      link.click()
      link.remove()
      URL.revokeObjectURL(url)
      setFeedback('Downloaded high-res PNG (2400×1260)')
    } catch {
      setFeedback('Could not export PNG in this browser')
    } finally {
      setBusy(null)
    }
  }

  async function handleCopyImage() {
    if (!svg || !activeCard) return
    setBusy('copy')
    setFeedback(null)
    try {
      const blob = await renderShareCardPng(svg)
      if (typeof ClipboardItem !== 'undefined' && 'clipboard' in navigator) {
        await navigator.clipboard.write([new ClipboardItem({ 'image/png': blob })])
        setFeedback('Copied PNG image to clipboard')
      } else {
        await handleDownloadPng()
      }
    } catch {
      setFeedback('Clipboard image copy blocked — use Download PNG instead')
    } finally {
      setBusy(null)
    }
  }

  return (
    <>
      <button
        type="button"
        aria-haspopup="dialog"
        onClick={openModal}
        className={[
          'inline-flex cursor-pointer items-center whitespace-nowrap border border-brand-orange bg-brand-orange text-xs font-semibold text-content-primary transition-colors hover:bg-brand-orange/90 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-brand-peach',
          compact ? 'gap-1.5 px-2.5 py-1' : 'gap-2 px-3.5 py-2',
        ].join(' ')}
      >
        <svg
          aria-hidden="true"
          viewBox="0 0 12 12"
          className="h-3 w-3 shrink-0"
          fill="none"
          stroke="currentColor"
          strokeWidth="1.5"
          strokeLinecap="round"
          strokeLinejoin="round"
        >
          <circle cx="9.25" cy="2.75" r="1.45" />
          <circle cx="2.75" cy="6" r="1.45" />
          <circle cx="9.25" cy="9.25" r="1.45" />
          <path d="M4.05 5.35L7.95 3.4M4.05 6.65L7.95 8.6" />
        </svg>
        <span>{triggerLabel}</span>
      </button>

      {isOpen
        ? createPortal(
            <dialog
              ref={dialogRef}
              aria-labelledby={titleId}
              onClick={(event) => {
                if (event.target === event.currentTarget) {
                  dialogRef.current?.close()
                }
              }}
              onClose={() => {
                setIsOpen(false)
              }}
              className="fixed inset-0 m-auto max-h-[92dvh] w-[calc(100%-2rem)] max-w-4xl overflow-y-auto overscroll-contain border border-brand-border/40 bg-surface-panel p-0 text-content-primary shadow-2xl backdrop:bg-black/75"
            >
              <header className="sticky top-0 z-10 flex items-center justify-between gap-4 border-b border-brand-border/25 bg-surface-panel px-5 py-4">
                <div>
                  <h2 id={titleId} className="text-lg font-semibold">
                    Shareable social card
                  </h2>
                  <p className="text-xs text-content-tertiary">
                    Export a verified 2400×1260 retina PNG for X, Discord, or Farcaster
                  </p>
                </div>
                <button
                  type="button"
                  onClick={() => {
                    dialogRef.current?.close()
                  }}
                  aria-label="Close share card modal"
                  className="min-h-10 cursor-pointer px-3 text-sm font-semibold text-brand-peach hover:underline"
                >
                  Close
                </button>
              </header>

              <div className="space-y-5 p-5 sm:p-6">
                {cards.length > 1 ? (
                  <div
                    role="tablist"
                    aria-label="Choose share card type"
                    className="flex flex-wrap gap-1.5"
                  >
                    {cards.map((card) => {
                      const selected = card.id === activeCard.id
                      return (
                        <button
                          key={card.id}
                          type="button"
                          role="tab"
                          aria-selected={selected}
                          onClick={() => {
                            setSelectedId(card.id)
                            setFeedback(null)
                          }}
                          className={`border px-3 py-1.5 text-xs font-semibold transition-colors ${
                            selected
                              ? 'border-brand-orange bg-brand-orange text-content-primary'
                              : 'border-brand-border/30 bg-app-bg text-content-secondary hover:border-brand-orange/50 hover:text-content-primary'
                          }`}
                        >
                          {card.label}
                        </button>
                      )
                    })}
                  </div>
                ) : null}

                <div className="overflow-hidden border border-brand-border/30 bg-app-bg-deep">
                  <img
                    src={previewSrc}
                    alt={`Preview of ${activeCard.label} share card`}
                    width={1200}
                    height={630}
                    className="block aspect-[1200/630] w-full"
                  />
                </div>

                <div className="flex flex-col items-start justify-between gap-3 sm:flex-row sm:items-center">
                  <div className="flex flex-wrap gap-2.5">
                    <button
                      type="button"
                      disabled={busy !== null}
                      onClick={() => {
                        void handleDownloadPng()
                      }}
                      className="inline-flex items-center border border-brand-orange bg-brand-orange px-4 py-2 text-xs font-semibold text-content-primary hover:bg-brand-orange/90 disabled:opacity-60"
                    >
                      {busy === 'download' ? 'Exporting PNG…' : 'Download PNG'}
                    </button>
                    <button
                      type="button"
                      disabled={busy !== null}
                      onClick={() => {
                        void handleCopyImage()
                      }}
                      className="inline-flex items-center border border-brand-border/40 bg-app-bg px-4 py-2 text-xs font-semibold text-brand-peach hover:bg-brand-peach/10 disabled:opacity-60"
                    >
                      {busy === 'copy' ? 'Copying…' : 'Copy image'}
                    </button>
                    <a
                      href={tweetUrl}
                      target="_blank"
                      rel="noreferrer"
                      className="inline-flex items-center border border-brand-border/30 bg-surface-raised px-4 py-2 text-xs font-semibold text-content-primary hover:border-brand-peach hover:text-brand-peach"
                    >
                      Post on X ↗
                    </a>
                  </div>
                  {feedback ? (
                    <p role="status" className="text-xs font-medium text-positive">
                      {feedback}
                    </p>
                  ) : null}
                </div>
              </div>
            </dialog>,
            document.body,
          )
        : null}
    </>
  )
}
