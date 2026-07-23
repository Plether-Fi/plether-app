type TabId = 'trade' | 'leverage' | 'lending'

interface TabConfig {
  id: TabId
  label: string
  shortLabel: string
  sublabel: string
  icon: string
}

const tabs: TabConfig[] = [
  { id: 'trade', label: 'Dollar Hedge', shortLabel: 'Spot', sublabel: 'Spot trading', icon: 'security' },
  { id: 'leverage', label: 'Leverage', shortLabel: 'Leverage', sublabel: 'Margin trading', icon: 'trending_up' },
  { id: 'lending', label: 'Lending', shortLabel: 'Lending', sublabel: 'Supply and borrow USDC', icon: 'grass' },
]

const tabStyles = {
  trade: {
    border: 'border-brand-peach',
    hoverBorder: 'hover:border-[#FFAB96]',
    activeBg: 'bg-brand-peach/15',
    activeIconBg: 'bg-brand-peach',
    activeIconText: 'text-app-bg',
    text: 'text-brand-peach',
    textMuted: 'text-brand-peach/70',
  },
  leverage: {
    border: 'border-brand-orange',
    hoverBorder: 'hover:border-brand-orange',
    activeBg: 'bg-brand-orange/15',
    activeIconBg: 'bg-brand-orange',
    activeIconText: 'text-content-primary',
    text: 'text-brand-orange',
    textMuted: 'text-brand-orange/70',
  },
  lending: {
    border: 'border-positive',
    hoverBorder: 'hover:border-positive',
    activeBg: 'bg-positive/15',
    activeIconBg: 'bg-positive',
    activeIconText: 'text-app-bg',
    text: 'text-positive',
    textMuted: 'text-positive/70',
  },
}

export interface MainTabNavProps {
  activeTab: TabId
  onTabChange: (tab: TabId) => void
}

export function MainTabNav({ activeTab, onTabChange }: MainTabNavProps) {
  const activeStyles = tabStyles[activeTab]

  return (
    <div className={`grid grid-cols-3 border-b-2 ${activeStyles.border}`}>
      {tabs.map((tab) => {
        const isActive = activeTab === tab.id
        const styles = tabStyles[tab.id]
        return (
          <button
            key={tab.id}
            type="button"
            onClick={() => { onTabChange(tab.id); }}
            className={`
              -mb-[2px] flex min-h-12 min-w-0 cursor-pointer flex-col items-center justify-center gap-1 px-1 py-3 text-center transition-colors hover:underline hover:decoration-current hover:underline-offset-4 sm:min-h-0 sm:flex-row sm:justify-start sm:gap-3 sm:px-4 sm:py-4 sm:text-left lg:px-6 lg:py-5
              ${isActive
                ? `${styles.activeBg} ${styles.text} border-b-2 ${styles.border}`
                : `group text-content-primary hover:bg-[#3B212D] border-b-2 border-transparent ${activeStyles.hoverBorder}`
              }
            `}
          >
            <div className={`hidden p-1.5 min-[430px]:block sm:p-2 ${isActive ? `${styles.activeIconBg} ${styles.activeIconText}` : 'bg-content-secondary/20 text-content-secondary'}`}>
              <span className="material-symbols-outlined text-xl">{tab.icon}</span>
            </div>
            <div className="min-w-0">
              <div className={`truncate text-sm font-semibold sm:text-base ${isActive ? styles.text : 'text-content-primary'}`}>
                <span className="sm:hidden">{tab.shortLabel}</span>
                <span className="hidden sm:inline">{tab.label}</span>
              </div>
              <div className={`hidden truncate text-xs md:block ${isActive ? styles.textMuted : 'text-content-secondary'}`}>{tab.sublabel}</div>
            </div>
          </button>
        )
      })}
    </div>
  )
}
