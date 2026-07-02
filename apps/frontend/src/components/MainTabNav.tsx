type TabId = 'trade' | 'leverage' | 'lending'

interface TabConfig {
  id: TabId
  label: string
  sublabel: string
  icon: string
}

const tabs: TabConfig[] = [
  { id: 'trade', label: 'Dollar Hedge', sublabel: 'Spot trading', icon: 'security' },
  { id: 'leverage', label: 'Leverage', sublabel: 'Margin trading', icon: 'trending_up' },
  { id: 'lending', label: 'Lending', sublabel: 'Supply and borrow USDC', icon: 'grass' },
]

const tabStyles = {
  trade: {
    border: 'border-brand-peach',
    hoverBorder: 'hover:border-[#FFAB96]',
    bg: 'bg-brand-peach/20',
    text: 'text-brand-peach',
    textMuted: 'text-brand-peach/70',
    hoverText: 'group-hover:text-[#FFAB96]',
    hoverTextMuted: 'group-hover:text-[#FFAB96]/70',
  },
  leverage: {
    border: 'border-brand-orange',
    hoverBorder: 'hover:border-brand-orange',
    bg: 'bg-brand-orange/20',
    text: 'text-brand-orange',
    textMuted: 'text-brand-orange/70',
    hoverText: 'group-hover:text-brand-orange',
    hoverTextMuted: 'group-hover:text-brand-orange/70',
  },
  lending: {
    border: 'border-positive',
    hoverBorder: 'hover:border-positive',
    bg: 'bg-positive/20',
    text: 'text-positive',
    textMuted: 'text-positive/70',
    hoverText: 'group-hover:text-positive',
    hoverTextMuted: 'group-hover:text-positive/70',
  },
}

export interface MainTabNavProps {
  activeTab: TabId
  onTabChange: (tab: TabId) => void
}

export function MainTabNav({ activeTab, onTabChange }: MainTabNavProps) {
  const activeStyles = tabStyles[activeTab]

  return (
    <div className={`flex flex-col sm:flex-row border-b-2 ${activeStyles.border}`}>
      {tabs.map((tab) => {
        const isActive = activeTab === tab.id
        const styles = tabStyles[tab.id]
        return (
          <button
            key={tab.id}
            onClick={() => { onTabChange(tab.id); }}
            className={`
              flex-1 flex items-center gap-3 px-6 py-5 text-left transition-colors -mb-[2px] cursor-pointer
              ${isActive
                ? `bg-surface-muted border-b-2 ${styles.border}`
                : `group hover:bg-[#3B212D] border-b-2 border-transparent ${activeStyles.hoverBorder}`
              }
            `}
          >
            <div className={`p-2 ${isActive ? `${styles.bg} ${styles.text}` : `bg-content-secondary/20 text-content-secondary ${styles.hoverText}`}`}>
              <span className="material-symbols-outlined text-xl">{tab.icon}</span>
            </div>
            <div>
              <div className={`font-semibold ${isActive ? styles.text : `text-content-primary ${styles.hoverText}`}`}>{tab.label}</div>
              <div className={`text-xs ${isActive ? styles.textMuted : `text-content-secondary ${styles.hoverTextMuted}`}`}>{tab.sublabel}</div>
            </div>
          </button>
        )
      })}
    </div>
  )
}
