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
    <div className={`flex flex-col sm:flex-row border-b-2 ${activeStyles.border}`}>
      {tabs.map((tab) => {
        const isActive = activeTab === tab.id
        const styles = tabStyles[tab.id]
        return (
          <button
            key={tab.id}
            onClick={() => { onTabChange(tab.id); }}
            className={`
              flex-1 flex items-center gap-3 px-6 py-5 text-left transition-colors -mb-[2px] cursor-pointer hover:underline hover:underline-offset-4 hover:decoration-current
              ${isActive
                ? `${styles.activeBg} ${styles.text} border-b-2 ${styles.border}`
                : `group text-content-primary hover:bg-[#3B212D] border-b-2 border-transparent ${activeStyles.hoverBorder}`
              }
            `}
          >
            <div className={`p-2 ${isActive ? `${styles.activeIconBg} ${styles.activeIconText}` : 'bg-content-secondary/20 text-content-secondary'}`}>
              <span className="material-symbols-outlined text-xl">{tab.icon}</span>
            </div>
            <div>
              <div className={`font-semibold ${isActive ? styles.text : 'text-content-primary'}`}>{tab.label}</div>
              <div className={`text-xs ${isActive ? styles.textMuted : 'text-content-secondary'}`}>{tab.sublabel}</div>
            </div>
          </button>
        )
      })}
    </div>
  )
}
