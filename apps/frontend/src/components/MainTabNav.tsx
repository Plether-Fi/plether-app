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
    border: 'border-cyber-bright-blue',
    hoverBorder: 'hover:border-cyber-bright-blue',
    shadow: 'shadow-cyber-bright-blue/50',
    bg: 'bg-cyber-bright-blue/20',
    text: 'text-cyber-bright-blue',
    textMuted: 'text-cyber-bright-blue/70',
    hoverText: 'group-hover:text-cyber-bright-blue',
    hoverTextMuted: 'group-hover:text-cyber-bright-blue/70',
  },
  leverage: {
    border: 'border-cyber-electric-fuchsia',
    hoverBorder: 'hover:border-cyber-electric-fuchsia',
    shadow: 'shadow-cyber-electric-fuchsia/50',
    bg: 'bg-cyber-electric-fuchsia/20',
    text: 'text-cyber-electric-fuchsia',
    textMuted: 'text-cyber-electric-fuchsia/70',
    hoverText: 'group-hover:text-cyber-electric-fuchsia',
    hoverTextMuted: 'group-hover:text-cyber-electric-fuchsia/70',
  },
  lending: {
    border: 'border-cyber-neon-green',
    hoverBorder: 'hover:border-cyber-neon-green',
    shadow: 'shadow-cyber-neon-green/50',
    bg: 'bg-cyber-neon-green/20',
    text: 'text-cyber-neon-green',
    textMuted: 'text-cyber-neon-green/70',
    hoverText: 'group-hover:text-cyber-neon-green',
    hoverTextMuted: 'group-hover:text-cyber-neon-green/70',
  },
}

export interface MainTabNavProps {
  activeTab: TabId
  onTabChange: (tab: TabId) => void
}

export function MainTabNav({ activeTab, onTabChange }: MainTabNavProps) {
  const activeStyles = tabStyles[activeTab]

  return (
    <div className={`flex flex-col sm:flex-row border-b-2 ${activeStyles.border} shadow-[0_2px_20px_-2px] ${activeStyles.shadow}`}>
      {tabs.map((tab) => {
        const isActive = activeTab === tab.id
        const styles = tabStyles[tab.id]
        return (
          <button
            key={tab.id}
            onClick={() => { onTabChange(tab.id); }}
            className={`
              flex-1 flex items-center gap-3 px-6 py-5 text-left transition-colors -mb-[2px]
              ${isActive
                ? `bg-cyber-surface-light border-b-2 ${styles.border}`
                : `group hover:bg-cyber-surface-light border-b-2 border-transparent ${activeStyles.hoverBorder} opacity-60 hover:opacity-100`
              }
            `}
          >
            <div className={`p-2 ${isActive ? `${styles.bg} ${styles.text}` : `bg-cyber-text-secondary/20 text-cyber-text-secondary ${styles.hoverText}`}`}>
              <span className="material-symbols-outlined text-xl">{tab.icon}</span>
            </div>
            <div>
              <div className={`font-semibold ${isActive ? styles.text : `text-cyber-text-primary ${styles.hoverText}`}`}>{tab.label}</div>
              <div className={`text-xs ${isActive ? styles.textMuted : `text-cyber-text-secondary ${styles.hoverTextMuted}`}`}>{tab.sublabel}</div>
            </div>
          </button>
        )
      })}
    </div>
  )
}
