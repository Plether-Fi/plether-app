interface Tab {
  id: string
  label: string
}

interface TabsProps {
  tabs: Tab[]
  activeTab: string
  onChange: (tabId: string) => void
}

export function Tabs({ tabs, activeTab, onChange }: TabsProps) {
  return (
    <div className="flex gap-1 p-1 bg-cyber-surface-light  border border-cyber-border-glow/30">
      {tabs.map((tab) => (
        <button
          key={tab.id}
          onClick={() => { onChange(tab.id); }}
          className={`
            flex-1 px-4 py-2 text-sm font-medium transition-colors hover:underline hover:underline-offset-4
            ${
              activeTab === tab.id
                ? 'bg-cyber-surface-dark text-cyber-neon-green border border-cyber-neon-green/50'
                : 'text-cyber-text-secondary hover:bg-[#3B212D] hover:text-[#FFAB96]'
            }
          `}
        >
          {tab.label}
        </button>
      ))}
    </div>
  )
}
