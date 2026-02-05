import { type ReactNode } from 'react'

interface TabPanelProps {
  children: ReactNode
  isActive: boolean
}

export function TabPanel({ children, isActive }: TabPanelProps) {
  if (!isActive) return null
  return <div>{children}</div>
}
