import type { ReactNode } from 'react'
import {
  PerpsAaRuntimeContext,
  type PerpsAaSmartAccountRuntime,
} from './runtimeContext'

export function PerpsAaRuntimeProvider({
  children,
  runtime,
}: {
  children: ReactNode
  runtime?: PerpsAaSmartAccountRuntime
}) {
  return (
    <PerpsAaRuntimeContext value={runtime}>
      {children}
    </PerpsAaRuntimeContext>
  )
}
