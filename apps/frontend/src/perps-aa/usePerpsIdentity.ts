import { use } from 'react'
import {
  PerpsIdentityContext,
  type PerpsIdentityContextValue,
} from './PerpsIdentityContext'

export function usePerpsIdentity(): PerpsIdentityContextValue {
  const context = use(PerpsIdentityContext)
  if (context === undefined) {
    throw new Error(
      'usePerpsIdentity must be used within a PerpsIdentityProvider'
    )
  }
  return context
}
