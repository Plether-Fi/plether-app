import { lazy } from 'react'

export function isModuleLoadError(error: unknown): boolean {
  return error instanceof Error && /^(Failed to fetch dynamically imported module|error loading dynamically imported module|Importing a module script failed|Unable to preload CSS for )/i.test(error.message)
}

// Retry only module fetching, once. Never reload the page automatically: a
// wallet prompt or transaction submission may be active. Persistent/stale-asset
// failures go to the root recovery screen, preserving all local recovery data.
export async function retryModuleLoad<T>(load: () => Promise<T>): Promise<T> {
  try {
    return await load()
  } catch (error) {
    if (!isModuleLoadError(error)) throw error
    await new Promise(resolve => setTimeout(resolve, 500))
    return load()
  }
}

export const lazyWithRetry: typeof lazy = load => lazy(() => retryModuleLoad(load))
