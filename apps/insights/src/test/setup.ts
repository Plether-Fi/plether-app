import '@testing-library/jest-dom/vitest'
import { Storage } from 'happy-dom'
import { beforeEach } from 'vitest'

// Node 25 exposes a localStorage global that can mask Happy DOM's storage.
// Use the browser implementation for both access paths, isolated per test file.
if (typeof window !== 'undefined') {
  const browserLocalStorage = new Storage()
  for (const target of [globalThis, window]) {
    Object.defineProperty(target, 'localStorage', {
      configurable: true,
      value: browserLocalStorage,
    })
  }

  beforeEach(() => {
    browserLocalStorage.clear()
  })
}
