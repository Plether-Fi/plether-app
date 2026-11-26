import { StrictMode } from 'react'
import { renderToString } from 'react-dom/server'
import { App, getRouteMeta } from './App'

export function render(pathname: string) {
  const html = renderToString(
    <StrictMode>
      <App pathname={pathname} />
    </StrictMode>,
  )
  const meta = getRouteMeta(pathname)

  return { html, meta }
}
