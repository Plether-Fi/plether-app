import '@fontsource/uncut-sans/400.css'
import '@fontsource/uncut-sans/500.css'
import '@fontsource/uncut-sans/600.css'
import '@fontsource/uncut-sans/700.css'
import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'
import { App } from './App'
import './styles.css'

createRoot(document.getElementById('root')!).render(
  <StrictMode>
    <App />
  </StrictMode>
)
