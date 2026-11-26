import { defineConfig, type Plugin } from 'vite'
import react from '@vitejs/plugin-react-swc'

const HERO_FONT_PATTERNS = [
  /uncut-sans-latin-700-normal-[^/]+\.woff2$/,
  /uncut-sans-latin-600-normal-[^/]+\.woff2$/,
]

function preloadHeroFonts(): Plugin {
  return {
    name: 'plether-preload-hero-fonts',
    transformIndexHtml: {
      order: 'post',
      handler(html, ctx) {
        if (!ctx.bundle) {
          return html
        }

        const preloadTags = Object.keys(ctx.bundle)
          .filter((fileName) => HERO_FONT_PATTERNS.some((pattern) => pattern.test(fileName)))
          .sort()
          .map((fileName) => ({
            tag: 'link',
            attrs: {
              rel: 'preload',
              as: 'font',
              type: 'font/woff2',
              href: `/${fileName}`,
              crossorigin: '',
            },
            injectTo: 'head' as const,
          }))

        return { html, tags: preloadTags }
      },
    },
  }
}

export default defineConfig({
  plugins: [react(), preloadHeroFonts()],
})
