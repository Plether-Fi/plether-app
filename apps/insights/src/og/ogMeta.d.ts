export interface RouteOgMeta {
  title: string
  description: string
  imageUrl: string
  imageAlt: string
  canonicalUrl: string
}

export type OgImageRoute =
  | { kind: 'liquidity' }
  | { kind: 'wallet'; account: string }
  | { kind: 'divisions'; account: string }

export declare function resolveRouteOgMeta(url: URL): RouteOgMeta
export declare function parseOgImageRoute(pathname: string): OgImageRoute | null
export declare function isValidTradeIdParam(value: unknown): boolean
export declare function rewriteHtmlOgTags(html: string, meta: RouteOgMeta): string
