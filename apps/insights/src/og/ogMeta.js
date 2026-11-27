const ADDRESS_RE = /^0x[0-9a-fA-F]{40}$/;
const TX_HASH_RE = /^0x[0-9a-fA-F]{64}$/;
const TRADE_ID_RE = /^0x[0-9a-fA-F]{64}:\d+$/;
const LOG_INDEX_RE = /^\d+$/;

function shortAddr(address) {
  return `${address.slice(0, 6)}...${address.slice(-4)}`;
}

function escapeHtmlAttr(value) {
  return String(value ?? '')
    .replaceAll('&', '&amp;')
    .replaceAll('<', '&lt;')
    .replaceAll('>', '&gt;')
    .replaceAll('"', '&quot;')
    .replaceAll("'", '&#39;');
}

export function resolveRouteOgMeta(url) {
  const origin = url.origin;
  const pathname = url.pathname;
  const canonicalUrl = `${origin}${pathname}`;
  const defaultImage = `${origin}/og/default.png`;

  const walletMatch = pathname.match(/^\/wallets\/(0x[0-9a-fA-F]{40})\/?$/);
  if (walletMatch) {
    const addr = walletMatch[1].toLowerCase();
    const label = shortAddr(addr);
    const tradeParam = url.searchParams.get('trade');
    const cardParam = url.searchParams.get('card');

    if (tradeParam && TRADE_ID_RE.test(tradeParam)) {
      const encodedTrade = encodeURIComponent(tradeParam);
      return {
        title: `Closed Position PnL · ${label} — Plether Insights`,
        description: `Verified on-chain perpetual trade receipt and realized PnL for ${label} on Plether Insights.`,
        imageUrl: `${origin}/og/wallet/${addr}.png?trade=${encodedTrade}`,
        imageAlt: `Plether Insights closed position PnL card for ${label}`,
        canonicalUrl: `${origin}/wallets/${addr}?trade=${encodedTrade}`,
      };
    }

    if (cardParam === 'position') {
      return {
        title: `Live Perps Position · ${label} — Plether Insights`,
        description: `Live on-chain perpetual position, unrealized PnL, and margin health for ${label} on Plether Insights.`,
        imageUrl: `${origin}/og/wallet/${addr}.png?card=position`,
        imageAlt: `Plether Insights live perpetual position card for ${label}`,
        canonicalUrl: `${origin}/wallets/${addr}?card=position`,
      };
    }

    return {
      title: `Trading Account ${label} — Plether Insights`,
      description: `Verified on-chain realized PnL, balance history, executions, and LP activity for ${label} on Plether Insights.`,
      imageUrl: `${origin}/og/wallet/${addr}.png`,
      imageAlt: `Plether Insights trading account summary card for ${label}`,
      canonicalUrl: `${origin}/wallets/${addr}`,
    };
  }

  const txMatch = pathname.match(/^\/tx\/(0x[0-9a-fA-F]{64})\/?$/);
  if (txMatch && TX_HASH_RE.test(txMatch[1])) {
    const hash = txMatch[1].toLowerCase();
    const label = shortAddr(hash);
    const logParam = url.searchParams.get('log');
    const canonicalTxUrl =
      logParam && LOG_INDEX_RE.test(logParam)
        ? `${origin}/tx/${hash}?log=${logParam}`
        : `${origin}/tx/${hash}`;
    return {
      title: `Transaction ${label} — Plether Insights`,
      description: `Verified on-chain execution receipt, fee & VPI breakdown, and settlement reconciliation for transaction ${label} on Plether Insights.`,
      imageUrl: defaultImage,
      imageAlt: `Plether Insights transaction receipt for ${label}`,
      canonicalUrl: canonicalTxUrl,
    };
  }

  const divWalletMatch = pathname.match(/^\/divisions\/wallets\/(0x[0-9a-fA-F]{40})\/?$/);
  if (divWalletMatch) {
    const addr = divWalletMatch[1].toLowerCase();
    const label = shortAddr(addr);
    return {
      title: `Plether Divisions · ${label} — Plether Insights`,
      description: `Weekly Plether Divisions standing, verified return, and tier badge for ${label} on Plether Insights.`,
      imageUrl: `${origin}/og/divisions/${addr}.png`,
      imageAlt: `Plether Divisions standing card for ${label}`,
      canonicalUrl,
    };
  }

  const epochMatch = pathname.match(/^\/liquidity\/epochs\/(\d+)\/?$/);
  if (epochMatch) {
    const epochNum = epochMatch[1];
    return {
      title: `LP Epoch #${epochNum} — Plether Insights`,
      description: `LP epoch #${epochNum} settlement transactions, Senior & Junior vault snapshots, decoded SettlementMonitorLens bitmasks, and return attribution audit.`,
      imageUrl: defaultImage,
      imageAlt: `Plether Insights — LP Epoch #${epochNum}`,
      canonicalUrl: `${origin}/liquidity/epochs/${epochNum}`,
    };
  }

  if (pathname === '/liquidity' || pathname.startsWith('/liquidity/')) {
    return {
      title: 'LP Epoch Settlement & Forensics — Plether Insights',
      description: 'Real-time LP epoch settlement readiness, hourly snapshot freshness, and Junior vault return attribution audit.',
      imageUrl: defaultImage,
      imageAlt: 'Plether Insights — LP Epoch Settlement & Forensics',
      canonicalUrl,
    };
  }

  if (pathname === '/execution' || pathname.startsWith('/execution/')) {
    return {
      title: 'Execution Quality & Trade Feed — Plether Insights',
      description: 'Order fill reliability, keeper execution latency percentiles, TP/SL protection metrics, and verified trade receipts.',
      imageUrl: defaultImage,
      imageAlt: 'Plether Insights — Execution Quality & Trade Feed',
      canonicalUrl,
    };
  }

  const keeperMatch = pathname.match(/^\/keepers\/(0x[0-9a-fA-F]{40})\/?$/);
  if (keeperMatch) {
    const addr = keeperMatch[1].toLowerCase();
    const label = shortAddr(addr);
    return {
      title: `Keeper ${label} — Plether Insights`,
      description: `Live on-chain execution share, latency distribution, ETH runway, bounties earned, and recent transactions for permissionless keeper ${label} on Plether Insights.`,
      imageUrl: defaultImage,
      imageAlt: `Plether Insights — Permissionless Keeper ${label}`,
      canonicalUrl: `${origin}/keepers/${addr}`,
    };
  }

  if (pathname === '/keepers' || pathname.startsWith('/keepers/')) {
    return {
      title: 'Permissionless Keepers — Plether Insights',
      description: 'Live activity, latency, ETH balances, and bounties across every permissionless keeper executing Plether Perps.',
      imageUrl: defaultImage,
      imageAlt: 'Plether Insights — Permissionless Keepers',
      canonicalUrl,
    };
  }

  if (pathname === '/sponsored' || pathname.startsWith('/sponsored/')) {
    return {
      title: 'Sponsored Transactions & Paymaster Health — Plether Insights',
      description: 'Live ERC-4337 Paymaster EntryPoint deposit runway, issuance kill-switch, budget caps, AA reconciler lag, Alto bundler readiness, and UserOperations.',
      imageUrl: defaultImage,
      imageAlt: 'Plether Insights — Sponsored Transactions & Paymaster Health',
      canonicalUrl,
    };
  }

  if (pathname === '/protocol' || pathname.startsWith('/protocol/')) {
    return {
      title: 'Protocol Contracts & Parameters — Plether Insights',
      description: 'Live on-chain bytecode hash verification, economic risk parameters vs. release defaults, and emergency pause controls for Plether Perpetuals.',
      imageUrl: defaultImage,
      imageAlt: 'Plether Insights — Protocol Contracts & Parameters',
      canonicalUrl,
    };
  }

  if (pathname === '/infra' || pathname.startsWith('/infra/')) {
    return {
      title: 'AWS Infrastructure & Action Readiness — Plether Insights',
      description: 'Live AWS ECS, RDS PostgreSQL, Arbitrum Nitro EC2 node, Alto bundler, Pyth oracle, and keeper health translated into user action readiness.',
      imageUrl: defaultImage,
      imageAlt: 'Plether Insights — AWS Infrastructure & Action Readiness',
      canonicalUrl,
    };
  }

  if (
    pathname === '/divisions' ||
    pathname.startsWith('/divisions/') ||
    pathname === '/results' ||
    pathname === '/methodology' ||
    pathname === '/register' ||
    pathname.startsWith('/archive/')
  ) {
    return {
      title: 'Plether Divisions — Weekly Perps Competition',
      description: 'Verified weekly perpetuals rankings, official division badges, and on-chain performance on Plether Insights.',
      imageUrl: defaultImage,
      imageAlt: 'Plether Divisions — Weekly Perps Competition',
      canonicalUrl,
    };
  }

  return {
    title: 'Plether Insights — Protocol, Trader & LP Analytics',
    description: 'Real-time Perps execution reliability, LP settlement engine audit, public Trading Account inspector, and Plether Divisions.',
    imageUrl: defaultImage,
    imageAlt: 'Plether Insights — Protocol, Trader & LP Analytics',
    canonicalUrl,
  };
}

export function parseOgImageRoute(pathname) {
  const walletMatch = pathname.match(/^\/og\/wallet\/(0x[0-9a-fA-F]{40})\.png$/);
  if (walletMatch && ADDRESS_RE.test(walletMatch[1])) {
    return { kind: 'wallet', account: walletMatch[1].toLowerCase() };
  }
  const divMatch = pathname.match(/^\/og\/divisions\/(0x[0-9a-fA-F]{40})\.png$/);
  if (divMatch && ADDRESS_RE.test(divMatch[1])) {
    return { kind: 'divisions', account: divMatch[1].toLowerCase() };
  }
  return null;
}

export function isValidTradeIdParam(value) {
  return typeof value === 'string' && TRADE_ID_RE.test(value);
}

export function rewriteHtmlOgTags(html, meta) {
  if (typeof html !== 'string' || !html.includes('</head>')) return html;

  // Strip existing OG / Twitter / description / title tags we replace so crawlers never see duplicates.
  const stripped = html
    .replace(/<title>[^<]*<\/title>\s*/gi, '')
    .replace(/<meta\s+(?:name|property)=["'](?:description|og:title|og:description|og:type|og:url|og:image|og:image:width|og:image:height|og:image:type|og:image:alt|twitter:card|twitter:title|twitter:description|twitter:image|twitter:image:alt)["'][^>]*>\s*/gi, '');

  const t = escapeHtmlAttr(meta.title);
  const d = escapeHtmlAttr(meta.description);
  const u = escapeHtmlAttr(meta.canonicalUrl);
  const img = escapeHtmlAttr(meta.imageUrl);
  const alt = escapeHtmlAttr(meta.imageAlt || meta.title);

  const injected = [
    `    <title>${t}</title>`,
    `    <meta name="description" content="${d}" />`,
    `    <meta property="og:title" content="${t}" />`,
    `    <meta property="og:description" content="${d}" />`,
    `    <meta property="og:type" content="website" />`,
    `    <meta property="og:url" content="${u}" />`,
    `    <meta property="og:image" content="${img}" />`,
    `    <meta property="og:image:type" content="image/png" />`,
    `    <meta property="og:image:width" content="1200" />`,
    `    <meta property="og:image:height" content="630" />`,
    `    <meta property="og:image:alt" content="${alt}" />`,
    `    <meta name="twitter:card" content="summary_large_image" />`,
    `    <meta name="twitter:title" content="${t}" />`,
    `    <meta name="twitter:description" content="${d}" />`,
    `    <meta name="twitter:image" content="${img}" />`,
    `    <meta name="twitter:image:alt" content="${alt}" />`,
  ].join('\n');

  return stripped.replace('</head>', `${injected}\n  </head>`);
}
