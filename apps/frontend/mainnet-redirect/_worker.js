const SEPOLIA_APP_ORIGIN = 'https://app.sepolia.plether.com';

export function redirectToSepolia(request) {
  const sourceUrl = new URL(request.url);
  const destinationUrl = new URL(SEPOLIA_APP_ORIGIN);
  destinationUrl.pathname = sourceUrl.pathname;
  destinationUrl.search = sourceUrl.search;

  return new Response(null, {
    status: 308,
    headers: {
      'Cache-Control': 'public, max-age=3600',
      Location: destinationUrl.href,
    },
  });
}

export default {
  fetch(request) {
    return redirectToSepolia(request);
  },
};
