import { Resvg, initWasm } from '@resvg/resvg-wasm';
import resvgWasm from './resvg.wasm';
import worker, { setSvgToPngRenderer } from '../public/_worker.js';

let wasmInitPromise = null;

function ensureResvgWasm() {
  if (!wasmInitPromise) {
    wasmInitPromise = initWasm(resvgWasm).catch((error) => {
      wasmInitPromise = null;
      throw error;
    });
  }
  return wasmInitPromise;
}

setSvgToPngRenderer(async (svg, fontBuffers) => {
  await ensureResvgWasm();
  const resvg = new Resvg(svg, {
    fitTo: { mode: 'width', value: 1200 },
    font: {
      fontBuffers: Array.isArray(fontBuffers) ? fontBuffers : [],
      loadSystemFonts: false,
      defaultFontFamily: 'Uncut Sans',
      sansSerifFamily: 'Uncut Sans',
      monospaceFamily: 'JetBrains Mono',
    },
  });
  const rendered = resvg.render();
  return rendered.asPng();
});

export default worker;
export { fetchOriginWithResilience } from '../public/_worker.js';
