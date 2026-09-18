// Model a translator/extension replacing React-owned text, without a global
// DOM monkey-patch that would conceal real reconciliation bugs.
export function replaceTextNodes(element: Element): void {
  const walker = document.createTreeWalker(element, NodeFilter.SHOW_TEXT)
  const nodes: Text[] = []
  while (walker.nextNode()) nodes.push(walker.currentNode as Text)
  for (const node of nodes) {
    const replacement = document.createElement('font')
    replacement.textContent = node.textContent
    node.replaceWith(replacement)
  }
}
