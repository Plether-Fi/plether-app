import { Fragment, memo, type ReactNode } from 'react'
import orderRouterSource from './solidity/OrderRouter.sol?raw'

const SOLIDITY_KEYWORDS = new Set([
  'address',
  'bool',
  'break',
  'bytes',
  'constant',
  'constructor',
  'contract',
  'delete',
  'else',
  'emit',
  'enum',
  'event',
  'external',
  'false',
  'for',
  'function',
  'if',
  'immutable',
  'import',
  'internal',
  'is',
  'library',
  'mapping',
  'memory',
  'modifier',
  'override',
  'private',
  'public',
  'pure',
  'require',
  'return',
  'returns',
  'solidity',
  'storage',
  'struct',
  'true',
  'uint256',
  'uint64',
  'uint8',
  'using',
  'view',
])

const SOLIDITY_TOKEN_PATTERN = /(\/\/.*|"(?:\\.|[^"\\])*"|'(?:\\.|[^'\\])*'|\b[A-Za-z_][A-Za-z0-9_]*\b|\b\d+(?:_\d+)*\b)/g

function highlightSolidityLine(line: string, lineIndex: number) {
  const nodes: ReactNode[] = []
  let cursor = 0
  let match: RegExpExecArray | null

  SOLIDITY_TOKEN_PATTERN.lastIndex = 0

  while ((match = SOLIDITY_TOKEN_PATTERN.exec(line)) !== null) {
    const [token] = match

    if (match.index > cursor) {
      nodes.push(line.slice(cursor, match.index))
    }

    let tokenClass = ''

    if (token.startsWith('//')) {
      tokenClass = 'token-comment'
    } else if (token.startsWith('"') || token.startsWith("'")) {
      tokenClass = 'token-string'
    } else if (/^\d/.test(token)) {
      tokenClass = 'token-number'
    } else if (SOLIDITY_KEYWORDS.has(token)) {
      tokenClass = 'token-keyword'
    }

    nodes.push(
      tokenClass ? (
        <span className={tokenClass} key={`${lineIndex}-${match.index}`}>
          {token}
        </span>
      ) : (
        token
      ),
    )

    cursor = match.index + token.length

    if (token.startsWith('//')) {
      break
    }
  }

  if (cursor < line.length) {
    nodes.push(line.slice(cursor))
  }

  return nodes
}

// Highlight once when this lazy chunk loads rather than on every React render.
const HIGHLIGHTED_LINES = orderRouterSource.split('\n').map((line, index) => (
  <Fragment key={index}>
    {highlightSolidityLine(line, index)}
    {'\n'}
  </Fragment>
))

const SolidityCodeBlock = memo(function SolidityCodeBlock() {
  return (
    <pre className="source-code" aria-label="OrderRouter Solidity source">
      <code>{HIGHLIGHTED_LINES}</code>
    </pre>
  )
})

export default SolidityCodeBlock
