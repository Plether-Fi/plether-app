import type { Meta, StoryObj } from '@storybook/react-vite'
import { expect, userEvent, within } from 'storybook/test'
import { Tooltip, InfoTooltip } from '../components/ui'
import { Button } from '../components/ui/Button'
import { DOCS_LINKS } from '../config/docs'

const EXAMPLE_DOCS_LINK = DOCS_LINKS.positionLeverage

const meta: Meta<typeof Tooltip> = {
  title: 'UI/Tooltip',
  component: Tooltip,
  tags: ['autodocs'],
  argTypes: {
    position: {
      control: 'select',
      options: ['top', 'bottom', 'left', 'right'],
    },
  },
  decorators: [
    (Story) => (
      <div className="flex items-center justify-center p-20">
        <Story />
      </div>
    ),
  ],
}

export default meta
type Story = StoryObj<typeof meta>

export const Top: Story = {
  args: {
    content: 'Tooltip on top',
    docsLink: EXAMPLE_DOCS_LINK,
    position: 'top',
    children: <Button>Hover me</Button>,
  },
}

export const Bottom: Story = {
  args: {
    content: 'Tooltip on bottom',
    docsLink: EXAMPLE_DOCS_LINK,
    position: 'bottom',
    children: <Button>Hover me</Button>,
  },
}

export const Left: Story = {
  args: {
    content: 'Tooltip on left',
    docsLink: EXAMPLE_DOCS_LINK,
    position: 'left',
    children: <Button>Hover me</Button>,
  },
}

export const Right: Story = {
  args: {
    content: 'Tooltip on right',
    docsLink: EXAMPLE_DOCS_LINK,
    position: 'right',
    children: <Button>Hover me</Button>,
  },
}

export const WithRichContent: Story = {
  args: {
    content: (
      <div>
        <strong className="text-positive">Pro tip:</strong>
        <br />
        Use leverage responsibly
      </div>
    ),
    docsLink: EXAMPLE_DOCS_LINK,
    position: 'top',
    children: <Button variant="secondary">Hover for tip</Button>,
  },
}

export const InfoTooltipExample: Story = {
  render: () => (
    <div className="flex items-center gap-2">
      <span className="text-content-primary">Health Factor</span>
      <InfoTooltip
        content="Ratio of collateral to debt. Below 1.0 risks liquidation."
        docsLink={EXAMPLE_DOCS_LINK}
      />
    </div>
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    const page = within(canvasElement.ownerDocument.body)
    const trigger = canvas.getByLabelText('More information')

    await userEvent.hover(trigger)
    const docsLink = await page.findByRole('link', { name: `Read: ${EXAMPLE_DOCS_LINK.title}` })

    expect(docsLink).toBeVisible()
    expect(docsLink).toHaveAttribute('href', EXAMPLE_DOCS_LINK.href)

    docsLink.addEventListener('click', (event) => {
      event.preventDefault()
    }, { once: true })

    await userEvent.unhover(trigger)
    await userEvent.hover(docsLink)
    await userEvent.click(docsLink)

    expect(page.getByRole('tooltip')).toBeInTheDocument()
  },
}

export const AllPositions: Story = {
  render: () => (
    <div className="grid grid-cols-2 gap-8">
      <Tooltip content="Top tooltip" position="top" docsLink={EXAMPLE_DOCS_LINK}>
        <Button variant="secondary">Top</Button>
      </Tooltip>
      <Tooltip content="Bottom tooltip" position="bottom" docsLink={EXAMPLE_DOCS_LINK}>
        <Button variant="secondary">Bottom</Button>
      </Tooltip>
      <Tooltip content="Left tooltip" position="left" docsLink={EXAMPLE_DOCS_LINK}>
        <Button variant="secondary">Left</Button>
      </Tooltip>
      <Tooltip content="Right tooltip" position="right" docsLink={EXAMPLE_DOCS_LINK}>
        <Button variant="secondary">Right</Button>
      </Tooltip>
    </div>
  ),
}
