# Frontend spacing

Use Tailwind's existing 4px spacing unit. Small 2px increments are appropriate
for icon alignment, badges, and labels. Choose spacing by the element's role;
compact controls and full panels should not have identical padding.

## Shared layout rules

The utilities in `src/index.css` own responsive layout spacing:

| Role | Utility | Below 640px | 640px and above | 1024px and above |
| --- | --- | --- | --- | --- |
| Page, header, footer, banner gutters | `page-gutter` | 16px | 24px | 32px |
| Page sections | `page-stack` | 24px gap | 32px gap | 32px gap |
| Panel or modal body | `panel-padding` | 16px | 24px | 24px |
| Panel header, footer, table-cell horizontal inset | `panel-padding-x` | 16px | 24px | 24px |

`page-stack` is a flex column: it owns the gap between rendered sections,
including sections returned through React fragments. Do not add exterior
vertical margins to its children. Page width constraints remain independent.

Panel headers and footers generally use `py-3 sm:py-4` or `py-4` depending on
content density. Their horizontal insets must match the body. Dense perps panels
use `panel-padding-x py-3 sm:py-4`; full card bodies use `panel-padding`.

For internal content, use 4–8px for labels and helpers, 8–12px for related rows,
16px between compact sections, and 20–24px between form or modal groups. Nested
input previews and notices use `p-3 sm:p-4`; fixed compact tiles can use `p-4`.
The `Card` sizes remain explicit: small 12px, medium 16px, large responsive
16/24px. Spacious empty states may use `p-6 sm:p-8` or `p-6 sm:p-12`.

Give each vertical interval one owner: either a parent `gap`/`space-y`, or a
child margin. Group an input and its Max action with `space-y-2`, rather than
subtracting from the surrounding form's spacing with a negative margin.

## Intentional exceptions

- Input end adornments reserve space with `pr-7`, `pr-20`, or `pr-24`.
- Close buttons use negative margins to preserve their 44px hit area while
  aligning the icon with the panel edge.
- Joined borders and tab underlines overlap by 1–2px. `MainTabNav` uses
  `-mb-0.5`; perps joined panels use `-mb-px`.
- Modal sheets and transaction sheets retain safe-area padding. The footer owns
  clearance for the fixed 56px mobile navigation, its 1px border, and the device
  safe area. Navigation height and clearance share `--spacing-mobile-nav`; main
  content does not reserve it a second time.
- Perps uses equal outer padding and workspace panel gaps: 16px below 640px,
  24px from 640px upward. This keeps the instrument panel's top and left gutters
  equal to its bottom and right gaps. Desktop float
  widths and their 24px gap calculation must change together.
- Chart coordinates, SVG insets, sticky-section scroll margins, and scrollbar
  clearance are functional geometry and remain independent of panel padding.
- Storybook canvas/decorator spacing is presentation scaffolding. Stories that
  render production components inherit these utilities. Insights stories import
  a separate app and do not define this app's spacing conventions.

## Audit scope and findings

The September 2026 audit inventoried margin, padding, gap, and space utilities
throughout frontend source, including all nine page components, shared UI,
wallet and layout components, perps and spot widgets, transaction flows, chart
containers, and documentation components. It also checked CSS and inline styles.

Corrected inconsistent 8/12/16px mobile shell gutters, 20px panel insets mixed
with 16/24px insets, oversized dashboard/mint widget padding, competing section
margins, mismatched transaction-modal insets, and duplicated mobile-nav clearance.
Compact controls, border alignment, chart geometry, and hit-area spacing were
reviewed separately and retained where intentional.

## Validation

- Production build and ESLint pass; Vite reports its bundle-size advisory.
- Existing component and vault tests pass: 21 files, 231 tests.
- All nine routes checked at 320, 375, 768, and 1440px: no horizontal page
  overflow; standard page, header, and footer gutters match. Perps uses the
  equal workspace insets described above. Route checks use disconnected
  wallets and a controlled unavailable API response.
- Representative Storybook views checked at 375, 768, and 1440px: vault overview
  and detail, instrument panel, trade ticket, account position, dashboard tiles,
  and scrollable modal. Fixture data covers populated panels without transacting.
- Computed panel insets are 16px on phones and 24px at larger widths; page gaps
  are 24px and 32px respectively. Mobile footer clearance was checked separately.
