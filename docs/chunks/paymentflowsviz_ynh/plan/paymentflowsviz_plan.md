# Yorkshire and the Humber payment flows: plan for a single-page visualisation

*Written 23 November 2026 by Claude Code, and updated the same day with Dan's decisions (section 6) and three follow-up tests (sections 2.7 to 2.9). Section 10, added later that day, scopes three further changes that are not built. Builds on [`llm_output/payment_flows_handover.md`](../../../../llm_output/payment_flows_handover.md): read its sections 1 to 3 first for what the data is and what's hidden in it. Every number below comes from the scripts in [`../tests/`](../tests/README.md), run on the September and October 2026 editions.*

## 1. Summary

**What to build.** One web page showing payments between Yorkshire and the Humber's 77 industries (SIC divisions), month by month from January 2019 to September 2026, that can be stepped or played. Industries are drawn as circles and payments as links between them.

**The four questions, answered.**

1. **Is there a supplier-below-buyer structure? Yes, as a gradient, not as tiers.** The flows put industries in a stable order from suppliers to buyers. 71% of the money runs the "right" way down it (50% would be chance), the order hardly moves from month to month, and it agrees with the national input-output table about as well as could be hoped. But the order is loose: the UK's real supply chain is just as loose on the same measure, so this is how the economy is, not a data fault. Divisions are the right level. Sections blur the order, and nothing finer exists regionally.
2. **Showing the order, the industrial strategy and sections together.** Vertical position carries the order. Sector groups work best as columns. The industrial strategy and single sector groups work as a highlight that picks out one set at a time, with part-covered divisions shown as part-filled. Colour is kept for one job: which way money runs. Outside flows are three times the inside ones, so they can't be links: a headline strip plus an optional pale halo on each circle shows their scale. Five payment groups (industries that pay each other more than their sizes predict) hold up across the years and can be named and shown as columns.
3. **Files** are in `plan/` (this document) and `tests/` (code, outputs and a working prototype).
4. **Drawing approach: 2D canvas with D3. WebGL isn't needed.** Canvas drew every link with moving dots at 60 frames a second in the test. The whole dataset is 2 MB.

**Decisions** are recorded in section 6: widths start as each month's share with a switch to pounds, the four intermediary divisions get a switch, the page is public, payment groups are named, and nothing is deflated. Moving dots show how many payments a link carries (section 2.8).

![Supplier order with sector-group columns, September 2026, top 500 links](../tests/outputs/screens/02_order_groups.png)

*The prototype's main view. Buyers sit higher, suppliers lower. Blue links carry money down the order, orange ones against it; a link is widest at the paying end.*

## 2. What the tests found

Unless it says otherwise, figures are for payments where both sides are registered in Yorkshire and the Humber, with the four intermediary divisions (63, 64, 66, 82) and non-industries (00, 97, 98, 99) left out, as the handover advises.

### 2.1 The data over time

- **93 months,** January 2019 to September 2026, so nearly eight years.
- **About 4,500 cells a month** inside the region (payer division by payee division). 56% are hidden.
- **Hidden months are held,** using the handover's rule: at the same cell's average published month in that year, or failing that in the nearest years. Of all cell-months, 44% are published, 29% are held, and 27% belong to cells never published in any year, which stay at zero. Holding adds 11% to the inside total (£71.0bn published, £79.0bn with held values).
- **3,210 links** (ordered pairs of divisions) have some value over the period. They're very unequal: half the value is in the top 89 links, 80% in the top 430 and 90% in the top 781.
- **No break at the edition joins.** The changes from December to January at the two joins (2022: down 7%; 2025: up 1%) are inside the range of other Januaries (down 9% to up 3%).
- **The totals swell for reasons that may not be real.** Value per month rose 72% from 2019 to 2025 and the number of payments 67%, while the average payment stayed flat at about £4,100. That's the pattern the caveats note reads as more payments being captured. Played as raw pounds, the page would mostly show this.
- **Months are noisy.** Among the top 500 links the typical change from one month to the next is 18%. A three-month average brings that to 6%. February runs 8% below trend; no other month is more than 4% off.

### 2.2 Is there a supplier-to-buyer order?

**Method.** Each industry gets a level so that, as far as the data allow, a buyer sits one level above each of its suppliers, weighted by the size of the payment (trophic levels, from MacKay, Johnson and Sansom 2020). The same method gives an incoherence score: 0 means perfect tiers, with every payment dropping exactly one level, and 1 means no order at all. As a check, each pair's two directions were swapped at random 200 times, which keeps who trades with whom but scrambles which way round.

| Network | Incoherence score | Same, directions shuffled | Money running down the order |
|----|----|----|----|
| Y&H payments, by value | 0.74 | 0.90 | 71% |
| Y&H payments, by number of payments | 0.64 | 0.93 | 77% |
| UK input-output table 2019 | 0.71 | 0.91 | 74% |

- **The order is real.** Both payment scores are well below their shuffled versions (the lowest 5% of shuffles score 0.88 for value).
- **It's as strong as the real thing.** The UK input-output table scores 0.71 on the same method. The economy's own supply chain is a gradient with a lot of two-way trade, and the payments reproduce that.
- **It agrees with the input-output table.** Levels from Y&H payments by value and from the table have a rank correlation of 0.71, across 73 groups of divisions. By number of payments it's 0.40. So value should set the order.
- **It's stable.** Each year's levels correlate 0.94 to 0.98 with the whole-period levels, and each single month's correlate 0.94 at the median (lowest month 0.89). A division's level wobbles by 0.08 from month to month, against a spread of 0.35 between divisions.
- **Held values matter for stability.** On published cells only, single months drop to 0.85 at the median and 0.67 at worst.
- **The span is short:** about 1.5 levels from bottom to top by value. Draw it as a continuous scale, not as rows.
- **A simpler measure gives nearly the same order.** Levels correlate 0.96 with each division's balance of payments received against payments made. The level is still the better choice because it's the one with the input-output comparison behind it, but the balance is an easy way to explain it on the page.

**What the order looks like.** At the bottom, receiving more than they pay: insurance and pensions, security, advertising, architecture and engineering, printing, telecoms, and near them wholesale, haulage and the building trades. At the top, paying more than they receive: education, health, food and drink service, accommodation and retail. Manufacturing divisions are spread across the whole range, from printing (0.26) to motor vehicles (1.22) and pharmaceuticals (1.35).

**Three things to say on the page about reading it.**

- **The top is partly an artefact of who's missing.** Households aren't in the data, and public funding mostly arrives from outside the region. So industries that sell to households or are publicly funded show up almost only as buyers.
- **"Supplier" means "gets paid".** That includes rent, insurance premiums and business rates, not only goods and services.
- **Tiny industries sit at the extremes on thin evidence.** Gambling tops the order on £5m of payments in eight years. Mark or clamp divisions below a size threshold (section 4.3).

### 2.3 Which SIC level?

**The regional data only exist at division (two-digit) level.** The choice is divisions, something coarser, or borrowing detail from the national five-digit file.

**Coarser is worse.** Y&H, by value:

| Grouping | Groups | Incoherence score | Same, shuffled | Gap |
|----|----|----|----|----|
| Divisions | 77 | 0.74 | 0.90 | 0.15 |
| Mixed: divisions inside C, F, G, H, M and N; sections elsewhere | 59 | 0.74 | 0.90 | 0.16 |
| Sections | 19 | 0.88 | 0.96 | 0.08 |

Sections net most of the direction away (the share of each pair's flow left after netting falls from 55% to 33%). The mixed grouping gains nothing over plain divisions.

**Finer helps a little nationally, then stops, and costs stability.** The national five-digit file for 2023, rolled up:

| Level | Industries | Links | Incoherence score | Gap to shuffled | Rank correlation of levels, 2019 against 2023 |
|----|----|----|----|----|----|
| Section | 19 | 336 | 0.92 | 0.03 | 0.91 |
| Two-digit | 81 | 4,169 | 0.84 | 0.09 | 0.90 |
| Three-digit | 250 | 19,745 | 0.81 | 0.13 | 0.76 |
| Four-digit | 565 | 47,886 | 0.80 | 0.14 | 0.75 |
| Five-digit | 661 | 56,391 | 0.79 | 0.14 | 0.73 |

- **Most of the order is already there at division level:** 69% of the variation in five-digit levels lies between divisions.
- **The divisions hiding the most variety** are retail, wholesale, warehousing and transport support, education, insurance, and food and drink service.
- **The five-digit file is the weakest source.** 68% of its rows are marked `[c]` (hidden) and 18% `[-]`; only 14% carry a value. Rolled up to divisions, its order agrees least with everything else: 0.30 with Y&H's own order and 0.48 with the input-output table, where Y&H's own order scores 0.71. What `[-]` means hasn't been checked with ONS.

**Decision: divisions throughout.** A national "inside this division" panel at three digits for the six divisions above is possible later, but I'd leave it out of the first version: it's a different place, a different period (2017 to November 2024) and a much more hidden file.

### 2.4 Layouts: supplier order, connections and a hybrid

All three were built in the prototype and looked at on the real data.

- **Supplier order with sector-group columns works best as the opening view.** Vertical position is the level. Seven columns hold the sector groups (primary and utilities, manufacturing, construction, trade and transport, business services, public services, hospitality with arts and other services). You can see at once that manufacturing runs the full height while public services cluster at the top.
- **Supplier order with free columns** also works: horizontal position is then set only by who trades with whom.
- **A plain force-directed "connections" view is a ball** with wholesale, retail and construction in the middle. Three divisions account for 31% of all payments and one pair for 10%, so the big ones pull everything in. Grouping on raw payments finds only 4 loose groups (modularity 0.20).
- **Pulling by affinity fixes that.** Affinity is a pair's payments divided by what the two industries' sizes alone would predict. It finds 7 groups in a single run (modularity 0.27), most of which make sense as supply chains.
- **A single run isn't steady enough to name.** Membership agrees only moderately from year to year (0.38 to 0.76 on a 0 to 1 scale where 1 is identical) and between repeat runs on the same data (0.66). Section 2.7 takes this further and finds five groups that do hold.
- **The hybrid is the view worth having as the second option:** vertical position pulled towards the level, horizontal position by affinity. The neighbourhoods above show up left to right while buyers stay high and suppliers low.

![Hybrid layout, top 300 links](../tests/outputs/screens/03_hybrid.png)

*Hybrid: engineering and construction materials to the right, food and professional services to the left, consumer-facing industries at the top. A few barely connected divisions drift to the edges and need reining in.*

### 2.5 Sector groups, the industrial strategy, and outside flows

**Colour.** Colouring the seven sector groups fails the dataviz skill's validator when any two circles can sit side by side (the worst pair is 3.2 apart for colour-blind readers, against a floor of 6). Blue and orange for the two link directions passes easily. So colour does one job, the direction of money, and groups are shown by position and by highlighting.

**The industrial strategy list** (the sector definitions behind `Yorkshire_n_Humber_sectors_Nov2025.html`, file `RegionalEconomicTools/data/industrialstrategy2025sectordefs.csv`) fits division-level data only partly:

| | |
|----|----|
| Divisions the list touches | 40 of 88 |
| Listed as whole divisions | 29 |
| Only partly covered, by a three to five-digit code inside them | 11 |
| In more than one of the eight sectors | 14 |
| Y&H jobs in a listed code (BRES 2024) | 27% |
| Y&H jobs in a division the list touches | 45% |

- **Clean energy has no SIC codes** and can't be shown. Defence has only two part-codes, worth 210 Y&H jobs.
- **Part coverage varies enormously:** basic metals is 85% covered (steel), energy supply 31% (electricity networks), warehousing 5% (ports), wholesale 4%, education 0.2%.
- **So show the share.** `tests/lookups/indstrat_sic2.csv` gives, for each division and strategy sector, the share of the division's Y&H jobs in the listed codes. In the prototype a highlighted circle is filled like a pie to that share, and a whole division is filled completely.

![Industrial strategy highlight](../tests/outputs/screens/06_order_groups_lens_is8_any.png)

*"Any industrial strategy sector" highlighted. Wholesale's thin wedge is its 4%; building trades and head offices are whole divisions on the list.*

**Outside flows.** Payments between Y&H and the rest of the UK, over the whole period with held values:

| | Value | Hidden cell-months |
|----|----|----|
| Inside Y&H | £79bn | 56% |
| Y&H pays the rest of the UK | £265bn | 77% |
| The rest of the UK pays Y&H | £230bn | 76% |

- **Outside is 3.1 times inside** for the region, and between 1.6 and 4.8 times for most divisions. The typical division keeps 27% of its spending and 26% of its income in the region.
- **London takes 23% of outside spending and gives 24% of outside income,** then the South East (19% and 14%) and the North West (14% and 15%). The handover's warning applies in full here: these are registered offices, and outside cells are more hidden, with held values adding 25 to 27%.

Three ways of showing them were tried or sketched:

| Idea | Result |
|----|----|
| **Ring gauge** round each circle, showing the outside share of income and of spending | Dropped. Nearly every industry sends around three-quarters outside, so every ring looks the same and the page just gets heavier outlines ([screenshot](../tests/outputs/screens/09a_order_groups_ring.png)). |
| **Halo:** a pale half-disc above each circle for income from outside and one below for spending outside, with areas on the same scale as the circle | Works with a thinner set of links (see below). It shows scale honestly and fits the layout, since outside buyers would sit above and outside suppliers below. With 500 links drawn it gets lost. |
| **Headline strip** above the chart: three figures and bars for the month (inside, paid out, paid in) | Not built yet. It shows the scale with no cost to the network. |

**Decision:** the headline strip always; the halo as a switch, off at first; the exact inside and outside figures for an industry in its detail panel. No outside links.

![Halo with the top 150 links](../tests/outputs/screens/09c_order_groups_halo_top150.png)

*Halos with the top 150 links. Retail's large lower half-disc is its spending outside the region. This view also shows how much clearer the page is with fewer links.*

### 2.6 Drawing: canvas, SVG or WebGL?

The prototype was played for five seconds per case in headless Chrome on an Apple M4, on a 1,400 by 1,080 window at double pixel density. Sixty frames a second is the ceiling.

| Drawing method | Links drawn | Script time per frame | Frames a second |
|----|----|----|----|
| Canvas, one fill per link | 500 | 1.7 ms | 60 |
| SVG paths | 500 | 5.2 ms | 60 |
| Canvas, one fill per link | 3,152 (every link, both directions) | 3.1 ms | 60 |
| Canvas, the same with moving dots | 3,152 | 4.3 ms | 59 |
| SVG paths | 3,157 | 23.6 ms | 35 |
| Canvas, one fill per link | about 16,000 | 17.6 ms | 18 |
| Canvas, one fill per link | about 47,000 | 41.7 ms | 6 |

- **Canvas handles everything this page needs with room to spare.** The full network is about 3,200 links and the page will usually draw a few hundred.
- **SVG is fine up to the top 500 links** and halves the frame rate with all of them.
- **WebGL isn't needed.** On these figures canvas would start dropping frames somewhere above 5,000 links, and WebGL would be worth its extra complexity from roughly 10,000. Nothing planned comes near that. The five-digit network (56,000 links) would, which is one more reason to leave it out.
- **Two traps found on the way:**
    - Merging all links of one colour into a single path looks like an optimisation and is 20 times slower (15 frames a second with every link).
    - Reading pixels back from the canvas each frame cut it to 18 frames a second. Don't do either.
- **Data size:** 77 industries, 3,210 links, 93 months, two measures, hidden-month flags and outside series come to 2.06 MB, or 0.34 MB as a web server compresses it.
- **Not tested:** a slow laptop, a phone, Firefox or Safari. The margin is large (3 ms of a 16 ms budget), but check before publishing.

**Decision:** D3 for scales, force layout and controls; a 2D canvas for drawing; ordinary HTML for controls, the tooltip, the detail panel and a table view.

### 2.7 Payment groups: which can be named?

**Method.** For each of the eight years the affinity grouping was run 2,000 times, and every pair of divisions scored on how often they landed together. Groups were then found from those scores, once per year and once for all years together. That takes out the luck of a single run. It needs that many: with 100 runs a year the result still changed with the random seed (the food group and security moved in and out of the core), and with 2,000 it came out identical on four seeds. The 11 divisions under £0.5m a month were left out of the comparisons.

**Groups that change by year don't work.** Even with the luck taken out, between 5 and 19 of the 66 divisions change group from one year to the next, and the year-on-year agreement runs from 0.46 to 0.79. A quarter of industries don't change supply chain in a year. My reading is that the boundaries are soft and small shifts in the data tip divisions across them, so a per-year view would show movement that isn't there.

**The most common grouping over all years gives five groups with a steady core.** A division counts as core if it sits with its group in at least 6 of the 8 years and lands with the group's other members in at least 60% of runs.

| Group (my names) | Core divisions |
|----|----|
| Distribution | Wholesale, retail, land transport, warehousing and transport support, furniture, textiles, post and courier, clothing, fishing |
| Engineering and metals | Fabricated metal, machinery repair and install, architecture and engineering, machinery, basic metals, electrical equipment, electronics |
| Construction | Building trades, building construction, rental and leasing, civil engineering, waste, glass, cement and ceramics, remediation, quarrying |
| Local and public services | Real estate, health, personal services, education, legal and accounting, residential care, sport and recreation, accommodation, social work, arts and entertainment, membership organisations, film, TV and music, travel agencies, air transport, libraries and museums, public administration |
| Food | Food manufacturing, farming, veterinary |

- **43 divisions are core,** and they make or receive 74% of inside payments.
- **23 float** between groups: IT and software, head offices and consultancy, motor trades, chemicals, food and drink service and insurance are the larger ones. Security sits just under the line. Printing, paper and publishing stay together but attach to a different larger group in different years, so the "media" group from the single run doesn't survive.
- **The groups aren't SIC sections under another name.** 77% of core divisions are in their section's main group; the rest cut across, such as architecture and engineering sitting with metals, and legal and accounting with local services.
- **The names are mine,** read off the members. Change them freely.

**How to show them.**

- **As columns in the supplier-order layout: works.** Each group reads as a chain from its buyers at the top to its suppliers at the bottom, with a sixth column for divisions in no group.
- **As outlines in the hybrid and connections layouts: doesn't work.** The groups aren't separate enough in space and the outlines pile on top of each other ([hybrid](../tests/outputs/screens/17_hybrid_payment_group_outlines.png), [connections](../tests/outputs/screens/18_connections_payment_group_outlines.png)).
- **As a highlight:** each group is also an entry in the highlight control.
- **Floating divisions** go in the "not in a group" column. Their detail panel says which group they most often sit with and in how many years.

![Supplier order with payment-group columns](../tests/outputs/screens/16_payment_group_columns.png)

*Payment groups as columns. Distribution runs from retail at the top to wholesale and haulage at the bottom.*

### 2.8 Cadence: how many payments, as well as how much

**The number of payments says something value doesn't.** Among the 430 links that carry 80% of the value, value and number of payments have a rank correlation of only 0.55. The average payment runs from £1,700 at the 10th percentile to £13,900 at the 90th, a factor of eight.

| Link (average month, latest 12 months) | Value | Payments | Average payment |
|----|----|----|----|
| Education pays building construction | £3.8m | about 120 | £33,000 |
| Education pays wholesale | £2.3m | about 1,500 | £1,600 |
| Farming pays food manufacturing | £4.2m | about 110 | £39,000 |
| Food and drink service pays retail | £2.1m | about 3,000 | £700 |

**Proposal: let the moving dots count payments.** A link's width stays the amount. Each dot travelling along it stands for 100 payments that month. A wide link with a few dots is a few large payments; a thin link crowded with dots is many small ones. Nothing else is added to the page.

- **100 is the finest unit the data support,** because ONS rounds counts to the nearest 100. A link with under 50 payments a month shows as zero and gets no dots: 48 of the top 430.
- **It fits on screen.** At one dot per 100 payments the top 430 links carry about 1,400 dots. The busiest link, food and drink service paying wholesale, has 105.
- **Dots follow the direction drawn.** With links netted per pair, the dots are the payments made in the direction shown.
- **The tooltip gives the figures:** payments that month and the average payment.
- **Still to tune:** the dots are small against the widest links in a still image ([screenshot](../tests/outputs/screens/14_dots_payments_hybrid_top150.png)). They need to be slightly larger, or lighter on wide links.

The existing switch between value and number of payments stays, for anyone who wants widths by count.

### 2.9 Switching the intermediaries in

The four are information services (63), banking and finance (64), financial support services (66), and office and business support (82).

- **The other industries barely move.** Their levels with the four in and out have a rank correlation of 0.994. The typical shift is 0.015 of a level; the largest is insurance and pensions at 0.14.
- **Two of the four are big.** Office and business support is the seventh largest division by inside payments (£40m received and £27m paid a month) and banking and finance the ninth (£18m and £27m). The other two are small. Together, 15% of inside payments involve one of them.
- **Where they sit:** information services at the very bottom, financial support services just above it, office and business support low, and banking and finance in the middle.
- **Their largest links:** banking and finance paying building construction (£8.4m a month) and health paying office and business support (£5.1m).
- **The order is slightly looser with them in** (0.77 against 0.74), and their cells are a little less hidden (51% against 56%).
- **The data file grows a little:** 81 industries and 3,627 links, 2.34 MB, or 0.38 MB compressed.

So the switch is cheap. With it on, the four appear at their own levels and the rest shift by amounts too small to notice; each state uses levels worked out on its own set of industries. The four aren't in the payment groups, which were found without them.

![Intermediaries switched in](../tests/outputs/screens/15_intermediaries_in.png)

*Intermediaries in: banking and finance beside head offices in the middle; office and business support, financial support services and information services lower down.*

## 3. Design

### 3.1 Page, top to bottom

1. **Title and one sentence** saying what a link is.
2. **Headline strip** for the month shown: paid inside the region, paid to the rest of the UK, received from the rest of the UK, each with a bar on one scale.
3. **Controls,** one row: layout, columns, measure, how many links, highlight, and the switches in 3.3.
4. **Time bar:** play and pause, step back and forward, speed, and a slider with year marks over a small line of the monthly inside total.
5. **The chart** (canvas), with a tooltip on hover.
6. **Detail panel** for a clicked industry: its level over time, inside against outside both ways, its largest suppliers and customers that month, industrial strategy membership and how much of its data is hidden.
7. **Legend,** then a **table view** of the month's links for screen readers and copying.
8. **Notes:** what the data is and isn't (section 5), source and licence.

### 3.2 What each mark means

| Mark | Meaning |
|----|----|
| Circle | One division. Area is its payments made plus received inside the region in the period shown. |
| Vertical position | Its level in the supplier-to-buyer order, from the whole period by value, with held values. Fixed while time plays. |
| Horizontal position | By layout: sector-group column, or pulled towards the industries it trades with. |
| Link | Payments between two industries. Width is the square root of the amount. Wide at the paying end, narrow at the receiving end. |
| Link colour | Blue `#2a78d6`: money runs down the order, buyer to supplier. Orange `#eb6834`: it runs up. |
| Dots on a link | Each is 100 payments that month, moving the way the money went. |
| Dashed centre line | That month is hidden and held at an average. |
| Dark fill, or pie wedge | The industry is in the highlighted set; a wedge shows the share of its jobs that is. |
| Half-discs (halo, optional) | Above: income from the rest of the UK, in teal. Below: spending there, in green. Same area scale as the circle. In the goods and services view the two swap places, each keeping its colour. |

### 3.3 Controls and their starting values

| Control | Options | Starts at |
|----|----|----|
| Layout | Supplier order, hybrid, connections | Supplier order |
| Columns (supplier order only) | By sector group, by payment group, by how much its place shifts, free | By sector group |
| Measure | Value, number of payments | Value |
| Links | Net per pair, both directions | Net per pair |
| How many links | Enough for 50%, 80% or 90% of the month's value, or all | 80% (about 430) |
| Highlight | None, one sector group, one payment group, any or one industrial strategy sector | None |
| Flows of | Money, goods and services | Money |
| Direction | Both, down the order only, up the order only | Both |
| Rest of UK | Off, halo | Off |
| Smoothing | Single month, three-month average | Three-month average |
| Width scale | Share of the month's total, pounds | Share |
| Intermediaries | Out, in | Out |
| Hidden months | Held, published only | Held |
| Dots (one per 100 payments) | On, off | On |

### 3.4 Behaviour

- **Links point the way the money went,** from payer to payee, so they run down the page from buyers to suppliers. The data are payments and nothing else.
- **A switch shows goods and services instead.** It turns each link round (widest at the supplying end, dots moving with what was paid for) and flips the vertical order, so suppliers are at the top and things still mostly run down the page. Blue still means "down the page". This is an inference, and a wrong one for rent, insurance premiums, rates and refunds; the key and the notes say so whenever it's on.
- **Positions don't move with time.** Levels are stable enough (section 2.2) that fixing them costs little, and then everything that moves on screen is a change in payments. The detail panel shows how an industry's level drifted.
- **Changing layout** slides circles to their new places over about 0.7 seconds.
- **Switching the intermediaries in** adds four circles; the others shift slightly to the levels worked out with all 81.
- **Dots** drift along their links all the time, paused or playing, so cadence can be read on a single month.
- **Play** runs at about three months a second (the whole period in 30 seconds), blending between months, with speed options.
- **Hover** on a circle keeps its links and fades the rest, and shows the tooltip. **Click** pins it and opens the detail panel.
- **Labels** are drawn largest circle first and skipped where they would overlap; highlighted and hovered industries always get theirs.
- **Keyboard:** left and right arrows step a month, space plays and pauses.

## 4. Data pipeline

### 4.1 Steps

One R script, `build_data.R` in this chunk's folder, reusing `tests/functions_hierarchy.R`:

1. **Extract** every row touching `TLE` from the three zips (as `tests/01`). Cache in `local/`.
2. **Drop** non-industries. Keep the four intermediaries, flagged, for the switch.
3. **Hold** hidden months (as `hold_values()`), separately for inside and outside cells. Keep the basis of each cell-month: published, held, or never published.
4. **Levels:** whole-period, by value, held, worked out twice: on the 77 industries and on all 81 (as `tests/02` and `tests/11`). Also 12-month rolling levels for the detail panel.
5. **Payment groups:** the all-years grouping, with each division marked core, floating or too small (as `tests/12`). Names come from a small hand-kept table keyed on one division per group.
6. **Outside series:** for each division and month, paid to and received from the rest of the UK, with London separate so the panel can show it.
7. **Lookups:** short names, sections, sector groups, industrial strategy shares (from `tests/00`), plus each division's share of jobs in any listed code, which the prototype approximates.
8. **Write** one data file and print the checks in 4.3.

### 4.2 The data file

The same shape as the prototype's `ynh_flows.js`: one script that sets a global, so the page works from disk as well as from a server.

```
months:  ["2019-01", ...]                       93
nodes:   [{ id, short, name, section, group, role,
            level, level_all, level_rolling[93], small,        level: among the 77; level_all: among all 81
            pay_group, pay_status, pay_group_years,            pay_status: core, floating, too small, not grouped
            is8: [{ name, share, whole }], is8_any_share,
            ext_buy[93], ext_sell[93], ext_buy_london[93], ext_sell_london[93] }]
links:   { supplier[], buyer[],                 indices into nodes
           value[][93], count[][93],            GBP thousands; payments
           basis[] }                            93 characters each: p published, h held, x hidden with nothing to hold, 0 no row
```

Values are in thousands of pounds because ONS rounds to £1,000; counts are rounded to 100 at source.

### 4.3 Checks the build should print

- Totals match `tests/05`: £79.0bn inside, £265.2bn paid out, £230.2bn paid in.
- The incoherence score is 0.74 and the share running down the order 71%.
- The rank correlation with the input-output levels is still about 0.7.
- No link has a value in a month whose basis says "no row".
- The payment groups: five groups and 43 core divisions, with the same core members as section 2.7. The grouping uses random starts, so it runs 2,000 times a year; report any division that changes.
- Which divisions fall below the size threshold. I'd start at £0.5m a month of inside payments, which catches 11 of the 77: gambling, mining support, leather, broadcasting, coke and refined petroleum, water supply, sewerage, scientific R&D, forestry, water transport and other transport equipment. Draw them grey and keep them out of the vertical scale's range.

## 5. What the page must say about the data

Short, near the chart, not buried:

- **Payments, not trade.** Bacs and Faster Payments between businesses. Read a link as a sign of connection.
- **Registered offices, not activity.** A firm headquartered elsewhere pays from elsewhere. This is why the page is about flows inside the region, the more trustworthy half.
- **Households aren't in it,** so consumer-facing and publicly funded industries appear mainly as buyers.
- **Most of what the industries are paid isn't drawn, and public money is mostly left out** (added 23 November 2026, from section 11.10). The page shows payments where both sides have a known industry and region, which is about a third of what a region's industries were paid. Rows with an unknown industry or an unknown region are dropped, and public administration almost never has a region. There is no category for abroad. Each region's note quotes its own figures, worked out by the build (`left_out()` in `build_data.R`).
- **Over half the monthly cells are hidden,** and hidden doesn't mean small. Held values are averages of the months that were published. Over a quarter of cell-months have nothing to hold: those links are simply absent. Retail paying food manufacturing is one.
- **Growth over time is partly coverage.** More payments are captured in later years.
- **Nominal pounds, not seasonally adjusted, experimental statistics** that are revised between editions. The page opens on each month's shares, which sidesteps price changes.
- **Payment groups are a statistical grouping,** named by hand: industries that pay each other more than their sizes predict, in most years. About two in five industries don't sit firmly in any group: 23 float and 11 are too small to place.
- **Intermediaries, when switched in,** partly show money passing through (card takings, platform payouts) and not purchases.
- **Source:** ONS, Industry-to-industry payment flows by region, UK, from Pay.UK data, under the Open Government Licence.

## 6. Decisions

Settled with Dan on 23 November 2026.

| Question | Decision |
|----|----|
| What links show | Money, payer to payee, by default. A "Flows of" switch (added 23 November, after the first build) turns every link round and the order upside down, to read the page as goods and services running down from suppliers to buyers. That view is an inference and the page says so. |
| Width scale | Opens on each month's share, with a switch to pounds. |
| Cadence | Shown, simply: moving dots, one per 100 payments (section 2.8). |
| Intermediaries | A switch in the first version, starting off. They're part of the region's economy and should be showable. |
| Publishing | A public page: `docs/chunks/paymentflowsviz_ynh/index.html`, which the repo's index script picks up and GitHub Pages serves. |
| Payment groups | Identified on the page, using the most common grouping over all years. Not per year: section 2.7 found that would mostly show noise. |
| Deflating | None. Keep to proportions. |

**Still open.**

- **The payment group names** in section 2.7 are mine. Confirm or change them.
- **The `tests/` folder goes public with the page** unless it's moved or ignored: about 20 MB of screenshots and a 3 MB prototype. I'd leave it, as a record of how the page was arrived at, but trim the screenshots.

## 7. Build steps

**Built on 23 November 2026.** All eight steps below are done except publishing, which needs the folder committed and pushed. The page is `index.html` in this chunk's folder. What differs from the plan above:

- **Payment groups have 43 core divisions, not 44.** The build runs the grouping 2,000 times a year, which made it repeatable and moved security to floating (section 2.7).
- **Columns hold their industries.** Each column's width follows what it has to hold, hairlines separate them, and a circle can't leave its column. To make that possible a circle's height is its level give or take 14 pixels, which is under half the month-to-month wobble in a level.
- **Thin links in a hidden month are drawn dashed,** since there's no room on them for a dashed centre line, and a link's tooltip says in how many of the 93 months it was published. 473 links were published in fewer than 6 months; they stay in, visibly dashed.
- **The halo is stronger and two-coloured:** 30% fill with a firm outline, teal for what's received from the rest of the UK and green for what's paid to it. The pair was chosen with the validator to be alike but separable, and clear of the blue and orange used for links.
- **A "Flows of" switch** shows goods and services (section 3.4).
- **A "Direction" switch** shows both directions, only the links running down the order (blue), or only those running up it (orange). The "links carrying 80%" cut-off is then taken within the direction chosen, so the up-only view brings out more of the orange links than the combined view does.
- **A line for the average industry** runs across the chart in the supplier-order layout, and the same line in the side panel's level chart is drawn more strongly.
- **A fourth Columns option, "By how much its place shifts",** lines industries up from the steadiest to the one whose place in the order moves most. The measure is the standard deviation of the level worked out on the 12 months to each month, the same series the side panel draws. Industries are evenly spaced in rank order, with the standard deviation marked along the top; the 11 small industries sit in a strip of their own, since their levels are mostly noise. Among the rest it runs from 0.009 (retail) to 0.160 (head offices and consultancy), with a median of 0.042, and it tracks drift more than jitter: it correlates 0.80 with the size of the change between the first and last 12 months.
- **An expandable note, "How the order is worked out, and how far to trust it",** sits under the chart and is linked from the opening paragraph. It gives the calculation, the test results from section 2.2, and the caveats.
- **A "Pin an industry" list was added,** so an industry can be found and pinned without a pointer.
- **Hovering a link** shows both directions and, when links are netted, the difference drawn.
- **D3 is kept in `lib/`,** so the page loads nothing from outside.
- **Dots redraw at half rate when nothing else is moving,** and stand still for anyone whose system asks for reduced motion.
- **Dark mode follows the system setting.** There's no switch on the page.
- **The data file is 2.7 MB,** 0.5 MB compressed, a little over the estimate, because it carries London separately and both sets of rolling levels.

**Checked** in headless Chrome (`tests/13_page_checks.cjs`): no errors; every control worked; light, dark, tablet and phone layouts looked at; 60 frames a second playing the opening view and playing every link with intermediaries in. Headline totals, one industry's breakdown and one link's value and count were compared with the source data and matched.

**Not checked:** Firefox, Safari, a real phone, a slow laptop, or a screen reader.

The steps as planned:

Each step ends with something to look at.

1. **Data.** Write `build_data.R` and the data file, with intermediaries and payment groups in it; run the checks in 4.3.
2. **Skeleton.** Page structure, canvas, and the three layouts carried over from the prototype. Rein in barely connected industries in the hybrid and connections layouts. Check against the test screenshots.
3. **Time and dots.** Slider, step, play with blending, speed, the small line of monthly totals, keyboard control, and dots that count payments. Play the whole period and watch for jumps.
4. **Headline strip and width scale.** Inside and outside figures for the month; share or pounds.
5. **Groups, highlight, hover and detail panel.** Sector-group and payment-group columns, the highlight sets, industrial strategy wedges, tooltip, pinned detail panel.
6. **Switches.** Intermediaries; halo for outside flows; published only; dashed centre lines for held months.
7. **Reading aids.** Legend with a width key, the notes in section 5, the table view.
8. **Finish.** Dark mode with its own colour steps, narrow screens, a slower machine, Firefox and Safari, then publish as decided.

The prototype (`tests/prototype/prototype.html`) already does a rough version of steps 2, 3, 5 and 6, including the intermediaries switch, payment-group columns and payment dots. Most of the remaining work is tidying, the detail panel, the headline strip and the words.

## 8. Open questions carried forward

- **Missing rows** in the regional files are treated as zero. Nobody has checked that with ONS (handover, section 7).
- **`[-]` in the five-digit file** is unexplained. It only matters if the national panel is ever built.
- **Revisions.** When ONS issues the next edition, re-run and compare the levels before updating the page.
- **Bringing public administration's payments in.** They have no region, so they can't go in the inside or outside totals as they stand. See section 11.11.
- **Showing an industry's local links without losing its wider role.** Wholesale is the test case: a middle link nationally, an end of the line in every region's payments. Scoped in section 13.10, with one idea tested and found not to work.
- **Who the payers of unknown industry are.** They account for about half of what a region's industries are paid. Nothing here says who they are; ONS's own notes on the data would be the place to look.

## 9. Files

| Path | What |
|----|----|
| `index.html`, `flows.css`, `flows.js` | The page |
| `ynh_flows_data.js`, `build_data.R` | The page's data and the script that builds it |
| `lib/` | A local copy of D3 |
| `README.md` | What's in the folder and how to rebuild |
| `plan/paymentflowsviz_plan.md` | This document |
| `tests/README.md` | What each test script does and how to run it |
| `tests/00` to `12`, `functions_hierarchy.R` | The tests |
| `tests/13_page_checks.cjs` | Checks on the finished page; screenshots in `tests/outputs/page/` |
| `tests/14` to `16` | Scoping tests for section 10: all regions' data, links, and how alike regions are |
| `tests/17` to `20` | Scoping tests for section 11: the foundational economy list, how it fits divisions, how it sits against tradeable, and whether the input-output table could fill what the payments leave out |
| `tests/21_ties_columns.cjs` | The test behind the columns "by ties to what is highlighted" (section 11.13) |
| `tests/22_local_columns.cjs` | The test behind the columns "by trade inside the region", and whether that goes with the order (section 12) |
| `tests/23_middle_links.cjs`, `23b_middle_links_io.R` | The check on Other manufacturing as a middle link, and scoping for a chain measure, with the same measure on the input-output table (section 13) |
| `tests/24_chains_across_the_edge.R` | Scoping: whether counting payments across the region's edge makes the chain measure match an industry's wider role (section 13.10) |
| `tests/lookups/` | Division names and sections; industrial strategy shares by division; the foundational economy list |
| `tests/outputs/` | Result tables as CSV; `screens/` holds 20 prototype screenshots |
| `tests/prototype/` | `prototype.html` and its data file `ynh_flows.js` |
| `local/paymentflowsviz_ynh/` (ignored by git) | Extracted monthly data, held values, and the scripts' printed logs |

## 10. Scoped, not built: links, all twelve regions, and how alike regions are

*Scoped on 23 November 2026, with tests `tests/14_all_regions_extract.R`, `tests/15_url_roundtrip.cjs` and `tests/16_region_similarity.R`. Sections 10.2 and 10.3 have since been built for a sample of three regions: see 10.5. Section 10.1 is not built.*

**In short.**

- **Links:** nearly done already. Every control on the page already writes itself to the address and comes back from it. What's left is small.
- **All regions:** feasible, and the data work is modest (one three-minute pass over the files, about 32 MB of data files). The real work is on the page, which was written for one region. Three regions have much thinner data than Yorkshire and the Humber.
- **Similarity:** the overall measure works and the differences between regions are real, about three times the size of a region's difference from its own earlier self. The chain-or-web measure separates the extremes but not the middle, and it lines up closely with how big a region is and how much of its data is hidden. The page should say so.

### 10.1 A link for every view

**What's there.** `tests/15` set every value of every control through the page itself, read the address, loaded that address afresh and compared.

| Control | In the address as | Values tried | Came back |
|----|----|----|----|
| Layout | `layout` = `order`, `hybrid`, `force` | 3 | 3 |
| Columns | `cols` = `group`, `paygroup`, `shift`, `free` | 4 | 4 |
| Highlight | `lens` = `group:…`, `pay:…`, `is8:…` | 22 | 22 |
| Pin an industry | `sel` = its SIC code | 78 | 78 |
| Flows of | `show` = `money`, `goods` | 2 | 2 |
| Direction | `dir` = `both`, `down`, `up` | 3 | 3 |
| Measure, Widths, Links | `measure`, `scale`, `net` | 6 | 6 |
| Show | `cover` = `0.5`, `0.8`, `0.9`, `1` | 4 | 4 |
| Speed | `speed` | 4 | 4 |
| Five tick boxes | `smooth`, `dots`, `inter`, `halo`, `pubonly` = `1` or `0` | 10 | 10 |
| Month | `m` = `2021-06` | 1 | 1 |

All 17 settings together also came back from one address of 190 characters. A mangled address falls back to the opening view without an error. `#order-note` on the end opens the note.

**What's missing.**

- **Opening the table or the note** isn't written to the address. The note opens from a link only if `#order-note` is typed in.
- **Playing** isn't in the address, so there's no link that starts the animation.
- **The address lists only settings that differ from the opening view.** If an opening setting changes later, old links silently change with it.
- **Highlight values are full names** (`pay:Local and public services`). Rename a payment group and its links break.
- **A bad month** falls back to January 2019, not the latest month. Small bug.
- **Opened from disk,** some browsers refuse to update the address. The page already ignores that, but then there's nothing to copy.
- **The region** (section 10.2) will need its own entry.

**Work.** All small.

1. A "Copy link" button that writes every setting explicitly, with a version number, so a copied link means the same thing later.
2. `table=1`, `note=1` and `play=1`.
3. Short stable codes for highlight sets, in place of names.
4. Fix the bad-month fallback.
5. A table of the settings in the folder's README.
6. Keep `tests/15` as a check, failing if any control stops coming back.

### 10.2 All twelve regions, loaded on demand

**The data.** One pass over the three ONS files (51.7 million rows) took three minutes. Each region's inside flows, between different industries, with intermediaries out and hidden months held:

| Region | Inside payments, whole period | Monthly figures hidden | In links never published | Held values add | Industries under £0.5m a month | Order's agreement with input-output table |
|----|----|----|----|----|----|----|
| North East | £11bn | 71% | 42% | 26% | 25 | 0.52 |
| North West | £96bn | 50% | 22% | 10% | 6 | 0.52 |
| Yorkshire and the Humber | £61bn | 56% | 27% | 12% | 11 | 0.71 |
| East Midlands | £42bn | 59% | 29% | 12% | 14 | 0.75 |
| West Midlands | £64bn | 56% | 28% | 17% | 10 | 0.55 |
| East of England | £69bn | 54% | 25% | 10% | 8 | 0.65 |
| London | £401bn | 52% | 25% | 8% | 4 | 0.63 |
| South East | £146bn | 48% | 21% | 12% | 4 | 0.46 |
| South West | £40bn | 55% | 27% | 12% | 6 | 0.54 |
| Wales | £12bn | 71% | 40% | 24% | 24 | 0.42 |
| Scotland | £48bn | 59% | 29% | 10% | 7 | 0.51 |
| Northern Ireland | £36bn | 68% | 38% | 11% | 17 | 0.29 |

- **Three regions are much thinner:** the North East, Wales and Northern Ireland have around 70% of monthly figures hidden and about 40% in links never published. In the North East and Wales about a fifth of the value drawn would be held averages, and a third of industries are too small to place.
- **Yorkshire and the Humber is one of the better cases.** Its order agrees with the input-output table more than any region's except the East Midlands. Northern Ireland's barely agrees (0.29).
- **Size of the job:** each region's file would be about 2.7 MB (0.5 MB compressed), so about 32 MB in the repository for all twelve, added again to its history on every rebuild. A full build would be roughly half an hour as the script stands (twelve times the Yorkshire and the Humber build), less if it reads the files once.

**What carries over unchanged.** Extraction, holding, levels, outside flows, the links and their flags, and every drawing routine.

**What doesn't.**

- **The page assumes one dataset loaded at the start.** Everything worked out from the data (series, totals, layouts, the key, the slider) needs gathering into one step that can run again when the region changes. That's the main piece of work. The data would load by adding a script for the chosen region, which still works from disk.
- **Typed-in figures.** The notes quote Yorkshire and the Humber's numbers (71%, 0.74, 0.71 and so on). For twelve regions they must be worked out by the build, carried in each data file and dropped into the text.
- **Payment groups don't travel.** Worked out region by region with the same method, the regions get 3 to 6 groups. Yorkshire and the Humber's five named groups stay separate only there. Its grouping agrees well with the North East, East Midlands and West Midlands (0.80 to 0.87 on a 0 to 1 scale) and only moderately with the rest (0.37 to 0.59). One grouping on all regions' flows added together fits no better (0.35 to 0.51 for eight of the twelve). So groups have to be worked out per region, and the hand-given names can't be reused.
- **Industrial strategy shares** need jobs by region. The five-digit jobs file by ITL3 area covers the eleven regions of Great Britain for 2023. Northern Ireland isn't in it.
- **London.** The side panel's "London" split means nothing there, and the registered-office caveat bites hardest: head offices make London's inside flows partly other regions' business.
- **"Actual amounts" aren't comparable between regions** as the page stands, because each dataset sets its own reference width. Shares are.

**Steps.**

1. `build_data.R` takes a region; a wrapper builds all twelve and a small index file listing them with the quality figures above.
2. The build writes each region's quoted figures into its file.
3. The page gathers its start-up into one re-runnable step; adds a region picker, a `region` entry in the address, and a load-on-demand script.
4. Text that names the region or quotes a figure reads from the data.
5. A visible line on data quality for each region, and a stronger warning for the three thin ones.
6. `tests/13` runs over every region.

### 10.3 Which regions are alike

Both measures use each region's inside flows over the whole period, between different industries, by value, with held values and intermediaries out.

**A. Overall similarity, sector makeup included.**

*The measure.* Write each region's flows as shares of its own total: one share for every pair of paying and paid industry. The distance between two regions is the share of one region's payments that would have to move to a different pair to match the other. 0 is identical; 1 is nothing in common.

*What it gives.*

| Region | Its two closest | Its furthest |
|----|----|----|
| North East | South West (0.38), Wales (0.39) | London (0.60) |
| North West | South West (0.33), Yorkshire and the Humber (0.34) | London (0.49) |
| Yorkshire and the Humber | North West (0.34), East of England (0.37) | London (0.56) |
| East Midlands | Yorkshire and the Humber (0.39), East of England (0.39) | London (0.62) |
| West Midlands | Yorkshire and the Humber (0.38), North West (0.39) | London (0.57) |
| East of England | South West (0.35), North West (0.36) | London (0.55) |
| London | North West (0.49), South East (0.52) | Northern Ireland (0.66) |
| South East | South West (0.41), North West (0.42) | Northern Ireland (0.60) |
| South West | North West (0.33), East of England (0.35) | London (0.54) |
| Wales | South West (0.38), North East (0.39) | London (0.63) |
| Scotland | South West (0.37), North East (0.39) | London (0.58) |
| Northern Ireland | East of England (0.41), Yorkshire and the Humber (0.44) | London (0.66) |

- **The differences are real.** Distances between regions run from 0.33 to 0.66. The same distance between a region's first half of the period and its second half runs from 0.11 to 0.17. No pair of regions is as close as a region is to its own earlier self.
- **The ranking is steady.** The 66 distances worked out on the first half and on the second half correlate 0.98.
- **Hiding doesn't drive it.** Restricting each comparison to links published in both regions gives nearly the same ranking (0.98), and a region's hidden share is only weakly related to how far it sits from the others (0.22).
- **It's mostly sector makeup.** Comparing only each industry's share of payments gives almost the same ranking (0.97). Comparing who each industry pays, with makeup taken out, also agrees (0.95): regions with similar industries also wire them up similarly.
- **London stands apart** from everything, with Northern Ireland next. The rest form a loose core around the North West, South West, East of England and Yorkshire and the Humber.
- **Even the closest pair is 0.33 apart.** With about 6,000 possible pairs of industries, a third of payments sit in different places. "Most alike" is relative.

**B. Chain or web.**

*The measure.* Two numbers, because "chain-like" means two things.

- **How one-way it is:** the share of money running down the order (section 2.2). Higher is more chain-like.
- **How spread out it is:** for the typical pound spent, the number of supplier industries its buyer's spending is in effect spread across. Lower is more chain-like. (It's the usual "effective number": 1 if a buyer pays one industry, 20 if it pays twenty equally.)

*What it gives,* most chain-like first:

| Region | Money running down the order | Supplier industries per buyer, in effect | Monthly figures hidden |
|----|----|----|----|
| Northern Ireland | 78% | 10.4 | 68% |
| North East | 75% | 16.6 | 71% |
| East of England | 73% | 16.0 | 54% |
| Wales | 73% | 15.7 | 71% |
| East Midlands | 72% | 17.5 | 59% |
| South West | 72% | 18.4 | 55% |
| Yorkshire and the Humber | 71% | 17.7 | 56% |
| Scotland | 71% | 18.5 | 59% |
| North West | 70% | 19.3 | 50% |
| West Midlands | 70% | 20.0 | 56% |
| London | 68% | 19.1 | 52% |
| South East | 67% | 21.4 | 48% |

- **The ends differ; the middle doesn't.** The spread between regions is three times a region's own year-to-year movement for the first number and six times for the second. But the seven regions from the North West to the East of England sit within 3 points of each other on the first, and neighbours in that run differ by less than one year's wobble (about 1 point). Yorkshire and the Humber is in that run. Northern Ireland at one end, and the South East and London at the other, are clearly apart.
- **It follows size and hiding.** Across the twelve regions the first number has a rank correlation of 0.70 with the share of figures hidden and of -0.80 with the region's total payments. Small regions with more hidden look more chain-like. That could be true (a smaller economy has fewer two-way links) or it could be that ONS hides the thin links that make a web. The data can't separate the two.
- **A partial check.** Using only the 1,417 links published at some point in all twelve regions, which carry 76% to 88% of each region's value, keeps the ranking (0.90 for the first number, 0.76 for the second). So it isn't only which links are hidden. Size remains tangled in.
- **One case stands out as not just size:** the East of England is large and not especially hidden, yet sits near the chain end on both numbers.

**How to show them.**

- **In the region picker:** "Most like this region: North West, East of England", from measure A.
- **A small chart of the twelve regions** on the two chain-or-web numbers, each with a bar for its own year-to-year range, so overlaps are visible. Regions whose bars overlap are described as the same.
- **A sentence that changes with the region:** for Yorkshire and the Humber, "about average: not distinguishable from six other regions". For Northern Ireland, "the most chain-like, though it is also among the most hidden".
- **A standing caveat** that chain-or-web tracks size and hiding.
- **Data:** both measures for all regions fit in one small file loaded at the start, so the picker works before any second region is fetched.

### 10.4 Decisions for you

1. **Payment groups in other regions.** I'd work them out per region and label each by its largest members, keeping hand-given names for Yorkshire and the Humber only. The alternative is to drop payment groups outside Yorkshire and the Humber.
2. **The three thin regions.** I'd include them with a prominent warning. The alternative is to leave them out.
3. **Northern Ireland's industrial strategy shares.** I'd leave the highlight off there unless you have a jobs source for it.
4. **Chain or web.** I'd show it with its caveat, as above. If the caveat makes it not worth the space, measure A alone is on firmer ground.
5. **Order of work.** I'd do 10.1 first, since 10.2 needs the region in the address anyway, then 10.2, then 10.3.

### 10.5 Built so far: the foundation, on three regions

**Built on 23 November 2026,** after the scoping above. The page now switches between regions and compares them. Three regions have data: Yorkshire and the Humber, the North West (well published, and Yorkshire and the Humber's nearest neighbour) and the North East (the most hidden). The other nine appear in the region list as "not built yet".

**What was done,** against the steps in 10.2 and the design in 10.3:

| Step | State |
|----|----|
| `build_data.R` takes a region | Done. `Rscript build_data.R TLE TLD`, `all`, or `index`. One file per region in `data/`. |
| Index of all twelve regions | Done: `data/regions.js`, 8 KB, loaded with the page. Data quality, distances, and the chain-or-web numbers with year-to-year ranges, for all twelve. |
| The build writes each region's quoted figures | Done. 29 figures and sentences per region, in the data file. Nothing about a region is typed into the page any more. |
| The page's start-up can re-run | Done. Choosing a region loads its file on demand and rebuilds everything from it. The old picture stays dimmed until the new one arrives. |
| Region picker, region in the address | Done. The pinned industry, month and every other setting carry over when the region changes. |
| Data-quality line | Done, on every region, with a boxed warning on thin ones. |
| "Most like this region" | Done, beside the picker and in the comparison section. |
| Chart of the twelve regions, with ranges | Done, in an expandable "How this region compares with the others". It states how many regions this one overlaps, and carries the size-and-hiding caution. |
| Checks run over every built region | Done in `tests/13`: each region loads from its own address, has no unfilled text, and switching keeps the view. |

Yorkshire and the Humber's build still matches all 14 test checks after the rework.

**What the patchy region showed.** The North East was chosen to break things. What it turned up, and what was done:

- **Numbers of payments are unusable there.** 40% of the North East's value sits in links with under 50 payments a month, which ONS records as none (Yorkshire and the Humber: 16%; London: 4%). On the raw figures the North East's payment count "rose 152%" and its average payment "fell 30%", which is rounding, not fact. The build now measures this for every region. Above 25% it stops quoting payment counts in the text, and the quality line says how much value gets no payment dots.
- **A third of its industries are too small to place** (25 of 74). They're drawn hollow as before. The chart is still readable, and the order's span stretches to 2.2 levels because small industries sit at the ends.
- **Text written for Yorkshire and the Humber was wrong elsewhere.** "Retail sits at the top" isn't true of the North East. Headings like "It agrees with the input-output table" claimed too much for a region where the figure is 0.52. Those are now neutral, with the region's own figure and the range across regions beside it.
- **Being thin didn't make its order worse.** The North East's order agrees with the input-output table as well as the North West's does (0.52 each), and is as steady from year to year.
- **"Thin" needed a rule.** A region is flagged when 35% or more of its monthly figures sit in links never published. That picks out the North East, Wales and Northern Ireland, and nothing else is close (the next highest is 29%).

**Other things settled along the way.**

- **Payment groups** are worked out per region and labelled by their largest members ("Wholesale, Retail and 11 more"), with hand-given names kept for Yorkshire and the Humber. The North West and North East each get four groups.
- **Industrial strategy shares** now come from one source for every region: jobs by ITL3 area and five-digit SIC for 2023. Yorkshire and the Humber's shares were previously from 2024 local-authority figures, so its wedges have moved slightly (by 4 points at most).
- **The outside partner shown separately** in the side panel is named by the data: London everywhere, and for London its largest partner.
- **A bad month in the address** now falls back to the latest month (the bug noted in 10.1).

**Not done.**

- **The other nine regions** were built and staged, then added to the site: see 10.6.
- **Section 10.1,** the link work, apart from the region entry and the month fix.
- **"Actual amounts" still aren't comparable between regions.** Each region sets its own reference width.
- **The payment groups outside Yorkshire and the Humber** haven't been looked over.

### 10.6 The other nine: built, staged, then added to the site

**Added to the site on 23 November 2026,** at Dan's request, by copying the staged folder over `data/`. All twelve regions are now offered. `tests/13` on the site itself: all twelve load with no errors and no unfilled text. The points under "To settle" below are still open.

**Built on 23 November 2026** with `Rscript build_data.R stage …`, which writes to `local/paymentflowsviz_ynh/staged_data/` and leaves the site's `data/` alone. All nine built without error in 21 minutes. The staged folder holds all twelve regions and its own index, 30 MB in all, so putting them on the site is a matter of copying it over `data/`.

**Checked on a private copy of the page** holding the staged data (`tests/13` pointed at it): all twelve load with no errors and no unfilled text, and switching through all twelve keeps the pinned industry, month and settings.

| Region | Money running down the order | Agreement with input-output table | Hidden | Never published | Value with no payment count | Too small to place | Payment groups (core industries in each) |
|----|----|----|----|----|----|----|----|
| North East * | 74% | 0.52 | 71% | 41% | 40% | 25 of 74 | 4 (10, 5, 12, 5) |
| North West | 70% | 0.52 | 49% | 22% | 11% | 6 of 78 | 4 (13, 18, 17, 5) |
| Yorkshire and the Humber | 71% | 0.71 | 56% | 27% | 16% | 11 of 77 | 5 (9, 8, 16, 3, 7) |
| East Midlands | 72% | 0.75 | 58% | 28% | 22% | 14 of 76 | 2 (6, 12) |
| West Midlands | 69% | 0.55 | 56% | 28% | 15% | 10 of 77 | 4 (16, 20, 7, 5) |
| East of England | 73% | 0.65 | 54% | 24% | 14% | 8 of 76 | 4 (9, 11, 20, 13) |
| London | 68% | 0.63 | 52% | 24% | 4% | 4 of 79 | 5 (16, 9, 16, 7, 7) |
| South East | 67% | 0.46 | 48% | 21% | 9% | 4 of 78 | 4 (15, 14, 16, 8) |
| South West | 72% | 0.54 | 55% | 27% | 20% | 6 of 76 | 6 (10, 10, 10, 10, 10, 3) |
| Wales * | 73% | 0.42 | 71% | 39% | 38% | 24 of 74 | 4 (12, 8, 11, 10) |
| Scotland | 71% | 0.51 | 58% | 28% | 22% | 7 of 78 | 4 (13, 14, 13, 11) |
| Northern Ireland * | 78% | 0.29 | 68% | 38% | 22% | 17 of 75 | 3 (10, 16, 14) |

\* Flagged as thin.

**What turned up.**

- **The East Midlands barely has payment groups.** Only 18 of its 76 industries sit firmly in one. Its first build gave two "groups" of a single industry each; the build now needs three core industries before it calls something a group, which doesn't change any region already on the site.
- **Northern Ireland is a different kind of case, not only a thin one.** It is the only region where more is paid inside than to or from the rest of the UK: for every £1 inside, 87p goes out and 68p comes in. London (£1.29 out) and Scotland (£1.93) come nearest; the other nine run from £2.36 (South East) to £4.30 (North East). An earlier version of this note said every other region sent out three to four times its inside total, which was wrong. Northern Ireland's order is also the most one-way (78%) and agrees least with the input-output table (0.29). It has no industrial strategy highlight, as expected, and the page says why.
- **London works as built.** The partner shown separately in its side panel is the South East. It is the least hidden on payment counts (4%).
- **Wales behaves like the North East:** 38% of its value has no usable payment count, so the build drops payment counts from its text too.
- **Agreement with the input-output table varies more than anything else,** from 0.29 to 0.75, and not with how thin the data are: the South East, the best published region, is second lowest at 0.46.

**To settle before these go on the site.**

- **Whether Northern Ireland needs its own wording.** Its inside-heavy balance and low agreement suggest its payments are being recorded differently, or that its economy really is that self-contained within the UK. Cross-border trade with Ireland isn't in the data at all. I'd add a sentence saying so. Not yet added.
- **London and Scotland are also inside-heavy** compared with the English regions outside London. For London that fits head offices registering other regions' business there. Worth a line on those two pages as well.
- **Whether a region with almost no payment groups should offer them.** For the East Midlands I'd keep the option but say on the page that few industries group there.
- **Repository size.** 30 MB of data files, added to the history again on each rebuild.
- **A look at each region's picture by a person,** as Yorkshire and the Humber had. The checks show the pages work, not that they read sensibly.

## 11. Scoped, not built: a foundational economy mapping

*Scoped on 23 November 2026, with tests `tests/17_foundational_read_list.py`, `tests/18_foundational_divisions.R` and `tests/19_foundational_tradeable.R`. The decisions in 11.9 are taken. Nothing here is on the page yet.*

**In short.**

- **Use the Foundational Economy Collective's own list.** It is the only definition I found that goes down to codes: 731 five-digit SIC codes, each placed in one of four zones. Counted on 2023 jobs it gives back the Collective's published totals, so it has been read correctly.
- **It fits two-digit divisions far better than the industrial strategy list does.** 70 of 84 divisions sit almost wholly in one zone. Eight are split, and two of those are big: retail and wholesale.
- **So carry shares, as the industrial strategy highlight already does,** and offer the zones as separate highlights. Keep "overlooked" apart from the foundational economy proper.
- **The payments show the foundational economy as a buyer, not at its real size.** In Yorkshire and the Humber the public-service industries hold 30% of jobs but receive 2% of the payments between industries. Their income comes from the state and from households, which this data doesn't see.

### 11.1 What existing research offers

| Source | What it gives | Codes |
|----|----|----|
| Foundational Economy Collective, [activity classification](https://foundationaleconomy.com/activity-classification/) (spreadsheet uploaded January 2019) | Every five-digit SIC 2007 code placed in a zone: material, providential, overlooked or other | 731 five-digit codes |
| The Collective, "Wales through a foundational lens" (2018 slides, linked from the same page) | Each zone's share of jobs, GVA and pay in Wales, Scotland and the UK, 2007 and 2016 | None, but totals to check against |
| Welsh Government, [foundational economy mission statement](https://www.gov.wales/foundational-economy-mission-statement-html) (April 2025) | Seven named sectors: care and health; management of social housing; construction of buildings, energy and utilities; food; high street retail and services; tourism; public transport | None |
| Bevan Foundation, "Fair work in the foundational economy: key data" (2021) | Six divisions, which it calls "a subset": 47, 55, 56, 86, 87, 88. 35% of Welsh employment | Two-digit |
| This project's handover (the South Yorkshire deck) | A "foundational" segment: retail (47), transport (H), hospitality (I), public services (O, P, Q) | Sections |
| `whole_economy_thinking`, tradeable jobs round 2 (`functions_io.R`) | Material foundational (farming, food and drink, utilities, construction, wholesale and motor trades, transport); everyday services (retail, hospitality, arts and personal); housing; providential (O, P, Q) | Sections and division ranges |

**The Collective's zones,** in its words:

- **Material:** "the pipe and cable utilities, supermarkets etc. which through networks and branches continuously connect households to daily essentials". Named: energy, water, telecoms, transport, food (growing, processing, distribution), retail banking, and car retailing and servicing but not car making.
- **Providential:** "a subset of (mainly) public sector welfare activities providing the universal services available to all citizens". Named: health, education, care, police, public administration, and close private suppliers such as dispensing chemists "but not the whole chain".
- **Overlooked:** "goods and services culturally defined as essential and requiring occasional purchase, for example, the sofa for your house, the holiday from work". Named: clothing, furniture, house maintenance, hairdressing, vets, tourism, hospitality.
- **Other:** the rest. The Collective counts only material and providential as the foundational economy. Adding overlooked gives what it calls "FE+".

**Two things about the spreadsheet.** It has no zone column: the zone is the colour of each row, with a key at the foot, so test 17 reads the colours. And the Collective says activities have "occasionally shifted in and out" since 2013 without changing the totals much. I found no later version.

**Not read.** Three papers that apply the classification refused automated access (Sissons at Keele; Earle and others 2018 at Cardiff; Calafati and others 2019 in Renewal), and the Wales Centre for Public Policy's job progression annex has gone. The Collective's 43 to 44% figures below come from how those papers are cited, not from the papers. A Welsh Government spreadsheet of "business statistics for the foundational economy" is mentioned in a Federation of Small Businesses submission; I couldn't find it, and it would be the nearest thing to an official code list.

### 11.2 Has the list been read correctly?

Jobs are BRES employment for 2023 by five-digit code, Great Britain. Every code with jobs matched the list.

| | This test, 2023 | The Collective's figure |
|----|----|----|
| Material | 16.3% | |
| Providential | 27.0% | |
| Material and providential | 43.3% | 43% of UK employment in 2016; 44% in a 2020 estimate |
| Overlooked | 19.8% | |
| All three | 63.2% | About 62% (UK, 2016) |
| Wales, material and providential | 51.4% | 49% (2016) |
| Wales, all three | 71.7% | 70% (2016) |

They agree to within a couple of points, seven years apart. The colours have been read the right way round.

### 11.3 How well it fits divisions

Of the 84 divisions in the payments data that have jobs recorded (metal ore mining has none):

| Fit | Divisions | Share of jobs |
|----|----|----|
| Whole: 90% or more of jobs in one zone | 70 | 79% |
| Most: two thirds or more | 6 | 6% |
| Split | 8 | 15% |

Compare the industrial strategy list (section 2.5), where 11 of 40 divisions are covered only by a code inside them and coverage runs from 85% down to 0.2%.

**The eight split divisions** (share of Great Britain jobs in each zone):

| Division | Material | Providential | Overlooked | Other | What's inside |
|----|----|----|----|----|----|
| 47 Retail | 43% | 6% | 23% | 28% | Supermarkets, food shops and fuel are material; chemists and opticians providential; clothes, furniture and hardware overlooked; online, sports goods and general stores other |
| 46 Wholesale | 8% | 0% | 30% | 61% | Food wholesale is material; building materials, plumbing and household goods overlooked |
| 41 Building construction | 0% | 0% | 55% | 45% | House building is overlooked; commercial building and development other |
| 53 Post and courier | 55% | 0% | 0% | 45% | The universal postal service is material; couriers other |
| 33 Machinery repair and installation | 34% | 0% | 6% | 60% | Repair of aircraft, trains and ships is material |
| 23 Glass, cement and ceramics | 0% | 0% | 51% | 49% | Bricks, tiles, ceramics, cement and concrete are overlooked; glass and stone cutting other |
| 95 Repair of goods | 0% | 0% | 35% | 65% | Repair of household goods is overlooked; computer and phone repair other |
| 50 Water transport | 44% | 0% | 56% | 0% | Freight and inland passenger boats are material; sea passenger transport overlooked |

**What one label per division would lose.**

- **Label every division by its main zone:** 91% of jobs get the right zone. But material is overstated, at 21% of jobs against a true 16%, because all of retail takes the label when 43% of it is material.
- **Leave the eight split divisions unlabelled:** 85% of jobs sit in a labelled division, and 98% of those get the right zone.
- **Carry shares:** nothing is lost. Each division's national shares, applied to a region's jobs by division, give that region's zone shares to within 0.8 of a point of the five-digit answer. So national shares can stand in where a region has no five-digit jobs data, which is Northern Ireland.

### 11.4 The mapping

Where a division is not whole, the share of its jobs in the zone is in brackets.

| Zone | Divisions |
|----|----|
| **Material** (17) | 01 Farming, 03 Fishing, 05 Coal mining, 06 Oil and gas extraction, 09 Mining support, 10 Food manufacturing, 11 Drinks manufacturing, 35 Energy supply, 36 Water supply, 37 Sewerage, 38 Waste (85%), 42 Civil engineering, 45 Motor trades, 49 Land transport, 52 Warehousing and transport support, 61 Telecoms, 64 Banking and finance (74%) |
| **Providential** (5) | 84 Public administration, 85 Education, 86 Health, 87 Residential care, 88 Social work |
| **Overlooked** (18) | 02 Forestry, 08 Quarrying, 13 Textiles, 14 Clothing, 15 Leather, 16 Wood products, 17 Paper, 18 Printing, 31 Furniture, 43 Building trades, 51 Air transport, 55 Accommodation, 56 Food and drink service, 75 Veterinary, 79 Travel agencies, 91 Libraries and museums, 93 Sport and recreation, 96 Personal services (68%) |
| **Split** (8) | 23, 33, 41, 46, 47, 50, 53, 95, as in the table above |
| **Other, with a part in a zone** (3) | 20 Chemicals (28% overlooked: soap and toiletries), 27 Electrical equipment (19% material: electricity distribution gear; 12% overlooked: domestic appliances), 68 Real estate (26% overlooked: estate agents) |
| **Other** (33, and metal ore mining) | 12, 19, 21, 22, 24, 25, 26, 28, 29, 30, 32, 39, 58, 59, 60, 62, 63, 65, 66, 69 to 74, 77, 78, 80, 81, 82, 90, 92, 94 |

The full table, with every division's shares, is `tests/outputs/foundational_division_shares.csv`. The five-digit list is `tests/lookups/foundational_sic5.csv`.

**Each region's jobs by zone** (five-digit, 2023):

| Region | Material | Providential | Overlooked | Other |
|----|----|----|----|----|
| North East | 16% | 33% | 20% | 31% |
| North West | 16% | 28% | 20% | 37% |
| Yorkshire and the Humber | 19% | 28% | 20% | 32% |
| East Midlands | 20% | 27% | 20% | 33% |
| West Midlands | 18% | 27% | 19% | 37% |
| East of England | 18% | 25% | 21% | 37% |
| London | 12% | 22% | 17% | 49% |
| South East | 15% | 27% | 21% | 37% |
| South West | 17% | 27% | 23% | 33% |
| Wales | 18% | 34% | 20% | 28% |
| Scotland | 21% | 31% | 20% | 29% |

### 11.5 Simpler mappings, measured against the list

"Foundational" here means material plus providential.

| Mapping | Divisions | Share of all jobs | Of its jobs, foundational on the list | Share of foundational jobs it catches |
|----|----|----|----|----|
| Handover segment: retail, transport, hospitality, public services | 13 | 48% | 73% | 81% |
| Tradeable jobs typology: material foundational plus providential | 26 | 46% | 82% | 86% |
| The same with everyday services | 36 | 66% | 62% | 95% |
| Bevan Foundation subset | 6 | 30% | 59% | 41% |
| The list by division, main zone material or providential | 24 | 48% | 89% | 99% |
| The same, leaving out split divisions | 22 | 39% | 99% | 88% |

- **The handover segment** misses a fifth of foundational jobs: utilities, food manufacturing, farming, telecoms, motor trades, civil engineering and banks. A quarter of what it does include is not foundational on the Collective's terms, mainly hospitality, which the Collective calls overlooked.
- **The tradeable jobs typology** is closer. Its main differences are counting all construction and all wholesale as material, where the list has only civil engineering and food wholesale.
- **Both get the providential zone almost exactly right:** it is sections O, P and Q, plus dispensing chemists and opticians inside retail. Those five divisions hold 98% of providential jobs.

### 11.6 Where the list is odd, or needs a caution here

- **Housing is mostly left out.** Housing associations and letting are "other"; house building and estate agents are overlooked. The Collective says housing was excluded from its original count, and the Welsh Government includes social housing management. Division 68 can't separate them anyway. I'd follow the list and say housing isn't covered.
- **Overlooked includes whole manufacturing divisions** (textiles, paper, printing, wood, quarrying), because what they make ends up as everyday household goods. In a picture of payments between businesses these are suppliers to other firms. This is the list's weakest fit for this page, and the main reason to keep overlooked as its own highlight.
- **Coal, oil and gas extraction are material** (energy supply), but refining is other. Small in most regions.
- **Banking is 74% material** (banks and building societies), but here it is an intermediary whose payments are mostly money passing through. Show its share only when intermediaries are switched in.
- **Non-specialised food wholesale is "other"** while every specialised food wholesaler is material. It looks like an oversight. I'd leave it and note it.
- **Funerals** are providential in the Collective's text and overlooked in its spreadsheet. The test follows the spreadsheet; 28,000 jobs.
- **Outsourced foundational work is counted as "other".** Agency staff, cleaning and facilities firms, and medical instrument makers work for hospitals, schools and care homes but are classed by their own industry. The Collective's rule is to include close suppliers "but not the whole chain". The chain is what this page can show.
- **A clash of names.** The industrial strategy highlight already has a sector called "Foundational industries" (chemicals, steel, materials and energy inputs). It has nothing to do with the foundational economy. Two unrelated "foundationals" in one menu need a word of explanation.

### 11.7 What the payments can and can't show

Yorkshire and the Humber, 77 industries, payments between different industries inside the region over the 93 months. Each division's payments are split by its job shares.

| Zone | Jobs | Payments made | Payments received |
|----|----|----|----|
| Material | 19% | 26% | 22% |
| Providential | 30% | 8% | 2% |
| Overlooked | 21% | 25% | 26% |
| Other | 31% | 40% | 51% |

- **The providential zone is nearly invisible as a seller, because of what the page leaves out.** Checked in test 19. Yorkshire and the Humber's education, health and care divisions received £82 billion over the period (published values). 4% came from industries in the region and 10% from industries in other regions. 69% came from payers whose industry isn't known and 15% from public administration with no region, and the page drops both. What does show is the zone's buying from other industries.
- **Public administration barely appears at all:** about £21 million paid to other industries in the region over the whole period, against about £2.1 billion by health. That is because it almost never has a region in the data. With no region it paid the region's 77 industries £49 billion; with a region, £0.05 billion.
- **So it sits at the buying end of the order.** On the page's scale, where the average industry is at zero and buyers are positive, and taking each division by its largest zone: providential +0.61 (education +0.74, health +0.63), material +0.15, overlooked +0.03, other −0.17. Material is spread out: telecoms, warehousing and land transport are on the supplier side (around −0.35), food manufacturing on the buyer side (+0.38).
- **Every zone buys about half its supplies from "other".** That holds for all four, from 48% (material) to 54% (providential).
- **Who the five public-service divisions pay.** 8% of what they pay inside the region goes to each other. Of the rest, the largest shares go to wholesale (14%), retail (10%), other manufacturing, which includes medical and dental instruments (10%), building trades (10%), IT and software (8%) and building construction (8%). Employment agencies and building services take 3% each.

That is what a foundational highlight would add to the page. It can't show how big the foundational economy is. It can show who supplies it, and how much of that supply is local.

### 11.8 Recommended use on the page

- **The lookup.** `tests/lookups/foundational_sic5.csv`, as read from the Collective's spreadsheet, unchanged.
- **The build.** For each region, the share of each division's jobs in each zone, from that region's five-digit jobs, as the industrial strategy shares are worked out now. Northern Ireland takes the national shares. Three numbers on each industry in the data file.
- **Highlight.** A new group in the Highlight menu with four choices: material; providential; material and providential; overlooked. A division is filled like a pie to its share, which the page already does for the industrial strategy.
- **Columns** could take the zones as a fifth option, with a split division placed by its largest share. Worth trying once the highlight is in; I haven't tested how it looks.
- **Side panel.** For a pinned industry, a line giving its share in each zone.
- **A note on the page** saying: whose list it is and its date; that it was made to count jobs; that payments here are between businesses, so public services show as buyers; that public administration is almost absent; and that this "foundational" is not the industrial strategy's.
- **If a single label per division is ever needed** (for text, or for grouping), use the table in 11.4 and leave the eight split divisions unlabelled.

### 11.9 Decisions (taken 23 November 2026)

All four went with the recommendation.

- **Zones are kept apart.** Four highlight choices: material; providential; material and providential; overlooked. The Collective keeps them apart, and overlooked is the weakest fit here.
- **The list is followed exactly.** Its oddities (section 11.6) are listed in the page's note, not corrected, so the page can cite a published source.
- **The list stays in the public repository, credited.** The spreadsheet is public but its page states no licence. The lookup holds its codes, the standard descriptions and the zone, with the source named. Asking the Collective would settle the licence question for good.
- **Menu name:** "Foundational economy", with a few words after each zone, for example "Providential: health, education, care".

Not yet built. The steps are in section 11.8.

### 11.10 Foundational against tradeable

*Test `tests/19_foundational_tradeable.R`, 23 November 2026. Done before deciding how to show the zones.*

**The question.** Is "foundational" the same thing as "non-tradeable"? If it were, one could stand in for the other on the page.

**Three measures of tradeable,** because the payments see only part of it:

| | Measure | What it can't see |
|----|----|----|
| A. Payments | The share of an industry's receipts that comes from outside its region, placed among the other industries in the same region. All twelve regions | Households, exports abroad, and where work is done (firms sit where they are registered) |
| B. Jobs | How unevenly an industry's jobs are spread across 168 areas of Great Britain, against all jobs. Local-serving industries spread like people do | Why an industry bunches; small industries look bunched anyway |
| C. Input-output table, UK 2019 | The share of each product that goes abroad, to households, to government, and to other businesses (from the tradeable jobs work in `whole_economy_thinking`) | Trade between regions; anything regional |

Measure A is test 05's "own-region share of income" turned round, for every region. For Yorkshire and the Humber it matches test 05 exactly.

**The answer: they are two different things, except for public services.**

| Zone | Jobs (m) | A. Place among a region's industries (0 most local, 1 most outside) | B. Bunching of jobs | C. Sold abroad | C. Sold to households and government | Jobs in products that are a quarter or more exported |
|----|----|----|----|----|----|----|
| Material | 4.9 | 0.57 | 0.24 | 8% | 40% | 3% |
| Providential | 8.7 | 0.35 | 0.13 | 3% | 86% | 0% |
| Overlooked | 6.4 | 0.45 | 0.16 | 10% | 54% | 18% |
| Other | 10.6 | 0.51 | 0.25 | 20% | 17% | 33% |

Averaged over each zone's jobs; the four intermediary divisions are left out.

- **Providential is local on every measure.** Two thirds of its jobs are in the most local-selling third of industries on the payments and none in the most outside-selling third. Its jobs are spread like the population, and 86% of what it produces goes to households and government.
- **Material is foundational but not local.** On the payments it sells outside its region slightly more than the average industry, in 11 of 12 regions. Food manufacturing and telecoms are among the most outside-selling industries of all, and motor trades, energy and warehousing are above the middle. Its jobs bunch as much as the "other" zone's do. Nationally, little of what it makes is sold abroad (8%). That figure is from the input-output table: the payments have no category for abroad. Taken together the measures point to trade between regions more than between countries, but none of them shows it directly. The payments show where the payer is registered, and cover a minority of receipts (below).
- **"Other" is not the tradeable economy.** It sits in the middle on the payments. A third of its jobs are in products that are a quarter or more exported, and about a quarter are in the most local-selling third of industries: legal and accounting, real estate, security, building services. These are local business services, neither foundational nor tradeable.
- **Overlooked is mostly local with an exporting tail:** clothing, textiles, air transport and accommodation.

**The payments measure is weak, and the page shouldn't lean on it.**

- **No category for abroad.** Regions are the twelve plus "unknown". Exports and imports aren't in the data, so anything said here about selling abroad comes from the input-output table.
- **It covers about a third of receipts.** Of what Yorkshire and the Humber's industries received, 36% came from industries with a known region. 47% came from payers of unknown industry, 10% from the intermediaries and 7% from public administration with no region. This measure leaves all of those out, as the page does.
- **Little spread.** In Yorkshire and the Humber eight in ten industries get between 59% and 85% of their receipts from outside the region. This is why the ring gauge was dropped (section 2.5).
- **An industry's place changes from region to region.** The rank correlation between two regions' orderings averages 0.38. London and Northern Ireland are near 0.15 against everyone else. If selling outside belonged to the industry, this would be high. It belongs as much to where firms are registered.
- **It agrees only loosely with the other two:** 0.21 with bunching of jobs and 0.47 with exports abroad. Those two agree with each other at 0.54.
- **It misses the customers that make an industry local.** Retail and food and drink service sell over 80% to households and government, which the payments don't see. They come out in the middle on what they sell to other businesses.
- **Outside payments are more hidden than inside ones:** 20% of outside value is held in Yorkshire and the Humber against 10% of inside value, and 40% against 8% in Northern Ireland. Using published values only barely changes the ordering (0.92).
- **Regions differ on public services.** The providential zone's outside share of receipts is below the regional average in ten regions, by 3 to 27 points (Wales 27, the North West 21). It is level with the average in Yorkshire and the Humber and above it in Northern Ireland. Not looked into.

**What this means for showing the zones.**

- **Don't let "foundational" stand for "local", or "other" for "tradeable".** If both ideas are wanted on the page they need to be two separate things to look at.
- **If tradeable is shown, take it from a measure fixed to the industry,** the same in every region: the input-output shares (abroad, households, government, other businesses) are the clearest, and they already exist in the tradeable jobs work. The payments' outside share is too flat and too tied to registration to carry it.
- **The combination is the interesting part.** Four kinds of industry come out, not two: local public services (providential); networks and supply chains that reach beyond the region (material); local business services (a quarter of "other"); and exporters (a third of "other", and overlooked's tail). A view that could show foundational zone and who the industry sells to together would show that.

Results by division are in `tests/outputs/foundational_tradeable_divisions.csv`, by zone in `foundational_tradeable_zones.csv`, and by zone and region in `foundational_tradeable_zone_by_region.csv`.

### 11.11 Later: bringing public administration's payments in

*An idea to come back to, not scoped. Raised 23 November 2026.*

**What is there.** In every region, public administration with no region is a large payer of the region's industries, and the page drops it:

| Region | Public administration's payments to the region's industries (£bn, published) | Share of what those industries were paid | Share of what education, health and care were paid |
|----|----|----|----|
| North East | 22 | 11% | 28% |
| North West | 102 | 9% | 39% |
| Yorkshire and the Humber | 49 | 7% | 15% |
| East Midlands | 60 | 9% | 42% |
| West Midlands | 92 | 10% | 49% |
| East of England | 109 | 11% | 47% |
| London | 320 | 10% | 40% |
| South East | 188 | 10% | 34% |
| South West | 72 | 12% | 43% |
| Wales | 18 | 9% | 19% |
| Scotland | 19 | 4% | 12% |
| Northern Ireland | 10 | 5% | 13% |

**Why it isn't simple.**

- **It has no region,** so it is neither inside the region nor from the rest of the UK. It would have to be shown as a third thing: a payer that stands outside the map.
- **It runs both ways.** Counting every payer and payee in Yorkshire and the Humber, £56 billion was received from public administration with no region and £20 billion was paid to it. Some of what is paid to it will be tax and rates.
- **More of it is hidden:** 40% of its monthly figures touching Yorkshire and the Humber, so held values would matter.
- **It varies a lot by region.** It is 49% of what education, health and care were paid in the West Midlands and 12% in Scotland. That may be devolved funding going by another route, or bodies being registered differently. It would need looking into before any comparison between regions.
- **Payers of unknown industry are bigger still,** at 38% to 78% of what education, health and care were paid, and nothing says who they are.

**A first thought.** Treat it like the halo: for each industry, an amount received from and paid to public administration, drawn separately from the rest-of-UK halo and off unless switched on. It wouldn't join the order, because it has no counterpart inside the region.

### 11.12 Filling the gaps from the input-output table: examined, and not recommended as a switch

*Test `tests/20_io_estimates_scoping.R`, 23 November 2026. Nothing built.*

**The idea.** Use the UK input-output or supply and use tables to estimate what the payments leave out (sales to households, to government and abroad), split the national figures between regions with a proxy, and put a switch on the page to add them.

**In short: I'd push back.** The tables exist for most of the years, and the arithmetic is easy. But where the table and the payments can both be seen they disagree too much for one to fill in for the other, and three separate problems would each make the added figures misleading: no steady rate of exchange between the two, no way to know what is already counted, and a different geography. There are sturdier ways to get most of what the idea is after, at the end of this section.

**What data there is.**

| | Years | Notes |
|----|----|----|
| Input-output analytical tables (ONS) | An edition each for 2019, 2020, 2021, 2022 and 2023; 2024 is provisionally due on 26 November 2026 | Two years behind. Each is built on a different year's national accounts, so they are not one consistent series |
| Supply and use tables (ONS) | Every year 1997 to 2023, one consistent set | The same lag |
| The payments | Monthly, January 2019 to September 2026 | |
| On disk | The 2019 analytical table only | Also Scotland's own supply and use tables for 1998 to 2022, and a 2019 workbook of trade between UK regions, neither examined |

So the tables cover five of the payments' eight years, annually. 2025 and 2026 won't have one for two years, and 2020 and 2021 are pandemic years. A month-by-month picture of the estimated part would have to be invented.

Regional proxies exist too: ONS regional GVA by industry, the Treasury's public spending by region and function, ONS regional household spending, and regional trade figures. I haven't checked their latest years. Only jobs by region and industry are on disk, and that is what the test uses.

**Test 1: does the table describe the payments where both can be seen?** National payments for 2019 (the file with no regions, so public administration is all there) against the 2019 table, 75 groups of industries, intermediaries left out of both.

- **There is no steady rate of exchange.** Payments between industries come to £482 billion against the table's £1,231 billion of sales between industries. Industry by industry, payments run from 0.06 of the table's figure (a tenth of industries are below that) to 2.1 times (a tenth are above), with a middle of 0.30. So a table figure can't be turned into "payment pounds" by one factor, and a factor for each industry is the thing being guessed.
- **The patterns are alike in outline only.** Ranked by size, who-sells-to-whom agrees at 0.62. But 62% of the money would have to move for one pattern to match the other. Two regions' payments differ from each other by 33 to 66% (section 10.3), so the national table is as far from the national payments as one region is from another.
- **Public administration's payments are not the table's government spending.** It paid other industries £73 billion. The table has £54 billion of purchases by public administration and £276 billion of government spending on other industries' output, mostly health, education and care. Health gets 4% of public administration's payments and 46% of the table's two figures together. The payments go instead to construction (15%, against 1.5% in the table), consultancy and IT. So the funding of health and education doesn't arrive as "public administration pays health". It is somewhere else in the data, or not in it, and a table-based figure for it could double count.
- **Nothing says what the unknown half is.** Industries received £482 billion from each other, another £482 billion from payers of unknown industry, £156 billion from the intermediaries and £73 billion from public administration. The table's household spending on their output is £942 billion. Receipts from unknown payers follow the table's sales to businesses (0.74) more than its sales to households (0.47) or government (0.21): the unknown payers look like businesses. Retail gets £18 billion through the intermediaries against £124 billion of household spending in the table. So an estimate of household or government spending can't be said to fill a gap, because the size of the gap isn't known.
- **Scaling up from what is seen fails where it matters most.** The alternative to a rate of exchange is to multiply each industry's business receipts by the table's ratio of other sales to business sales. That multiple is 42 for health, 10 for public administration, 7 for food and drink service and 5 for retail and for education. The estimate would be many times the data, resting on the smallest and least reliable part of it.

**Test 2: does a regional proxy put income where the payments put it?** Each industry's receipts in 2019 split between the eleven regions of Great Britain by its jobs, against where the payments record them.

- **A jobs split puts a quarter of the money in the wrong region:** 25% for the middle industry's receipts from businesses (20% weighted by value), and 40% for receipts from public administration (26% by value).
- **The payments' geography is registered offices.** London has 17% of jobs and 29% of industries' receipts. Scotland has 8% of jobs, 4% of receipts from businesses and 2% of receipts from public administration. Wales has 4% of jobs and 1% of receipts.
- **So an estimate split by jobs or GVA would sit on a different map** from the payments beside it: where people work, against where firms are registered. Adding them would make London look smaller and Scotland and Wales larger than the rest of the page says, for the estimated part only.

**What it would take anyway.** Four more tables to download and one mapping of their 105 products to divisions; regional GVA by industry for the split; a rule for 2024 to 2026; a monthly shape for annual figures; a rule against double counting when intermediaries are switched in; and the same for all twelve regions. A few days' work. The work isn't the objection. The result would be a modelled layer several times the size of the data for exactly the industries in question, and switching it on would make the page's measures more complete-looking, not more complete.

**What I'd do instead.**

- **Bring in what the data already has, as data.** Public administration's payments to each region's industries are observed, monthly and by region (section 11.11). The intermediaries are already a switch. These make the picture more complete without estimating anything.
- **Use the table for shares, not pounds.** For each industry, the national share of its output that goes to other businesses, households, government and abroad, shown in the side panel and offered as a highlight or columns. It is fixed to the industry, needs no regional split and no rate of exchange, and one year's table is enough. This is also the measure section 11.10 recommends for "tradeable". It tells the reader what the page can't see for that industry, which is the honest version of filling the gap.
- **If a full estimated picture is wanted, make it its own view, in the table's pounds,** not a switch on the payments: a regional input-output estimate with households, government and abroad as extra buyers, set beside the payments for comparison. That is a known kind of model with known errors, and a project in its own right. It belongs with the flow model in `modelling_demand` more than on this page.
- **Ask ONS what "unknown industry" is.** If it is businesses whose industry couldn't be matched, the gap is mostly coverage of payments between businesses, which no table can fill. That one answer would settle more than any proxy.

Results are in `tests/outputs/io_estimates_by_industry.csv` and `io_estimates_region_proxy.csv`.

### 11.13 Built: the highlight, and columns by ties

*Built 23 November 2026, from the payment flows and the jobs data only. Sections 11.11 and 11.12 are parked.*

**What was asked.** How best to show foundational against other: highlight the circles where they are, or find a column split that isn't simply the zones against the rest.

**Both, because they answer different questions.**

- **Highlight in place** shows where the foundational industries sit in the order and which links are theirs. It is the clearer of the two and the one to lead with.
- **Columns by ties** answers the question the payments are best placed to answer: who trades with the foundational economy, and who doesn't.

**Why not the zones as columns.** "Other" would hold 36 industries and the eight split divisions belong to no single column. It would also only repeat what the highlight shows.

**The highlight.**

- **Four choices** in a new "Foundational economy" group of the Highlight menu: material and providential; material; providential; overlooked (section 11.9).
- **Shares by region.** Each industry carries the share of its jobs in each zone, from that region's five-digit jobs (`fe_for()` in `build_data.R`). Northern Ireland, and any division with no jobs recorded in a region, takes the Great Britain shares.
- **Drawn as the industrial strategy highlight is:** a filled circle is wholly in the zone, a wedge is the share. Retail in Yorkshire and the Humber is 44% material, 6% providential and 23% overlooked.
- **Side panel:** a "Foundational economy" line for the pinned industry.
- **A note on the page** names the list, says it was made to count jobs, gives the region's jobs in each zone, says these industries show by what they buy, and separates it from the industrial strategy's "Foundational industries".

**Columns "By ties to what is highlighted".**

- **The measure:** of everything an industry pays to and is paid by other industries in the region, over the whole period, the share that is with the highlighted industries. A partner that is 40% in the highlighted set counts for 40% of the payment.
- **It works with any highlight,** not only the foundational one: a sector group, a payment group or an industrial strategy sector. With no highlight chosen the columns fall back to sector groups and the key says to choose one.
- **Laid out like "by how much its place shifts":** industries in rank order from least tied to most, percentages along the top, small industries in a strip on the right. Height is still the place in the order.
- **There is enough spread to use** (`tests/21_ties_columns.cjs`). For material and providential in Yorkshire and the Humber a tenth of industries are below 17%, the middle is 27% and a tenth are above 43%. Across the twelve regions the middle runs from 21% to 31%, and the South East has the narrowest spread (15% to 34%). For providential alone Yorkshire and the Humber has 1%, 4% and 15%, with a few far above.

**What it shows in Yorkshire and the Humber** (material and providential highlighted).

- **Most tied:** farming (58%), post and courier (57%), social work (52%), personal services (51%), residential care (45%), wholesale (43%), warehousing (40%), rental and leasing (40%). Personal services, wholesale and rental are not foundational themselves: they are its close local partners.
- **Least tied:** electronics (12%), wood products (13%), fabricated metal (15%), basic metals (15%), civil engineering (16%). Mostly manufacturing.
- **Education sits near the least-tied end** (17%, eighth of 66 from the least) and health below the middle (23%). Their trade inside the region is mostly with industries outside the foundational economy, which is section 11.7's finding seen from the other side.

**To keep in mind.**

- **It measures payments between businesses inside the region,** about a tenth of what the region's industries are paid (section 11.10). It says nothing about households or public money.
- **The axis is a ranking,** so equal distances are not equal differences. The percentages along the top give the scale.
- **A highlighted industry is placed by its payments with the rest of the set,** so a set whose members trade little with each other, like the public services, ends up towards the least-tied end.

**Checks** (`tests/13_page_checks.cjs`, `tests/15_url_roundtrip.cjs`).

- **All twelve regions** were rebuilt and load with no errors and no gaps in the text. Yorkshire and the Humber's 14 build checks still match.
- **The address** carries the new settings: all 5 column choices and all 26 highlights come back.
- **Speed is unchanged:** 60 frames a second on the opening view, 56 with every link drawn.
- **Screenshots:** `tests/outputs/page/06b` to `06e`.

**Not done.** Nobody has looked at the eleven other regions' pictures with these options on. The ticks along the top skip any that would overlap, so a bunched measure (providential alone) shows few of them.

**Added afterwards: "sells to" and "buys from"** (23 November 2026).

- **Why.** The ties measure adds payments in both directions, so it can't tell a supplier of the highlighted industries from a customer of them.
- **Two more column choices** beside it: "By sales to what is highlighted" counts only what an industry is paid, and how much of that comes from the highlighted industries. "By purchases from what is highlighted" counts only what it pays, and how much of that goes to them.
- **The axis wording** is now "least tied to the highlighted industries" to "most tied", "sells least to" to "sells most", and "buys least from" to "buys most". The key gives the full definition.
- **The side panel** gives all three figures for a pinned industry whenever something is highlighted.
- **Spread** (`tests/21_ties_columns.cjs`, Yorkshire and the Humber, material and providential): sales to, a tenth of industries below 18%, the middle 31%, a tenth above 54%; purchases from, 12%, 19% and 40%. Sales spread industries out more than purchases in ten of the twelve regions; the East of England and Northern Ireland are the other way round.
- **What "sells to" shows for the public services:** after social work (69%) and residential care (46%), which sell to each other, come libraries and museums (39%), membership organisations (33%), sport and recreation (30%), publishing (27%) and other manufacturing (27%), which includes medical instruments.
- **"Buys from" the public services says little:** the middle industry makes 1% of its purchases from them, because they sell so little to other businesses in this data.
- **Checks:** all 7 column choices come back from the address; no page or console errors; 60 frames a second on the opening view and 54 with every link drawn. Screenshots `06f` and `06g`.

## 12. Built: columns by trade inside the region

*Built 24 November 2026. No new data: the page's data files already held everything it needs, so nothing was rebuilt.*

**What was asked.** Another set of column choices beside the ties ones: industries left to right by how much of their buying and selling is inside the region against outside it, in the same three versions (both together, sales, purchases). And, out of interest, whether that goes with the supplier-to-buyer order up the page.

### 12.1 The measure

- **Trade inside the region:** of everything an industry pays to and is paid by industries anywhere in the UK, over the whole period, the share that is with industries in its own region.
- **Sales inside** counts only what it is paid. **Purchases inside** counts only what it pays.
- **Where the sums come from:** the links (inside, between different industries), the payments within each industry inside the region, and the rest-of-the-UK series behind the halo. All were already in the data files.
- **It follows the intermediaries switch,** as the rest of the page does.
- **It needs no highlight,** and any highlight can be laid over it: the foundational economy zones on columns by sales inside, for example. That is not the view section 11.10 recommended, which would take who an industry sells to from the input-output table.

**Payments within an industry are treated like any others:** inside if both firms are in the region, outside if one is elsewhere. This needs saying only because of how the page's data is cut:

- **The links leave them out.** A link joins two different industries, so a health body paying another health body in the same region is in no link. Those payments are held separately in the data file.
- **The rest-of-the-UK figures keep them in.** They are a total for each industry over every industry elsewhere, its own included, and can't be split without a rebuild.
- **So a measure built only from what the page draws** (links against the halo) would count within-industry payments when they leave the region and drop them when they stay. The measure adds the within-industry payments inside the region back, so both sides are counted the same way.

**What adding them back changes.**

- **It matters for some industries.** In Yorkshire and the Humber, health's sales inside go from 7% without them to 26% with them, education's from 10% to 25%, retail's from 16% to 31%. In Wales health goes from 15% to 56%.
- **It moves the ranking a little in most regions and a lot in two.** The rank correlation between the two versions is 0.88 to 0.96 in Yorkshire and the Humber. For sales it falls to 0.73 in the West Midlands and 0.76 in Wales.
- **The side panel's bars had the mismatch, and now don't** (changed 24 November 2026, on request). Their "inside" segment used to be other industries here only, while the London and rest-of-the-UK segments included the same industry elsewhere. The inside segment now includes payments within the industry, its label is "Industries here", and the line beneath says how much of it that is. So an industry's monthly total and its inside percentage in the side panel are higher than before. Land transport in Yorkshire and the Humber, three months to September 2026: it pays £19.0m a month to other industries here and £9.5m within the industry, so inside goes from 16% of what it pays to 23%.
- **Not changed:** the circle's area and the hover box, which still count payments with other industries here, as the links do.

### 12.2 This is the measure section 11.10 called weak

"Sales inside" is section 11.10's measure A turned round, and section 2.5's ring gauge, which was dropped. What was said there still holds:

- **It covers about a third of what industries are paid.** Households, payers of unknown industry and public administration are on neither side.
- **No abroad,** so an exporter's sales abroad don't count as outside.
- **Registered offices,** so a chain run from elsewhere looks as if it trades outside. Veterinary practices come out least inside of all in Yorkshire and the Humber (10%).
- **An industry's place changes from region to region** (rank correlation 0.38 on average, section 11.10).

**Why it is still worth having as columns, where it wasn't as a gauge.**

- **A ranking spreads out anything.** The ring gauge failed because every ring looked alike. Columns in rank order always fill the width.
- **The cost is that small gaps look large.** In Yorkshire and the Humber half the industries sit between 21% and 34% (both together), 22% and 31% (sales) and 22% and 33% (purchases), so half the width of the chart covers about ten points. The percentages along the top show this.
- **So the page describes it as where the recorded payments go,** not as how local or how tradeable an industry is. The key and a note at the foot say what is left out.

**Spread** (`tests/22_local_columns.cjs`; a tenth of industries below, the middle, a tenth above):

| | Both together | Sales inside | Purchases inside |
|----|----|----|----|
| Yorkshire and the Humber | 17%, 26%, 37% | 15%, 26%, 41% | 17%, 28%, 40% |
| Middle industry, lowest region (East Midlands) | 20% | 21% | 22% |
| Middle industry, Scotland | 35% | 40% | 34% |
| Middle industry, London | 44% | 48% | 43% |
| Middle industry, Northern Ireland | 62% | 65% | 57% |

The other eight regions' middle industries are between 22% and 31% on all three.

### 12.3 Does it go with the supplier-to-buyer order?

**Barely.** Rank correlation with the level (buyers have high levels), across the industries placed in each region:

| Measure | Lowest region | Middle | Highest region | Regions above zero |
|----|----|----|----|----|
| Both together | -0.10 | 0.10 | 0.30 | 9 of 12 |
| Sales inside | -0.32 | -0.01 | 0.12 | 6 of 12 |
| Purchases inside | 0.06 | 0.28 | 0.46 | 12 of 12 |

Yorkshire and the Humber: 0.08, -0.08 and 0.27.

- **Purchases inside go weakly with being a buyer,** in every region. Industries towards the top of the page buy a little more of what they buy inside the region.
- **Sales inside and the two together don't go with the order.**
- **Part of the purchases link is built in.** Levels are worked out on payments inside the region alone. An industry that buys here and sells elsewhere is a net buyer here, so it sits high. Motor vehicles in Yorkshire and the Humber is the clear case: 50% of purchases inside, 17% of sales, and sixth of 66 from the top of the order.
- **But that is not all of it.** If it were, "purchases inside less sales inside" would track the level much better than purchases alone. It tracks it only slightly better: 0.15 to 0.56 across regions, about 0.32 in the middle.
- **Sales and purchases are related but not the same thing:** their rank correlation is 0.42 to 0.76 across regions, 0.50 in Yorkshire and the Humber.
- **For the chart this is good news.** Left-to-right position adds something height doesn't already show.

The correlation isn't shown on the page. It could be worked out there for whichever region is showing.

### 12.4 What it shows in Yorkshire and the Humber

- **Most inside, both together:** quarrying (63%), fishing (61%), farming (46%), libraries and museums (41%), remediation (39%), drinks manufacturing (38%), social work, legal and accounting, motor vehicles (37% each).
- **Least inside:** veterinary (10%), travel agencies (11%), insurance and pensions, energy supply, pharmaceuticals (13% each), IT and software, health (17% each).
- **Sells most inside:** libraries and museums (64%), fishing (60%), quarrying (58%), farming (50%), building construction (49%), repair of goods (48%). **Least:** pharmaceuticals (3%), veterinary (6%), energy supply (11%), travel agencies (12%), electronics, chemicals (14% each), food manufacturing (15%).
- **Buys most inside:** quarrying (73%), fishing (65%), motor vehicles (50%), textiles (48%), glass, cement and ceramics (46%). **Least:** insurance and pensions (7%), travel agencies (10%), IT and software (14%), health, rental and leasing (15% each).
- **Sales and purchases pull apart for some:** building construction sells 49% inside and buys 29%; retail 31% and 18%; motor vehicles 17% and 50%; food manufacturing 15% and 23%.

Figures for every industry and region are in `tests/outputs/local_by_industry.csv`.

### 12.5 On the page

- **Columns menu:** "By trade inside the region", "By sales inside the region", "By purchases inside the region", after the three ties choices. In the address: `cols=inside`, `sellsinside`, `buysinside`.
- **Axis wording:** "trades least inside the region" to "trades most"; "sells least inside the region" to "sells most"; "buys least inside the region" to "buys most".
- **Laid out like the ties columns:** rank order, percentages along the top, small industries in a strip on the right, height still the place in the order.
- **Ticks:** round tens first, then fives where there is room. Ticks on all the ranked columns now need at least 30 pixels between them, so they no longer touch on a narrow screen.
- **Key:** the definition for whichever of the three is chosen, and what is left out.
- **Side panel:** under the inside, London and rest-of-the-UK bars, one sentence with all three whole-period figures and the industry's place in the ranking. It shows whatever columns are chosen. The bars themselves now count payments within the industry as inside (section 12.1).
- **A note at the foot of the page** gives the three cautions: only the payments the page can place, registered offices, no abroad.

### 12.6 Checks, and what isn't done

**Checks** (`tests/13_page_checks.cjs`, `tests/15_url_roundtrip.cjs`).

- **No page or console errors** in any of the twelve regions, and no gaps in the text.
- **The address** carries the new settings: all 10 column choices and all 26 highlights come back.
- **Speed is unchanged:** 60 frames a second on the opening view and with every link drawn.
- **Screenshots:** `tests/outputs/page/06h` to `06j`. All three choices were also drawn for every region, and on a narrow screen, in the dark theme, with intermediaries in and in the goods view, with no errors. London's and Northern Ireland's were looked at; the rest were not.

**Not done.**

- **The correlation with the order isn't on the page.**
- **The axis wording** is a first go, as the ties wording was.

## 13. A note: Other manufacturing as a middle link, and how a chain might be shown

*24 November 2026. Tests `tests/23_middle_links.cjs` and `tests/23b_middle_links_io.R`. Sections 13.1 to 13.8 were written before anything was built; section 13.9 records what was then built.*

### 13.1 What was noticed

With columns by trade inside the region, Other manufacturing (division 32) in Yorkshire and the Humber sits in the middle left to right and in the middle of the supplier-to-buyer order. Its links come down to it from industries above and go on down to industries below. That looks like one stage in a chain. The order is known to be loose, so the question was how much the picture means.

**In short:** it is a real middle link, and the input-output table agrees. The default view makes it look cleaner than it is.

### 13.2 What holds up

All figures are for payments with other industries in the region, over the whole period, intermediaries left out.

- **It is balanced.** It pays £16.0m a month and is paid £17.5m.
- **Its buyers and its suppliers are different industries.**

| Paid most by | Pays it, a month | It pays back |
|----|----|----|
| Health | £4.1m | £0.2m |
| Retail | £2.4m | £1.1m |
| Building construction | £1.7m | £0.3m |
| Motor vehicles | £0.7m | £0.0m |

| Pays most to | It pays, a month | Paid back |
|----|----|----|
| Wholesale | £2.6m | £1.4m |
| Building trades | £1.5m | £1.0m |
| Fabricated metal | £1.4m | £0.4m |
| Rubber and plastics | £1.0m | £0.3m |

- **The jobs fit.** 57% of the division's jobs in the region are in medical and dental instruments (49% across Great Britain). Another 26% are in the catch-all "manufacturing not elsewhere classified", and the rest in sports goods, jewellery and toys. Jobs from the Business Register and Employment Survey 2023.
- **Its place is steady.** Worked out on the 12 months to each month, its level stays between -0.02 and 0.04.
- **The view the page opens on shows the same.** In the three months to September 2026 health pays it £5.1m a month net, and 97% of its net value runs down the order.

### 13.3 What makes it look cleaner than it is

| Measure | Other manufacturing | Middle industry of 66 |
|----|----|----|
| Value running down the order, one net link a pair | 98% (third highest) | 87% |
| Value running down the order, both directions counted | 75% (18th highest) | 70% |
| Payments that are two-way with the same partner | 48% | 46% |

- **The page draws one net link a pair.** Wholesale pays it £1.4m a month and it pays wholesale £2.6m, and the page draws one link for the difference. Nearly half of its payments are two-way like this. Switching Links to both directions shows them.
- **The level is fitted from these same payments.** Each industry is placed so that its buyers sit above it and its suppliers below, as far as the rest allow. Some agreement between an industry's links and its height is therefore automatic.
- **What isn't automatic** is how well the fit works for it: its buyers are industries that buy from nearly everyone, and its suppliers are ones that supply nearly everyone.
- **19 of its 152 links** in the region were never published.

### 13.4 Other regions, and the input-output table

- **Its level is near the middle in every region** (-0.21 to 0.14).
- **The clean picture is mostly Yorkshire and the Humber's.** Net value running down the order goes from 65% in the North West to 98% here, the highest of the twelve.
- **Health is its largest payer** only here and in the South East. Elsewhere it is building construction, wholesale, retail or, in the West Midlands, motor vehicles.
- **The UK input-output table has the same chain.** It has the division sell most to health and building construction, and buy most from wholesale, retail, fabricated metal, land transport and basic metals. On the table's own figures it is 16th of 69 industries by share passed along (section 13.6).

### 13.5 Not vertical integration

Vertical integration means one firm owning several stages of a chain. Payments between separate businesses show something closer to the opposite: stages done by different firms that pay each other. "A middle link in a chain" is the better description.

### 13.6 Scoped: a chain measure for every industry

**The idea.** Everything an industry pays to and is paid by other industries in the region splits exactly three ways. The split doesn't use the levels, so it isn't circular in the way section 13.3 describes.

| Part | What it is |
|----|----|
| Two-way | Paid to a partner that pays the same back: twice the smaller direction, pair by pair |
| Passed along | What remains is net: some partners pay it, it pays others. Twice the smaller of those two totals is money that comes in from one industry and goes on to another |
| End of the line | The rest. More paid out than in is a buyer end: money enters the picture here, from households, public money, payers of unknown industry and other regions. More in than out is a supplier end: money leaves the picture here, to wages, taxes, profit, imports and other regions |

Other manufacturing in Yorkshire and the Humber: 48% two-way, 47% passed along, 4% end of the line. Ninth of 66 by share passed along.

**What it shows.**

- **About a fifth of payments between industries are passed along inside a region:** 21% in Yorkshire and the Humber, between 19% (Northern Ireland) and 28% (Wales) across the twelve. Between 34% and 52% are two-way.
- **The input-output table passes along nearly twice as much:** 39%, with 27% two-way and 34% end of the line. Payments inside a region have more two-way money and shorter chains than the national supply chain. Part of that is the region's edge cutting chains off, and part is what payments include that sales don't (rent, refunds, money moving within groups).
- **Most passed along in Yorkshire and the Humber:** drinks manufacturing (69%), textiles (60%), air transport (58%), glass, cement and ceramics (56%), publishing, veterinary and clothing (51% each), furniture (49%), other manufacturing and civil engineering (47% each).
- **Least:** education (1%, a buyer end), legal and accounting (2%, mostly two-way), food and drink service and pharmaceuticals (5%, buyer ends), telecoms, IT and software and wholesale (5% to 7%, supplier ends).
- **Spread:** a tenth of industries are below 6%, the middle is 27% and a tenth are above 51%.

**Three weaknesses, which is why it shouldn't be a headline measure.**

- **It mostly restates height.** Passed along is what is left after two-way and the imbalance between paid and received, and the level is worked out from that same imbalance. Its rank correlation with closeness to the average level is 0.63 to 0.80 across regions. Columns or a highlight by it would largely show the middle of the page again.
- **It doesn't belong firmly to the industry.** The rank correlation between two regions' values for the same industries is 0.35 in the middle (0.06 to 0.64). The level manages 0.64 and two-way 0.52.
- **It agrees only loosely with the input-output table.** The same split on the table gives a rank correlation of 0.44 with the payments when each division takes its middle value across regions, and 0.30 for Yorkshire and the Humber alone. Balance agrees much better (0.66), which is the level's agreement seen again.

**What is new in it is the two-way share,** which the page shows nowhere for an industry, and which is steadier from region to region than passed along.

### 13.7 Scoped: finding the chains themselves

**Three-step chains** (net buyer, middle link, net supplier). What a middle link passes along is shared among its net buyers and net suppliers in proportion.

- **Chains are many and thin.** Yorkshire and the Humber has 42,240 of them carrying £136m a month. The ten largest carry 9% and the forty largest 19%. Across regions the ten largest carry 8% to 18%.
- **The largest are recognisable.** Health to retail to wholesale (£1.8m a month). Building construction to building trades to wholesale (£1.5m). Head offices, real estate and education each to building construction to building trades (£1.1m to £1.2m). Food manufacturing to farming to wholesale and land transport (£0.7m to £0.8m).
- **Only about half run down the order at both steps:** 54% here, 47% (London) to 68% (East Midlands) elsewhere. The largest of all, food manufacturing to retail to wholesale (£2.3m), runs up at its first step.
- **The largest through Other manufacturing is small:** health to other manufacturing to wholesale, £0.5m a month, then to fabricated metal (£0.4m) and rubber and plastics (£0.3m).

**Tracing from one industry** answers "who is in this chain" more directly. Follow net links worth a tenth or more of the industry's net receipts upwards, and of its net payments downwards, two steps each way. From Other manufacturing:

- **Up:** health (42% of its net receipts), building construction (16%), retail (14%).
- **Up again, the trail runs out.** Health's own net receipts from industries here are small (its largest net payer is head offices, at £0.7m a month), because public money and households aren't in the data. Any chain that reaches a public service or a shop stops there.
- **Down:** wholesale (15% of its net payments) and fabricated metal (12%).
- **Down again:** fabricated metal pays rubber and plastics, basic metals and employment agencies; wholesale pays warehousing, fabricated metal, IT and insurance.
- **So one reading of the chain** is basic metals, fabricated metal, other manufacturing, health. The amounts at the far end are small (£0.5m a month net from fabricated metal to basic metals).

**A chain is not a payment group.** Other manufacturing sits firmly in no payment group (loosely with Engineering and metals in 6 of 8 years). Its chain crosses four: health is in Local and public services, building construction in Construction, retail and wholesale in Distribution, fabricated metal and basic metals in Engineering and metals. Groups say who trades together; a chain runs through them.

**Published methods, if something heavier is wanted.**

- **Average propagation lengths** were built to pick production chains out of an input-output table: Dietzenbacher, Romero and Bosma (2005), [Using average propagation lengths to identify production chains in the Andalusian economy](https://real.web.illinois.edu/wp-content/uploads/2022/01/05-T-6.pdf), Estudios de Economía Aplicada 23(2). The three-step chains above are a cut-down relative of it: one step each way, on net payments. The measure has critics: [Oosterhaven and Bouwmeester (2013)](https://ideas.repec.org/a/sae/inrsre/v36y2013i4p481-491.html) argue it is of little use for comparing single industries. Not tried here.
- **Upstreamness,** the average distance from final use, is the closest published relative of the page's vertical order: Antràs, Chor, Fally and Hillberry (2012), [Measuring the upstreamness of production and trade flows](https://www.nber.org/papers/w17819), American Economic Review Papers and Proceedings 102(3).

### 13.8 What I'd put on the page, and what I wouldn't

| Option | What the reader gets | Recommendation |
|----|----|----|
| **Side panel, for the pinned industry:** a three-part bar (two-way, passed along, end of the line), and the share of its value running down the order, net and with both directions counted | The check in sections 13.2 and 13.3, for any industry, with the caution built in | Yes. Small, and nothing else on the page shows two-way trade for an industry |
| **"Trace its chain" when an industry is pinned:** also draw the main net links of its main partners, a second step up and down | Who is in the chain, as a picture, including where the trail runs out | Yes, as a switch. Needs a look at how busy it gets |
| **Columns or a highlight by share passed along** | A ranking of middle links | No. It mostly repeats height, and an industry's place changes from region to region |
| **The input-output table's split as a fixed fact about each industry** | The national chain position, the same in every region, to set beside what the payments show | Maybe, with the other input-output attributes parked in sections 11.10 and 11.12 |
| **A line in the notes:** about a fifth of payments between industries here are passed on to a third industry inside the region, against two fifths in the national supply chain | The scale of chains inside a region | Yes, if the side panel bar goes in |

**To keep in mind for any of these.**

- **Inside the region only.** Most of an industry's trade leaves the region (section 12), so a chain that crosses the edge shows only its local stretch.
- **A buyer end is often a missing payer.** Health, education and shops end chains because who pays them isn't in the data.
- **Divisions are broad.** Two firms in the same pair of divisions can be at different stages, which shows as two-way trade when it is really two one-way trades.
- **A payment is not always a purchase.**

Figures for every industry and region are in `tests/outputs/middle_links_by_industry.csv`, region totals in `middle_links_regions.csv`, the forty largest chains in each region in `middle_links_chains.csv`, and the comparison with the input-output table in `middle_links_payments_vs_io.csv`.

### 13.9 Built: the side panel bar, the trace, and columns by how much is passed along

*Built 24 November 2026, on request: the first two rows of the table in section 13.8, and columns by share passed along, which that table advised against. No data files were rebuilt; the page works all of it out from the links it already has.*

**Side panel, for the pinned industry** ("Chain or two-way trade, whole period").

- **A three-part bar:** two-way, passed along, and buyer end or supplier end, with the percentages.
- **A sentence saying what each part is,** and for an end, whether money enters or leaves the picture there.
- **Which way its payments run:** the share running down the order with one net link a pair, and with both directions counted, beside the same two figures for the middle industry in the region. Left out for industries too small to place.
- **It matches the test.** Other manufacturing in Yorkshire and the Humber shows 48%, 47% and 4.5%, then 98% and 75% against 87% and 70%.

**"Trace the pinned industry's chain"** (a new tick box; `trace=1` in the address).

- **The rule is section 13.7's:** net payments worth a tenth or more of what an industry is paid net (upwards) or pays net (downwards), two steps each way, worked out on the whole period so the chain holds still while the months play.
- **On the chart:** the chain's links are drawn strongly, second-step links fainter, and every other link almost invisible. Industries outside the chain are greyed. A chain's links are drawn even when they fall below the "Show" cut-off. Widths and dots are still the month's.
- **Hovering over another industry** shows that industry's own links, as before.
- **In the side panel:** the chain as a list, each step with its pounds a month and its share, and a line saying where trails stop.
- **How busy it gets:** Other manufacturing's chain is 19 links and building construction's 13. Both read clearly (screenshots `06m` and `06n`).
- **The tenth and the two steps are fixed,** not controls.

**Columns "By how much it passes along"** (`cols=passed`).

- **The same ranked axis** as the other measures: "passes least along to another industry" to "passes most", percentages along the top.
- **The picture is a sideways V,** as section 13.6 predicted. Buyer ends (education, health, food and drink service) are top left, supplier ends (wholesale, legal and accounting, IT) bottom left, and the middle links run out to the right along the average line. That is the repeat of height made visible. The key and the note say so.
- **What it adds over height:** among industries at the same height it separates those that pass money on (drinks manufacturing, textiles, other manufacturing) from those that trade it back and forth (real estate, machinery).
- **A different measure would be less tied to height:** one that also counted an industry as in a chain when it starts or ends one. Education starts several of the largest chains and sits at the far left here. Not tried.

**A note at the foot of the page** ("Chains") explains the three parts, gives the region's share passed along and two-way (worked out by the page, so it follows the intermediaries switch), sets them beside the input-output table's 39% and 27%, and lists the four cautions from section 13.8.

**Checks** (`tests/13_page_checks.cjs`, `tests/15_url_roundtrip.cjs`).

- **No page or console errors** in any of the twelve regions.
- **The address** carries the new settings: all 11 column choices come back, and the trace switch on and off.
- **Speed is unchanged:** 60 frames a second on the opening view and with every link drawn.
- **The note's figures match the test:** 21% and 45% in Yorkshire and the Humber, 19% and 34% in Northern Ireland, 28% and 40% in Wales.
- **The trace was also loaded** in the goods view, with intermediaries in, and in London, with no errors.
- **Screenshots:** `tests/outputs/page/06k` to `06n`.

**Not done.** Only Yorkshire and the Humber's traces were looked at. The side panel is now long when the trace is on.

**Added afterwards: the chain wording follows "Flows of"** (24 November 2026).

- **The question.** In the goods view wholesale sits at the "passes least along" end although its goods outflows are huge. Is the measure about money, and does it need a goods version?
- **It is one figure either way.** Passed along takes the smaller of what comes in and what goes out. Turning every payment round swaps the two, and the smaller is still the smaller. So there is no separate goods version to build, only wording.
- **Why wholesale is at that end.** Passing along needs both an in and an out inside the region. In Yorkshire and the Humber wholesale sells £142m a month to other industries here and buys £47m from them, so most of what it sells here was not bought here. Of its purchases from UK industries, 18% are inside the region, and imports aren't in the data at all. It comes out 7% passed along and 51% supplier end.
- **It is a supplier end in all twelve regions.** In the national input-output table wholesale passes along 53%. The region's edge is only part of the gap: section 13.10 counts payments across the edge and wholesale still comes out low.
- **What changed on the page.** With "Flows of" set to goods and services, the key, the side panel and the trace now speak of goods and services bought from one industry and sold on to another, measured by what was paid, and of the directions as they are on the flipped chart (suppliers above, customers below). With money, they speak of payments, as before.
- **The note at the foot** now gives both readings, says a buyer end is where chains finish and a supplier end where they start, and uses wholesale as the example.
- **Screenshots:** `06o` (goods view, wholesale pinned, columns by how much is passed along) and `06p` (trace in the goods view).

### 13.10 To come back to: an industry's local links and its wider role

*Scoped 24 November 2026, not built. Test `tests/24_chains_across_the_edge.R`.*

**The problem.** The page is good evidence on what an industry does with other industries in its region. It is poor evidence on the industry's place in the wider economy, and the chain measure makes that gap visible.

- **Wholesale is the test case.** Everyone's picture of a supply chain has wholesale in the middle: it buys from makers and sells to shops and other businesses. The national input-output table agrees (53% passed along, 28% two-way, 18% supplier end).
- **In every region's payments it is an end of the line.** In Yorkshire and the Humber: 7% passed along, 43% two-way, 51% supplier end. It sells £142m a month to other industries here and buys £47m from them.
- **So the chart shows its local links faithfully and its role wrongly.** A reader in the goods view sees a large circle at the supplier end with goods pouring out and nothing coming in, and reasonably asks where the goods came from.
- **The same will be true of other industries** whose inputs or customers lie mostly outside the region or outside the data: food manufacturing, retail, anything that imports.

**What is already on the page that bears on it.**

- **"By purchases inside the region"** (section 12): wholesale buys 18% of what it buys from UK industries inside the region, and sells 25% inside. Those two figures already say its trade is mostly across the edge.
- **The halo** shows the amounts paid to and from the rest of the UK.
- **Neither is tied to the chain bar or the trace,** so the reader has to join them up.

**The idea that was floated: count payments across the edge, by partner industry.** The data files hold rest-of-the-UK payments as one total per industry. The monthly data the build caches has them by partner industry and region. A rebuild could carry them, and the chain measure could then treat an industry elsewhere as a partner like any other.

**Tested, and it doesn't do the job** (`tests/24_chains_across_the_edge.R`; partners are the other industries in the region, plus every industry elsewhere in the UK, the industry's own included).

| Wholesale | Two-way | Passed along | End of the line |
|----|----|----|----|
| Yorkshire and the Humber, inside the region | 43% | 7% | 51%, supplier |
| Yorkshire and the Humber, UK-wide | 58% | 12% | 30%, supplier |
| Twelve regions, UK-wide | 46% to 61% | 9% to 43% | up to 40% |
| Input-output table | 28% | 53% | 18%, supplier |

- **Wholesale's share passed along rises in seven regions and falls in five.** It is a clear middle link only in Wales (43%) and the East Midlands (40%).
- **Payments as a whole get no more chain-like.** UK-wide, 19% to 29% of payments are passed along and 43% to 55% are two-way, against 19% to 28% and 34% to 52% inside. The input-output table has 39% and 27%.
- **Agreement with the input-output table doesn't improve.** With each division at its middle value across regions the rank correlation of passed along falls from 0.44 to 0.38. Region by region it rises in four, is level in one and falls in seven.
- **It reshuffles industries.** The rank correlation between the inside and UK-wide versions is 0.39 to 0.74. In Yorkshire and the Humber pharmaceuticals goes from 5% to 63% and motor vehicles from 9% to 42%, which is sensible for makers that sell elsewhere. Veterinary goes from 51% to 17% and motor trades from 44% to 12%.
- **It leans on the weaker half of the data.** Payments across the edge are 1.0 (Northern Ireland) to 5.4 (East Midlands) times those inside. Between 15% and 37% of their value is held, not published, against 7% to 20% inside. They are also where registered offices mislead most, which is why the page was built on the inside half (section 5).

**Why it fails for wholesale.**

- **Much of what wholesale buys isn't in the data at all:** imports, and payees whose industry isn't recorded. UK-wide it is still paid far more by industries than it pays to them.
- **At the level of whole industries its trade is mostly two-way.** The industries it buys from are largely the ones it sells to: between 46% and 61% of its UK-wide payments net off against the same partner, depending on the region. Which partners those are hasn't been looked at.
- **So the gap between 7% and 53% is mostly about what payments are,** not about where the region's edge falls.

**Where the region's edge does matter: a first look at roles.** Of what Yorkshire and the Humber's industries pass along UK-wide, shared out in proportion to where it was bought and sold:

| Industry | Bought here, sold here | Brought in (bought elsewhere, sold here) | Sent out (bought here, sold elsewhere) | Through (both elsewhere) |
|----|----|----|----|----|
| Building construction | 13% | 48% | 9% | 30% |
| Retail | 9% | 49% | 6% | 36% |
| Wholesale | 3% | 27% | 8% | 62% |
| Other manufacturing | 9% | 15% | 28% | 48% |
| Food manufacturing | 1% | 4% | 25% | 70% |

This is the more promising use of the across-the-edge data: not a better chain score, but a description of what an industry does at the region's edge. Builders and shops bring in. Makers send out. It is rough: sharing in proportion assumes any pound in could be any pound out.

**Ways forward, cheapest first.**

| Option | What it shows | Needs | View |
|----|----|----|----|
| **A sentence in the side panel** when an industry is an end of the line: how much of what it buys, or sells, is outside the region | Why this industry looks like an end here | Nothing new: the figures are section 12's | Do first. It answers the reader's question at the moment they ask it |
| **Stubs on the trace:** where a trail reaches an industry that buys or sells mostly elsewhere, draw a short link off towards the edge with the share | The chain leaving the region, not just stopping | Nothing new: the rest-of-the-UK totals behind the halo | Worth trying. Needs a look at how it reads |
| **The national role beside the local one:** for each industry, the input-output table's three-way split, and who it sells to (other businesses, households, government, abroad) | The wider role, stated as national and fixed, next to what the payments show locally | A small lookup from tests 19 and 23b. No payments rebuild | The real answer to the question. Wholesale would read: here 7% passed along, nationally 53%; nationally a third of what it sells goes abroad and half to other businesses |
| **Roles at the edge** (the table above) as a side panel line or a highlight: brings in, sends out, local, through | What the industry does for the region | A rebuild carrying across-the-edge payments by partner industry, whole-period totals only (about 13,000 numbers a region) | Later. Interesting, rough, and on the weaker data |
| **A UK-wide chain measure** | Meant to be the wider role | The same rebuild | No. Tested above |

**The principle these point to.** Keep two things apart on the page and show them side by side: what the payments show an industry doing here, and what the national tables say it does in the economy. Don't try to make the payments yield the second.

**Open.**

- **The input-output table is national, for 2019, and by product.** It can't say that wholesale in one region differs from wholesale in another.
- **Whether a fixed national fact belongs on a page about regional payments** was left open in sections 11.10 and 11.12, for tradeable and for filling gaps. This is the third time it has come up, and the strongest case for it.
- **Imports can't be recovered from this data** by any route.

Figures by industry and region are in `tests/outputs/chains_across_the_edge.csv`.
