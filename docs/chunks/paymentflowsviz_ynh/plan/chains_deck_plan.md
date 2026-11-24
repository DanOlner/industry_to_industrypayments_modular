# Slide deck plan: local links and wider roles

*Written 24 November 2026. A side piece to the payment flows page: a short deck that walks through what the page can and can't show about supply chains, using three industries in Yorkshire and the Humber. The findings are from sections 12 and 13 of `paymentflowsviz_plan.md`.*

## 1. What the deck is for

**The point to land.** Payments between industries are good evidence of an industry's links inside its region. They are poor evidence of its role in the wider economy. The two should be shown side by side, not confused.

**Who it is for.** Someone who knows what a supply chain is and has seen the page, but hasn't followed the tests. No statistics beyond percentages and one correlation.

**How it should read.** One idea a slide. Plain English. Each claim is shown with a picture from the page or a chart from the tests, and each number is worked out by the deck's own code from the files in this folder.

## 2. The three industries

All in Yorkshire and the Humber, all read as goods and services (each payment turned round), because a supply chain is about what is made and passed on.

| Industry | Why it is in the deck | What the payments show here | What they miss |
|----|----|----|----|
| **Other manufacturing** (mostly medical and dental instruments) | Sits in the centre. The case where the picture is right | A middle link: 47% of its trade with other industries here is bought from one and sold on to another. Metals in, health out | Most of what it makes goes abroad |
| **Education** | Sits at the buyer end. The case where an "end" is right in spirit | A buyer end: it buys from industries here and sells them almost nothing | Who pays it: government and households, neither in the data |
| **Wholesale** | Sits at the supplier end. The case where the picture is wrong | A supplier end: it sells three times what it buys here, so goods seem to start with it | Where its goods come from: other regions and abroad. Nationally it is a middle link |

## 3. Storyline, slide by slide

| No. | Slide | What it says | What it shows | From |
|----|----|----|----|----|
| 1 | Title | Local links and wider roles | | |
| 2 | The chain in our heads | Goods pass from maker to maker to seller to household, gaining value at each step | A five-box diagram | Drawn in the deck |
| 3 | What the data is | Payments between businesses. Turn each round and it is goods or a service | A two-arrow diagram | Drawn in the deck |
| 4 | What the data leaves out | Of each £1 these industries were paid, about 10p came from industries here | One bar in five parts | `tests/outputs/foundational_tradeable_receipts_by_source_ynh.csv` |
| 5 | The page | Suppliers at the top, buyers at the bottom; where the three sit | The chart, goods view | Screenshot |
| 6 | A test for "in a chain" | Three things can happen to what an industry trades: passed along, two-way, end of the line. No cut-off splits them: trade is matched up one partner at a time | Three small diagrams | Drawn in the deck |
| 7 | The three, tested | Other manufacturing passes along 47%, wholesale 7%, education 1% | Three stacked bars | `tests/outputs/middle_links_by_industry.csv` |
| 8 | Drawing a chain, one pair at a time | The test gives a figure; the page's trace shows which industries. Net each pair off, keep links of a tenth or more of one-way buying or selling, repeat once | A diagram of the rule, with one link left out each side | Drawn in the deck |
| 9 | The test and the trace | Two tools, different questions. A traced chain is pieced together from pairs, each link is a whole one-way flow, and step two is looser than step one | A three-row table | |
| 10 | Other manufacturing: who it trades with | It buys from metals, plastics and wholesale, and sells to health, shops and builders | Paired bars by partner | `data/flows_TLE.js` |
| 11 | Other manufacturing's chain: step one | Two suppliers and three customers clear the 10% line. Rubber and plastics misses at 9.97%. The links kept carry 27% of its one-way buying and 72% of its one-way selling | Bars of each partner's share, with the line | `data/flows_TLE.js` |
| 12 | Other manufacturing's chain: step two | The same rule from each of the five: their suppliers and their customers. Step two is about them, not about other manufacturing. Basic metals to fabricated metal to other manufacturing to health | The trace | Screenshot |
| 13 | The national table agrees | Same customers, same suppliers; 57% passed along nationally | Two bars, here and national | `tests/outputs/middle_links_payments_vs_io.csv` |
| 14 | Cautions, even here | One net link a pair flatters it; nearly half its trade is two-way; and most of what it makes is exported | Three figures | `middle_links_by_industry.csv`, `foundational_tradeable_divisions.csv` |
| 15 | Education: where chains finish | It buys £20m a month here and sells £2m | Paired bars by partner | `data/flows_TLE.js` |
| 16 | Its chain on the page | Education to building trades to wholesale | The trace | Screenshot |
| 17 | Why it is an end | Three quarters of what it provides goes to government and households, and they aren't in the data | Four figures: who it provides for nationally | `foundational_tradeable_divisions.csv` |
| 18 | Wholesale: the puzzle | In our heads and in the national table it is the middle. Here it is where goods start | The chart with wholesale pinned; here 7%, nationally 53% | Screenshot, `middle_links_payments_vs_io.csv` |
| 19 | Where do its goods come from? | It sells £142m a month here and buys £47m. 18% of what it buys from UK industries is bought here. Imports aren't in the data | Four figures | `middle_links_by_industry.csv`, `local_by_industry.csv` |
| 20 | Does counting the rest of the UK fix it? | No: 7% becomes 12%, against 53% | Three stacked bars | `tests/outputs/chains_across_the_edge.csv` |
| 21 | Why not | Imports and unrecorded payees are missing; and across whole industries its trade nets off | Two points | Section 13.10 |
| 22 | Three industries, three blind spots | The table in section 2 above | Table | |
| 23 | Every industry, here against the national table | They agree loosely. The three are marked | Scatter | `middle_links_by_industry.csv`, `middle_links_payments_vs_io.csv` |
| 24 | Who each sells to, nationally | Businesses, households, government, abroad | Three small bar charts | `foundational_tradeable_divisions.csv` |
| 25 | The principle | Keep local links and wider role apart, and show them side by side | | |
| 26 | What the page could do next | A sentence at an end of the line; stubs on the trace; the national role beside the local one | | Section 13.10 |
| 27 | What no route recovers | Imports, households, where goods physically go, head offices | | |
| 28 | Sources and method | Data, tests, the page | Links | |

## 4. Pictures

**Charts** are drawn by R code inside the deck, from the files named above, so they change when the tests are re-run.

- **Three colours, used for one thing only:** blue for passed along, orange for two-way, green for end of the line. Checked with the dataviz skill's validator (passes every check for colour-blind and full-colour readers; the green is too light to stand alone, so every bar is labelled).
- **Everything else is grey and ink.** Single-series bars are dark grey. In the scatter the three industries are black and the rest grey. In the step-one chart of a trace, links kept are dark grey and links left out pale grey, each with its figure beside it.
- **Passed along is drawn first** in each stacked bar, so it starts from the same edge in every bar and can be compared by eye.
- **Blue and orange also colour the links in the page's screenshots,** where they mean down and up the page. Each chart has a legend so the two uses can't be confused.

**Screenshots** are taken from the page itself by a script, at fixed settings, so they can be re-taken after the page changes. All in the goods view. Each slide that shows one links to the live view.

**Diagrams** (the chain in our heads, payment against goods, the three-way test) are drawn as SVG in the deck.

## 5. Files and how to build

| Path | What |
|----|----|
| `slides/chains_deck.qmd` | The deck: text, R code for the charts, diagrams |
| `slides/deck.scss` | Its look, matched to the page |
| `slides/make_screens.cjs` | Takes the screenshots from `index.html` into `slides/img/` |
| `slides/img/` | The screenshots |
| `slides/chains_deck.html` | The built deck, one file, everything inside it |

To rebuild, from the repo root:

```
NODE_PATH=/path/to/node_modules node docs/chunks/paymentflowsviz_ynh/slides/make_screens.cjs
quarto render docs/chunks/paymentflowsviz_ynh/slides/chains_deck.qmd
```

The first line is only needed when the page has changed. It needs puppeteer-core and Google Chrome, as the page checks do. The second needs Quarto, and R with data.table, ggplot2, ggrepel, jsonlite and svglite.

## 6. Rules for the words

- **Goods and services language throughout,** with one slide near the start saying they are an inference from payments.
- **"Here"** means Yorkshire and the Humber. **"Nationally"** means the UK input-output table for 2019.
- **No named firms.**
- **Every caution once, where it bites,** not as a list at the front.

## 7. Checks before it is done

- **It builds from a clean start** with the two commands above.
- **Every slide looked at** as an image, for overflow, clipped labels and collisions.
- **Every number on a slide matches** the test output it came from.

## 8. Not in the deck

- **The foundational economy and tradeable work** (section 11).
- **Other regions,** beyond one line saying wholesale is a supplier end in all twelve.
- **How levels are worked out.** The deck says "suppliers at the top, buyers at the bottom" and leaves it there.

## 9. Built

*24 November 2026.*

- **The deck is `slides/chains_deck.html`:** 25 slides in one file of about 7.5MB, with the charts, screenshots and styling inside it. Open it in a browser; arrow keys move through it.
- **It follows the storyline in section 3** slide for slide.
- **Numbers in the text are worked out by the deck** from the test outputs wherever they come from a file, so they follow the tests. Three things are typed in by hand, because their sources aren't files in this folder: the share of jobs in medical instruments (the build's jobs cache), the input-output table's list of other manufacturing's main partners (printed by test 23b), and the dates in the sources slide.
- **Four screenshots,** all of Yorkshire and the Humber in the goods view: the whole chart, other manufacturing traced, education traced, wholesale pinned.
- **Seven charts and three diagrams.**
- **No author is named on the title slide.** Add an `author:` line at the top of the `.qmd` if one is wanted.

**Checks done.**

- **Built from the two commands in section 5,** with no errors or warnings from Quarto and none in the browser.
- **Every slide was looked at as an image.** Nothing overflows or is clipped. Two things were fixed on the way: the date on the title slide, and labels that collided in the scatter.
- **The chart colours pass the dataviz validator** for all pairs of the three.
- **Figures on the slides match the tests:** 47%, 48% and 4% for other manufacturing; 1%, 18% and 81% for education; 7%, 43% and 51% for wholesale inside the region, 12%, 58% and 30% UK-wide, and 53%, 28% and 18% in the national table.

**Not done.**

- **Nobody else has read it.** The words are a first draft.
- **No speaker notes.**
- **It hasn't been tried on a projector or a phone.** It is laid out for a 16:9 screen.
- **The links to the live page are relative** (`../index.html?...`), so they work from this folder and from the published site, but not if the deck file is sent on its own.

**Changed afterwards, 24 November 2026,** after reading ONS's two articles and methodology note (section 14 of the main plan).

- **"What the data is"** no longer says nothing travels back for rent and insurance. Those are services and count with goods. It now says the payments that bought nothing are taxes, subsidies and dividends, and that ONS can't yet tell the types apart.
- **"What the data leaves out"** has a line on why so many payers have no industry, with one figure from test 25.
- **"What no route recovers"** uses ONS's term, the headquarter effect.
- **Sources** link ONS's article and methodology note.

**And later the same day.**

- **"A test for 'in a chain'"** now says how passed along is told from two-way. There is no cut-off. Trade is matched up one partner at a time: whatever goes both ways with a partner is two-way, and only what is left over, which runs one way, can be passed along or an end of the line. The slide gives a worked example (buy £10, sell £4 back).

**Three slides added, 24 November 2026,** to explain how the page draws a chain, which the deck had shown without explaining. The deck is now 28 slides, with eight charts and four diagrams.

- **Why.** Slide 6 calls "passed along" a chain, and the page has a switch that draws a chain. They are different things and the deck didn't say so. The 10% cut-off belongs to the trace, not the test.
- **"Drawing a chain, one pair at a time"** (slide 8) gives the rule the page's trace follows, as `traceOf()` in `flows.js` does it. Each pair of industries is netted off, so one one-way link is left. From the pinned industry, a supplier is kept if it provides a tenth or more of its one-way buying, and a customer if it takes a tenth or more of its one-way selling. The same is done once more from each industry reached. The diagram shows one link left out on each side.
- **"The test and the trace"** (slide 9) sets the two side by side and gives three cautions. The data has only pairs, so any chain of three is an inference. Each link drawn is the whole one-way flow between two industries, not the part that carries on. Step two shows a supplier's main suppliers, whoever the supplies were for.
- **These two come after "The three industries, tested",** not straight after slide 6, so the bars still follow the test they come from.
- **Other manufacturing's trace is now two slides** (11 and 12), in place of "Its chain on the page". Step one is a new chart: each partner's share of its one-way buying and selling against the 10% line, links kept in dark grey and links left out in pale grey. Step two is the screenshot, with what the second step added.
- **What step one shows.** Two suppliers clear the line (wholesale 15%, fabricated metal 12%) and three customers (health 42%, building construction 16%, retail 14%). Rubber and plastics misses at 9.97%, then comes back at step two as a supplier of fabricated metal. The links kept carry 27% of its one-way buying and 72% of its one-way selling. The other 65 links are left out.
- **Figures match test 23's trace.** The deck works them out itself from `data/flows_TLE.js`, the same way the page does.
- **Typed by hand on slide 12:** the names of the industries reached at step two. They were checked against test 23's printed trace.
- **Education's trace is still one slide.**
- **Checked** by building the deck and looking at slides 8, 9, 11 and 12 as images. No errors or warnings.
