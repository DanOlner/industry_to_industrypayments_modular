# Payment flows: handover for a new visualisation project

*Written 23 November 2026 by the Claude Code session that used ONS's industry-to-industry payment flows for the South Yorkshire deck (`companieshouseopen`, slides 39 to 43). It's for a new Claude Code instance starting a visualisation of the same data in its own folder. Copy this file into that folder.*

**Read sections 1 to 3 before touching the data.** The data looks like an input-output table but isn't one, and about half of it is hidden.

## 1. The data

### What it is

ONS, **"Industry-to-industry payment flows by region, UK"** (official statistics in development).

- **Source.** Pay.UK's Bacs and Faster Payments, aggregated and anonymised by Vocalink before ONS gets them.
- **What a cell is.** The monthly value (£) and number of payments from one two-digit SIC industry in one ITL1 region to another industry in another region.
- **Coverage.** Business-to-business only: households as buyers aren't in it.
- **Dates.** January 2019 to September 2026, in three editions.

| File | Covers | Edition date |
|----|----|----|
| `industryregionbusinesspayments1921dataset17092026.zip` | 2019 to 2021 | 17 September 2026 |
| `industryregionbusinesspayments2224dataset17092026.zip` | 2022 to 2024 | 17 September 2026 |
| `industryregionbusinesspayments2526dataset22102026.zip` | January 2025 to September 2026 | 22 October 2026 |

- **Local copies:** `~/Code/claude/whole_economy_thinking/local/tradeable_jobs/data_raw/payments/`. There's a duplicate set in `~/Code/Regecon_modular_writeup/local/...`.
- **Download URLs:** base `https://www.ons.gov.uk/file?uri=/economy/economicoutputandproductivity/output/datasets/industrytoindustrypaymentflowsbyregionuk/` plus `2019to2021/…`, `2022to2024/…`, `2025to2026/…`. These are set out in `whole_economy_thinking/code/tradeable_jobs/functions_payments.R`.
- **Dataset page:** <https://www.ons.gov.uk/economy/economicoutputandproductivity/output/datasets/industrytoindustrypaymentflowsbyregionuk>. Methods: the [2019 to 2025 article](https://www.ons.gov.uk/economy/economicoutputandproductivity/output/articles/industrytoindustrypaymentflowsuk/2019to2025) and the [quality and methodology page](https://www.ons.gov.uk/economy/economicoutputandproductivity/output/methodologies/industrytoindustrymonthlypaymentsukqmi).

### File format

Each zip holds one large CSV; 2025–26 is 395 MB, 11.8 million rows. The header starts with a byte-order mark and has a `£` in it, so rename columns by position:

```
payer_2_digit_sic,payer_region,payee_2_digit_sic,payee_region,date,value_£,number_of_transactions
00,TLC,00,TLC,01-JAN-25,3632000,200
```

- **Dates** look like `01-JAN-25`, so year = 2000 + characters 8 to 9.
- **Rounding.** Values are rounded to £1,000, counts to 100.
- **Hidden cells** have `[C]` in the value (and count) field.
- **Regions** are ITL1 codes plus `unknown`:

| Code | Region |
|----|----|
| TLC | North East |
| TLD | North West |
| TLE | Yorkshire and the Humber |
| TLF | East Midlands |
| TLG | West Midlands |
| TLH | East of England |
| TLI | London |
| TLJ | South East |
| TLK | South West |
| TLL | Wales |
| TLM | Scotland |
| TLN | Northern Ireland |

- **SIC codes** are two-character strings: keep them as character, with leading zeros. `00` is unknown industry, `97` and `98` are households as employers, `99` is extraterritorial.
- **Missing rows.** Each month has about 560,000 rows, far fewer than every possible combination. A missing row presumably means no payments. That's an assumption: nobody has checked it against ONS.

### Ready-made yearly sums

`~/Code/claude/whole_economy_thinking/local/tradeable_jobs/cache/pay_yearly.rds` holds them.

- **Reading it.** `readRDS(...)$value`. It's a list, from that project's cache wrapper.
- **Columns:** `year, payer_sic, payer_region, payee_sic, payee_region, value, n, months_published, months_suppressed`.
- **What's in it.** Every region, every industry and 2019 to 2026. `value` and `n` are sums over published months only. The monthly detail is lost, so read the zips for anything monthly.
- **How it's built.** By `pay_yearly()` in `functions_payments.R`, which needs `functions_common.R`.

The reading pattern:

```r
d <- fread(cmd = paste('unzip -p', shQuote(f)), colClasses = 'character')
setnames(d, c('payer_sic', 'payer_region', 'payee_sic', 'payee_region', 'date', 'value', 'n'))
d[, suppressed := value == '[C]'][, `:=`(value = as.numeric(value), n = as.numeric(n))]   # [C] becomes NA
```

### Related files

- **A national five-digit SIC file,** monthly, January 2017 to November 2024, national only: `~/Code/RegionalEconomicTools/local/data/ukindustrytoindustrypaymentflowssic5january2017tonovember2024_FINAL.csv`. It's the only way to see "who pays whom" below two-digit SIC.
- **Older yearly files** in `RegionalEconomicTools/data/` and `local/data/`: `payments_itl2_sic2_yearly_w_sections.rds` and `payments_itl2_sic2_yearly.rds`. They're misnamed: they hold ITL1 regions only, and there's no ITL2 level in this data. They come from an earlier edition.

## 2. What's hidden, and how this project handled it

### ONS's rule

ONS's article lists the disclosure control as "minimum contributor thresholds for each flow, limits on concentration to prevent dominance by a small number of organisations, consistent suppression across time periods and related tables where required, and the rounding of published values".

- **No thresholds are published.** The contact is card.payments.team@ons.gov.uk.
- **There's no rule on value per payment.**

So a hidden cell has either few organisations, which suggests small, or one or two dominant ones, which can be large. **Hidden doesn't mean small.**

### How much is hidden

- **Nationally,** 73% of the 2025–26 file's rows are hidden.
- **Yorkshire and the Humber's own-region cells,** with intermediaries out, are 54% hidden (2019: 57%, 2025: 54%).
- **Some cells are hidden every month of every year.** For example, Yorkshire and the Humber retail paying Yorkshire and the Humber food manufacturing, perhaps because a few big retailers dominate it.

### Holding values

This is from `companieshouseopen/analysis/sy_seen_unseen/15_directionality.R`.

- **The fill.** Each hidden month is held at the same cell's average published month: in the same year if the cell has any published months, otherwise in the nearest years that together give at least six published months. That means 2024, 2026, 2023 and so on, all nominal.
- **Cells never published** in any year stay at zero and are flagged.
- **For the cells used there** (Yorkshire and the Humber, cluster and supplier industries, 2025), 1,157 hidden months were filled from 2025, 1,050 from other years, and 912 had no basis. The fill added about £245m to £2,080m published.
- **Held values are a middle guess.** A month hidden because one payer dominated it was probably bigger than average.
- **Unsure links were drawn dashed.** That covers two cases:
    - the smaller direction is mostly never published (at least half its division-months), so it's held at zero;
    - half or more of the pair's two-way total is held values.
- **Effect on the results.** Holding changed the direction of 1 of 150 industry pairs by count and 9 by value. Supplier-level balances moved by 5 points or less, except one at 10.

## 3. What earlier work found about reading it

These come from `~/Code/RegionalEconomicTools/claude/payment_flows_method_and_caveats.md` and from `whole_economy_thinking`'s tradeable-jobs round 2 (tests T4 and T5).

- **Places are registered entities, not activity.** Payments are placed at the registered paying and receiving organisation, with whole organisations assigned to one region. Headquarters, VAT groups and central payment teams pull flows to London and the South East.
- **Intermediaries distort the geography.** Card takings arrive as one payment from the card acquirer (SIC 64). Platform payouts come from SIC 63 and 82, mostly London-registered. Strip SIC 63, 64, 66 and 82 before asking where an industry's income comes from. Pass-through payments also under-represent small organisations using indirect access.
- **Own-region flows are "the trustworthy half".** When both ends are in the same region, intermediaries can't move the geography.
- **It isn't an input-output proxy.** Payments cover a median of 16% of a division's intermediate sales and fail a check against Scotland's input-output tables (T4). So the regional input-output and multiplier work (T3 regional, T5) wasn't built on them. **Read frequency as a signal of connection, not a measure of trade.**
- **Capital spending is in it.** That's why construction's share is higher than in input-output tables, which count buildings as investment.
- **Trends can be capture artefacts.** The Wales and West Midlands "localisation" in an earlier edition may be growing coverage rather than real change. The caveats note sets out a test to tell them apart, but it isn't settled.
- **Nominal values, not seasonally adjusted, experimental and revised between editions.**
- **Unresolved:** retail's own-region share comes out as 16.6% (Yorkshire) in one analysis and 28% in another (`whole_economy_thinking/docs/chunks/tradeable_jobs/tradeable_jobs_audit.md`, item 8). Check before quoting any retail own-region figure.

## 4. What's been done with it so far

### RegionalEconomicTools (earlier editions)

- **Locality metrics:** each sector's spending and income within its own region, "sourcing against serving" scatters, and a split of every flow into own region, London and the rest of the UK.
- **Notes:** `claude/ind_to_ind_regionalpayments_IO.md` (the dataset, location quotients and input-output ideas) and `claude/payment_flows_method_and_caveats.md`.
- **Network centrality (random walk):**
    - the plan, `claude/payment_network_centrality_plan.md`;
    - the script, `bits_of_code/ind_to_indpayments_network_centrality.R`;
    - results in `local/data/payment_network_centrality_results.rds`;
    - the report, `quarto_docs/payment_network_connectivity_2025.qmd`.

### whole_economy_thinking (tradeable jobs, round 2, November 2026)

- **Loader and cache:** `code/tradeable_jobs/functions_payments.R`. Includes `pay_seller_measures()`: where each selling industry's income comes from, with intermediaries stripped.
- **Tests:** T4 (payments as an input-output proxy, mostly negative) and T5 (limited). Results are in `docs/chunks/tradeable_jobs/tradeable_jobs_round2.qmd`; plan and log in `tradeable_jobs_further_tests_plan.md`.

### companieshouseopen, the South Yorkshire deck (22 and 23 November 2026)

**Files:**

- **Scripts:** `analysis/sy_seen_unseen/13_connections.R`, `14_connective_tissue.R`, `15_directionality.R`.
- **Figures:** `fig_*` functions in `analysis/sy_seen_unseen/sy_figures.R`, in particular `fig_connective_tissue()` and `fig_directionality(measure = 'n' | 'value')`.
- **Outputs:** in `local/reports/sy_seen_unseen/data/`: `pay_matrix.csv`, `pay_suppression.csv`, `pay_cluster_payees.csv`, `sy_implied.csv`, `segment_visibility.csv`, `tissue_edges.csv`, `tissue_nodes.csv`, `direction_pairs*.csv`, `direction_suppliers*.csv`.
- **Write-ups:** `claude/sy_connections_dig.md` (plan, findings, build log) and `analysis/sy_seen_unseen/methods.md` (the methods for every slide).

**Set-up.** Yorkshire and the Humber paying itself, 2025, intermediaries and non-industries stripped. Industries are grouped into segments:

| Segment | SIC |
|----|----|
| Clusters (steered by South Yorkshire's growth plan) | Sections C and J: manufacturing and digital |
| Foundational (named, not steered) | G47, H, I, O, P, Q |
| Construction | F |
| Business services | K, L, M, N |
| Everything else | The rest |

South Yorkshire figures are by implication. Each paying industry's flows are weighted by South Yorkshire's share of Yorkshire and the Humber's BRES jobs in that industry; the BRES file is `~/Code/claude/MONIAC2/claude_research/productivity/data/raw/bres_lad21_sic2.rds`.

**What it found:**

- **Who the clusters pay.** Of the clusters' local payments, 39% by value (31% by count) go to other cluster firms. Business services get 12% (18%), the foundational economy 12% (14%) and construction 7% (6%).
- **Payment sizes.** The average is £8,455 to other cluster firms, £4,621 to business services and £5,373 to the foundational economy.
- **A match with input-output.** The value shares match the 2019 national input-output tables' direct purchases closely for the clusters (39%) and the foundational economy (12%).
- **Reweighted to South Yorkshire,** 61% by value and 69% by count goes outside the clusters. The suppliers are 51% visible in Companies House accounts.
- **The connective tissue.** Among non-manufacturing suppliers, wholesale takes 31% of the clusters' payments, averaging £5,600; accountants and lawyers get many small payments, averaging £1,600. In South Yorkshire, 83 to 99% of firms in those supplier industries have under 10 staff.
- **Direction by count** (share of each pair's payments running from the clusters, with held values):
    - **The clusters pay:** wholesale 68%, IT 74%, accountants and lawyers 83%, haulage 66%, warehousing 73% and cleaning 79%.
    - **The supplier pays the clusters more often:** retail 38% and building trades 37%. They're customers too.
- **Direction by value.** The clusters pay out more to almost every supplier except retail (32%).
    - The building trades even out (51%): many small purchases one way, fewer and bigger bills the other.
    - Real estate (65%) and motor trades (63%) turn round, probably rents and vehicles.

**The figures:**

- **Layout.** A bipartite layout: cluster industries on the left, supplier industries on the right, joined by sigmoid curves.
- **Line width** is the number of payments or the amount.
- **Direction.** Each link is coloured by which way the money mostly runs (blue: the clusters pay more; orange: the supplier does). A thin path sits over a translucent body, with a separate solid arrow tip so dashes don't break it.
- **Side columns:** counts or amounts both ways, and a strip showing each balance regionally (hollow dot) and for South Yorkshire (filled dot).
- **Palette:** from the dataviz skill. Clusters-pay blue `#2a78d6`, supplier-pays orange `#eb6834`, ink `#0b0b0b` and `#52514e`, muted `#898781`, surface `#fcfcfb`.

## 5. What else it could do

These are ideas, roughly from quickest to most work.

- **The whole regional network, not just clusters and suppliers.**
    - All two-digit divisions in one region as an adjacency matrix ordered by segment, or a chord diagram, with value and count side by side.
    - Net direction for every pair, to show who sells to whom inside a region, then compare with the national use table.
- **Every region at once.**
    - Small multiples of the clusters-to-segments split for all 12 regions.
    - A heatmap of each industry's own-region share by region.
    - Region-to-region flows, with intermediaries stripped and the headquarters caveat on the page.
- **Payment size as its own measure.**
    - Average payment (value ÷ count) for each pair: where trade is many small bills and where it's few big ones.
    - A frequency ratio (share of payments ÷ share of value) by industry.
- **Over time.** Monthly series from January 2019 to September 2026 need the raw zips:
    - the pandemic shock and recovery, and the 2022 energy and inflation spike (deflate first);
    - seasonality;
    - whether the links between segments strengthened or weakened.

  Watch for edition breaks and capture artefacts.
- **Suppression made visible.**
    - Where the data are hidden, by pair, region and year, and how stable that is.
    - Sensitivity of any result to the hiding: published only, same-year held, nearest-year held, or an upper bound.
- **Joins.**
    - Companies House firm counts and sizes by division, for payments per firm.
    - BRES jobs, for payments per job.
    - The input-output coefficients, to show where payments track trade and where they don't.
    - The national five-digit file, for finer "who signals to whom" (national only).
- **Centrality,** rebuilt on the new editions with held values: which industries are hubs by frequency and which by value.
- **An interactive explorer.** Pick an industry and region and see flows in and out, with toggles for value or count and for published or held. Claude can publish it as a private artifact page.

## 6. Practical gotchas

- **Size.** The raw CSVs are big. Use `data.table::fread(cmd = 'unzip -p …')` or unzip once and use DuckDB. Don't load all three editions as character columns at once without a memory check.
- **Leading zeros.** Keep SIC and region as character. `fread` drops leading zeros on SIC codes unless you set `colClasses = 'character'` or `colClasses = list(character = ...)`.
- **data.table:** a constant inside `by = .()` errors (add the column after aggregating). A vector from outside the table inside grouped `j` is recycled whole, so join weights in as a column.
- **Yorkshire and the Humber's 2021 authorities**, for BRES:
    - South Yorkshire: E08000016 to E08000019;
    - West Yorkshire: E08000032 to E08000036;
    - E06000010 to E06000014 and E06000065;
    - E07000163 to E07000169.
- **Input-output tables** load with `io_tables()` in `whole_economy_thinking/code/tradeable_jobs/functions_io.R` (2019 tables: Z, A, L and product totals).
- **No names.** Name no individual. If a visualisation joins in Companies House data, show aggregates, not named firms, unless there's a reason.

## 7. Open questions

- **Missing rows.** Are they true zeros or just unlisted? This affects holding values and any count of hidden months.
- **Thresholds.** ONS's actual disclosure thresholds, and whether hidden cells are left out of any totals ONS publishes.
- **Revisions.** How much do figures change between editions, for example 2025 in the October 2026 edition against the next one?
- **Retail.** The own-region share discrepancy in section 3.
