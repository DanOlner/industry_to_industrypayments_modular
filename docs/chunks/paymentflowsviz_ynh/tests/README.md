# Tests for the Y&H payment flows visualisation

Test code behind [`../plan/paymentflowsviz_plan.md`](../plan/paymentflowsviz_plan.md). The finished page is `../index.html`. The scripts here answer the plan's questions, `prototype/` is the rough page used to try layouts on the real data, and `13` checks the finished page.

## Run order

Run the R scripts from the repo root, for example `Rscript docs/chunks/paymentflowsviz_ynh/tests/00_lookups.R`. They run in number order, with one exception: `06` reads the outputs of `11` and `12`, so run it after them.

| Script | What it does | Main outputs |
|----|----|----|
| `00_lookups.R` | Division names, short labels and sections; industrial strategy membership of each division with the share of its Y&H jobs covered | `lookups/` |
| `01_extract_ynh_monthly.R` | Pulls every monthly row touching Y&H out of the three ONS zips (about 90 seconds) | `local/paymentflowsviz_ynh/ynh_monthly.rds` |
| `02_hierarchy_tests.R` | Is there a supplier-to-buyer order, how strong, how stable over 93 months, and what grouping does to it | `outputs/levels_sic2*.csv`, `hierarchy_variants.csv`, `hierarchy_groupings_ynh.csv`, `links_ranked_sic2.csv` |
| `03_io_check.R` | The same calculation on the UK input-output table, and how far the two orders agree | `outputs/levels_payments_vs_io.csv` |
| `04_sic_levels_national.R` | The national five-digit file rolled up to each SIC level: order, stability, clustering, and which divisions hide the most variety | `outputs/hierarchy_by_sic_level_national.csv`, `division_internal_variety_national.csv` |
| `04b_order_across_sources.R` | The division order from four sources compared | `outputs/order_across_sources.csv` |
| `05_external_flows.R` | Payments to and from the rest of the UK against payments inside the region | `outputs/external_by_division.csv`, `external_by_partner_region.csv` |
| `06_export_prototype_data.R` | Writes the prototype's data file; measures its size and the month-to-month noise | `prototype/ynh_flows.js` |
| `07_screenshots_and_timing.cjs` | Drives the prototype in headless Chrome: a screenshot of each layout, and canvas against SVG timing | `outputs/screens/`, `outputs/drawing_timing.csv` |
| `08_clusters.R` | Whether divisions fall into connected groups, with raw and size-adjusted link weights | `outputs/clusters_sic2.csv` |
| `09_edition_breaks.R` | Whether the joins between the three ONS editions show as jumps | `outputs/inside_monthly_totals.csv` |
| `10_cadence.R` | What the number of payments adds to value, and how many dots a "one dot per 100 payments" rule puts on screen | `outputs/cadence_links_latest12.csv` |
| `11_intermediaries.R` | What changes when the four intermediary divisions are switched in | `outputs/levels_sic2_with_intermediaries.csv` |
| `12_consensus_groups.R` | Payment groups that hold across the years: 2,000 runs a year, core and floating members, and why per-year groups don't work | `outputs/payment_groups_consensus.csv`, `payment_groups_by_year.csv` |
| `13_page_checks.cjs` | Checks on the finished page: loads without errors, every control works, screenshots in light, dark and narrow layouts, frames a second while playing | `outputs/page/` |
| `14_all_regions_extract.R` | Scoping: one pass over the ONS files for every region's inside flows, and row counts for every pair of regions | `local/…/all_regions_own_monthly.rds`, `outputs/region_pair_row_counts.csv` |
| `15_url_roundtrip.cjs` | Scoping: whether each control's setting survives a round trip through the address bar | `outputs/url_roundtrip.csv` |
| `16_region_similarity.R` | Scoping: each region's data quality, how alike regions are overall, chain or web, and whether payment groups recur (about seven minutes) | `outputs/region_*.csv`, `payment_groups_uk_pooled.csv` |

`functions_hierarchy.R` holds the shared pieces: held values for hidden months, the flow matrix, levels and the direction-shuffle check.

## What they need

- **Data** from other project folders, as listed in the handover (`llm_output/payment_flows_handover.md`): the three ONS zips and the input-output workbook in `~/Code/claude/whole_economy_thinking/local/tradeable_jobs/`, and the five-digit file, SIC lookup, industrial strategy list and BRES file in `~/Code/RegionalEconomicTools/`.
- **R packages:** `data.table`, `igraph`, `jsonlite`, `readxl`.
- **For `07`, `13` and `15`:** Google Chrome and `puppeteer-core`. Install it anywhere outside this folder (`npm install puppeteer-core`) and run with `NODE_PATH=/path/to/node_modules node 07_screenshots_and_timing.cjs`. Add `--shots-only` or `--timing-only` to do half.

Large intermediate files go to `local/paymentflowsviz_ynh/`, which git ignores.

## The prototype

Open `prototype/prototype.html` in a browser (it loads D3 from a CDN, so it needs a connection). Every control can also be set in the address bar, for example `prototype.html?layout=order&cols=paygroup&top=430&dots=1&inter=1`.
