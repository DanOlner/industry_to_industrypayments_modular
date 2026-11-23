# Regional payment flows (Yorkshire and the Humber first)

## Status: built, not yet published

`index.html` is a single-page visualisation of monthly payments between a region's industries, January 2019 to September 2026. It opens on Yorkshire and the Humber and can switch to any of the twelve ITL1 regions. Open it in a browser straight from disk; it needs no server and no internet connection.

It becomes public on GitHub Pages once this folder is committed and pushed to `master`.

## What's here

| File | What |
|----|----|
| `index.html`, `flows.css`, `flows.js` | The page |
| `data/flows_<region>.js` | One data file for each region built, loaded when that region is chosen |
| `data/regions.js` | A small index of all twelve regions: data quality, how alike they are, and how chain-like, loaded with the page |
| `build_data.R` | Builds those from the three ONS zips and prints checks |
| `lib/d3.v7.9.0.min.js` | D3 7.9.0 (ISC licence), kept locally so the page has no outside dependencies |
| `plan/paymentflowsviz_plan.md` | The plan, the test findings behind each design choice, and the decisions taken |
| `tests/` | The tests, their outputs, a rough prototype, and checks on the finished page. See `tests/README.md` |

## Rebuilding the data

From the repo root:

```
Rscript docs/chunks/paymentflowsviz_ynh/build_data.R              # the sample regions named at the top of the script
Rscript docs/chunks/paymentflowsviz_ynh/build_data.R TLE TLD      # named regions
Rscript docs/chunks/paymentflowsviz_ynh/build_data.R all          # all twelve
Rscript docs/chunks/paymentflowsviz_ynh/build_data.R index        # only the index of regions
Rscript docs/chunks/paymentflowsviz_ynh/build_data.R stage TLF    # build without touching the site (see below)
```

- **Time.** A region takes about three minutes the first time (extracting it from the ONS files, then holding hidden months) and under a minute after that. Intermediate tables are cached in `local/paymentflowsviz_ynh/`, which git ignores.
- **The index** is rebuilt on every run. It needs every region's inside flows, which `tests/14_all_regions_extract.R` extracts once (three minutes).
- **Checks.** The run ends with a table of each region built. Yorkshire and the Humber is compared with the test results and the script says whether all 14 match.
- **Inputs.** The raw ONS files, the industrial strategy list and the jobs data are read from the folders listed in `tests/README.md`. `tests/00_lookups.R` makes the division names and sections.
- **Staging.** With `stage` as the first word, files go to `local/paymentflowsviz_ynh/staged_data/` and the site's `data/` folder is left alone. Regions already on the site are copied in beside them and an index is written there too, so the staged folder is a complete candidate for `data/`. To put staged regions on the site, copy the folder's contents over `data/`. To look at them first, copy `index.html`, `flows.js`, `flows.css` and `lib/` somewhere private, put the staged folder beside them as `data/`, and open that copy; `tests/13_page_checks.cjs` can be pointed at it with `PAGE_DIR`.
- **Region codes:** TLC North East, TLD North West, TLE Yorkshire and the Humber, TLF East Midlands, TLG West Midlands, TLH East of England, TLI London, TLJ South East, TLK South West, TLL Wales, TLM Scotland, TLN Northern Ireland.

Payment group names are set by hand at the top of `build_data.R`, for Yorkshire and the Humber only. Other regions' groups are labelled by their largest members.

The figures the page quotes about a region (in "How the order is worked out" and "What this shows") are worked out by the build and carried in that region's data file, so they follow the data.

## Checking the page

`tests/13_page_checks.cjs` loads the page in headless Chrome, works every control, saves screenshots to `tests/outputs/page/` and times it playing. How to run it is in `tests/README.md`.

## Feedback

See [Issues](../../../../issues) or open a new one.
