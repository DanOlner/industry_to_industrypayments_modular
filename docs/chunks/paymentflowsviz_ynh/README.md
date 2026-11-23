# Yorkshire and the Humber payment flows

## Status: built, not yet published

`index.html` is a single-page visualisation of monthly payments between Yorkshire and the Humber's industries, January 2019 to September 2026. Open it in a browser straight from disk; it needs no server and no internet connection.

It becomes public on GitHub Pages once this folder is committed and pushed to `master`.

## What's here

| File | What |
|----|----|
| `index.html`, `flows.css`, `flows.js` | The page |
| `ynh_flows_data.js` | Its data, written by `build_data.R` |
| `build_data.R` | Builds the data file from the three ONS zips and prints checks |
| `lib/d3.v7.9.0.min.js` | D3 7.9.0 (ISC licence), kept locally so the page has no outside dependencies |
| `plan/paymentflowsviz_plan.md` | The plan, the test findings behind each design choice, and the decisions taken |
| `tests/` | The tests, their outputs, a rough prototype, and checks on the finished page. See `tests/README.md` |

## Rebuilding the data

From the repo root:

```
Rscript docs/chunks/paymentflowsviz_ynh/tests/00_lookups.R     # only if the lookups need remaking
Rscript docs/chunks/paymentflowsviz_ynh/build_data.R
```

The second takes about a minute the first time and half that afterwards. It reads the raw ONS files from the folders listed in `tests/README.md` and caches intermediate tables in `local/paymentflowsviz_ynh/`, which git ignores. It ends by printing each check beside the value expected from the tests. When ONS issues a new edition, compare that table before publishing.

Payment group names are set by hand at the top of `build_data.R`.

The figures quoted on the page, in "How the order is worked out" and "What this shows", are typed into `index.html`. They come from `tests/02`, `03`, `09`, `11` and `12`. After a rebuild on new data, re-run those and update the text.

## Checking the page

`tests/13_page_checks.cjs` loads the page in headless Chrome, works every control, saves screenshots to `tests/outputs/page/` and times it playing. How to run it is in `tests/README.md`.

## Feedback

See [Issues](../../../../issues) or open a new one.
