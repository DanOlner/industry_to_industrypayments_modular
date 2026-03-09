# Data

This folder contains **small, example, or synthetic data only**. Large or restricted datasets should not be committed to the repo.

## Data Dictionary

| File | Description | Source | Licence |
|---|---|---|---|
| `example_flows.csv` | Synthetic example data for the expected-vs-observed module | Generated | CC0 |

## Notes

- If you are using restricted data (e.g. via the ONS Secure Research Service or ADR UK), do **not** commit it here. Instead, document what the data is, where it can be accessed, and what approvals are needed.
- For ONS open data, consider using the [ONS API](https://developer.ons.gov.uk/) in your scripts so that data is fetched reproducibly rather than stored as static files.
