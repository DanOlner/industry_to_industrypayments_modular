# 14_all_regions_extract.R
# Scoping for "every ITL1 region": one pass over the three ONS zips that
#   (a) keeps every row where payer and payee are in the same region (each region's inside flows), and
#   (b) counts rows and hidden rows for every pair of regions, to size what a build for all 12 would handle.
# Run from the repo root: Rscript docs/chunks/paymentflowsviz_ynh/tests/14_all_regions_extract.R   (about five minutes)
# Outputs: local/paymentflowsviz_ynh/all_regions_own_monthly.rds; outputs/region_pair_row_counts.csv
suppressPackageStartupMessages(library(data.table))
raw_dir <- path.expand('~/Code/claude/whole_economy_thinking/local/tradeable_jobs/data_raw/payments')
zips <- file.path(raw_dir, c('industryregionbusinesspayments1921dataset17092026.zip', 'industryregionbusinesspayments2224dataset17092026.zip',
                             'industryregionbusinesspayments2526dataset22102026.zip'))
stopifnot(all(file.exists(zips)))
local_dir <- 'local/paymentflowsviz_ynh'; out <- 'docs/chunks/paymentflowsviz_ynh/tests/outputs'

read_one <- function(f) {
  tally <- tempfile(fileext = '.csv')
  awk <- sprintf("awk -F, -v out=%s 'NR>1 { k=$2\",\"$4; c[k]++; if ($6==\"[C]\") h[k]++; if ($2==$4) print } END { for (k in c) print k\",\"c[k]\",\"h[k]+0 > out }'", shQuote(tally))
  t0 <- Sys.time()
  d <- fread(cmd = paste('unzip -p', shQuote(f), '|', awk), colClasses = 'character', header = FALSE, showProgress = FALSE)
  setnames(d, c('payer_sic', 'payer_region', 'payee_sic', 'payee_region', 'date', 'value', 'n'))
  d[, suppressed := value == '[C]'][, `:=`(value = suppressWarnings(as.numeric(value)), n = suppressWarnings(as.numeric(n)))]
  d[, date := as.Date(date, format = '%d-%b-%y')]
  tl <- fread(tally, header = FALSE, col.names = c('payer_region', 'payee_region', 'rows', 'hidden')); unlink(tally)
  message(basename(f), ': ', nrow(d), ' same-region rows of ', sum(tl$rows), ' in ', round(as.numeric(Sys.time() - t0, units = 'secs')), 's')
  list(own = d, tally = tl)
}
res <- lapply(zips, read_one)
own <- rbindlist(lapply(res, `[[`, 'own'))[payer_region != 'unknown']
stopifnot(!anyNA(own$date), !anyDuplicated(own[, .(payer_sic, payee_sic, payer_region, date)]))
saveRDS(own, file.path(local_dir, 'all_regions_own_monthly.rds'))
tally <- rbindlist(lapply(res, `[[`, 'tally'))[, .(rows = sum(rows), hidden = sum(hidden)), by = .(payer_region, payee_region)]
fwrite(tally[order(payer_region, payee_region)], file.path(out, 'region_pair_row_counts.csv'))

regs <- setdiff(sort(unique(tally$payer_region)), 'unknown')
size <- rbindlist(lapply(regs, function(r) tally[payer_region == r | payee_region == r, .(region = r, rows_touching_m = round(sum(rows) / 1e6, 2),
  inside_rows_k = round(sum(rows[payer_region == r & payee_region == r]) / 1e3), inside_hidden = round(sum(hidden[payer_region == r & payee_region == r]) / sum(rows[payer_region == r & payee_region == r]), 3),
  outside_hidden = round(sum(hidden[payer_region != payee_region]) / sum(rows[payer_region != payee_region]), 3))]))
cat('\nRows a build would handle for each region (all months, all industries):\n'); print(size)
cat('All rows:', round(sum(tally$rows) / 1e6, 1), 'm; rows with an unknown region:', round(tally[payer_region == 'unknown' | payee_region == 'unknown', sum(rows)] / 1e6, 2), 'm\n')
