# 01_extract_ynh_monthly.R
# Pull every monthly row that touches Yorkshire and the Humber (TLE) out of the three ONS
# "Industry-to-industry payment flows by region" zips, and save one tidy monthly table.
# Run from the repo root: Rscript docs/chunks/paymentflowsviz_ynh/tests/01_extract_ynh_monthly.R
#
# Output (gitignored, in local/): local/paymentflowsviz_ynh/ynh_monthly.rds
#   payer_sic, payer_region, payee_sic, payee_region, date (Date, first of month),
#   value (GBP, NA if suppressed), n (payments, NA if suppressed), suppressed (logical)
suppressPackageStartupMessages(library(data.table))

raw_dir <- path.expand('~/Code/claude/whole_economy_thinking/local/tradeable_jobs/data_raw/payments')
zips <- file.path(raw_dir, c('industryregionbusinesspayments1921dataset17092026.zip',
                             'industryregionbusinesspayments2224dataset17092026.zip',
                             'industryregionbusinesspayments2526dataset22102026.zip'))
stopifnot(all(file.exists(zips)))
out_dir <- 'local/paymentflowsviz_ynh'
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

read_one <- function(f) {
  # grep keeps only rows with TLE somewhere, which cuts each file to about a sixth before R sees it
  d <- fread(cmd = paste('unzip -p', shQuote(f), "| grep -F ',TLE,'"), colClasses = 'character',
             header = FALSE, showProgress = FALSE)
  setnames(d, c('payer_sic', 'payer_region', 'payee_sic', 'payee_region', 'date', 'value', 'n'))
  d <- d[payer_region == 'TLE' | payee_region == 'TLE']
  d[, suppressed := value == '[C]']
  d[, `:=`(value = suppressWarnings(as.numeric(value)), n = suppressWarnings(as.numeric(n)))]
  d[, date := as.Date(date, format = '%d-%b-%y')]
  d[]
}

ynh <- rbindlist(lapply(zips, function(f) { message('reading ', basename(f)); read_one(f) }))
stopifnot(!anyNA(ynh$date), !anyDuplicated(ynh[, .(payer_sic, payer_region, payee_sic, payee_region, date)]))
setkey(ynh, date, payer_region, payee_region, payer_sic, payee_sic)
saveRDS(ynh, file.path(out_dir, 'ynh_monthly.rds'))

cat('rows:', nrow(ynh), '\n')
cat('months:', uniqueN(ynh$date), 'from', format(min(ynh$date)), 'to', format(max(ynh$date)), '\n')
cat('own-region rows:', ynh[payer_region == 'TLE' & payee_region == 'TLE', .N],
    ' suppressed share:', ynh[payer_region == 'TLE' & payee_region == 'TLE', round(mean(suppressed), 3)], '\n')
print(ynh[, .(rows = .N, suppressed = round(mean(suppressed), 3)), by = .(year = year(date))])
