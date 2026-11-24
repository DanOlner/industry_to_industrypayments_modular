# 31_abs_coverage.R
# How much of each industry's business do the payments see, and how far does placing a business at its registered office
# move it between regions? Step 3 of plan/other_connections_plan.md, section 8.
#
# The yardstick is ONS's regional Annual Business Survey: turnover and purchases of goods, materials and services, by
# two-digit division and region, 2008 to 2024. It differs from the payments in the two ways that matter here: it is a
# survey grossed up to all businesses, and its regional figures are for sites, not registered offices. It covers the
# non-financial business economy, so not finance, most of farming, or the public sector.
#
# For 2023 (pass another year to change it) this sets, for each division and region:
#   what the division's businesses were paid, in the payments (by everyone; and by known industries only), against turnover;
#   what they paid (to everyone; to known industries only), against purchases.
# and asks three things:
#   1. Coverage: payments over the survey figure, for the UK, by division.
#   2. Place: each region's share of a division's payments against its share of the division's turnover. A region with a
#      larger share of payments than of turnover is one businesses are registered in more than they work in.
#   3. Whether, within a division, regions with more turnover also have more payments (so the payments are a guide to
#      where an industry is, even if not to how big it is).
# Payments are published values (no held values), so they are a floor. They include VAT; turnover does not.
#
# The survey workbook is kept in local/paymentflowsviz_ynh/downloads/abs_regional_2024 and fetched if missing.
# Needs the monthly data the build caches (local/paymentflowsviz_ynh/monthly_<region>.rds), so run build_data.R first.
# Run from the repo root: Rscript docs/chunks/paymentflowsviz_ynh/tests/31_abs_coverage.R [year] [region to print]   (default 2023 TLE)
source('docs/chunks/paymentflowsviz_ynh/tests/functions_hierarchy.R')
suppressPackageStartupMessages(library(readxl))
out <- file.path(chunk_dir, 'outputs'); args <- commandArgs(trailingOnly = TRUE); yr <- if (length(args)) as.integer(args[1]) else 2023L; show <- if (length(args) > 1) args[2] else 'TLE'
regions <- c('TLC', 'TLD', 'TLE', 'TLF', 'TLG', 'TLH', 'TLI', 'TLJ', 'TLK', 'TLL', 'TLM', 'TLN')
f <- file.path(local_dir, 'downloads/abs_regional_2024/absregionalsectionas2024.xlsx')
if (!file.exists(f)) { dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  download.file('https://www.ons.gov.uk/file?uri=/businessindustryandtrade/business/businessservices/datasets/uknonfinancialbusinesseconomyannualbusinesssurveyregionalresultssectionsas/current/absregionalsectionas2024.xlsx',
                f, mode = 'wb', quiet = TRUE, headers = c('User-Agent' = 'Mozilla/5.0 (research script)')) }
pc <- function(v) ifelse(is.finite(v), sprintf('%.0f%%', 100 * v), ''); short <- setNames(sic2_lk$short, sic2_lk$sic2)

# ---- the survey: divisions (two digits) by region, one year. [c] is suppressed, [x] no data, [low] under half the rounding
a <- as.data.table(suppressMessages(read_excel(f, sheet = 'All Regions', skip = 6, col_types = 'text')))
setnames(a, c('sic', 'description', 'region', 'region_name', 'year', 'turnover', 'agva', 'purchases', 'employment_costs'))
num <- function(v) suppressWarnings(as.numeric(v))
abs <- a[year == yr & grepl('^[0-9]{2}( \\(Part\\))?$', sic) & region %in% regions, .(sic2 = substr(sic, 1, 2), part = grepl('Part', sic), region, turnover_bn = num(turnover) / 1e3, purchases_bn = num(purchases) / 1e3)]
abs_uk <- a[year == yr & grepl('^[0-9]{2}( \\(Part\\))?$', sic) & region_name == 'United Kingdom', .(sic2 = substr(sic, 1, 2), uk_turnover_bn = num(turnover) / 1e3, uk_purchases_bn = num(purchases) / 1e3)]
cat(sprintf('Annual Business Survey %d: %d divisions. Region-division cells with turnover published: %d of %d.\n', yr, uniqueN(abs$sic2), abs[is.finite(turnover_bn), .N], nrow(abs)))

# ---- the payments: what each division in each region was paid and paid in the year (published values)
pay <- rbindlist(lapply(regions, function(r) { fr <- file.path(local_dir, sprintf('monthly_%s.rds', r)); if (!file.exists(fr)) { cat(r, ': no monthly data cached, skipped\n'); return(NULL) }
  cat('reading', r, '\n'); x <- readRDS(fr)[year(date) == yr & !is.na(value)]
  ind <- function(s) !s %in% c('00', not_industry, intermediary, '84')                         # a known industry, other than the intermediaries and public administration
  rec <- x[payee_region == r, .(received_bn = sum(value) / 1e9, received_industries_bn = sum(value[ind(payer_sic)]) / 1e9, received_inside_bn = sum(value[ind(payer_sic) & payer_region == r]) / 1e9), by = .(sic2 = payee_sic)]
  paid <- x[payer_region == r, .(paid_bn = sum(value) / 1e9, paid_industries_bn = sum(value[ind(payee_sic)]) / 1e9), by = .(sic2 = payer_sic)]
  merge(rec, paid, by = 'sic2', all = TRUE)[, region := r] }))
d <- merge(abs, pay, by = c('region', 'sic2'), all.x = TRUE)
for (v in c('received_bn', 'received_industries_bn', 'received_inside_bn', 'paid_bn', 'paid_industries_bn')) d[is.na(get(v)), (v) := 0]

# ---- 1. coverage, UK, by division. Regions where the survey hides a figure are left out of both sides
ok <- d[is.finite(turnover_bn) & turnover_bn > 0]
cov <- ok[, .(regions = .N, turnover_bn = sum(turnover_bn), received_bn = sum(received_bn), received_industries_bn = sum(received_industries_bn)), by = sic2]
okp <- d[is.finite(purchases_bn) & purchases_bn > 0, .(purchases_bn = sum(purchases_bn), paid_bn = sum(paid_bn), paid_industries_bn = sum(paid_industries_bn)), by = sic2]
cov <- merge(cov, okp, by = 'sic2', all.x = TRUE)[, `:=`(received_over_turnover = received_bn / turnover_bn, from_industries_over_turnover = received_industries_bn / turnover_bn,
                                                     paid_over_purchases = paid_bn / purchases_bn, to_industries_over_purchases = paid_industries_bn / purchases_bn, industry = short[sic2])]
big <- cov[turnover_bn >= 20][order(-received_over_turnover)]
cat(sprintf('\n== 1. Coverage, %d: what the payments record against the survey, all regions together ==\n', yr))
cat(sprintf('All divisions in the survey: paid to them GBP %.0fbn against turnover of GBP %.0fbn (%s); from known industries only, %s.\n', sum(cov$received_bn), sum(cov$turnover_bn), pc(sum(cov$received_bn) / sum(cov$turnover_bn)),
            pc(sum(cov$received_industries_bn) / sum(cov$turnover_bn))))
cat(sprintf('They paid GBP %.0fbn against purchases of GBP %.0fbn (%s); to known industries only, %s.\n', sum(cov$paid_bn, na.rm = TRUE), sum(cov$purchases_bn, na.rm = TRUE), pc(sum(cov$paid_bn, na.rm = TRUE) / sum(cov$purchases_bn, na.rm = TRUE)),
            pc(sum(cov$paid_industries_bn, na.rm = TRUE) / sum(cov$purchases_bn, na.rm = TRUE))))
cat(sprintf('By division, received over turnover: median %s, a quarter under %s and a quarter over %s (%d divisions).\n', pc(median(cov$received_over_turnover)), pc(quantile(cov$received_over_turnover, 0.25)),
            pc(quantile(cov$received_over_turnover, 0.75)), nrow(cov)))
cat('\nDivisions with turnover of GBP 20bn or more, most covered first:\n')
print(big[, .(industry, turnover_bn = round(turnover_bn), received = pc(received_over_turnover), from_industries = pc(from_industries_over_turnover), purchases_bn = round(purchases_bn), paid = pc(paid_over_purchases), to_industries = pc(to_industries_over_purchases))], nrows = 80)

# ---- 2. place: a region's share of a division's payments against its share of its turnover
sh <- ok[, `:=`(turnover_share = turnover_bn / sum(turnover_bn), payment_share = received_bn / sum(received_bn), n = .N), by = sic2][n >= 10 & is.finite(payment_share)]
reg <- sh[, .(divisions = .N, turnover_bn = sum(turnover_bn), received_bn = sum(received_bn)), by = region][, `:=`(share_of_turnover = turnover_bn / sum(turnover_bn), share_of_payments = received_bn / sum(received_bn))]
reg[, payments_over_turnover_share := share_of_payments / share_of_turnover]
med <- sh[, .(median_ratio = median(payment_share / turnover_share)), by = region]
cat('\n== 2. Place: each region\'s share of what the divisions were paid, against its share of their turnover ==\n')
print(merge(reg, med, by = 'region')[, .(region, share_of_turnover = pc(share_of_turnover), share_of_payments = pc(share_of_payments), ratio = round(payments_over_turnover_share, 2), median_division_ratio = round(median_ratio, 2))])
cat('Over 1: more of the payments are placed there than the turnover is. The last column is the same ratio division by division, at the median.\n')
lon <- sh[region == 'TLI'][order(-(payment_share / turnover_share))][turnover_bn >= 2, .(industry = short[sic2], turnover_share = pc(turnover_share), payment_share = pc(payment_share))]
cat('\nLondon, divisions where its share of payments most exceeds its share of turnover:\n'); print(lon[1:10])

# ---- 3. within a division, do regions with more turnover have more payments?
ag <- sh[, .(rho = cor(turnover_share, payment_share, method = 'spearman'), rho_no_london = cor(turnover_share[region != 'TLI'], payment_share[region != 'TLI'], method = 'spearman')), by = sic2]
cat(sprintf('\n== 3. Within a division, across regions: rank correlation of turnover share with payment share ==\nMedian %.2f over %d divisions; a quarter under %.2f. Without London: median %.2f.\n', median(ag$rho), nrow(ag), quantile(ag$rho, 0.25),
            median(ag$rho_no_london, na.rm = TRUE)))
cat('Lowest:\n'); print(ag[order(rho)][1:8, .(industry = short[sic2], rho = round(rho, 2))])

# ---- one region in full
one <- d[region == show & is.finite(turnover_bn) & turnover_bn >= 1][order(-turnover_bn)]
cat(sprintf('\n== %s, %d: divisions with turnover of GBP 1bn or more ==\n', show, yr))
print(one[, .(industry = short[sic2], turnover_bn = round(turnover_bn, 1), received_bn = round(received_bn, 1), received = pc(received_bn / turnover_bn), from_industries_here = pc(received_inside_bn / turnover_bn),
              purchases_bn = round(purchases_bn, 1), paid_bn = round(paid_bn, 1), paid = pc(paid_bn / purchases_bn))], nrows = 80)

res <- merge(d, sh[, .(region, sic2, turnover_share, payment_share)], by = c('region', 'sic2'), all.x = TRUE)[, industry := short[sic2]]
numc <- setdiff(names(res), c('region', 'sic2', 'industry', 'part')); res[, (numc) := lapply(.SD, function(v) signif(v, 4)), .SDcols = numc]
setcolorder(res, c('region', 'sic2', 'industry')); fwrite(res[order(region, sic2)], file.path(out, 'abs_coverage_by_region_division.csv'))
fwrite(cov[order(sic2), lapply(.SD, function(v) if (is.numeric(v)) signif(v, 4) else v)], file.path(out, 'abs_coverage_by_division.csv'))
cat('\nGBP billions. "part": the survey covers only part of the division. Wrote abs_coverage_by_region_division.csv and abs_coverage_by_division.csv\n')
