# 26_card_takings.R
# Households are not in the payments data. But when a customer pays a shop by card, the shop is later paid by its card
# acquirer, and that payment is in the data: from a finance firm (SIC 64 or 66), usually registered in London, to the
# shop. Earlier work (RegionalEconomicTools, payment_flows_method_and_caveats.md) found this for hotels and restaurants.
# Could those payments be used as a guess at how much of retail's sales are to households? This asks five things:
#   1. Who pays the consumer-facing industries, against a few industries that sell to businesses? By class of payer,
#      with the average payment, for one region and for the UK.
#   2. Do the payments from finance behave like household spending? Two marks of it: a December peak, and the first
#      lockdown (April to June 2020 against the same months of 2019). Each is worked out on cells published in every
#      month compared, so a cell being hidden one year and not the next can't move it.
#   3. How large are they? Finance's payments to UK retail, against what is known about card spending.
#   4. Is the money placed where the shops are? Each region's share of retail's receipts against its share of retail jobs.
#   5. The other direction, which is what was first noticed: retail's own payments to London finance. What are they?
# Published values only (no held values): this is about what ONS issued.
# Uses the monthly data the build caches (local/paymentflowsviz_ynh/monthly_<region>.rds), so run build_data.R first.
# Run from the repo root: Rscript docs/chunks/paymentflowsviz_ynh/tests/26_card_takings.R [region]   (default TLE; every region is read)
source('docs/chunks/paymentflowsviz_ynh/tests/functions_hierarchy.R')
out <- file.path(chunk_dir, 'outputs')
args <- commandArgs(trailingOnly = TRUE); show <- if (length(args)) args[1] else 'TLE'
regions <- c('TLC', 'TLD', 'TLE', 'TLF', 'TLG', 'TLH', 'TLI', 'TLJ', 'TLK', 'TLL', 'TLM', 'TLN')
consumer <- c('47', '56', '55', '96', '45', '93'); business <- c('46', '25', '41', '69'); watch <- c(consumer, business)
short <- setNames(sic2_lk$short, sic2_lk$sic2)
classes <- c('other industries', 'finance (64)', 'finance, auxiliary (66)', 'information services (63)', 'office support (82)', 'public administration (84)', 'unknown industry')
class_of <- function(s) fcase(s == '64', classes[2], s == '66', classes[3], s == '63', classes[4], s == '82', classes[5], s == '84', classes[6], s == '00', classes[7], default = classes[1])
pc <- function(v) ifelse(is.finite(v), sprintf('%.0f%%', 100 * v), '')

# ---- read: what the watched industries in each region are paid (by payer), and what retail there pays (by payee)
got <- lapply(regions, function(r) {
  f <- file.path(local_dir, sprintf('monthly_%s.rds', r)); if (!file.exists(f)) { cat(r, ': no monthly data cached, skipped\n'); return(NULL) }
  cat('reading', r, '\n'); x <- readRDS(f)[!is.na(value)]
  list(inn = x[payee_region == r & payee_sic %in% watch & !payer_sic %in% not_industry][, region := r],
       out = x[payer_region == r & payer_sic == '47' & !payee_sic %in% not_industry][, region := r],
       all_out = x[payer_region == r & !payer_sic %in% c('00', not_industry) & year(date) == 2025, .(v = sum(value), n = sum(n, na.rm = TRUE)),
                   by = .(payer_sic, to = fifelse(payee_sic == '64', fifelse(payee_region == 'TLI', 'finance in London', 'finance elsewhere'), 'anyone else'))][, region := r]) })
inn <- rbindlist(lapply(got, `[[`, 'inn')); retail_out <- rbindlist(lapply(got, `[[`, 'out')); all_out <- rbindlist(lapply(got, `[[`, 'all_out')); rm(got); invisible(gc())
inn[, `:=`(class = class_of(payer_sic), year = year(date), month = month(date))]

# ---- 1. who pays them, 2025
by_class <- function(d) { t <- d[, .(gbp_m = sum(value) / 1e6, payments_k = sum(n, na.rm = TRUE) / 1e3), by = .(payee_sic, class)]
  t[, `:=`(share = gbp_m / sum(gbp_m), average_gbp = ifelse(payments_k > 0, 1e3 * gbp_m / payments_k, NA_real_)), by = payee_sic][] }
one <- by_class(inn[region == show & year == 2025]); uk <- by_class(inn[year == 2025])
wide <- function(t, v, f = identity) { w <- dcast(t, payee_sic ~ factor(class, classes), value.var = v, fill = NA); w[match(watch, payee_sic)][, payee_sic := short[payee_sic]][, (classes) := lapply(.SD, f), .SDcols = classes][] }
cat(sprintf('\n== 1. Who pays each industry, 2025, share of what it was paid (published values). First six sell mostly to households, last four to businesses ==\n%s:\n', show))
print(wide(one, 'share', pc)); cat('\nAll twelve regions together:\n'); print(wide(uk, 'share', pc))
cat('\nAverage payment, GBP, all twelve regions together:\n'); print(wide(uk, 'average_gbp', function(v) ifelse(is.finite(v), format(round(v, -1), big.mark = ','), '')))
cat('\nRetail, all twelve regions, 2025: GBP billions and millions of payments by class of payer:\n')
print(uk[payee_sic == '47'][order(-gbp_m), .(class, gbp_bn = round(gbp_m / 1e3, 1), payments_m = round(payments_k / 1e3, 1), share = pc(share), average_gbp = round(average_gbp, -1))])

# ---- 2. do they behave like household spending?
cell <- c('payer_sic', 'payer_region', 'payee_sic', 'payee_region')
balanced <- function(d, months_needed) d[, if (.N == months_needed) .SD, by = cell]      # cells published in every month asked for
lock <- balanced(inn[month %in% 4:6 & year %in% 2019:2020], 6)[, .(a = sum(value[year == 2019]), b = sum(value[year == 2020]), cells = uniqueN(paste(payer_sic, payer_region, payee_region))), by = .(payee_sic, class)]
xmas <- rbindlist(lapply(c(2019, 2021:2024), function(y) balanced(inn[year == y], 12)[, .(dec = sum(value[month == 12]), mean_month = sum(value) / 12, year = y), by = .(payee_sic, class)]))
xmas <- xmas[, .(december = mean(dec / mean_month)), by = .(payee_sic, class)]
grow <- balanced(inn[year %in% c(2019, 2025) & month <= 9], 18)[, .(growth = sum(value[year == 2025]) / sum(value[year == 2019])), by = .(payee_sic, class)]
sig <- merge(merge(lock[a > 0, .(payee_sic, class, lockdown = b / a, cells, base_gbp_m = a / 1e6)], xmas, by = c('payee_sic', 'class'), all = TRUE), grow, by = c('payee_sic', 'class'), all = TRUE)
sig <- sig[is.na(base_gbp_m) | base_gbp_m >= 5]                                              # too little money to read anything from
show_sig <- function(v, f) { w <- dcast(sig, payee_sic ~ factor(class, classes), value.var = v); w[match(watch, payee_sic)][, payee_sic := short[payee_sic]][, (intersect(classes, names(w))) := lapply(.SD, f), .SDcols = intersect(classes, names(w))][] }
cat('\n== 2. Marks of household spending, all twelve regions together ==\n')
cat('First lockdown: April to June 2020 as a share of April to June 2019, on cells published in all six months. 100% is no change.\n'); print(show_sig('lockdown', pc))
cat('\nDecember against the average month of the same year (2019 and 2021 to 2024), on cells published in all twelve months. 100% is no peak.\n'); print(show_sig('december', pc))
cat('\nJanuary to September 2025 against the same months of 2019, on cells published in all eighteen months.\n'); print(show_sig('growth', pc))

# ---- 3. how large are the payments from finance to retail?
fin <- inn[payee_sic == '47' & year %in% 2019:2025, .(gbp_bn = sum(value) / 1e9, payments_m = sum(n, na.rm = TRUE) / 1e6), by = .(year, class)]
cat('\n== 3. What UK retail was paid, GBP billions a year, by class of payer (published values) ==\n')
print(dcast(fin, year ~ factor(class, classes), value.var = 'gbp_bn')[, (classes) := lapply(.SD, round, 1), .SDcols = classes][])
f23 <- fin[year == 2023 & class %in% classes[2:3], sum(gbp_bn)]
cat(sprintf('\nFinance (64 and 66) paid UK retail GBP %.1fbn in 2023, or GBP %.2fbn a month. For scale: UK Finance put all UK card purchases at GBP 84bn in June 2023 alone (quoted by ONS), and retail is a large part of that.\n', f23, f23 / 12))

# ---- 4. is the money where the shops are?
bres <- file.path(local_dir, 'bres_itl1_sic5.rds')
place <- inn[payee_sic == '47' & year == 2023, .(all_gbp_bn = sum(value) / 1e9, from_finance_gbp_bn = sum(value[class %in% classes[2:3]]) / 1e9), by = region]
place[, `:=`(share_of_receipts = all_gbp_bn / sum(all_gbp_bn), share_from_finance = from_finance_gbp_bn / sum(from_finance_gbp_bn))]
if (file.exists(bres)) { j <- readRDS(bres)[substr(sic5, 1, 2) == '47', .(jobs = sum(jobs)), by = .(region = itl1)]; place <- merge(place, j[, .(region, share_of_retail_jobs = jobs / sum(jobs))], by = 'region', all.x = TRUE)
  place[, receipts_over_jobs := share_of_receipts / share_of_retail_jobs] }
cat('\n== 4. Retail, 2023: each region\'s share of what UK retail was paid, against its share of retail jobs ==\n')
print(place[, .(region, receipts_gbp_bn = round(all_gbp_bn, 1), share_of_receipts = pc(share_of_receipts), share_from_finance = pc(share_from_finance),
                share_of_retail_jobs = if ('share_of_retail_jobs' %in% names(place)) pc(share_of_retail_jobs) else NA, receipts_over_jobs = if ('receipts_over_jobs' %in% names(place)) round(receipts_over_jobs, 2) else NA)])

# ---- 5. the other direction: what retail pays to finance
retail_out[, `:=`(year = year(date), to = fifelse(payee_sic == '64', fifelse(payee_region == 'TLI', 'finance (64) in London', 'finance (64) elsewhere'), fifelse(payee_sic == '00', 'unknown industry', 'everyone else')))]
ro <- retail_out[year == 2025, .(gbp_bn = sum(value) / 1e9, payments_k = sum(n, na.rm = TRUE) / 1e3), by = .(region, to)][, `:=`(share = gbp_bn / sum(gbp_bn), average_gbp = 1e6 * gbp_bn / payments_k), by = region]
cat('\n== 5. What retail pays to finance in London, 2025, by where the retailer is registered ==\n')
print(ro[to == 'finance (64) in London', .(region, gbp_bn = round(gbp_bn, 2), share_of_what_retail_pays = pc(share), payments_k = round(payments_k), average_gbp = format(round(average_gbp, -2), big.mark = ','))])
cat(sprintf('\nFor comparison, the average payment retail in %s makes to everyone else: GBP %s; and the average it receives from finance in London: GBP %s.\n', show,
            format(round(ro[region == show & to == 'everyone else', average_gbp], -2), big.mark = ','),
            format(round(inn[region == show & payee_sic == '47' & year == 2025 & payer_sic == '64' & payer_region == 'TLI', 1e0 * sum(value) / sum(n, na.rm = TRUE)], -1), big.mark = ',')))
ao <- dcast(all_out[region == show], payer_sic ~ to, value.var = 'v', fill = 0)[, total := `anyone else` + `finance elsewhere` + `finance in London`][total > 5e8]
ao[, share_london_finance := `finance in London` / total]
avg <- all_out[region == show & to == 'finance in London', .(payer_sic, average_gbp = v / n)]
cat(sprintf('\nIs retail unusual? %s, 2025: the industries paying the largest share of all they pay to finance in London (those paying over GBP 0.5bn in all):\n', show))
print(merge(ao, avg, by = 'payer_sic')[order(-share_london_finance)][1:12, .(industry = short[payer_sic], pays_gbp_bn = round(total / 1e9, 1), to_london_finance = pc(share_london_finance), average_gbp = format(round(average_gbp, -2), big.mark = ','))])
ry <- retail_out[region == show & to == 'finance (64) in London', .(gbp_bn = sum(value) / 1e9, payments_k = sum(n, na.rm = TRUE) / 1e3, months_published = uniqueN(date)), by = year][order(year)]
cat(sprintf('\n%s retail to finance in London, by year:\n', show)); print(ry[, .(year, gbp_bn = round(gbp_bn, 2), payments_k = round(payments_k), average_gbp = format(round(1e6 * gbp_bn / payments_k, -2), big.mark = ','), months_published)])

# ---- write
res <- rbind(one[, region := show], uk[, region := 'UK'])[, `:=`(industry = short[payee_sic], sells_mostly_to = fifelse(payee_sic %in% consumer, 'households', 'businesses'))]
res <- merge(res, sig[, .(payee_sic, class, lockdown_apr_jun_2020_over_2019 = lockdown, december_over_average_month = december, jan_sep_2025_over_2019 = growth)], by = c('payee_sic', 'class'), all.x = TRUE)
numc <- c('gbp_m', 'payments_k', 'share', 'average_gbp', 'lockdown_apr_jun_2020_over_2019', 'december_over_average_month', 'jan_sep_2025_over_2019'); res[, (numc) := lapply(.SD, function(v) signif(v, 4)), .SDcols = numc]
setcolorder(res, c('region', 'payee_sic', 'industry', 'sells_mostly_to', 'class')); fwrite(res[order(region, match(payee_sic, watch), match(class, classes))], file.path(out, 'card_takings_by_payer.csv'))
fwrite(place[, lapply(.SD, function(v) if (is.numeric(v)) signif(v, 4) else v)], file.path(out, 'card_takings_retail_by_region.csv'))
cat('\nThe marks in part 2 are for the UK and are repeated on both regions\' rows in the file. Wrote card_takings_by_payer.csv and card_takings_retail_by_region.csv\n')
