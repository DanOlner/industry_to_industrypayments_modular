# 32_household_spending.R
# Households are not in the payments. What do they spend in each region on the things each industry sells, and how does
# that sit beside the industry's turnover and what the payments show it being paid? Step 4 of
# plan/other_connections_plan.md, section 8.
#
# The source is ONS's regional household final consumption expenditure, 2009 to 2023 (official statistics in development):
# spending by kind of good or service (COICOP), for ITL1 regions. It comes two ways. "Domestic" is spending that takes
# place in the region, whoever does it, which is what a region's shops and restaurants take. "National" is spending by
# the region's residents, wherever they do it. The gap between the two is spending that crosses the region's edge.
#
# Kinds of spending are matched to industries by hand below. The match is loose: a kind of spending is a product and an
# industry is who sold it, so "food" is put with retail though some is sold by wholesalers and caterers. Spending
# includes VAT. Turnover (from the regional Annual Business Survey, tests/31) does not, and includes sales to businesses.
#
# For one year (default 2023) it sets, for each line and region: households' spending in the region; the turnover of the
# matched divisions; what the payments show them being paid, by everyone, by finance (card takings, tests/26) and by
# known industries. Published payment values, so a floor.
#
# The workbook is kept in local/paymentflowsviz_ynh/downloads/regional_hhfce_2009_2023 and fetched if missing. Needs
# tests/31 to have been run (for turnover) and the monthly data the build caches.
# Run from the repo root: Rscript docs/chunks/paymentflowsviz_ynh/tests/32_household_spending.R [year] [region to print]   (default 2023 TLE)
source('docs/chunks/paymentflowsviz_ynh/tests/functions_hierarchy.R')
suppressPackageStartupMessages(library(readxl))
out <- file.path(chunk_dir, 'outputs'); args <- commandArgs(trailingOnly = TRUE); yr <- if (length(args)) as.integer(args[1]) else 2023L; show <- if (length(args) > 1) args[2] else 'TLE'
regions <- c('TLC', 'TLD', 'TLE', 'TLF', 'TLG', 'TLH', 'TLI', 'TLJ', 'TLK', 'TLL', 'TLM', 'TLN')
f <- file.path(local_dir, 'downloads/regional_hhfce_2009_2023/regionalhouseholdexpenditure2009to2023.xlsx')
if (!file.exists(f)) { dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  download.file(paste0('https://www.ons.gov.uk/file?uri=/economy/regionalaccounts/grossdisposablehouseholdincome/datasets/regionalhouseholdfinalconsumptionexpenditureinternationalterritoriallevel1countriesandregions',
                       'andinternationalterritoriallevel2subregions/2009to2023/regionalhouseholdexpenditure2009to2023.xlsx'), f, mode = 'wb', quiet = TRUE, headers = c('User-Agent' = 'Mozilla/5.0 (research script)')) }
pc <- function(v) ifelse(is.finite(v), sprintf('%.0f%%', 100 * v), ''); r1 <- function(v) round(v, 1)

# ---- kinds of spending, matched to the divisions that mostly sell them
lines <- list(
  list('Goods bought in shops', c('01', '02.1', '02.2', '03', '05', '06.1', '08.2', '09.1', '09.2', '09.3', '09.5', '12.3'), '47'),
  list('Vehicles', '07.1', '45'),
  list('Fuel, parts and repairs for vehicles', '07.2', c('45', '47')),
  list('Eating and drinking out', '11.1', '56'),
  list('Hotels and other accommodation', '11.2', '55'),
  list('Transport fares', '07.3', c('49', '50', '51')),
  list('Phone and internet services', '08.3', '61'),
  list('Recreation and culture services', '09.4', c('59', '90', '91', '92', '93')),
  list('Education', '10', '85'),
  list('Health services', c('06.2', '06.3'), '86'),
  list('Rent paid', '04.1', '68'),
  list('Electricity, gas and other fuels', '04.5', '35'),
  list('Insurance', '12.5', '65'),
  list('Social care', '12.4', c('87', '88')))
line_names <- vapply(lines, `[[`, '', 1)

# ---- household spending: domestic (in the region) and national (by its residents), GBP billions
read_t <- function(sheet) { d <- as.data.table(suppressMessages(read_excel(f, sheet = sheet, skip = 2, col_types = 'text')))
  d[`ITL code` %in% c(regions, 'UK'), .(region = `ITL code`, code = `COICOP code`, commodity = Commodity, bn = suppressWarnings(as.numeric(get(as.character(yr)))) / 1e3)] }
dom <- read_t('Table 3'); nat <- read_t('Table 2')
stopifnot(all(unlist(lapply(lines, `[[`, 2)) %in% dom$code))
hh <- rbindlist(lapply(lines, function(l) merge(dom[code %in% l[[2]], .(domestic_bn = sum(bn)), by = region], nat[code %in% l[[2]], .(national_bn = sum(bn)), by = region], by = 'region')[, line := l[[1]]]))
tot <- merge(dom[code == '0', .(region, domestic_bn = bn)], nat[code == '0', .(region, national_bn = bn)], by = 'region')
covered <- hh[region == 'UK', sum(domestic_bn)] / tot[region == 'UK', domestic_bn]; imputed <- dom[region == 'UK' & code == '04.2', bn] / tot[region == 'UK', domestic_bn]
cat(sprintf('Household spending in the UK, %d: GBP %.0fbn. The lines here cover %s of it. Another %s is imputed rent, which nobody pays.\n', yr, tot[region == 'UK', domestic_bn], pc(covered), pc(imputed)))

# ---- turnover (tests/31) and the payments, for the matched divisions
abs_file <- file.path(out, 'abs_coverage_by_region_division.csv'); stopifnot(file.exists(abs_file))
ab <- fread(abs_file, colClasses = list(character = 'sic2'))
watch <- unique(unlist(lapply(lines, `[[`, 3)))
pay <- rbindlist(lapply(regions, function(r) { fr <- file.path(local_dir, sprintf('monthly_%s.rds', r)); if (!file.exists(fr)) return(NULL)
  cat('reading', r, '\n'); x <- readRDS(fr)[year(date) == yr & !is.na(value) & payee_region == r & payee_sic %in% watch]
  x[, .(received_bn = sum(value) / 1e9, from_finance_bn = sum(value[payer_sic %in% c('64', '66')]) / 1e9, from_platforms_bn = sum(value[payer_sic %in% c('63', '82')]) / 1e9,
        from_industries_bn = sum(value[!payer_sic %in% c('00', '84', not_industry, intermediary)]) / 1e9, from_unknown_bn = sum(value[payer_sic == '00']) / 1e9), by = .(sic2 = payee_sic)][, region := r] }))
by_line <- rbindlist(lapply(lines, function(l) { s <- l[[3]]
  p <- pay[sic2 %in% s, lapply(.SD, sum), by = region, .SDcols = c('received_bn', 'from_finance_bn', 'from_platforms_bn', 'from_industries_bn', 'from_unknown_bn')]
  t <- ab[sic2 %in% s, .(turnover_bn = if (all(is.finite(turnover_bn))) sum(turnover_bn) else NA_real_), by = region]
  merge(p, t, by = 'region', all = TRUE)[, `:=`(line = l[[1]], divisions = paste(s, collapse = ' '))] }))
uk <- by_line[, lapply(.SD, sum), by = .(line, divisions), .SDcols = c('received_bn', 'from_finance_bn', 'from_platforms_bn', 'from_industries_bn', 'from_unknown_bn', 'turnover_bn')][, region := 'UK']
res <- merge(hh, rbind(by_line, uk, use.names = TRUE), by = c('region', 'line'), all.x = TRUE)[order(region, match(line, line_names))]
res[, `:=`(spending_over_turnover = domestic_bn / turnover_bn, received_over_spending = received_bn / domestic_bn, finance_over_spending = from_finance_bn / domestic_bn, in_over_residents = domestic_bn / national_bn)]

show_tab <- function(r) res[region == r, .(line, households_spend_here = r1(domestic_bn), turnover = r1(turnover_bn), spend_over_turnover = pc(spending_over_turnover), payments_received = r1(received_bn),
                                          from_finance = r1(from_finance_bn), from_industries = r1(from_industries_bn), from_unknown = r1(from_unknown_bn), finance_over_spend = pc(finance_over_spending))]
cat(sprintf('\n== UK, %d, GBP billions: what households spend, the turnover of the industries that sell it, and what the payments show those industries being paid ==\n', yr)); print(show_tab('UK'))
cat(sprintf('\n== %s ==\n', show)); print(show_tab(show))
cat('\nTurnover is blank where the survey hides a division or doesn\'t cover it (education, health and finance are mostly outside it).\n')

# ---- spending that crosses the region's edge: spending in the region against spending by its residents
cat('\n== Spending in the region as a share of spending by its residents (over 100%: more is spent there than its residents spend) ==\n')
edge <- dcast(res[region != 'UK' & line %in% c('Goods bought in shops', 'Eating and drinking out', 'Hotels and other accommodation', 'Recreation and culture services', 'Transport fares')], region ~ line, value.var = 'in_over_residents')
edge <- merge(tot[region != 'UK', .(region, `All spending` = domestic_bn / national_bn)], edge, by = 'region'); print(edge[, lapply(.SD, function(v) if (is.numeric(v)) pc(v) else v)])

# ---- do regions where households spend more also have more turnover, and more payments? Across the twelve regions, line by line
sp <- function(x, y) { ok <- is.finite(x) & is.finite(y); if (sum(ok) < 6) NA_real_ else cor(x[ok], y[ok], method = 'spearman') }
ag <- res[region != 'UK', .(with_turnover = sp(domestic_bn, turnover_bn), with_payments = sp(domestic_bn, received_bn), with_finance = sp(domestic_bn, from_finance_bn),
                            spread_of_finance_over_spending = paste(pc(range(finance_over_spending[is.finite(finance_over_spending)])), collapse = ' to ')), by = line]
cat('\n== Across the twelve regions: rank correlation of households\' spending with turnover, with all payments received, and with payments from finance ==\n'); print(ag, digits = 2)
cat('The last column is finance\'s payments as a share of what households spend, lowest region to highest: if card takings were captured evenly it would be the same everywhere.\n')

numc <- names(res)[vapply(res, is.numeric, TRUE)]; res[, (numc) := lapply(.SD, function(v) signif(v, 4)), .SDcols = numc]
setcolorder(res, c('region', 'line', 'divisions')); fwrite(res, file.path(out, 'household_spending_by_region.csv'))
cat('\nWrote household_spending_by_region.csv\n')
