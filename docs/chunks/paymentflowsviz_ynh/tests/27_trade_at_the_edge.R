# 27_trade_at_the_edge.R
# What do official figures say crosses a region's edge, industry by industry, and how do the payments sit beside them?
# The payments show an industry's links with other UK businesses. They show nothing of what it buys from or sells to
# other countries, and section 13.10 of the plan found that is why wholesale looks like the start of a chain. ONS
# publishes two sets of estimates that fill some of this in, both official statistics in development:
#   - subnational trade in goods and in services, 2023: what each ITL1 region's businesses import from and export to
#     other countries, by eight or nine industry groups. Trade is shared out to a business's sites by employment, so
#     it is not placed at the registered office as the payments are.
#   - interregional trade in goods and services, 2021: what each ITL1 region's businesses buy from and sell to each
#     other ITL1 region, by eight industry groups. For English regions ONS splits goods by road freight and services by
#     these same payments data, so the services side is not an independent check on the payments.
# This sets them beside the payments for the same industry groups: what each group in a region is paid by, and pays to,
# businesses inside the region and in the rest of the UK (held values, as the page uses). It prints one region in full
# and writes every region.
#
# Each ONS workbook is kept in its own folder under local/paymentflowsviz_ynh/downloads and fetched if it is missing.
# Needs the held values the build caches (local/paymentflowsviz_ynh/held_<region>.rds), so run build_data.R first.
# Run from the repo root: Rscript docs/chunks/paymentflowsviz_ynh/tests/27_trade_at_the_edge.R [region]   (default TLE)
source('docs/chunks/paymentflowsviz_ynh/tests/functions_hierarchy.R')
suppressPackageStartupMessages(library(readxl))
out <- file.path(chunk_dir, 'outputs'); dl <- file.path(local_dir, 'downloads')
args <- commandArgs(trailingOnly = TRUE); show <- if (length(args)) args[1] else 'TLE'
ons <- 'https://www.ons.gov.uk/file?uri=/businessindustryandtrade/internationaltrade/datasets/'
files <- list(
  goods    = c('subnational_goods_2023', 'subnationaltradeingoods2023.xlsx', 'subnationaltradeingoods/2023/subnationaltradeingoods2023.xlsx'),
  services = c('subnational_services_2023', 'subnationaltradeinservices2023.xlsx', 'subnationaltradeinservices/2023/subnationaltradeinservices2023.xlsx'),
  irt      = c('interregional_2021', '2021interregionaltradeuk.xlsx', 'interregionaltradeingoodsandservicesuk/2021/2021interregionaltradeuk.xlsx'))
get_file <- function(x) {                       # a downloaded workbook is only ever read as data, from a folder holding nothing else
  f <- file.path(dl, x[1], x[2]); if (file.exists(f)) return(f)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  download.file(paste0(ons, x[3]), f, mode = 'wb', quiet = TRUE, headers = c('User-Agent' = 'Mozilla/5.0 (research script)')); f
}
num <- function(v) suppressWarnings(as.numeric(v))   # [c] hidden and [z] not applicable become NA

# ---- industry groups, as ONS defines them (the workbooks' own "Industry Groupings" and "Sector Breakdowns" sheets)
sections <- function(s) { n <- as.integer(s)
  fcase(n <= 3, 'A', n <= 9, 'B', n <= 33, 'C', n == 35, 'D', n <= 39, 'E', n <= 43, 'F', n <= 47, 'G', n <= 53, 'H', n <= 56, 'I', n <= 63, 'J', n <= 66, 'K',
        n == 68, 'L', n <= 75, 'M', n <= 82, 'N', n == 84, 'O', n == 85, 'P', n <= 88, 'Q', n <= 93, 'R', n <= 96, 'S', default = 'TU') }
grp_names <- c(prod = 'Non-manufacturing production', manuf = 'Manufacturing', whole = 'Wholesale and motor trades', retail = 'Retail',
               transp = 'Transport, storage, accommodation and food', info = 'Information and communication', fin = 'Finance and insurance',
               prof = 'Professional, scientific and technical', realprof = 'Real estate, professional and technical', other = 'Other services')
group_of <- function(s, irt) { sec <- sections(s)    # irt: the interregional groups (L with M, no K); otherwise the subnational ones (K and M apart, L with other)
  fcase(sec %in% c('A', 'B', 'D', 'E', 'F'), 'prod', sec == 'C', 'manuf', s %in% c('45', '46'), 'whole', s == '47', 'retail', sec %in% c('H', 'I'), 'transp', sec == 'J', 'info',
        sec == 'K', 'fin', sec == 'M', if (irt) 'realprof' else 'prof', sec == 'L', if (irt) 'realprof' else 'other', default = 'other') }
region_names <- c(TLC = 'North East', TLD = 'North West', TLE = 'Yorkshire and the Humber', TLF = 'East Midlands', TLG = 'West Midlands', TLH = 'East of England',
                  TLI = 'London', TLJ = 'South East', TLK = 'South West', TLL = 'Wales', TLM = 'Scotland', TLN = 'Northern Ireland')
code_of <- function(nm) { nm <- sub('^Greater London$', 'London', sub('The Humber', 'the Humber', nm)); names(region_names)[match(nm, region_names)] }

# ---- 1. trade with other countries, 2023: ITL1 by industry group
intl <- rbindlist(lapply(c('goods', 'services'), function(k) {
  d <- as.data.table(suppressMessages(read_excel(get_file(files[[k]]), sheet = 'Table 1 ITL1 Industry', skip = if (k == 'goods') 4 else 5, col_types = 'text')))
  setnames(d, c('level', 'region', 'name', 'dir', 'prod', 'manuf', 'whole', 'retail', 'transp', 'info', 'fin', 'prof', 'other', 'unknown', 'all'))
  melt(d[region %in% names(region_names) & dir != 'Balance'], id.vars = c('region', 'dir'), measure.vars = c('prod', 'manuf', 'whole', 'retail', 'transp', 'info', 'fin', 'prof', 'other', 'all'),
       variable.name = 'group', value.name = 'gbp_m')[, .(region, group = as.character(group), kind = k, dir, gbp_bn = num(gbp_m) / 1e3)] }))

# ---- 2. trade with other UK regions, 2021: ITL1 to ITL1 by industry group, summed over the eleven partners
irt_pairs <- rbindlist(lapply(c(goods = 'Table 3 ITL1 Industry TiG', services = 'Table 6 ITL1 Industry TiS'), function(s) {
  d <- as.data.table(suppressMessages(read_excel(get_file(files$irt), sheet = s, skip = 7, col_types = 'text')))
  setnames(d, c('name', 'partner', 'dir', 'prod', 'manuf', 'whole', 'retail', 'transp', 'info', 'realprof', 'other', 'all'))
  melt(d[dir != 'Balance'], id.vars = c('name', 'partner', 'dir'), variable.name = 'group', value.name = 'gbp_m')[
    , .(region = code_of(name), partner = code_of(partner), group = as.character(group), dir, gbp_bn = num(gbp_m) / 1e3)] }), idcol = 'kind')
stopifnot(!anyNA(irt_pairs$region), !anyNA(irt_pairs$partner))
irt <- irt_pairs[group != 'all', .(gbp_bn = sum(gbp_bn, na.rm = TRUE), partners_missing = sum(is.na(gbp_bn))), by = .(kind, region, group, dir)]

# ---- 3. the payments: what each group in a region is paid by, and pays to, UK businesses, by where the other side is
pay_one <- function(here) {
  f <- file.path(local_dir, sprintf('held_%s.rds', here)); if (!file.exists(f)) { cat(here, ': no held values cached, skipped\n'); return(NULL) }
  x <- readRDS(f)[year %in% c(2021, 2023) & !payer_sic %in% c('00', not_industry) & !payee_sic %in% c('00', not_industry)]
  side <- function(sells) {                     # sells: the region's businesses are the payee; otherwise they are the payer
    d <- if (sells) x[payee_region == here, .(sic = payee_sic, other_sic = payer_sic, other_region = payer_region, year, v = value_held)]
         else x[payer_region == here, .(sic = payer_sic, other_sic = payee_sic, other_region = payee_region, year, v = value_held)]
    d <- d[sic != other_sic | other_region != here]                                           # payments within an industry inside the region are not on the page's links
    d[, `:=`(where = fifelse(other_region == here, 'inside', fifelse(other_region == 'unknown', 'no_region', 'rest_uk')), via = fifelse(other_sic %in% intermediary, 'intermediaries', 'industries'))]
    list(tot = rbind(d[, .(gbp_bn = sum(v) / 1e9), by = .(group = group_of(sic, TRUE), year, where, via)][, basis := 'irt'],
                     d[, .(gbp_bn = sum(v) / 1e9), by = .(group = group_of(sic, FALSE), year, where, via)][, basis := 'intl'])[, side := if (sells) 'sells' else 'buys'],
         by_partner = d[year == 2021 & where == 'rest_uk' & via == 'industries', .(gbp_bn = sum(v) / 1e9), by = .(group = group_of(sic, TRUE), partner = other_region)][, side := if (sells) 'sells' else 'buys']) }
  s <- side(TRUE); b <- side(FALSE)
  list(tot = rbind(s$tot, b$tot)[, region := here], by_partner = rbind(s$by_partner, b$by_partner)[, region := here])
}
pay_all <- lapply(names(region_names), function(r) { cat('reading', r, '\n'); pay_one(r) })
pay <- rbindlist(lapply(pay_all, `[[`, 'tot')); pay_partner <- rbindlist(lapply(pay_all, `[[`, 'by_partner'))
payw <- dcast(pay[via == 'industries'], region + basis + group + year + side ~ where, value.var = 'gbp_bn', fill = 0)
pay_int <- pay[via == 'intermediaries', .(via_intermediaries = sum(gbp_bn)), by = .(region, basis, group, year, side)]
payw <- merge(payw, pay_int, by = c('region', 'basis', 'group', 'year', 'side'), all.x = TRUE)[is.na(via_intermediaries), via_intermediaries := 0]

# ---- 4. side by side
# with other UK regions (2021): payments to and from industries in the rest of the UK, against ONS's interregional estimate
irt_w <- dcast(irt, region + group ~ kind + dir, value.var = 'gbp_bn')
a <- merge(dcast(payw[basis == 'irt' & year == 2021], region + group ~ side, value.var = c('inside', 'rest_uk')), irt_w, by = c('region', 'group'), all = TRUE)[order(region, match(group, names(grp_names)))]
a[, `:=`(ons_sells_rest_uk = goods_Exports + services_Exports, ons_buys_rest_uk = goods_Imports + services_Imports)]
# with other countries (2023): the same payments, against ONS's subnational trade
intl_w <- dcast(intl, region + group ~ kind + dir, value.var = 'gbp_bn')
b <- merge(dcast(payw[basis == 'intl' & year == 2023], region + group ~ side, value.var = c('inside', 'rest_uk')), intl_w[group != 'all'], by = c('region', 'group'), all = TRUE)[order(region, match(group, names(grp_names)))]
r2 <- function(v) round(v, 1)

cat(sprintf('\n== %s: trade with the rest of the UK, 2021, GBP billions a year ==\n', region_names[show]))
cat('Payments are to and from industries (intermediaries and payers of unknown industry left out), held values. ONS is the interregional estimate, goods and services.\n')
print(a[region == show, .(group = grp_names[group], pay_sells_inside = r2(inside_sells), pay_sells_rest_uk = r2(rest_uk_sells), ons_sells_goods = r2(goods_Exports), ons_sells_services = r2(services_Exports),
                         pay_buys_inside = r2(inside_buys), pay_buys_rest_uk = r2(rest_uk_buys), ons_buys_goods = r2(goods_Imports), ons_buys_services = r2(services_Imports))])
cat(sprintf('\n== %s: trade with other countries, 2023, GBP billions a year, beside the payments for the same groups ==\n', region_names[show]))
print(b[region == show, .(group = grp_names[group], exports_goods = r2(goods_Exports), exports_services = r2(services_Exports), imports_goods = r2(goods_Imports), imports_services = r2(services_Imports),
                         pay_sells_uk = r2(inside_sells + rest_uk_sells), pay_buys_uk = r2(inside_buys + rest_uk_buys))])
cat('NA: hidden by ONS. Finance has no interregional estimate.\n')

# Where do the goods a group brings into the region come from, and where do those it sends out go? Official figures only:
# other countries (2023) against other UK regions (2021). Two different years and two different methods, so a rough share.
e <- merge(irt_w[, .(region, group, uk_in = goods_Imports, uk_out = goods_Exports)], intl_w[, .(region, group, abroad_in = goods_Imports, abroad_out = goods_Exports)], by = c('region', 'group'))
e[, `:=`(in_from_abroad = abroad_in / (abroad_in + uk_in), out_to_abroad = abroad_out / (abroad_out + uk_out))]
cat(sprintf('\n== %s: goods crossing the region\'s edge, by the group that buys or sells them. Share that is with other countries ==\n', region_names[show]))
print(e[region == show & group %in% c('prod', 'manuf', 'whole', 'retail'), .(group = grp_names[group], in_from_uk = r2(uk_in), in_from_abroad = r2(abroad_in), share_abroad_in = sprintf('%.0f%%', 100 * in_from_abroad),
                                                                          out_to_uk = r2(uk_out), out_to_abroad = r2(abroad_out), share_abroad_out = sprintf('%.0f%%', 100 * out_to_abroad))])
cat('\nWholesale and motor trades, every region: goods in from other countries as a share of all goods in from outside the region:\n')
print(e[group == 'whole', .(region, in_from_uk = r2(uk_in), in_from_abroad = r2(abroad_in), share_abroad_in = sprintf('%.0f%%', 100 * in_from_abroad))])

# ---- 5. do the payments and ONS's interregional estimates agree? Three checks that the size of a region or industry can't drive
cmp <- a[!is.na(ons_sells_rest_uk) & group != 'fin']
sp <- function(x, y) { ok <- is.finite(x) & is.finite(y); if (sum(ok) < 4) NA_real_ else cor(x[ok], y[ok], method = 'spearman') }
goods_groups <- c('prod', 'manuf', 'whole', 'retail'); eng <- setdiff(names(region_names), c('TLL', 'TLM', 'TLN'))
cat('\n== Payments against the interregional estimates, 2021 ==\n')
cat('How much of ONS\'s interregional trade the payments record, by group, all regions together (payments to and from industries in the rest of the UK, over the ONS estimate):\n')
print(cmp[, .(sells = sprintf('%.0f%%', 100 * sum(rest_uk_sells) / sum(ons_sells_rest_uk)), buys = sprintf('%.0f%%', 100 * sum(rest_uk_buys) / sum(ons_buys_rest_uk))), by = .(group = grp_names[group])])

# (a) the balance: does a group in a region sell more to the rest of the UK than it buys there, on both sources? (sells - buys) / (sells + buys)
cmp[, `:=`(bal_pay = (rest_uk_sells - rest_uk_buys) / (rest_uk_sells + rest_uk_buys), bal_goods = (goods_Exports - goods_Imports) / (goods_Exports + goods_Imports),
           bal_services = (services_Exports - services_Imports) / (services_Exports + services_Imports))]
cat('\n(a) Balance of trade with the rest of the UK, payments against ONS: rank correlation across the twelve regions, group by group.\n')
cat('    Goods are split by ONS using road freight, so that column is an independent check. Services are split using these payments in England, so that one is not.\n')
print(cmp[, .(with_ons_goods = sp(bal_pay, bal_goods), with_ons_services = sp(bal_pay, bal_services), same_sign_goods = sprintf('%d of %d', sum(sign(bal_pay) == sign(bal_goods), na.rm = TRUE), sum(is.finite(bal_goods)))),
          by = .(group = grp_names[group])], digits = 2)

# (b) the geography: of what a group in a region buys from (or sells to) the rest of the UK, the share with each of the eleven other regions
pp <- merge(pay_partner[partner %in% names(region_names)], irt_pairs[kind == 'goods' & group != 'all', .(region, partner, group, side = fifelse(dir == 'Exports', 'sells', 'buys'), ons_bn = gbp_bn)],
            by = c('region', 'partner', 'group', 'side'), all = TRUE)[is.na(gbp_bn), gbp_bn := 0]
pp[, `:=`(pay_share = gbp_bn / sum(gbp_bn), ons_share = ons_bn / sum(ons_bn, na.rm = TRUE)), by = .(region, group, side)]
geo <- pp[group %in% goods_groups, .(r = if (sum(is.finite(ons_share)) >= 8) cor(pay_share, ons_share, use = 'complete.obs') else NA_real_,
                                     london_pay = pay_share[partner == 'TLI'][1], london_ons = ons_share[partner == 'TLI'][1]), by = .(region, group, side)]
cat('\n(b) Which other regions a group trades goods with: correlation between the payments\' shares and ONS\'s (road freight) across the eleven partners. Median over regions, English regions only:\n')
print(geo[region %in% eng, .(median_r = median(r, na.rm = TRUE), lowest = min(r, na.rm = TRUE), regions = sum(is.finite(r))), by = .(group = grp_names[group], side)][order(group, side)], digits = 2)
cat('\n    London\'s share of each group\'s rest-of-UK trade, median over the eight other English regions: payments against ONS.\n')
print(geo[region %in% setdiff(eng, 'TLI'), .(london_in_payments = sprintf('%.0f%%', 100 * median(london_pay, na.rm = TRUE)), london_in_ons = sprintf('%.0f%%', 100 * median(london_ons, na.rm = TRUE))),
          by = .(group = grp_names[group], side)][order(group, side)])
cat(sprintf('\n    %s, wholesale and motor trades, where it buys goods from in the rest of the UK:\n', region_names[show]))
print(pp[region == show & group == 'whole' & side == 'buys', .(partner = region_names[partner], payments_bn = r2(gbp_bn), payments_share = sprintf('%.0f%%', 100 * pay_share), ons_bn = r2(ons_bn), ons_share = sprintf('%.0f%%', 100 * ons_share))][order(-ons_bn)])

# ---- 6. write every region
res <- merge(a[, .(region, group, pay_sells_inside_2021 = inside_sells, pay_sells_rest_uk_2021 = rest_uk_sells, pay_buys_inside_2021 = inside_buys, pay_buys_rest_uk_2021 = rest_uk_buys,
                   ons_rest_uk_sells_goods_2021 = goods_Exports, ons_rest_uk_sells_services_2021 = services_Exports, ons_rest_uk_buys_goods_2021 = goods_Imports, ons_rest_uk_buys_services_2021 = services_Imports)],
             b[, .(region, group, pay_sells_uk_2023 = inside_sells + rest_uk_sells, pay_buys_uk_2023 = inside_buys + rest_uk_buys,
                   exports_goods_2023 = goods_Exports, exports_services_2023 = services_Exports, imports_goods_2023 = goods_Imports, imports_services_2023 = services_Imports)], by = c('region', 'group'), all = TRUE)
res[, group_name := grp_names[group]]; numc <- setdiff(names(res), c('region', 'group', 'group_name')); res[, (numc) := lapply(.SD, function(v) signif(v, 4)), .SDcols = numc]
setcolorder(res, c('region', 'group', 'group_name')); fwrite(res[order(region, group)], file.path(out, 'trade_at_the_edge.csv'))
cat('\nGBP billions a year. The interregional groups put real estate with professional services; the subnational groups keep professional services apart and put real estate with other services. Wrote trade_at_the_edge.csv\n')
