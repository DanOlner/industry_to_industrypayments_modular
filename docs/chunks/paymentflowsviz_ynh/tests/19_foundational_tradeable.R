# 19_foundational_tradeable.R
# Do the foundational economy zones (test 18) line up with tradeable against non-tradeable?
# "Tradeable" is taken three ways, because the payments can only see part of it:
#   A. Payments: the share of an industry's receipts that comes from outside its region, set against the other
#      industries in the same region. All twelve regions. This is test 05's "own-region share of income", turned
#      round and repeated for every region. It sees payments between UK businesses only: no households, no
#      exports abroad, and firms are placed where they are registered.
#   B. Jobs: how unevenly an industry's jobs are spread across 179 areas, against all jobs. Industries that serve
#      local customers are spread like people are; industries that sell elsewhere bunch up. A standard stand-in.
#   C. The UK input-output table (2019): what share of each product goes abroad, to households, to government
#      and to other businesses. From whole_economy_thinking's tradeable jobs work (t2_products.csv).
# Run from the repo root, after 18: Rscript docs/chunks/paymentflowsviz_ynh/tests/19_foundational_tradeable.R  (about a minute)
source('docs/chunks/paymentflowsviz_ynh/tests/functions_hierarchy.R')
out <- file.path(chunk_dir, 'outputs')
regs <- c(TLC = 'North East', TLD = 'North West', TLE = 'Yorkshire and the Humber', TLF = 'East Midlands', TLG = 'West Midlands',
          TLH = 'East of England', TLI = 'London', TLJ = 'South East', TLK = 'South West', TLL = 'Wales', TLM = 'Scotland', TLN = 'Northern Ireland')
floor_month <- 0.5e6        # an industry needs this much in receipts a month in a region to be ranked there
min_regions <- 6            # and to be ranked in at least this many regions to get a pooled score
zones <- c('Material', 'Providential', 'Overlooked', 'Other'); zcol <- tolower(zones)
bres_itl3 <- '~/Code/RegionalEconomicTools/local/data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE428_internationalterritoriallevelslevel3asofJan2021_4_Employment_2022_2023_SIC_2DIGIT.csv'
io_products <- '~/Code/claude/whole_economy_thinking/docs/chunks/tradeable_jobs/outputs/t2_products.csv'

fe <- fread(file.path(out, 'foundational_division_shares.csv'), colClasses = list(character = 'sic2'))
drop <- c('00', not_industry, intermediary)
spear <- function(a, b) round(cor(a, b, method = 'spearman', use = 'pairwise.complete.obs'), 2)

# ---- A. Payments: outside share of receipts and of spending, every region -------------------------------------
pay <- rbindlist(lapply(names(regs), function(r) {
  x <- readRDS(file.path(local_dir, sprintf('held_%s.rds', r)))
  x <- x[!payer_sic %in% drop & !payee_sic %in% drop & payer_region != 'unknown' & payee_region != 'unknown']
  s <- x[payee_region == r, .(sold_in = sum(value_held[payer_region == r]), sold_out = sum(value_held[payer_region != r]),
                              sold_in_pub = sum(value[payer_region == r], na.rm = TRUE), sold_out_pub = sum(value[payer_region != r], na.rm = TRUE)),
         by = .(sic2 = payee_sic)]
  b <- x[payer_region == r, .(bought_in = sum(value_held[payee_region == r]), bought_out = sum(value_held[payee_region != r])), by = .(sic2 = payer_sic)]
  m <- merge(s, b, by = 'sic2', all = TRUE)
  for (k in names(m)[-1]) set(m, which(is.na(m[[k]])), k, 0)
  m[, `:=`(region = r, months = uniqueN(x$date))][]
}))
pay[, `:=`(out_sales = sold_out / (sold_in + sold_out), out_buys = bought_out / (bought_in + bought_out),
           out_sales_pub = sold_out_pub / (sold_in_pub + sold_out_pub), ranked = (sold_in + sold_out) / months >= floor_month)]
base <- pay[, .(base_sales = sum(sold_out) / sum(sold_in + sold_out), base_buys = sum(bought_out) / sum(bought_in + bought_out),
                held_share_in = 1 - sum(sold_in_pub) / sum(sold_in), held_share_out = 1 - sum(sold_out_pub) / sum(sold_out)), by = region]
pay <- merge(pay, base[, .(region, base_sales, base_buys)], by = 'region')
pay[, `:=`(rel_sales = out_sales - base_sales, rel_buys = out_buys - base_buys)]
pay[ranked == TRUE, pct := (frank(out_sales) - 0.5) / .N, by = region]       # place among the region's industries, 0 to 1

cat('\nA. Payments. Share of receipts from outside the region, all industries together\n')
print(base[, .(region = regs[region], outside_share_of_receipts = round(base_sales, 3), outside_share_of_spending = round(base_buys, 3),
               inside_value_held = round(held_share_in, 3), outside_value_held = round(held_share_out, 3))])

# check against test 05 (Yorkshire and the Humber, own-region share of income)
t05 <- fread(file.path(out, 'external_by_division.csv'), colClasses = list(character = 'sic2'))
chk <- merge(pay[region == 'TLE', .(sic2, here = 1 - out_sales)], t05[, .(sic2, test05 = own_share_income)], by = 'sic2')[!is.na(test05) & !is.na(here)]
cat(sprintf('\nCheck against test 05, Y&H own-region share of income: %d divisions, correlation %.4f, largest difference %.3f\n',
            nrow(chk), cor(chk$here, chk$test05), max(abs(chk$here - chk$test05))))

# how much spread is there to work with?
cat('\nSpread across ranked industries of the outside share of receipts (the lower the spread, the less the measure can separate)\n')
print(pay[ranked == TRUE, .(industries = .N, p10 = round(quantile(out_sales, 0.1), 2), median = round(median(out_sales), 2),
                            p90 = round(quantile(out_sales, 0.9), 2)), by = .(region = regs[region])])

# is an industry's place the same from region to region? (if tradeability belongs to the industry, it should be)
wide <- dcast(pay[ranked == TRUE], sic2 ~ region, value.var = 'pct')
cm <- cor(as.matrix(wide[, -1]), method = 'spearman', use = 'pairwise.complete.obs')
cat(sprintf('\nSame industry, different regions: rank correlation of its place, averaged over the 66 pairs of regions: %.2f (lowest pair %.2f, highest %.2f)\n',
            mean(cm[upper.tri(cm)]), min(cm[upper.tri(cm)]), max(cm[upper.tri(cm)])))
cat('Each region against the other eleven, on average:\n'); print(round(sort((colSums(cm) - 1) / (ncol(cm) - 1)), 2))
pubm <- pay[ranked == TRUE & is.finite(out_sales_pub)]
cat(sprintf('Held against published-only values, outside share of receipts: rank correlation %.2f\n', spear(pubm$out_sales, pubm$out_sales_pub)))

# one score for each division: its average place across the regions where it is ranked
div <- pay[ranked == TRUE, .(regions = .N, place = mean(pct), place_sd = sd(pct), rel_sales = mean(rel_sales), rel_buys = mean(rel_buys),
                             out_sales_ynh = out_sales[region == 'TLE'][1]), by = sic2][regions >= min_regions]
third <- function(v) cut(frank(v) / length(v), c(0, 1 / 3, 2 / 3, 1), labels = c('low', 'middle', 'high'))
div[, pay_third := third(place)]

# ---- B. Jobs: how unevenly each division is spread across areas -----------------------------------------------
b <- fread(bres_itl3, colClasses = list(character = 'SIC_2DIGIT_CODE'))[DATE == max(DATE)]
b <- b[, .(jobs = sum(JOBCOUNT, na.rm = TRUE)), by = .(area = GEOGRAPHY_CODE, sic2 = SIC_2DIGIT_CODE)]
b[, all_share := sum(jobs) / sum(b$jobs), by = area]
conc <- b[, .(jobs_gb = sum(jobs), concentration = 0.5 * sum(abs(jobs / sum(jobs) - all_share))), by = sic2][jobs_gb > 0]
cat(sprintf('\nB. Jobs spread: %d areas. 0 = spread exactly like all jobs, 1 = all in one small place.\n', uniqueN(b$area)))

# ---- C. Input-output table: where each product goes -----------------------------------------------------------
io <- fread(io_products)[group != 'excluded']
io <- io[, .(sic2 = unique(regmatches(cpa, gregexpr('(?<=[A-Z])[0-9]{2}', cpa, perl = TRUE))[[1]])),
         by = .(cpa, output, export_share, household_share, government_share, b2b_share, investment_share)]
io <- io[, .(export_share = weighted.mean(export_share, output), household_share = weighted.mean(household_share, output),
             government_share = weighted.mean(government_share, output), b2b_share = weighted.mean(b2b_share, output),
             investment_share = weighted.mean(investment_share, output)), by = sic2]
io[, local_final := household_share + government_share]

# ---- Put together by division -----------------------------------------------------------------------------------
d <- merge(fe[, .(sic2, short, section, role, material, providential, overlooked, other, main, fit)], div, by = 'sic2', all.x = TRUE)
d <- merge(d, conc, by = 'sic2', all.x = TRUE)
d <- merge(d, io, by = 'sic2', all.x = TRUE)
d <- d[role == 'industry']
d[!is.na(concentration), conc_third := third(concentration)]
d[!is.na(export_share), export_third := third(export_share)]
fwrite(d[, .(sic2, short, section, main, fit, material, providential, overlooked, other, regions_ranked = regions, pay_place = round(place, 3),
             pay_place_sd = round(place_sd, 3), pay_third, outside_receipts_vs_region = round(rel_sales, 3), outside_spending_vs_region = round(rel_buys, 3),
             outside_receipts_ynh = round(out_sales_ynh, 3), jobs_gb, concentration = round(concentration, 3), conc_third,
             export_share = round(export_share, 3), household_share = round(household_share, 3), government_share = round(government_share, 3),
             b2b_share = round(b2b_share, 3), export_third)], file.path(out, 'foundational_tradeable_divisions.csv'))

cat('\nDo the three measures agree with each other? Rank correlations across divisions\n')
ms <- d[, .(payments_place = place, jobs_concentration = concentration, exports_abroad = export_share, to_households_and_government = local_final, to_other_businesses = b2b_share)]
print(round(cor(ms, method = 'spearman', use = 'pairwise.complete.obs'), 2))

# ---- The zones against each measure -----------------------------------------------------------------------------
# jobs in each zone inside each division (Great Britain), so a split division counts in each of its zones
fe5 <- fread(file.path(chunk_dir, 'lookups/foundational_sic5.csv'), colClasses = 'character')
j <- merge(readRDS(file.path(local_dir, 'bres_itl1_sic5.rds'))[, .(jobs = sum(jobs)), by = sic5], fe5[, .(sic5, zone)], by = 'sic5', all.x = TRUE)
j[sic5 == '01000', zone := 'Material']
jz <- j[, .(jobs = sum(jobs)), by = .(sic2 = substr(sic5, 1, 2), zone)]
jz <- merge(jz, d[, .(sic2, place, pay_third, concentration, conc_third, export_share, export_third, local_final, b2b_share, rel_sales, rel_buys)], by = 'sic2')
wm <- function(v, w) { ok <- !is.na(v); round(sum(v[ok] * w[ok]) / sum(w[ok]), 3) }
zsum <- jz[, .(jobs_m = round(sum(jobs) / 1e6, 2),
               payments_place = wm(place, jobs), outside_receipts_vs_region = wm(rel_sales, jobs), outside_spending_vs_region = wm(rel_buys, jobs),
               jobs_concentration = wm(concentration, jobs), exports_abroad = wm(export_share, jobs),
               to_households_and_government = wm(local_final, jobs), to_other_businesses = wm(b2b_share, jobs),
               jobs_in_export_quarter = round(sum(jobs[!is.na(export_share) & export_share >= 0.25]) / sum(jobs[!is.na(export_share)]), 3)), by = zone][match(zones, zone)]
cat('\nEach zone, averaged over its jobs (payments place: 0 = sells most locally of a region\'s industries, 1 = most outside)\n'); print(zsum)
fwrite(zsum, file.path(out, 'foundational_tradeable_zones.csv'))

tab <- function(col, title) {
  x <- jz[!is.na(get(col)), .(jobs = sum(jobs)), by = c('zone', col)]
  x[, pct := round(100 * jobs / sum(jobs)), by = zone]
  w <- dcast(x, zone ~ get(col), value.var = 'pct', fill = 0)[match(zones, zone)]
  cat('\n', title, '\n', sep = ''); print(w)
}
tab('pay_third', 'Share of each zone\'s jobs by thirds of divisions, A. payments (low = sells locally more than most, high = outside more than most)')
tab('conc_third', 'B. jobs concentration (low = spread like the population, high = bunched)')
tab('export_third', 'C. exports abroad as a share of the product\'s use')

# the same from the money: each zone's outside share of receipts, region by region (division receipts split by zone shares)
pz <- merge(pay, fe[, .(sic2, material, providential, overlooked, other)], by = 'sic2')
zr <- rbindlist(lapply(zcol, function(z) pz[, .(zone = z, out_sales = sum(sold_out * get(z)) / sum((sold_in + sold_out) * get(z)),
                                                out_buys = sum(bought_out * get(z)) / sum((bought_in + bought_out) * get(z))), by = region]))
zr <- merge(zr, base[, .(region, base_sales, base_buys)], by = 'region')
cat('\nOutside share of receipts by zone, value-weighted, points above or below the region\'s all-industry share\n')
print(dcast(zr[, .(region = regs[region], zone, v = round(100 * (out_sales - base_sales), 1))], region ~ zone, value.var = 'v')[, c('region', zcol), with = FALSE])
cat('\nOutside share of spending by zone, the same way\n')
print(dcast(zr[, .(region = regs[region], zone, v = round(100 * (out_buys - base_buys), 1))], region ~ zone, value.var = 'v')[, c('region', zcol), with = FALSE])
fwrite(zr, file.path(out, 'foundational_tradeable_zone_by_region.csv'))

# ---- Where the two ideas part company -----------------------------------------------------------------------------
show <- function(x) print(x[, .(sic2, short, zone = main, fit, pay_place = round(place, 2), spread = round(place_sd, 2), jobs_conc = round(concentration, 2),
                                exports = round(export_share, 2), hh_gov = round(local_final, 2), jobs_k = round(jobs_gb / 1000))], nrows = 60)
cat('\nFoundational (material or providential, not split) divisions, most outside-selling first on the payments\n')
show(d[main %in% c('Material', 'Providential') & fit != 'split' & !is.na(place)][order(-place)])
cat('\nOverlooked divisions (not split)\n'); show(d[main == 'Overlooked' & fit != 'split' & !is.na(place)][order(-place)])
cat('\n"Other" divisions (not split), most local-selling first on the payments\n'); show(d[main == 'Other' & fit != 'split' & !is.na(place)][order(place)])
cat('\nSplit divisions\n'); show(d[fit == 'split' & !is.na(place)][order(-place)])
cat('\nNot ranked on the payments (too small in too many regions):', paste(d[is.na(place), paste(sic2, short)], collapse = '; '), '\n')

# ---- What measure A leaves out (Yorkshire and the Humber, published values) ---------------------------------------
# The data has the twelve regions plus "unknown". There is no category for abroad. Rows with an unknown region or
# an unknown industry (00) are dropped by the page and by A above. How big are they?
m <- readRDS(file.path(local_dir, 'monthly_TLE.rds'))
cat('\nRegion categories in the data:', paste(sort(unique(c(m$payer_region, m$payee_region))), collapse = ' '), '\n')
bn <- function(v) round(sum(v, na.rm = TRUE) / 1e9, 2)
src <- function(sic, reg) fifelse(sic == '00', 'payer of unknown industry', fifelse(reg == 'unknown', fifelse(sic == '84', 'public administration, no region', 'other payer, no region'),
       fifelse(sic %in% intermediary, 'intermediaries', fifelse(sic %in% not_industry, 'not an industry', fifelse(reg == 'TLE', 'industries in the region', 'industries in other regions')))))
r <- m[payee_region == 'TLE' & !payee_sic %in% drop][, source := src(payer_sic, payer_region)]
rs <- r[, .(bn = bn(value)), by = source][order(-bn)][, pct := round(100 * bn / sum(bn), 1)]
cat('\nReceipts of Y&H industries by who paid, GBP bn, Jan 2019 to Sep 2026\n'); print(rs)
cat(sprintf('Measure A uses the two "industries" rows: %.0f%% of receipts.\n', rs[source %like% '^industries', sum(pct)]))
cat('\nUnknown region, by industry of the side with no region (all rows touching Y&H, GBP bn)\n')
u <- rbind(m[payer_region == 'unknown', .(sic = payer_sic, value)], m[payee_region == 'unknown', .(sic = payee_sic, value)])
print(u[, .(bn = bn(value)), by = sic][order(-bn)][1:4])
cat(sprintf('\nPublic administration paying Y&H: with no region %.2f bn, with a region %.2f bn. Y&H paying public administration with no region: %.2f bn\n',
            bn(m[payer_sic == '84' & payer_region == 'unknown' & payee_region == 'TLE', value]), bn(m[payer_sic == '84' & payer_region != 'unknown' & payee_region == 'TLE', value]),
            bn(m[payee_sic == '84' & payee_region == 'unknown' & payer_region == 'TLE', value])))
cat('\nReceipts of Y&H\'s public-service divisions by who paid (GBP bn)\n')
print(dcast(r[payee_sic %in% c('85', '86', '87', '88'), .(bn = bn(value)), by = .(division = payee_sic, source)], source ~ division, value.var = 'bn', fill = 0)[, total := `85` + `86` + `87` + `88`][order(-total)])
fwrite(rs, file.path(out, 'foundational_tradeable_receipts_by_source_ynh.csv'))
cat('\nDone.\n')
