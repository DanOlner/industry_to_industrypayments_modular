# 18_foundational_divisions.R
# Can the foundational economy be shown on the payments page, which only has two-digit SIC divisions?
# The Foundational Economy Collective's list (read by 17) classes five-digit SIC codes as material, providential,
# overlooked or other. This weights that list by jobs to see, for each division:
#   - what share of its jobs falls in each zone;
#   - whether the division fits one zone (whole, most) or is split;
# and then: how much is lost by working at division level, what each region's zone shares are, and where the
# zones sit in Yorkshire and the Humber's payments.
# Jobs: BRES employment 2023 by five-digit SIC, Great Britain (the table the build already caches).
# Run from the repo root, after 17: Rscript docs/chunks/paymentflowsviz_ynh/tests/18_foundational_divisions.R
source('docs/chunks/paymentflowsviz_ynh/tests/functions_hierarchy.R')
out <- file.path(chunk_dir, 'outputs')
zones <- c('Material', 'Providential', 'Overlooked', 'Other')
whole_at <- 0.9; most_at <- 2 / 3            # share of a division's jobs in one zone to call it whole, or most

fe <- fread(file.path(chunk_dir, 'lookups/foundational_sic5.csv'), colClasses = 'character')
j <- readRDS(file.path(local_dir, 'bres_itl1_sic5.rds'))
j <- merge(j, fe[, .(sic5, zone)], by = 'sic5', all.x = TRUE)
j[sic5 == '01000', zone := 'Material']        # BRES's farm total; division 01 is material in the list bar one code
stopifnot(j[is.na(zone) & jobs > 0, .N] == 0)
j[, sic2 := substr(sic5, 1, 2)]

# ---- 1. Great Britain: jobs in each zone, to compare with the Collective's own figures ----------------------
gb <- j[, .(jobs = sum(jobs)), by = zone][, share := jobs / sum(jobs)][match(zones, zone)]
cat('\nGreat Britain, BRES 2023, share of jobs by zone\n'); print(gb[, .(zone, jobs, pct = round(100 * share, 1))])
cat(sprintf('Material and providential: %.1f%%. With overlooked: %.1f%%.\n',
            100 * gb[zone %in% zones[1:2], sum(share)], 100 * gb[zone != 'Other', sum(share)]))

# ---- 2. Each division: share of jobs in each zone ------------------------------------------------------------
d <- dcast(j[, .(jobs = sum(jobs)), by = .(sic2, zone)], sic2 ~ zone, value.var = 'jobs', fill = 0)
for (z in zones) if (!z %in% names(d)) d[, (z) := 0]
d[, jobs := Material + Providential + Overlooked + Other]
sh <- as.matrix(d[, ..zones]) / d$jobs
d[, main := zones[max.col(sh, ties.method = 'first')]]
d[, main_share := apply(sh, 1, max)]
d[, fit := fifelse(main_share >= whole_at, 'whole', fifelse(main_share >= most_at, 'most', 'split'))]
d[, `:=`(material = round(sh[, 1], 3), providential = round(sh[, 2], 3), overlooked = round(sh[, 3], 3), other = round(sh[, 4], 3))]
codes <- fe[, .(n_codes = .N, n_zones = uniqueN(zone)), by = .(sic2 = substr(sic5, 1, 2))]
d <- merge(d, codes, by = 'sic2', all.x = TRUE)
d <- merge(sic2_lk[, .(sic2, short, section, role)], d, by = 'sic2', all.x = TRUE)
d <- d[!is.na(jobs) & jobs > 0]

# the largest five-digit codes that go against a division's main zone: what a division label would mislabel
against <- merge(j[, .(jobs = sum(jobs)), by = .(sic2, sic5, zone)], d[, .(sic2, main)], by = 'sic2')[zone != main & jobs > 0]
against <- merge(against, fe[, .(sic5, description)], by = 'sic5', all.x = TRUE)
top_against <- against[order(-jobs), .(against = paste(sprintf('%s %s (%s, %s)', sic5, substr(description, 1, 40), zone,
                                                              format(jobs, big.mark = ',')), collapse = '; ')), by = sic2]
top3 <- against[order(-jobs), head(.SD, 3), by = sic2][, .(largest_against = paste(sprintf('%s %s [%s]', sic5, substr(description, 1, 45), zone), collapse = '; ')), by = sic2]
d <- merge(d, top3, by = 'sic2', all.x = TRUE)
fwrite(d[, .(sic2, short, section, role, jobs, material, providential, overlooked, other, main, main_share = round(main_share, 3), fit, largest_against)],
       file.path(out, 'foundational_division_shares.csv'))

cat('\nDivisions by fit (industries in the payments data only)\n')
ind <- d[role %in% c('industry', 'intermediary')]
print(dcast(ind[, .N, by = .(main, fit)], main ~ fit, value.var = 'N', fill = 0))
cat('\nShare of jobs by fit:\n'); print(ind[, .(divisions = .N, jobs_pct = round(100 * sum(jobs) / sum(ind$jobs), 1)), by = fit])

show <- function(x) print(x[, .(sic2, short, jobs = round(jobs / 1000), mat = round(100 * material), prov = round(100 * providential),
                                over = round(100 * overlooked), oth = round(100 * other), main, fit)], nrows = 100)
cat('\nWhole or most: foundational or overlooked\n'); show(ind[fit != 'split' & main != 'Other'][order(main, sic2)])
cat('\nSplit divisions\n'); show(ind[fit == 'split'][order(sic2)])
cat('\nOther, but with some foundational or overlooked jobs (5% or more)\n'); show(ind[fit != 'split' & main == 'Other' & other < 0.95][order(sic2)])
cat('\nLargest codes against the main zone, split divisions\n'); print(ind[fit == 'split', .(sic2, short, largest_against)], nrows = 50)

# ---- 3. What is lost at division level -----------------------------------------------------------------------
# If each division took its main zone, what share of jobs would carry the right label?
right <- ind[, sum(jobs * main_share)] / ind[, sum(jobs)]
cat(sprintf('\nLabel each division by its main zone: %.1f%% of jobs get the right zone.\n', 100 * right))
by_zone <- merge(ind[, .(labelled = sum(jobs)), by = .(zone = main)],
                 j[sic2 %in% ind$sic2, .(actual = sum(jobs)), by = zone], by = 'zone', all = TRUE)
by_zone[, `:=`(labelled_pct = round(100 * labelled / sum(labelled, na.rm = TRUE), 1), actual_pct = round(100 * actual / sum(actual), 1))]
cat('Jobs by zone: labelled by division against actual\n'); print(by_zone[match(zones, zone)])
# and with shares carried: each division's jobs split by its Great Britain shares, applied to each region
reg <- j[, .(jobs = sum(jobs)), by = .(itl1, sic2, zone)]
actual <- dcast(reg[, .(jobs = sum(jobs)), by = .(itl1, zone)][, share := jobs / sum(jobs), by = itl1], itl1 ~ zone, value.var = 'share')
div_reg <- reg[, .(jobs = sum(jobs)), by = .(itl1, sic2)]
est <- merge(div_reg, d[, .(sic2, material, providential, overlooked, other)], by = 'sic2')
est <- est[, .(Material = sum(jobs * material), Providential = sum(jobs * providential), Overlooked = sum(jobs * overlooked), Other = sum(jobs * other)), by = itl1]
est[, tot := Material + Providential + Overlooked + Other]
cmp <- merge(actual, est[, .(itl1, est_mat = Material / tot, est_prov = Providential / tot, est_over = Overlooked / tot)], by = 'itl1')
cat('\nRegions: share of jobs by zone (five-digit), and the estimate from division shares\n')
print(cmp[, .(itl1, material = round(100 * Material, 1), est = round(100 * est_mat, 1), providential = round(100 * Providential, 1), est_p = round(100 * est_prov, 1),
              overlooked = round(100 * Overlooked, 1), est_o = round(100 * est_over, 1), other = round(100 * Other, 1))])
fwrite(cmp, file.path(out, 'foundational_region_shares.csv'))

# one label for each division, but none for the split ones
lab <- ind[fit != 'split']
cat(sprintf('Leave the %d split divisions unlabelled: %.1f%% of jobs are in a labelled division, and %.1f%% of those get the right zone.\n',
            ind[fit == 'split', .N], 100 * sum(lab$jobs) / sum(ind$jobs), 100 * lab[, sum(jobs * main_share)] / sum(lab$jobs)))

# ---- 3b. Short cuts by whole division or section, measured against the list -----------------------------------
# "foundational" here = material plus providential. For each short cut: how many of its jobs the list calls
# foundational (or foundational plus overlooked), and how much of the list's foundational jobs it catches.
div <- function(...) sprintf('%02d', c(...))
cuts <- list(
  'Handover segment (retail, transport, hospitality, public services)' = div(47, 49:53, 55:56, 84:88),
  'Tradeable jobs typology: material foundational plus providential' = div(1:3, 10:12, 35:39, 41:43, 45:46, 49:53, 84:88),
  'Tradeable jobs typology, with everyday services' = div(1:3, 10:12, 35:39, 41:43, 45:46, 49:53, 84:88, 47, 55:56, 90:96),
  'Bevan Foundation subset (retail, hospitality, health, care)' = div(47, 55, 56, 86:88),
  'This list, divisions whose main zone is material or providential' = ind[main %in% c('Material', 'Providential'), sic2],
  'This list, the same but leaving out split divisions' = ind[main %in% c('Material', 'Providential') & fit != 'split', sic2])
jj <- j[sic2 %in% ind$sic2]
fe_jobs <- jj[zone %in% c('Material', 'Providential'), sum(jobs)]
cmp_cuts <- rbindlist(lapply(names(cuts), function(nm) {
  a <- jj[sic2 %in% cuts[[nm]]]
  data.table(short_cut = nm, divisions = length(cuts[[nm]]), pct_of_all_jobs = round(100 * sum(a$jobs) / sum(jj$jobs), 1),
             of_which_foundational = round(100 * a[zone %in% c('Material', 'Providential'), sum(jobs)] / sum(a$jobs), 1),
             of_which_with_overlooked = round(100 * a[zone != 'Other', sum(jobs)] / sum(a$jobs), 1),
             foundational_jobs_caught = round(100 * a[zone %in% c('Material', 'Providential'), sum(jobs)] / fe_jobs, 1))
}))
cat('\nShort cuts against the list (Great Britain jobs)\n'); print(cmp_cuts)
fwrite(cmp_cuts, file.path(out, 'foundational_short_cuts.csv'))

# ---- 4. Yorkshire and the Humber: the zones in the payments ---------------------------------------------------
x <- readRDS(file.path(local_dir, 'held_TLE.rds'))
x <- x[payer_region == 'TLE' & payee_region == 'TLE' & payer_sic != payee_sic]
x <- x[payer_sic %in% ind$sic2 & payee_sic %in% ind$sic2]
core <- sic2_lk[role == 'industry', sic2]
xc <- x[payer_sic %in% core & payee_sic %in% core]
zs <- d[, .(sic2, material, providential, overlooked, other)]
# divisions with no jobs recorded take their shares from the count of codes in the list
nojobs <- setdiff(core, zs$sic2)
if (length(nojobs)) {
  cc <- dcast(fe[substr(sic5, 1, 2) %in% nojobs, .N, by = .(sic2 = substr(sic5, 1, 2), zone)], sic2 ~ zone, value.var = 'N', fill = 0)
  for (z in zones) if (!z %in% names(cc)) cc[, (z) := 0]
  n <- cc$Material + cc$Providential + cc$Overlooked + cc$Other
  zs <- rbind(zs, cc[, .(sic2, material = Material / n, providential = Providential / n, overlooked = Overlooked / n, other = Other / n)])
  cat('\nNo jobs recorded, shares from the code list:', paste(nojobs, collapse = ', '), '\n')
}
flows <- xc[, .(v = sum(value_held, na.rm = TRUE)), by = .(payer_sic, payee_sic)]
paid <- merge(flows[, .(paid = sum(v)), by = .(sic2 = payer_sic)], flows[, .(got = sum(v)), by = .(sic2 = payee_sic)], by = 'sic2', all = TRUE)
paid[is.na(paid), paid := 0]; paid[is.na(got), got := 0]
paid <- merge(paid, zs, by = 'sic2')
tot <- function(col) sapply(c('material', 'providential', 'overlooked', 'other'), function(z) sum(paid[[col]] * paid[[z]]) / sum(paid[[col]]))
jobs_yh <- j[itl1 == 'TLE' & sic2 %in% core, .(jobs = sum(jobs)), by = zone][, share := jobs / sum(jobs)][match(zones, zone)]
cat('\nYorkshire and the Humber, 77 industries: share by zone (payments split by each division\'s job shares)\n')
print(data.table(zone = zones, jobs_pct = round(100 * jobs_yh$share, 1), payments_made_pct = round(100 * tot('paid'), 1), payments_received_pct = round(100 * tot('got'), 1)))

# who pays whom, zone to zone (rows pay columns), share of all inside payments
zm <- as.matrix(zs[match(core, sic2), .(material, providential, overlooked, other)]); rownames(zm) <- core
Wm <- flow_matrix(xc, 'value_held', core); diag(Wm) <- 0      # W[supplier, buyer]
Z <- t(zm) %*% t(Wm) %*% zm                                    # rows: paying zone, columns: paid zone
dimnames(Z) <- list(pays = zones, paid = zones)
cat('\nZone to zone, % of inside payments (rows pay columns)\n'); print(round(100 * Z / sum(Z), 1))
cat('Of what each zone pays out inside the region, the share going to each zone (rows sum to 100)\n'); print(round(100 * Z / rowSums(Z), 1))
fwrite(as.data.table(round(100 * Z / sum(Z), 2), keep.rownames = 'pays'), file.path(out, 'foundational_zone_to_zone_ynh.csv'))

# place in the order: the published levels, averaged by main zone, weighted by payments made plus received
lv <- trophic(Wm)
lev <- data.table(sic2 = core, level = lv$h - sum(lv$h * (rowSums(Wm) + colSums(Wm)), na.rm = TRUE) / sum(Wm) / 2)
lev <- merge(lev, merge(paid[, .(sic2, paid, got)], d[, .(sic2, short, main, fit)], by = 'sic2'), by = 'sic2')
cat('\nPlace in the supplier-to-buyer order by main zone (negative = supplier side), weighted by payments\n')
lev <- lev[!is.na(level)]
print(lev[, .(divisions = .N, level = round(weighted.mean(level, paid + got), 2), lowest = short[which.min(level)], highest = short[which.max(level)]), by = main])
cat('\nProvidential and material divisions, place in the order\n')
print(lev[main %in% c('Providential', 'Material') & fit != 'split', .(sic2, short, main, level = round(level, 2), paid_m = round(paid / 1e6), got_m = round(got / 1e6))][order(main, level)], nrows = 40)

# who the providential divisions pay inside the region: what a "providential" highlight would light up
prov <- ind[main == 'Providential', sic2]
pp <- flows[payer_sic %in% prov & !payee_sic %in% prov, .(v = sum(v)), by = .(sic2 = payee_sic)][order(-v)]
pp <- merge(pp, d[, .(sic2, short, main, fit)], by = 'sic2')[order(-v)][, pct := round(100 * v / sum(v), 1)]
cat(sprintf('\nProvidential divisions pay %.0f%% of their inside payments to each other; the rest goes to:\n',
            100 * flows[payer_sic %in% prov & payee_sic %in% prov, sum(v)] / flows[payer_sic %in% prov, sum(v)]))
print(pp[1:12, .(sic2, short, zone = main, fit, pct)])
cat('\nDone.\n')
