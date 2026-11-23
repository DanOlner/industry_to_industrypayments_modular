# 16_region_similarity.R
# Scoping for "which regions are most alike": two measures on each ITL1 region's inside flows, and a check on
# whether the differences between regions are bigger than the differences within one region over time.
#   A. Overall similarity, sector makeup included: how much of one region's pattern of who-pays-whom would have
#      to move to match another's.
#   B. Chain or web: how strongly money runs one way down the order, and how widely each industry spreads its
#      payments across partners.
# Also, for the all-regions build: what each region's data are like, and whether Y&H's payment groups recur.
# Run from the repo root, after 14: Rscript docs/chunks/paymentflowsviz_ynh/tests/16_region_similarity.R  (about four minutes)
source('docs/chunks/paymentflowsviz_ynh/tests/functions_hierarchy.R')
suppressPackageStartupMessages(library(igraph))
set.seed(1)
out <- file.path(chunk_dir, 'outputs')
reg_names <- c(TLC = 'North East', TLD = 'North West', TLE = 'Yorkshire and the Humber', TLF = 'East Midlands', TLG = 'West Midlands',
               TLH = 'East of England', TLI = 'London', TLJ = 'South East', TLK = 'South West', TLL = 'Wales', TLM = 'Scotland', TLN = 'Northern Ireland')
short <- c(TLC = 'NE', TLD = 'NW', TLE = 'Y&H', TLF = 'E Mids', TLG = 'W Mids', TLH = 'East', TLI = 'London', TLJ = 'SE', TLK = 'SW', TLL = 'Wales', TLM = 'Scot', TLN = 'NI')

held_f <- file.path(local_dir, 'all_regions_own_held.rds')
if (!file.exists(held_f)) {
  x <- readRDS(file.path(local_dir, 'all_regions_own_monthly.rds'))
  x <- x[!payer_sic %in% c('00', not_industry) & !payee_sic %in% c('00', not_industry)]
  x <- hold_values(x, by = c('payer_sic', 'payer_region', 'payee_sic', 'payee_region'))
  saveRDS(x, held_f)
}
x <- readRDS(held_f)[payer_sic != payee_sic & !payer_sic %in% intermediary & !payee_sic %in% intermediary]
setnames(x, 'payer_region', 'region'); x[, payee_region := NULL]
regs <- names(reg_names); nodes <- sic2_lk[role == 'industry', sic2]; nm <- uniqueN(x$date)
W_of <- function(d) { W <- flow_matrix(d, 'value_held', nodes); diag(W) <- 0; W }
W <- lapply(setNames(regs, regs), function(r) W_of(x[region == r]))

# ---- 0. What each region's data are like ------------------------------------------------------------
io <- fread(file.path(out, 'levels_payments_vs_io.csv'), colClasses = list(character = 'g'))
ent <- function(p) { p <- p[p > 0]; exp(-sum(p * log(p))) }                       # "effective number": 1 if all in one place, n if spread evenly over n
spread <- function(M) {                                                           # how many partners an industry's money is in effect spread over
  buy <- colSums(M); sup <- rowSums(M)
  c(suppliers_per_buyer = weighted.mean(vapply(which(buy > 0), function(j) ent(M[, j] / buy[j]), 0), buy[buy > 0]),
    customers_per_supplier = weighted.mean(vapply(which(sup > 0), function(i) ent(M[i, ] / sup[i]), 0), sup[sup > 0]))
}
measures <- function(M) { tr <- trophic(M); sp <- spread(M); v <- sort(M[M > 0], decreasing = TRUE)
  c(down_share = tr$up_share, incoherence = tr$F0, one_way = net_share(M), suppliers_per_buyer = sp[[1]], customers_per_supplier = sp[[2]],
    links_for_80pct = sum(cumsum(v) / sum(v) < 0.8) + 1, effective_links = ent(M / sum(M))) }
stats <- rbindlist(lapply(regs, function(r) { d <- x[region == r]; tr <- trophic(W[[r]]); pm <- (rowSums(W[[r]]) + colSums(W[[r]])) / nm
  data.table(region = r, name = reg_names[[r]], inside_bn = round(sum(W[[r]]) / 1e9, 1), hidden = round(mean(d$suppressed), 3),
             never_published = round(mean(d$basis == 'never_published'), 3), held_adds = round(d[suppressed == TRUE, sum(value_held)] / d[, sum(value, na.rm = TRUE)], 3),
             industries = tr$n, links = sum(W[[r]] > 0), small_under_half_m = sum(pm > 0 & pm < 0.5e6),
             io_rank_cor = round(cor(tr$h[io$g], io$io, method = 'spearman', use = 'complete.obs'), 2)) }))
cat('0. Each region\'s inside flows (intermediaries out, held values, Jan 2019 to Sep 2026)\n'); print(stats[, -'region'])
fwrite(stats, file.path(out, 'region_data_quality.csv'))

# ---- A. Overall similarity ----------------------------------------------------------------------------
# Each region's flows as shares of its own total, one share per ordered pair of industries. The distance between two
# regions is half the sum of absolute differences: the share of one region's payments that would have to move to
# another pair of industries to match the other region. 0 = identical, 1 = nothing in common.
dis <- function(a, b) 0.5 * sum(abs(a / sum(a) - b / sum(b)))
makeup <- function(M) rowSums(M) + colSums(M)                                      # each industry's payments made plus received
wiring <- function(A, B) {                                                         # with sector makeup taken out: who each buyer pays, buyer by buyer
  ba <- colSums(A); bb <- colSums(B); wt <- (ba / sum(ba) + bb / sum(bb)) / 2; ok <- which(ba > 0 & bb > 0)
  sum(wt[ok] * vapply(ok, function(j) 0.5 * sum(abs(A[, j] / ba[j] - B[, j] / bb[j])), 0)) / sum(wt[ok]) }
pub <- lapply(setNames(regs, regs), function(r) { p <- x[region == r, .(any = any(!suppressed)), by = .(payee_sic, payer_sic)][any == TRUE]
  m <- matrix(FALSE, length(nodes), length(nodes), dimnames = list(nodes, nodes)); m[cbind(match(p$payee_sic, nodes), match(p$payer_sic, nodes))] <- TRUE; m })
pairs <- CJ(a = regs, b = regs)[a < b]
pairs[, `:=`(overall = mapply(function(a, b) dis(W[[a]], W[[b]]), a, b), makeup = mapply(function(a, b) dis(makeup(W[[a]]), makeup(W[[b]])), a, b),
             wiring = mapply(function(a, b) wiring(W[[a]], W[[b]]), a, b),
             overall_common_cells = mapply(function(a, b) { m <- pub[[a]] & pub[[b]]; dis(W[[a]][m], W[[b]][m]) }, a, b))]
D <- matrix(0, 12, 12, dimnames = list(short[regs], short[regs])); D[cbind(match(pairs$a, regs), match(pairs$b, regs))] <- pairs$overall; D <- D + t(D)
cat('\nA. Overall distance between regions (share of payments that would have to move; whole period)\n'); print(round(D, 2))
fwrite(as.data.table(round(D, 3), keep.rownames = 'region'), file.path(out, 'region_distance_overall.csv'))
fwrite(pairs[, .(a = reg_names[a], b = reg_names[b], overall = round(overall, 3), makeup = round(makeup, 3), wiring = round(wiring, 3), overall_common_cells = round(overall_common_cells, 3))][order(overall)],
       file.path(out, 'region_distance_pairs.csv'))
cat('\n   Range across the 66 pairs: overall', paste(round(range(pairs$overall), 2), collapse = ' to '), '(median', round(median(pairs$overall), 2), ')',
    '; makeup only', paste(round(range(pairs$makeup), 2), collapse = ' to '), '; wiring only', paste(round(range(pairs$wiring), 2), collapse = ' to '), '\n')
cat('   Rank correlation, overall distance against the same on cells published in both regions:', round(cor(pairs$overall, pairs$overall_common_cells, method = 'spearman'), 2),
    '; against makeup:', round(cor(pairs$overall, pairs$makeup, method = 'spearman'), 2), '; against wiring:', round(cor(pairs$overall, pairs$wiring, method = 'spearman'), 2), '\n')
nn <- rbindlist(lapply(regs, function(r) { o <- sort(D[short[[r]], -match(r, regs)]); data.table(region = reg_names[[r]], closest = sprintf('%s (%.2f), %s (%.2f)', names(o)[1], o[1], names(o)[2], o[2]),
                                                                                             furthest = sprintf('%s (%.2f)', names(o)[11], o[11])) }))
cat('\n   Each region\'s two closest and its furthest:\n'); print(nn)
hc <- hclust(as.dist(D), method = 'average')
cat('   Order from grouping the regions by distance:', paste(hc$labels[hc$order], collapse = ', '), '\n')

# Is that more than a region differs from itself? The same distance between a region's first half (2019 to 2022) and
# second half (2023 to Sep 2026), against the distance between regions within the second half.
half <- function(r, late) W_of(x[region == r & (year(date) >= 2023) == late])
W1 <- lapply(setNames(regs, regs), half, late = FALSE); W2 <- lapply(setNames(regs, regs), half, late = TRUE)
within <- vapply(regs, function(r) dis(W1[[r]], W2[[r]]), 0)
between2 <- mapply(function(a, b) dis(W2[[a]], W2[[b]]), pairs$a, pairs$b)
cat('\n   A region against itself, first half of the period against the second: median', round(median(within), 2), ', range', paste(round(range(within), 2), collapse = ' to '), '\n')
cat('   Two different regions, both in the second half: median', round(median(between2), 2), ', range', paste(round(range(between2), 2), collapse = ' to '), '\n')
cat('   Pairs of regions closer to each other than the typical region is to its own earlier self:', sum(between2 < median(within)), 'of 66\n')
cat('   Rank correlation of the 66 distances, first half against second half:', round(cor(mapply(function(a, b) dis(W1[[a]], W1[[b]]), pairs$a, pairs$b), between2, method = 'spearman'), 2), '\n')
print(data.table(region = short[regs], against_own_earlier_self = round(within, 2), hidden = stats$hidden))
cat('   Rank correlation across regions between a region\'s hidden share and its mean distance to the others:',
    round(cor(stats$hidden, rowSums(D) / 11, method = 'spearman'), 2), '\n')

# ---- B. Chain or web ----------------------------------------------------------------------------------
whole <- rbindlist(lapply(regs, function(r) as.data.table(as.list(round(measures(W[[r]]), 3)))[, region := short[[r]]]))
yearly <- rbindlist(lapply(regs, function(r) rbindlist(lapply(2019:2025, function(y) as.data.table(as.list(measures(W_of(x[region == r & year(date) == y]))))[, `:=`(region = short[[r]], year = y)]))))
fwrite(yearly[, lapply(.SD, round, 3), by = .(region, year)], file.path(out, 'region_chain_measures_yearly.csv'))
setcolorder(whole, 'region'); cat('\nB. Chain or web: whole period\n'); print(whole)
fwrite(whole, file.path(out, 'region_chain_measures.csv'))
ms <- c('down_share', 'incoherence', 'one_way', 'suppliers_per_buyer', 'customers_per_supplier', 'links_for_80pct', 'effective_links')
noise <- rbindlist(lapply(ms, function(m) { by_r <- yearly[, .(mean = mean(get(m)), sd = sd(get(m))), by = region]
  data.table(measure = m, lowest = sprintf('%s %.3g', whole$region[which.min(whole[[m]])], min(whole[[m]])), highest = sprintf('%s %.3g', whole$region[which.max(whole[[m]])], max(whole[[m]])),
             spread_between_regions = signif(sd(by_r$mean), 2), year_to_year_within_a_region = signif(mean(by_r$sd), 2), ratio = round(sd(by_r$mean) / mean(by_r$sd), 1),
             cor_with_hidden_share = round(cor(whole[[m]], stats$hidden, method = 'spearman'), 2), cor_with_size = round(cor(whole[[m]], stats$inside_bn, method = 'spearman'), 2)) }))
cat('\n   Is the difference between regions more than a region\'s own year-to-year movement, and does it just follow how much is hidden or how big the region is?\n'); print(noise)
fwrite(noise, file.path(out, 'region_chain_measures_noise.csv'))
# the same measures on cells published in every one of the 12 regions, so that hiding cannot drive the differences
all_pub <- Reduce(`&`, pub); cat('\n   Links published at some point in all 12 regions:', sum(all_pub), 'of', sum(Reduce(`|`, pub)), 'published anywhere; they carry',
                                 paste(round(range(vapply(regs, function(r) sum(W[[r]][all_pub]) / sum(W[[r]]), 0)), 2), collapse = ' to '), 'of each region\'s value\n')
common <- rbindlist(lapply(regs, function(r) { M <- W[[r]]; M[!all_pub] <- 0; as.data.table(as.list(round(measures(M), 3)))[, region := short[[r]]] })); setcolorder(common, 'region')
cat('   The same measures on those links only:\n'); print(common[, .(region, down_share, incoherence, one_way, suppliers_per_buyer, customers_per_supplier)])
cat('   Rank correlation, all links against common links: down share', round(cor(whole$down_share, common$down_share, method = 'spearman'), 2),
    '; suppliers per buyer', round(cor(whole$suppliers_per_buyer, common$suppliers_per_buyer, method = 'spearman'), 2), '\n')
fwrite(common, file.path(out, 'region_chain_measures_common_links.csv'))

# ---- C. Do Y&H's payment groups recur? ----------------------------------------------------------------
keyed <- c('46' = 'Distribution', '25' = 'Engineering and metals', '43' = 'Construction', '86' = 'Local and public services', '10' = 'Food')
x[, year := year(date)]
grp <- lapply(setNames(regs, regs), function(r) { d <- x[region == r]; tr <- trophic(W[[r]]); core <- names(tr$h)[!is.na(tr$h)]
  pm <- (rowSums(W[[r]]) + colSums(W[[r]])) / nm; set.seed(1); payment_groups(d, core, names(pm)[pm < 0.5e6 & pm > 0])$groups })
ref <- grp[['TLE']]
gsum <- rbindlist(lapply(regs, function(r) { g <- grp[[r]]; cg <- g[status == 'core']; m <- merge(cg[, .(sic2, group)], ref[status == 'core', .(sic2, ref = group)], by = 'sic2')
  data.table(region = short[[r]], groups_with_a_core = cg[, uniqueN(group)], core = nrow(cg), floating = g[status == 'floating', .N], too_small = g[status == 'too small', .N],
             keyed_divisions_in_separate_groups = cg[sic2 %in% names(keyed), uniqueN(group)], keyed_divisions_that_are_core = cg[sic2 %in% names(keyed), .N],
             agreement_with_yh = if (nrow(m) > 5) round(compare(m$group, m$ref, method = 'adjusted.rand'), 2) else NA_real_, core_in_both = nrow(m)) }))
cat('\nC. Payment groups, worked out region by region (same method and thresholds as Y&H)\n'); print(gsum)
fwrite(gsum, file.path(out, 'region_payment_groups.csv'))

# One grouping for the whole UK: every region's inside flows added together, same method. Would a single set of
# named groups, shared by all 12 pages, fit each region better than Y&H's own groups travel?
pooled <- x[, .(value_held = sum(value_held)), by = .(payer_sic, payee_sic, date, year)]
Wp <- W_of(pooled); trp <- trophic(Wp); corep <- names(trp$h)[!is.na(trp$h)]; pmp <- (rowSums(Wp) + colSums(Wp)) / nm
set.seed(1); gp <- payment_groups(pooled, corep, names(pmp)[pmp < 0.5e6 * 12 & pmp > 0])$groups
gp <- merge(gp, sic2_lk[, .(sic2, short)], by = 'sic2')
cat('\n   One grouping on all 12 regions\' inside flows added together: groups with a core', gp[status == 'core', uniqueN(group)], '; core', gp[status == 'core', .N],
    '; floating', gp[status == 'floating', .N], '\n')
for (g in gp[status == 'core', .N, by = group][order(-N), group]) cat(sprintf('     %s\n', paste(gp[group == g & status == 'core', short], collapse = ', ')))
cat('     keyed Y&H divisions (wholesale, fabricated metal, building trades, health, food manufacturing) fall in', gp[status == 'core' & sic2 %in% names(keyed), uniqueN(group)], 'separate groups\n')
agree <- rbindlist(lapply(regs, function(r) { m <- merge(grp[[r]][status == 'core', .(sic2, group)], gp[status == 'core', .(sic2, ref = group)], by = 'sic2')
  data.table(region = short[[r]], agreement_with_uk_grouping = round(compare(m$group, m$ref, method = 'adjusted.rand'), 2), core_in_both = nrow(m)) }))
cat('   Each region\'s own grouping against the UK one:\n'); print(agree)
fwrite(gp[order(group, -firmness), .(sic2, short, group, status, firmness = round(firmness, 2), years_with_group)], file.path(out, 'payment_groups_uk_pooled.csv'))
