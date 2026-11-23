# 02_hierarchy_tests.R
# Question 1: do Y&H's own-region payment flows show a supply-chain order, with supplier sectors
# below buyer sectors, and is that order stable enough over 93 months to draw?
# Run from the repo root: Rscript docs/chunks/paymentflowsviz_ynh/tests/02_hierarchy_tests.R
# Needs 00_lookups.R and 01_extract_ynh_monthly.R to have run.
source('docs/chunks/paymentflowsviz_ynh/tests/functions_hierarchy.R')
set.seed(1)
out <- file.path(chunk_dir, 'outputs'); dir.create(out, showWarnings = FALSE)

ynh <- readRDS(file.path(local_dir, 'ynh_monthly.rds'))
own <- ynh[payer_region == 'TLE' & payee_region == 'TLE' &
             !payer_sic %in% c('00', not_industry) & !payee_sic %in% c('00', not_industry)]
own <- hold_values(own)
saveRDS(own, file.path(local_dir, 'ynh_own_held.rds'))
nodes_all <- sort(unique(c(own$payer_sic, own$payee_sic)))
nodes_strip <- setdiff(nodes_all, intermediary)

cat('Own-region cell-months:', nrow(own), ' divisions:', length(nodes_all), '\n')
cat('How each cell-month is valued:\n'); print(own[, .(cell_months = .N, share = round(.N / nrow(own), 3)), by = basis])
cat('Value: published', round(own[, sum(value, na.rm = TRUE)] / 1e9, 1), 'bn; held adds',
    round(own[suppressed == TRUE, sum(value_held)] / 1e9, 1), 'bn\n\n')

# ---- A. One network for the whole period, eight variants -----------------------------------------
variants <- CJ(measure = c('value', 'n'), hidden = c('published', 'held'), intermediaries = c('in', 'out'))
resA <- rbindlist(lapply(seq_len(nrow(variants)), function(i) {
  vr <- variants[i]
  col <- if (vr$hidden == 'held') paste0(vr$measure, '_held') else vr$measure
  nodes <- if (vr$intermediaries == 'out') nodes_strip else nodes_all
  W <- flow_matrix(own, col, nodes); tr <- trophic(W); nl <- null_F0(W, 200)
  data.table(vr, divisions = tr$n, F0 = round(tr$F0, 3), null_F0_mean = round(mean(nl), 3),
             null_F0_p05 = round(quantile(nl, 0.05), 3), up_share = round(tr$up_share, 3),
             net_share = round(net_share(W), 3), levels_span = round(diff(range(tr$h, na.rm = TRUE)), 2))
}))
cat('A. Whole period (Jan 2019 to Sep 2026), one network per variant\n'); print(resA)
fwrite(resA, file.path(out, 'hierarchy_variants.csv'))

# ---- B. Stability over time: yearly and monthly levels against the whole-period levels -----------
# Main variant from here: held values, intermediaries out.
levels_over_time <- function(col, nodes) {
  h_all <- trophic(flow_matrix(own, col, nodes))$h
  yearly <- rbindlist(lapply(sort(unique(own$year)), function(y) {
    tr <- trophic(flow_matrix(own[year == y], col, nodes))
    data.table(year = y, sic2 = nodes, h = tr$h, F0 = tr$F0)
  }))
  monthly <- rbindlist(lapply(sort(unique(own$date)), function(m) {
    tr <- trophic(flow_matrix(own[date == m], col, nodes))
    data.table(date = m, sic2 = nodes, h = tr$h, F0 = tr$F0)
  }))
  list(h_all = h_all, yearly = yearly, monthly = monthly)
}
rho <- function(a, b) suppressWarnings(cor(a, b, method = 'spearman', use = 'complete.obs'))

for (m in c('value', 'n')) {
  for (hid in c('held', 'published')) {
    col <- if (hid == 'held') paste0(m, '_held') else m
    lv <- levels_over_time(col, nodes_strip)
    yr <- lv$yearly[, .(F0 = round(F0[1], 3), rho_vs_whole_period = round(rho(h, lv$h_all[sic2]), 3)), by = year]
    mo <- lv$monthly[, .(F0 = F0[1], rho = rho(h, lv$h_all[sic2])), by = date]
    cat(sprintf('\nB. %s, %s, intermediaries out\n', m, hid)); print(yr)
    cat(sprintf('   monthly networks: F0 median %.3f (range %.3f to %.3f); rank correlation with whole-period levels median %.3f (min %.3f)\n',
                median(mo$F0), min(mo$F0), max(mo$F0), median(mo$rho), min(mo$rho)))
    # consecutive years
    wide <- dcast(lv$yearly, sic2 ~ year, value.var = 'h')
    yrs <- sort(unique(lv$yearly$year))
    cat('   year-on-year rank correlation:', paste(round(vapply(seq_along(yrs)[-1], function(i)
      rho(wide[[as.character(yrs[i])]], wide[[as.character(yrs[i - 1])]]), 0), 3), collapse = ' '), '\n')
    if (m == 'value' && hid == 'held') { lv_main <- lv; mo_main <- mo }
    if (m == 'n' && hid == 'held') lv_n <- lv
  }
}

# ---- C. The levels themselves --------------------------------------------------------------------
lev <- data.table(sic2 = nodes_strip, level_value = lv_main$h_all[nodes_strip], level_count = lv_n$h_all[nodes_strip])
W <- flow_matrix(own, 'value_held', nodes_strip); diag(W) <- 0
lev[, `:=`(received_bn = rowSums(W)[sic2] / 1e9, paid_bn = colSums(W)[sic2] / 1e9)]
lev[, balance := round((received_bn - paid_bn) / (received_bn + paid_bn), 3)]     # +1 only sells, -1 only buys
sd_y <- lv_main$yearly[, .(level_sd_years = sd(h, na.rm = TRUE)), by = sic2]
sd_m <- lv_main$monthly[, .(level_sd_months = sd(h, na.rm = TRUE)), by = sic2]
lev <- Reduce(function(a, b) merge(a, b, by = 'sic2'), list(lev, sd_y, sd_m, sic2_lk[, .(sic2, short, section, section_short)]))
setorder(lev, level_value)
num <- c('level_value', 'level_count', 'received_bn', 'paid_bn', 'level_sd_years', 'level_sd_months')
lev[, (num) := lapply(.SD, round, 3), .SDcols = num]
fwrite(lev, file.path(out, 'levels_sic2.csv'))
fwrite(lv_main$yearly[, .(year, sic2, h = round(h, 3))], file.path(out, 'levels_sic2_yearly.csv'))
fwrite(lv_main$monthly[, .(date, sic2, h = round(h, 3))], file.path(out, 'levels_sic2_monthly.csv'))

cat('\nC. Levels, value, held, intermediaries out (0 = most supplier-like)\n')
cat('   rank correlation, value levels against count levels:', round(rho(lev$level_value, lev$level_count), 3), '\n')
cat('   rank correlation, levels against the simple sell/buy balance:', round(rho(lev$level_value, -lev$balance), 3), '\n')
cat('   median month-to-month sd of a division\'s level:', round(median(lev$level_sd_months, na.rm = TRUE), 3),
    ' against a spread of levels (sd across divisions) of', round(sd(lev$level_value, na.rm = TRUE), 3), '\n')
cat('\n   Bottom 12 (suppliers):\n'); print(lev[1:12, .(sic2, short, section, level_value, level_count, received_bn, paid_bn, level_sd_months)])
cat('\n   Top 12 (buyers):\n'); print(lev[(.N - 11):.N, .(sic2, short, section, level_value, level_count, received_bn, paid_bn, level_sd_months)])
cat('\n   Mean level by section (weighted by payments in and out):\n')
print(lev[, .(divisions = .N, level = round(weighted.mean(level_value, received_bn + paid_bn, na.rm = TRUE), 2),
              min = min(level_value, na.rm = TRUE), max = max(level_value, na.rm = TRUE)), by = .(section, section_short)][order(level)])

# ---- D. Does coarser grouping help? Sections, and divisions with small ones pooled ----------------
agg_test <- function(map, label) {
  d <- copy(own)[, `:=`(payer_g = map[payer_sic], payee_g = map[payee_sic])][!is.na(payer_g) & !is.na(payee_g)]
  g <- sort(unique(c(d$payer_g, d$payee_g)))
  W <- flow_matrix(d, 'value_held', g, from = 'payee_g', to = 'payer_g'); tr <- trophic(W); nl <- null_F0(W, 200)
  data.table(grouping = label, groups = tr$n, F0 = round(tr$F0, 3), null_F0_mean = round(mean(nl), 3),
             up_share = round(tr$up_share, 3), net_share = round(net_share(W), 3),
             levels_span = round(diff(range(tr$h, na.rm = TRUE)), 2))
}
strip <- sic2_lk[role == 'industry']
map_div <- setNames(strip$sic2, strip$sic2)
map_sec <- setNames(strip$section, strip$sic2)
# mixed: keep divisions inside manufacturing, construction, wholesale/retail and professional services; sections elsewhere
keep_div <- c('C', 'F', 'G', 'H', 'M', 'N')
map_mix <- setNames(ifelse(strip$section %in% keep_div, strip$sic2, strip$section), strip$sic2)
resD <- rbind(agg_test(map_div, 'divisions (84)'), agg_test(map_mix, 'mixed: divisions in C, F, G, H, M, N; sections elsewhere'),
              agg_test(map_sec, 'sections'))
cat('\nD. Grouping (value, held, intermediaries out)\n'); print(resD)
fwrite(resD, file.path(out, 'hierarchy_groupings_ynh.csv'))

# ---- E. How much of the picture is a few big cells? ----------------------------------------------
e <- as.data.table(as.table(W))[N > 0]; setnames(e, c('supplier', 'buyer', 'value')); setorder(e, -value)
e[, cum := cumsum(value) / sum(value)]
cat('\nE. Links (pairs with any value over the whole period):', nrow(e), 'of', length(nodes_strip) * (length(nodes_strip) - 1), 'possible\n')
for (q in c(0.5, 0.8, 0.9, 0.95, 0.99)) cat(sprintf('   %2.0f%% of value sits in the top %d links\n', q * 100, e[cum <= q, .N] + 1))
per_month <- own[!payer_sic %in% intermediary & !payee_sic %in% intermediary & payer_sic != payee_sic,
                 .(published = sum(!suppressed), held_nonzero = sum(suppressed & value_held > 0), never = sum(basis == 'never_published')), by = date]
cat('   links per month: published median', median(per_month$published), '; hidden but held', median(per_month$held_nonzero),
    '; hidden with nothing to hold', median(per_month$never), '\n')
fwrite(e[, .(supplier, buyer, value_bn = round(value / 1e9, 4), cum = round(cum, 4))], file.path(out, 'links_ranked_sic2.csv'))
