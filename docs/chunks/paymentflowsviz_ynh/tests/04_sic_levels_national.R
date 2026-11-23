# 04_sic_levels_national.R
# Which SIC level shows the clearest structure? The regional data only comes at division (two-digit)
# level, so this uses the national five-digit file (Jan 2017 to Nov 2024, no regions) rolled up to
# five, four, three and two digits and to sections, and asks at each level:
#   - how strong the supplier-to-buyer order is (F0 against the direction-shuffled null)
#   - how stable the order is between years
#   - how clearly sectors fall into tightly connected groups (modularity), for the force-directed view
#   - which divisions hide the most internal variety (worth splitting if a mixed level is wanted)
# Run from the repo root: Rscript docs/chunks/paymentflowsviz_ynh/tests/04_sic_levels_national.R
source('docs/chunks/paymentflowsviz_ynh/tests/functions_hierarchy.R')
suppressPackageStartupMessages(library(igraph))
set.seed(1)
out <- file.path(chunk_dir, 'outputs')

f <- path.expand('~/Code/RegionalEconomicTools/local/data/ukindustrytoindustrypaymentflowssic5january2017tonovember2024_FINAL.csv')
cache <- file.path(local_dir, 'national_sic5_yearly.rds')
if (!file.exists(cache)) {
  d <- fread(f, colClasses = 'character', header = TRUE, showProgress = FALSE)
  setnames(d, c('payer', 'payee', 'date', 'value', 'n'))
  cat('rows:', nrow(d), ' non-numeric values:', d[is.na(suppressWarnings(as.numeric(value))), .N],
      ' distinct codes:', uniqueN(c(d$payer, d$payee)), '\n')
  print(d[is.na(suppressWarnings(as.numeric(value))), .N, by = value][order(-N)][1:5])
  d[, `:=`(year = as.integer(substr(date, 1, 4)), value = suppressWarnings(as.numeric(value)), n = suppressWarnings(as.numeric(n)))]
  y <- d[, .(value = sum(value, na.rm = TRUE), n = sum(n, na.rm = TRUE), months = .N, months_hidden = sum(is.na(value))),
         by = .(year, payer, payee)]
  saveRDS(y, cache); rm(d); gc()
}
y <- readRDS(cache)
cat('pair-years:', nrow(y), ' years:', paste(range(y$year), collapse = ' to '), '\n')
print(y[, .(pairs = .N, value_bn = round(sum(value) / 1e9), hidden_share_of_cell_months = round(sum(months_hidden) / sum(months), 3)), by = year])

sec <- setNames(sic2_lk$section, sic2_lk$sic2)
drop2 <- c('00', not_industry, intermediary)
y <- y[!substr(payer, 1, 2) %in% drop2 & !substr(payee, 1, 2) %in% drop2]

roll <- function(code, level) switch(level, sic5 = code, sic4 = substr(code, 1, 4), sic3 = substr(code, 1, 3),
                                     sic2 = substr(code, 1, 2), section = sec[substr(code, 1, 2)])
net_for <- function(yr, level) {
  d <- y[year %in% yr, .(value = sum(value)), by = .(payer_sic = roll(payer, level), payee_sic = roll(payee, level))]
  nodes <- sort(unique(c(d$payer_sic, d$payee_sic)))
  flow_matrix(d, 'value', nodes)
}
modularity_of <- function(W) {
  S <- W + t(W); diag(S) <- 0
  g <- graph_from_adjacency_matrix(S, mode = 'undirected', weighted = TRUE)
  cl <- cluster_louvain(g)
  c(modularity = modularity(cl), communities = length(unique(membership(cl)[strength(g) > 0])))
}

res <- rbindlist(lapply(c('section', 'sic2', 'sic3', 'sic4', 'sic5'), function(level) {
  W <- net_for(2023, level); W0 <- net_for(2019, level)
  tr <- trophic(W); tr0 <- trophic(W0)
  nl <- null_F0(W, if (level %in% c('sic4', 'sic5')) 20 else 100)
  common <- intersect(names(tr$h), names(tr0$h))
  md <- modularity_of(W); Wd <- W; diag(Wd) <- 0
  data.table(level, sectors = tr$n, links = sum(Wd > 0), density = round(sum(Wd > 0) / (nrow(W) * (nrow(W) - 1)), 3),
             F0 = round(tr$F0, 3), null_F0 = round(mean(nl), 3), gap = round(mean(nl) - tr$F0, 3),
             up_share = round(tr$up_share, 3), net_share = round(net_share(W), 3),
             levels_span = round(diff(range(tr$h, na.rm = TRUE)), 2),
             rho_2019_2023 = round(cor(tr$h[common], tr0$h[common], method = 'spearman', use = 'complete.obs'), 3),
             modularity = round(md[['modularity']], 3), communities = md[['communities']],
             self_share = round(sum(diag(W)) / sum(W), 3))
}))
cat('\nNational payments 2023, intermediaries out, by SIC level\n'); print(res)
fwrite(res, file.path(out, 'hierarchy_by_sic_level_national.csv'))

# ---- Which divisions hide the most variety? ------------------------------------------------------
# Five-digit levels (2023), then for each division: the flow-weighted spread of its members' levels,
# and how concentrated the division is in one five-digit code.
W5 <- net_for(2023, 'sic5'); tr5 <- trophic(W5); diag(W5) <- 0
m <- data.table(sic5 = names(tr5$h), h = tr5$h, flow = rowSums(W5) + colSums(W5))[!is.na(h) & flow > 0]
m[, sic2 := substr(sic5, 1, 2)]
wsd <- function(x, w) sqrt(sum(w * (x - weighted.mean(x, w))^2) / sum(w))
dv <- m[, .(codes = .N, flow_bn = round(sum(flow) / 1e9, 1), level_mean = round(weighted.mean(h, flow), 2),
            level_sd_within = round(wsd(h, flow), 3), level_min = round(min(h), 2), level_max = round(max(h), 2),
            top_code_share = round(max(flow) / sum(flow), 2)), by = sic2]
dv <- merge(dv, sic2_lk[, .(sic2, short, section)], by = 'sic2')
between <- wsd(dv$level_mean, dv$flow_bn)
cat('\nFive-digit levels: spread between divisions (flow-weighted sd of division means):', round(between, 3),
    '; typical spread within a division:', round(weighted.mean(dv$level_sd_within, dv$flow_bn), 3), '\n')
cat('Share of the variance in five-digit levels that sits between divisions:',
    round(between^2 / (between^2 + weighted.mean(dv$level_sd_within^2, dv$flow_bn)), 3), '\n')
dv[, split_score := round(level_sd_within * flow_bn / sum(flow_bn) * 100, 3)]
setorder(dv, -split_score)
cat('\nDivisions with the most to gain from splitting (within-division spread x size):\n')
print(dv[1:15, .(sic2, short, section, codes, flow_bn, level_mean, level_sd_within, level_min, level_max, top_code_share)])
fwrite(dv, file.path(out, 'division_internal_variety_national.csv'))

# Does national division-level order match Y&H's own-region order?
ynh_lev <- fread(file.path(out, 'levels_sic2.csv'), colClasses = list(character = 'sic2'))
tr2 <- trophic(net_for(2023, 'sic2'))
cmp <- merge(ynh_lev[, .(sic2, ynh = level_value)], data.table(sic2 = names(tr2$h), uk = tr2$h), by = 'sic2')
cat('\nRank correlation, Y&H own-region division levels against national division levels:',
    round(cor(cmp$ynh, cmp$uk, method = 'spearman', use = 'complete.obs'), 3), '\n')
