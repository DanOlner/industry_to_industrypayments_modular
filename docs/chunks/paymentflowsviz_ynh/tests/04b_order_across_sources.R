# 04b_order_across_sources.R
# How far does the supplier-to-buyer order depend on the source? Division-level levels from:
#   (a) Y&H own-region payments (regional file, held values)        -> outputs/levels_sic2.csv
#   (b) all UK payments, regional file summed over every region pair (published months only)
#   (c) all UK payments, national five-digit file rolled up to divisions (published cells only)
#   (d) the UK input-output table 2019                               -> outputs/levels_payments_vs_io.csv
# Run from the repo root: Rscript docs/chunks/paymentflowsviz_ynh/tests/04b_order_across_sources.R
source('docs/chunks/paymentflowsviz_ynh/tests/functions_hierarchy.R')
out <- file.path(chunk_dir, 'outputs')
drop2 <- c('00', not_industry, intermediary)
rho <- function(a, b) round(cor(a, b, method = 'spearman', use = 'complete.obs'), 2)

a <- fread(file.path(out, 'levels_sic2.csv'), colClasses = list(character = 'sic2'))[, .(sic2, ynh_own = level_value)]

py <- as.data.table(readRDS(path.expand('~/Code/claude/whole_economy_thinking/local/tradeable_jobs/cache/pay_yearly.rds'))$value)
py <- py[year == 2023 & !payer_sic %in% drop2 & !payee_sic %in% drop2]
nodes <- sort(unique(c(py$payer_sic, py$payee_sic)))
tr_b <- trophic(flow_matrix(py, 'value', nodes))
tr_b_own <- trophic(flow_matrix(py[payer_region == payee_region], 'value', nodes))   # every region's own-region flows pooled
cat(sprintf('(b) UK, regional file, all pairs 2023: F0 %.3f, uphill share %.3f, value GBP %.0f bn\n', tr_b$F0, tr_b$up_share, sum(py$value) / 1e9))
cat(sprintf('    UK, regional file, own-region flows only, pooled over 12 regions: F0 %.3f, uphill share %.3f\n', tr_b_own$F0, tr_b_own$up_share))

y5 <- readRDS(file.path(local_dir, 'national_sic5_yearly.rds'))[year == 2023]
y5 <- y5[, .(value = sum(value)), by = .(payer_sic = substr(payer, 1, 2), payee_sic = substr(payee, 1, 2))][!payer_sic %in% drop2 & !payee_sic %in% drop2]
tr_c <- trophic(flow_matrix(y5, 'value', nodes))
cat(sprintf('(c) UK, five-digit file rolled up 2023: F0 %.3f, uphill share %.3f, value GBP %.0f bn\n', tr_c$F0, tr_c$up_share, sum(y5$value) / 1e9))

io <- fread(file.path(out, 'levels_payments_vs_io.csv'), colClasses = list(character = 'g'))[, .(sic2 = g, io)]
m <- Reduce(function(x, y) merge(x, y, by = 'sic2', all = TRUE),
            list(a, data.table(sic2 = nodes, uk_regional = tr_b$h, uk_own_pooled = tr_b_own$h, uk_five_digit = tr_c$h), io))
cols <- c('ynh_own', 'uk_own_pooled', 'uk_regional', 'uk_five_digit', 'io')
cm <- sapply(cols, function(i) sapply(cols, function(j) rho(m[[i]], m[[j]])))
cat('\nRank correlation of division levels between sources (input-output compared on its merged groups only):\n'); print(cm)
fwrite(as.data.table(cm, keep.rownames = 'source'), file.path(out, 'order_across_sources.csv'))
