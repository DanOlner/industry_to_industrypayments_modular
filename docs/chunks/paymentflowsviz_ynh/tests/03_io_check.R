# 03_io_check.R
# Is the order found in the payments a supply-chain order? Compare it with the same calculation on the
# UK input-output table (ONS input-output analytical tables 2019, product by product, domestic use).
# Run from the repo root: Rscript docs/chunks/paymentflowsviz_ynh/tests/03_io_check.R
source('docs/chunks/paymentflowsviz_ynh/tests/functions_hierarchy.R')
suppressPackageStartupMessages(library(readxl))
set.seed(1)
out <- file.path(chunk_dir, 'outputs')

io_f <- path.expand('~/Code/claude/whole_economy_thinking/local/tradeable_jobs/data_raw/ioat_2019_nasu1719pr.xlsx')
d <- suppressMessages(read_excel(io_f, sheet = 'IOT', col_names = FALSE, col_types = 'text'))
codes_row <- which(apply(d, 1, function(r) sum(grepl('^CPA_', r))) > 50)[1]
col_codes <- unlist(d[codes_row, ]); pcols <- which(grepl('^CPA_', col_codes)); prows <- which(grepl('^CPA_', d[[1]]))
Z <- matrix(suppressWarnings(as.numeric(as.matrix(d[prows, pcols]))), length(prows), length(pcols),
            dimnames = list(d[[1]][prows], col_codes[pcols]))
Z[is.na(Z)] <- 0
stopifnot(identical(rownames(Z), unname(colnames(Z))))     # Z[i, j]: product i sold to industry j

# Each product's divisions: every two-digit number in its code, with "T" ranges filled in
cpa_divs <- function(x) {
  s <- sub('^CPA_', '', x)
  if (grepl('[0-9]T[0-9]', s)) { r <- as.integer(regmatches(s, gregexpr('[0-9]{2}', s))[[1]]); return(sprintf('%02d', seq(r[1], r[2]))) }
  parts <- strsplit(s, '&|,')[[1]]
  unique(vapply(parts, function(p) sub('^[^0-9]*([0-9]{2}).*$', '\\1', trimws(p)), ''))
}
links <- rbindlist(lapply(rownames(Z), function(x) data.table(cpa = x, sic2 = cpa_divs(x))))
# Group divisions that share a product (41, 42, 43; 59, 60; 87, 88 and so on) so both sources line up
grp <- setNames(sort(unique(links$sic2)), sort(unique(links$sic2)))
for (x in unique(links$cpa)) { dv <- links[cpa == x, sic2]; grp[grp %in% grp[dv]] <- min(grp[dv]) }
links[, g := grp[sic2]]
cpa_g <- unique(links[, .(cpa, g)]); stopifnot(!anyDuplicated(cpa_g$cpa))
groups <- sort(unique(cpa_g$g))
cat('IO products:', nrow(Z), ' common groups:', length(groups), '\n')

Zg <- rowsum(t(rowsum(Z, cpa_g$g[match(rownames(Z), cpa_g$cpa)])), cpa_g$g[match(colnames(Z), cpa_g$cpa)])
Zg <- t(Zg)[groups, groups]                                # Zg[supplier group, buyer group]

report <- function(W, label) {
  tr <- trophic(W); nl <- null_F0(W, 200)
  cat(sprintf('%-52s n = %3d  F0 = %.3f  (direction-shuffled %.3f)  uphill share = %.3f  net share = %.3f  span = %.2f\n',
              label, tr$n, tr$F0, mean(nl), tr$up_share, net_share(W), diff(range(tr$h, na.rm = TRUE))))
  invisible(tr)
}
tr_io105 <- report(Z, 'UK input-output table, 105 products')
tr_io <- report(Zg, 'UK input-output table, common groups')
strip_g <- setdiff(groups, grp[intermediary])
tr_io_s <- report(Zg[strip_g, strip_g], 'UK input-output table, groups, intermediaries out')

own <- readRDS(file.path(local_dir, 'ynh_own_held.rds'))
own[, `:=`(payer_g = grp[payer_sic], payee_g = grp[payee_sic])]
Wp <- flow_matrix(own[!is.na(payer_g) & !is.na(payee_g)], 'value_held', strip_g, from = 'payee_g', to = 'payer_g')
tr_pay <- report(Wp, 'Y&H own-region payments (value, held), groups')
Wn <- flow_matrix(own[!is.na(payer_g) & !is.na(payee_g)], 'n_held', strip_g, from = 'payee_g', to = 'payer_g')
tr_payn <- report(Wn, 'Y&H own-region payments (count, held), groups')

cmp <- data.table(g = strip_g, io = tr_io_s$h[strip_g], pay_value = tr_pay$h[strip_g], pay_count = tr_payn$h[strip_g])
cmp <- merge(cmp, sic2_lk[, .(g = sic2, short, section)], by = 'g')
rho <- function(a, b) round(cor(a, b, method = 'spearman', use = 'complete.obs'), 3)
cat('\nRank correlation of levels, input-output against Y&H payments: value', rho(cmp$io, cmp$pay_value),
    ' count', rho(cmp$io, cmp$pay_count), ' (groups compared:', cmp[!is.na(io) & !is.na(pay_value), .N], ')\n')
# share of pairs where the two sources agree on which way the net flow runs, weighted by payment value
dirs <- function(W) sign(W - t(W))
ut <- upper.tri(Wp); both <- ut & (Wp + t(Wp)) > 0 & (Zg[strip_g, strip_g] + t(Zg[strip_g, strip_g])) > 0
agree <- dirs(Wp)[both] == dirs(Zg[strip_g, strip_g])[both]
cat('Pairs where net direction matches the input-output table:', round(mean(agree), 3), 'of', sum(both),
    'pairs;', round(sum((Wp + t(Wp))[both][agree]) / sum((Wp + t(Wp))[both]), 3), 'by payment value\n')
cmp[, diff := round(pay_value - io, 2)]
num <- c('io', 'pay_value', 'pay_count'); cmp[, (num) := lapply(.SD, round, 2), .SDcols = num]
fwrite(cmp, file.path(out, 'levels_payments_vs_io.csv'))
cat('\nBiggest disagreements (payments level minus input-output level):\n')
print(cmp[order(-abs(diff))][1:12, .(g, short, section, io, pay_value, diff)])
