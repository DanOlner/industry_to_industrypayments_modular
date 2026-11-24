# 23b_middle_links_io.R
# The outside check for 23_middle_links.cjs. That test splits each industry's payments with other industries in its
# region three ways: two-way with the same partner, passed along (in from one industry and on to another), and end of
# line. Does the same split on the UK input-output table (2019, domestic use, sales between industries) pick out the
# same industries? If the payments' middle links were supply-chain middle links, the two should agree.
# Reads the table as 03_io_check.R does, and the payments' figures from outputs/middle_links_by_industry.csv, so run
# 23_middle_links.cjs first.
# Run from the repo root: Rscript docs/chunks/paymentflowsviz_ynh/tests/23b_middle_links_io.R
source('docs/chunks/paymentflowsviz_ynh/tests/functions_hierarchy.R')
suppressPackageStartupMessages(library(readxl))
out <- file.path(chunk_dir, 'outputs')

# ---- The table, grouped so that divisions sharing a product line up with the payments (as in 03)
io_f <- path.expand('~/Code/claude/whole_economy_thinking/local/tradeable_jobs/data_raw/ioat_2019_nasu1719pr.xlsx')
d <- suppressMessages(read_excel(io_f, sheet = 'IOT', col_names = FALSE, col_types = 'text'))
codes_row <- which(apply(d, 1, function(r) sum(grepl('^CPA_', r))) > 50)[1]
col_codes <- unlist(d[codes_row, ]); pcols <- which(grepl('^CPA_', col_codes)); prows <- which(grepl('^CPA_', d[[1]]))
Z <- matrix(suppressWarnings(as.numeric(as.matrix(d[prows, pcols]))), length(prows), length(pcols), dimnames = list(d[[1]][prows], col_codes[pcols]))
Z[is.na(Z)] <- 0; stopifnot(identical(rownames(Z), unname(colnames(Z))))      # Z[i, j]: product i sold to industry j
cpa_divs <- function(x) {
  s <- sub('^CPA_', '', x)
  if (grepl('[0-9]T[0-9]', s)) { r <- as.integer(regmatches(s, gregexpr('[0-9]{2}', s))[[1]]); return(sprintf('%02d', seq(r[1], r[2]))) }
  unique(vapply(strsplit(s, '&|,')[[1]], function(p) sub('^[^0-9]*([0-9]{2}).*$', '\\1', trimws(p)), ''))
}
links <- rbindlist(lapply(rownames(Z), function(x) data.table(cpa = x, sic2 = cpa_divs(x))))
grp <- setNames(sort(unique(links$sic2)), sort(unique(links$sic2)))
for (x in unique(links$cpa)) { dv <- links[cpa == x, sic2]; grp[grp %in% grp[dv]] <- min(grp[dv]) }
links[, g := grp[sic2]]; cpa_g <- unique(links[, .(cpa, g)]); groups <- sort(unique(cpa_g$g))
Zg <- rowsum(t(rowsum(Z, cpa_g$g[match(rownames(Z), cpa_g$cpa)])), cpa_g$g[match(colnames(Z), cpa_g$cpa)])
Zg <- t(Zg)[groups, groups]                                                    # Zg[supplier, buyer]
keep <- setdiff(groups, grp[intermediary]); W <- Zg[keep, keep]; diag(W) <- 0   # intermediaries out, as the page opens

# ---- The same three-way split. For industry i: r = what each partner pays it (row i), p = what it pays each partner (column i)
split3 <- function(W) rbindlist(lapply(rownames(W), function(i) {
  r <- W[i, ]; p <- W[, i]; gross <- sum(r) + sum(p); if (gross == 0) return(NULL)
  net_in <- sum(pmax(r - p, 0)); net_out <- sum(pmax(p - r, 0))
  data.table(g = i, io_two_way = 2 * sum(pmin(r, p)) / gross, io_passed_along = 2 * min(net_in, net_out) / gross, io_end_of_line = abs(net_in - net_out) / gross,
             io_end_kind = fifelse(net_out > net_in, 'buyer', 'supplier'), io_balance = (sum(p) - sum(r)) / gross)
}))
io <- split3(W)
tot <- function(col) sum(io[[col]] * (rowSums(W) + colSums(W))[io$g]) / sum(rowSums(W) + colSums(W))
cat(sprintf('UK input-output table, %d industry groups: two-way %.0f%%, passed along %.0f%%, end of line %.0f%% of sales between industries\n',
            nrow(io), 100 * tot('io_two_way'), 100 * tot('io_passed_along'), 100 * tot('io_end_of_line')))
cat('By industry, passed along: a tenth below', sprintf('%.0f%%', 100 * quantile(io$io_passed_along, 0.1)), ' middle', sprintf('%.0f%%', 100 * median(io$io_passed_along)),
    ' a tenth above', sprintf('%.0f%%', 100 * quantile(io$io_passed_along, 0.9)), '\n')

# ---- Against the payments: each division's middle value across the regions where it is placed. Only groups that are a
# single division are compared, so that like is set against like.
pay <- fread(file.path(out, 'middle_links_by_industry.csv'), colClasses = list(character = 'sic2'))
pay[, balance := (paid_gbp_m_month - received_gbp_m_month) / (paid_gbp_m_month + received_gbp_m_month)]
mid <- pay[, .(short = industry[1], regions = .N, two_way = median(two_way), passed_along = median(passed_along), end_of_line = median(end_of_line), balance = median(balance)), by = sic2]
single <- names(which(table(grp) == 1)); cmp <- merge(mid[sic2 %in% single], io, by.x = 'sic2', by.y = 'g')
rho <- function(a, b) round(cor(a, b, method = 'spearman'), 2)
cat('\nRank correlation, payments (middle of the regions) against the input-output table,', nrow(cmp), 'divisions:\n')
cat('  passed along', rho(cmp$passed_along, cmp$io_passed_along), '\n  two-way     ', rho(cmp$two_way, cmp$io_two_way), '\n  balance     ', rho(cmp$balance, cmp$io_balance), '\n')
ynh <- merge(pay[region == 'TLE' & sic2 %in% single], io, by.x = 'sic2', by.y = 'g')
cat('Yorkshire and the Humber alone,', nrow(ynh), 'divisions: passed along', rho(ynh$passed_along, ynh$io_passed_along), ' two-way', rho(ynh$two_way, ynh$io_two_way), ' balance', rho(ynh$balance, ynh$io_balance), '\n')
show <- function(x) paste(sprintf('%s %.0f%%', x$short, 100 * x$io_passed_along), collapse = '; ')
cat('\nInput-output table, most passed along:', show(cmp[order(-io_passed_along)][1:10]), '\n')
cat('Input-output table, least passed along:', show(cmp[order(io_passed_along)][1:8]), '\n')
x <- cmp[sic2 == '32']
if (nrow(x)) cat(sprintf('\nOther manufacturing in the input-output table: two-way %.0f%%, passed along %.0f%%, %s end %.0f%% (%d of %d by share passed along)\n',
                         100 * x$io_two_way, 100 * x$io_passed_along, x$io_end_kind, 100 * x$io_end_of_line, cmp[io_passed_along > x$io_passed_along, .N] + 1L, nrow(cmp)))
i <- '32'; if (i %in% rownames(W)) { nm <- function(g) sic2_lk[match(g, sic2), short]
  cat('  the table has it sell most to:', paste(nm(names(sort(W[i, ], decreasing = TRUE))[1:6]), collapse = ', '), '\n  and buy most from:', paste(nm(names(sort(W[, i], decreasing = TRUE))[1:6]), collapse = ', '), '\n') }
fwrite(cmp[, .(sic2, short, regions, pay_two_way = round(two_way, 3), pay_passed_along = round(passed_along, 3), pay_end_of_line = round(end_of_line, 3), pay_balance = round(balance, 3),
               io_two_way = round(io_two_way, 3), io_passed_along = round(io_passed_along, 3), io_end_of_line = round(io_end_of_line, 3), io_end_kind, io_balance = round(io_balance, 3))],
       file.path(out, 'middle_links_payments_vs_io.csv'))
cat('\nWrote middle_links_payments_vs_io.csv\n')
