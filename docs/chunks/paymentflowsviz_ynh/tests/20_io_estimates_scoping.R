# 20_io_estimates_scoping.R
# Scoping for plan section 11.12: could the input-output table fill in what the payments leave out (sales to
# households, to government and abroad), split between regions by a proxy, behind a switch on the page?
# Before building anything, check the two things the idea rests on:
#   1. Does the input-output table describe the payments where both can be seen? If the table can't predict the
#      payments between industries, or public administration's payments, its figures for the unseen parts can't be
#      put beside the payments as if they were the same kind of number.
#   2. Does a regional proxy (jobs) put an industry's income in the regions where the payments put it?
# National payments: the five-digit file with no regions (so public administration is all there), 2019.
# Input-output: ONS analytical tables 2019, product by product, domestic use at basic prices, GBP million.
# Run from the repo root, after 04 and 14: Rscript docs/chunks/paymentflowsviz_ynh/tests/20_io_estimates_scoping.R
source('docs/chunks/paymentflowsviz_ynh/tests/functions_hierarchy.R')
suppressPackageStartupMessages(library(readxl))
out <- file.path(chunk_dir, 'outputs')
spear <- function(a, b) round(suppressWarnings(cor(a, b, method = 'spearman', use = 'pairwise.complete.obs')), 2)
q <- function(v) round(quantile(v, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE), 2)

# ---- The input-output table -----------------------------------------------------------------------------------
io_f <- path.expand('~/Code/claude/whole_economy_thinking/local/tradeable_jobs/data_raw/ioat_2019_nasu1719pr.xlsx')
d <- suppressMessages(read_excel(io_f, sheet = 'IOT', col_names = FALSE, col_types = 'text'))
codes_row <- which(apply(d, 1, function(r) sum(grepl('^CPA_', r))) > 50)[1]
col_codes <- unlist(d[codes_row, ]); pcols <- which(grepl('^CPA_', col_codes)); prows <- which(grepl('^CPA_', d[[1]]))
numcol <- function(j) { v <- suppressWarnings(as.numeric(d[[j]][prows])); v[is.na(v)] <- 0; v }
Z <- sapply(pcols, numcol); dimnames(Z) <- list(d[[1]][prows], col_codes[pcols])      # Z[i, j]: product i sold to industry j
fd <- function(code) setNames(numcol(which(col_codes == code)), rownames(Z))
gov <- fd('P3 S13'); hh <- fd('P3 S14'); npish <- fd('P3 S15'); gfcf <- fd('P51G'); exports <- fd('P61EU') + fd('P61RW') + fd('P62'); total_use <- fd('TU')

# divisions behind each product, and groups of divisions that share a product (as in test 03)
cpa_divs <- function(x) {
  s <- sub('^CPA_', '', x)
  if (grepl('[0-9]T[0-9]', s)) { r <- as.integer(regmatches(s, gregexpr('[0-9]{2}', s))[[1]]); return(sprintf('%02d', seq(r[1], r[2]))) }
  parts <- strsplit(s, '&|,')[[1]]
  unique(vapply(parts, function(p) sub('^[^0-9]*([0-9]{2}).*$', '\\1', trimws(p)), ''))
}
links <- rbindlist(lapply(rownames(Z), function(x) data.table(cpa = x, sic2 = cpa_divs(x))))
grp <- setNames(sort(unique(links$sic2)), sort(unique(links$sic2)))
for (x in unique(links$cpa)) { dv <- links[cpa == x, sic2]; grp[grp %in% grp[dv]] <- min(grp[dv]) }
links[, g := grp[sic2]]
gc_ <- links[, .(g = g[1]), by = cpa]; g_of <- setNames(gc_$g, gc_$cpa)[rownames(Z)]
groups <- sort(unique(g_of))
agg <- function(v) tapply(v, g_of, sum)[groups]
Zg <- rowsum(t(rowsum(Z, g_of)), g_of); Zg <- t(Zg)[groups, groups]                   # Zg[seller, buyer]
io <- data.table(g = groups, io_to_business = rowSums(Zg), io_gov = agg(gov), io_households = agg(hh), io_npish = agg(npish),
                 io_investment = agg(gfcf), io_exports = agg(exports), io_total_use = agg(total_use))
gname <- function(g) { m <- vapply(g, function(k) { s <- names(grp)[grp == k]; paste0(sic2_lk[match(k, sic2), short], if (length(s) > 1) ' +' else '') }, ''); unname(m) }
io[, name := gname(g)]
inter_g <- unique(grp[intermediary]); core <- setdiff(groups, c(inter_g, grp[not_industry]))
core <- core[!is.na(core)]

# ---- National payments, 2019 ------------------------------------------------------------------------------------
y <- readRDS(file.path(local_dir, 'national_sic5_yearly.rds'))[year == 2019]
y[, `:=`(p2 = substr(payer, 1, 2), q2 = substr(payee, 1, 2))]
y[, `:=`(pg = fifelse(p2 == '00', '00', grp[p2]), qg = fifelse(q2 == '00', '00', grp[q2]))]
pm <- y[!is.na(pg) & !is.na(qg), .(v = sum(value, na.rm = TRUE) / 1e6), by = .(pg, qg)]         # GBP million, as the table
P <- matrix(0, length(groups), length(groups), dimnames = list(groups, groups))                  # P[seller (payee), buyer (payer)]
k <- pm[pg %in% groups & qg %in% groups]; P[cbind(k$qg, k$pg)] <- k$v
from <- function(payer) { v <- pm[pg == payer, .(v = sum(v)), by = qg]; o <- setNames(v$v, v$qg)[groups]; names(o) <- groups; o[is.na(o)] <- 0; o }
cat('\nNational payments 2019, GBP bn, by who pays\n')
print(y[, .(bn = round(sum(value, na.rm = TRUE) / 1e9)), by = .(payer = fifelse(p2 == '00', 'unknown industry', fifelse(p2 == '84', 'public administration',
        fifelse(p2 %in% intermediary, 'intermediaries', fifelse(p2 %in% not_industry, 'not an industry', 'other industries')))))][order(-bn)])

# ---- 1a. Payments between industries against the table's intermediate sales ----------------------------------------
Pc <- P[core, core]; Zc <- Zg[core, core]
cat(sprintf('\n1a. Between the %d industry groups both sources have (intermediaries left out of both), 2019, GBP bn\n', length(core)))
cat(sprintf('   payments %.0f (%.0f without payments inside one group); input-output intermediate sales %.0f (%.0f)\n',
            sum(Pc) / 1e3, (sum(Pc) - sum(diag(Pc))) / 1e3, sum(Zc) / 1e3, (sum(Zc) - sum(diag(Zc))) / 1e3))
off <- row(Pc) != col(Pc)
cmp <- data.table(g = core, name = gname(core), pay_sold = rowSums(Pc * off), io_sold = rowSums(Zc * off), pay_bought = colSums(Pc * off), io_bought = colSums(Zc * off))
cmp[, `:=`(sold_ratio = pay_sold / io_sold, bought_ratio = pay_bought / io_bought)]
cat('   Payments as a multiple of the table, industry by industry. Sales to other industries:\n'); print(q(cmp$sold_ratio))
cat('   Purchases from other industries:\n'); print(q(cmp$bought_ratio))
cat(sprintf('   Rank correlation across industries: sales %.2f, purchases %.2f\n', spear(cmp$pay_sold, cmp$io_sold), spear(cmp$pay_bought, cmp$io_bought)))
cat(sprintf('   Cell by cell (who sells to whom): rank correlation %.2f; share of the money that would have to move for the two patterns to match %.0f%%\n',
            spear(Pc[off], Zc[off]), 50 * sum(abs(Pc[off] / sum(Pc[off]) - Zc[off] / sum(Zc[off])))))
cat('   Largest and smallest multiples (sales):\n')
print(cmp[io_sold > 1000][order(-sold_ratio)][c(1:6, (.N - 5):.N), .(g, name, pay_bn = round(pay_sold / 1e3, 1), io_bn = round(io_sold / 1e3, 1), ratio = round(sold_ratio, 2))])

# ---- 1b. Public administration's payments against what the table says government buys --------------------------------
g84 <- grp[['84']]; oth <- setdiff(core, g84)
pay_gov <- from(g84); pay_gov[is.na(pay_gov)] <- 0
io_gov_inter <- Zg[, g84]; io_gov_final <- setNames(io$io_gov, io$g)
gv <- data.table(g = oth, name = gname(oth), pay = pay_gov[oth], io_purchases = io_gov_inter[oth], io_final = io_gov_final[oth])
gv[, io_both := io_purchases + io_final]
cat(sprintf('\n1b. Public administration paying other industries, 2019, GBP bn: payments %.0f; table: its purchases %.0f, government final spending on other products %.0f\n',
            sum(gv$pay) / 1e3, sum(gv$io_purchases) / 1e3, sum(gv$io_final) / 1e3))
cat(sprintf('   Rank correlation across receiving industries: with purchases %.2f, with final spending %.2f, with both %.2f\n',
            spear(gv$pay, gv$io_purchases), spear(gv$pay, gv$io_final), spear(gv$pay, gv$io_both)))
cat(sprintf('   Share of the money that would have to move to match: purchases %.0f%%, both %.0f%%\n',
            50 * sum(abs(gv$pay / sum(gv$pay) - gv$io_purchases / sum(gv$io_purchases))), 50 * sum(abs(gv$pay / sum(gv$pay) - gv$io_both / sum(gv$io_both)))))
gv[, `:=`(pay_pct = round(100 * pay / sum(pay), 1), io_pct = round(100 * io_both / sum(io_both), 1))]
cat('   Where the payments go, against the table (purchases plus final spending), % of each\n')
print(gv[order(-pay)][1:12, .(g, name, pay_bn = round(pay / 1e3, 1), pay_pct, io_pct)])
cat('   What the table expects that the payments barely show\n'); print(gv[order(-(io_pct - pay_pct))][1:5, .(g, name, pay_pct, io_pct)])

# ---- 1c. Are households already in the payments, through the intermediaries? And who are the unknown payers? ----------
pay_inter <- Reduce(`+`, lapply(inter_g, function(i) { v <- from(i); v[is.na(v)] <- 0; v })); pay_unknown <- from('00'); pay_unknown[is.na(pay_unknown)] <- 0
hx <- data.table(g = core, name = gname(core), from_intermediaries = pay_inter[core], from_unknown = pay_unknown[core], from_gov = pay_gov[core], from_business = rowSums(Pc))
hx <- merge(hx, io[, !'name'], by = 'g')
cat(sprintf('\n1c. Receipts of industries from the intermediaries %.0f bn and from payers of unknown industry %.0f bn; the table\'s household spending on their products %.0f bn\n',
            sum(hx$from_intermediaries) / 1e3, sum(hx$from_unknown) / 1e3, sum(hx$io_households) / 1e3))
cat('   Rank correlation across industries between each kind of receipt and each of the table\'s uses\n')
uses <- c('io_to_business', 'io_households', 'io_gov', 'io_exports', 'io_investment')
print(sapply(uses, function(u) sapply(c('from_business', 'from_intermediaries', 'from_unknown', 'from_gov'), function(r) spear(hx[[r]], hx[[u]]))))
cat('   Largest receivers from the intermediaries, and from unknown payers (GBP bn)\n')
print(hx[order(-from_intermediaries)][1:6, .(g, name, from_intermediaries = round(from_intermediaries / 1e3, 1), io_households = round(io_households / 1e3, 1))])
print(hx[order(-from_unknown)][1:8, .(g, name, from_unknown = round(from_unknown / 1e3, 1), io_households = round(io_households / 1e3, 1), io_gov = round(io_gov / 1e3, 1), io_to_business = round(io_to_business / 1e3, 1))])

# ---- 1d. Scaling by ratio: what the unseen part would be multiplied up from --------------------------------------------
hx[, `:=`(business_share = io_to_business / io_total_use, unseen_multiple = (io_households + io_gov + io_npish + io_exports + io_investment) / io_to_business)]
cat('\n1d. If the unseen sales were scaled from the seen ones, the multiple each industry\'s business receipts would be multiplied by\n'); print(q(hx$unseen_multiple))
print(hx[g %in% grp[c('47', '56', '85', '86', '87', '84', '55', '10', '62', '69')], .(g, name, sold_to_business_pct = round(100 * business_share), unseen_multiple = round(unseen_multiple, 1))][order(-unseen_multiple)])

# ---- 2. Does a regional proxy put income where the payments put it? ----------------------------------------------------
regs <- c('TLC', 'TLD', 'TLE', 'TLF', 'TLG', 'TLH', 'TLI', 'TLJ', 'TLK', 'TLL', 'TLM')          # Great Britain: the jobs data stops there
rp <- rbindlist(lapply(regs, function(r) {
  m <- readRDS(file.path(local_dir, sprintf('monthly_%s.rds', r)))[payee_region == r & substr(as.character(date), 1, 4) == '2019']
  m[, kind := fifelse(payer_sic == '84' & payer_region == 'unknown', 'gov', fifelse(payer_sic == '00', 'unknown',
              fifelse(payer_region != 'unknown' & !payer_sic %in% c(intermediary, not_industry), 'business', 'other')))]
  m[kind != 'other', .(v = sum(value, na.rm = TRUE)), by = .(sic2 = payee_sic, kind)][, region := r]
}))
jobs <- fread('~/Code/RegionalEconomicTools/local/data/BRES/separate_SIC_types_summedfrom5digitSIC/BRES_ALLYEARSWITHDATA_TYPE428_internationalterritoriallevelslevel3asofJan2021_4_Employment_2022_2023_SIC_2DIGIT.csv',
              colClasses = list(character = 'SIC_2DIGIT_CODE'))[DATE == max(DATE), .(jobs = sum(JOBCOUNT, na.rm = TRUE)), by = .(region = substr(GEOGRAPHY_CODE, 1, 3), sic2 = SIC_2DIGIT_CODE)]
rp <- dcast(rp, region + sic2 ~ kind, value.var = 'v', fill = 0)
rp <- merge(rp, jobs, by = c('region', 'sic2'), all = TRUE); for (kk in c('business', 'gov', 'unknown', 'jobs')) set(rp, which(is.na(rp[[kk]])), kk, 0)
rp <- rp[sic2 %in% sic2_lk[role == 'industry', sic2] & region %in% regs]
sh <- rp[, .(region, jobs_share = jobs / sum(jobs), business_share = business / sum(business), gov_share = gov / sum(gov), unknown_share = unknown / sum(unknown),
             business, gov, unknown), by = sic2]
per <- sh[, .(business_bn = sum(business) / 1e9, gov_bn = sum(gov) / 1e9, miss_business = 50 * sum(abs(business_share - jobs_share)), miss_gov = 50 * sum(abs(gov_share - jobs_share)),
              miss_unknown = 50 * sum(abs(unknown_share - jobs_share)), london_jobs = 100 * jobs_share[region == 'TLI'], london_business = 100 * business_share[region == 'TLI'],
              london_gov = 100 * gov_share[region == 'TLI']), by = sic2]
per <- merge(per, sic2_lk[, .(sic2, short)], by = 'sic2')
cat('\n2. Splitting an industry\'s income between regions by its jobs: share of the money that lands in the wrong region (11 regions, 2019 payments, 2023 jobs)\n')
cat('   From businesses with a known region, across industries:\n'); print(q(per[is.finite(miss_business), miss_business]))
cat('   From public administration:\n'); print(q(per[is.finite(miss_gov), miss_gov]))
cat('   From payers of unknown industry:\n'); print(q(per[is.finite(miss_unknown), miss_unknown]))
cat(sprintf('   Weighted by value: business %.0f%%, public administration %.0f%%\n', per[is.finite(miss_business), weighted.mean(miss_business, business_bn)],
            per[is.finite(miss_gov), weighted.mean(miss_gov, gov_bn)]))
tot <- rp[, .(jobs = sum(jobs), business = sum(business), gov = sum(gov), unknown = sum(unknown)), by = region]
tot[, `:=`(jobs = round(100 * jobs / sum(jobs), 1), business = round(100 * business / sum(business), 1), gov = round(100 * gov / sum(gov), 1), unknown = round(100 * unknown / sum(unknown), 1))]
cat('   Each region\'s share (%) of jobs, and of industries\' receipts from each kind of payer\n'); print(tot)
cat('   Public services and a few others: London\'s share of jobs against its share of receipts (%), and the share misplaced by a jobs split\n')
print(per[sic2 %in% c('85', '86', '87', '88', '47', '56', '10', '62', '41'), .(sic2, short, london_jobs = round(london_jobs), london_business = round(london_business), london_gov = round(london_gov),
                                                                             miss_business = round(miss_business), miss_gov = round(miss_gov))])

fwrite(merge(cmp, hx[, .(g, from_intermediaries, from_unknown, from_gov, io_households, io_gov, io_exports, io_investment, io_total_use, unseen_multiple)], by = 'g'),
       file.path(out, 'io_estimates_by_industry.csv'))
fwrite(per, file.path(out, 'io_estimates_region_proxy.csv'))
cat('\nDone.\n')
