# 06_export_prototype_data.R
# Write the data file the browser prototype reads, and measure how big and how noisy it is.
# Run from the repo root: Rscript docs/chunks/paymentflowsviz_ynh/tests/06_export_prototype_data.R
# Needs 00, 01, 02, 05, 11 and 12 to have run.
source('docs/chunks/paymentflowsviz_ynh/tests/functions_hierarchy.R')
suppressPackageStartupMessages(library(jsonlite))
out <- file.path(chunk_dir, 'outputs'); proto <- file.path(chunk_dir, 'prototype')

x <- readRDS(file.path(local_dir, 'ynh_all_held.rds'))          # outside flows; non-industries and intermediaries are out of this one
own <- readRDS(file.path(local_dir, 'ynh_own_held.rds'))        # inside flows; intermediaries in
months <- sort(unique(x$date)); nm <- length(months)
lev <- fread(file.path(out, 'levels_sic2.csv'), colClasses = list(character = 'sic2'))[!is.na(level_value)]
lev_all <- fread(file.path(out, 'levels_sic2_with_intermediaries.csv'), colClasses = list(character = 'sic2'))[!is.na(level_with_intermediaries)]
levy <- fread(file.path(out, 'levels_sic2_yearly.csv'), colClasses = list(character = 'sic2'))
is8 <- fread(file.path(chunk_dir, 'lookups/indstrat_sic2.csv'), colClasses = list(character = 'sic2'))
pg <- fread(file.path(out, 'payment_groups_consensus.csv'), colClasses = list(character = 'sic2'))
# Names for the payment groups are hand-given, keyed on one division each so they survive a re-run
pg_names <- c('46' = 'Distribution', '25' = 'Engineering & metals', '43' = 'Construction', '86' = 'Local & public services', '10' = 'Food')
pg[, group_name := { nm_ <- pg_names[names(pg_names) %in% sic2]; if (length(nm_)) nm_[[1]] else NA_character_ }, by = group]
ids <- sort(union(lev$sic2, intermediary))                      # 77 divisions plus the four intermediaries
stopifnot(all(ids %in% lev_all$sic2))

# ---- Nodes ---------------------------------------------------------------------------------------
ext <- function(kind_, sic_col) {
  a <- x[kind == kind_ & get(sic_col) %in% ids, .(v = sum(value_held)), by = c(sic_col, 'date')]
  m <- matrix(0, length(ids), nm, dimnames = list(ids, as.character(months)))
  m[cbind(match(a[[sic_col]], ids), match(a$date, months))] <- a$v
  round(m / 1e3)                                                 # GBP thousands
}
ext_buy <- ext('ynh_buys_from_outside', 'payer_sic'); ext_sell <- ext('outside_buys_from_ynh', 'payee_sic')
nodes <- lapply(ids, function(s) {
  l <- lev[sic2 == s]; i8 <- is8[sic2 == s]; lk <- sic2_lk[sic2 == s]; g <- pg[sic2 == s]
  list(id = s, short = lk$short, name = lk$name, section = lk$section, section_short = lk$section_short, role = lk$role,
       level = if (nrow(l)) l$level_value else NULL, level_all = lev_all[sic2 == s, level_with_intermediaries],
       level_yearly = levy[sic2 == s][order(year), h],
       pay_group = if (nrow(g) && !is.na(g$group_name) && g$status == 'core') g$group_name else NULL,   # core members only
       pay_group_loose = if (nrow(g) && !is.na(g$group_name)) g$group_name else NULL, pay_status = if (nrow(g)) g$status else 'not grouped',
       is8 = lapply(seq_len(nrow(i8)), function(k) list(name = i8$is8[k], share = i8$share_of_division_jobs[k], whole = i8$whole_division[k])),
       ext_buy = unname(ext_buy[s, ]), ext_sell = unname(ext_sell[s, ]))
})

# ---- Links: one row per directed pair, a value per month ----------------------------------------
ins <- own[payer_sic %in% ids & payee_sic %in% ids & payer_sic != payee_sic]
ins[, m := match(date, months)]
pairs <- ins[, .(total = sum(value_held)), by = .(payee_sic, payer_sic)][total > 0][order(-total)]
pairs[, k := .I]
ins <- merge(ins, pairs[, .(payee_sic, payer_sic, k)], by = c('payee_sic', 'payer_sic'))
V <- matrix(0, nrow(pairs), nm); N <- matrix(0, nrow(pairs), nm); B <- matrix('0', nrow(pairs), nm)   # B: 0 no row, p published, h held, x hidden with nothing to hold
V[cbind(ins$k, ins$m)] <- round(ins$value_held / 1e3); N[cbind(ins$k, ins$m)] <- round(ins$n_held)
B[cbind(ins$k, ins$m)] <- fifelse(ins$basis == 'published', 'p', fifelse(ins$basis == 'never_published', 'x', 'h'))
links <- list(supplier = match(pairs$payee_sic, ids) - 1L, buyer = match(pairs$payer_sic, ids) - 1L,
              value = lapply(seq_len(nrow(V)), function(i) V[i, ]), count = lapply(seq_len(nrow(N)), function(i) N[i, ]),
              basis = apply(B, 1, paste, collapse = ''))

js <- toJSON(list(months = format(months, '%Y-%m'), units = list(value = 'GBP thousands', count = 'payments'),
                  pay_groups = unname(pg_names), nodes = nodes, links = links), auto_unbox = TRUE, digits = NA, null = 'null')
# Written as a script that sets one global, so the prototype opens straight from disk (no web server)
f <- file.path(proto, 'ynh_flows.js'); writeLines(paste0('window.YNH_FLOWS = ', js, ';'), f)
system(paste('gzip -k -9 -f', shQuote(f)))
cat('Nodes:', length(ids), ' directed links:', nrow(pairs), ' months:', nm, '\n')
cat('Data file:', round(file.size(f) / 1e6, 2), 'MB; gzipped (as a web server would send it):', round(file.size(paste0(f, '.gz')) / 1e6, 2), 'MB\n')
unlink(paste0(f, '.gz'))

# ---- How noisy is month to month? (the 77 divisions without intermediaries) ----------------------
keep <- !pairs$payee_sic %in% intermediary & !pairs$payer_sic %in% intermediary
V <- V[keep, ]; B <- B[keep, ]; pairs <- pairs[keep]
top <- 1:500
mom <- abs(V[top, -1] - V[top, -nm]) / pmax((V[top, -1] + V[top, -nm]) / 2, 1)
cat('Top 500 links: median month-on-month change', round(median(mom), 3), '; 90th percentile', round(quantile(mom, 0.9), 3), '\n')
roll3 <- t(apply(V[top, ], 1, function(v) stats::filter(v, rep(1 / 3, 3), sides = 1)))[, -(1:2)]
mom3 <- abs(roll3[, -1] - roll3[, -ncol(roll3)]) / pmax((roll3[, -1] + roll3[, -ncol(roll3)]) / 2, 1)
cat('  with a three-month rolling mean: median', round(median(mom3), 3), '; 90th percentile', round(quantile(mom3, 0.9), 3), '\n')
tot <- colSums(V) / 1e3
cat('Monthly total inside value, GBP m: first year mean', round(mean(tot[1:12])), ' last 12 months mean', round(mean(tot[(nm - 11):nm])), '\n')
seas <- data.table(month = month(months), tot = tot / stats::filter(tot, rep(1 / 12, 12), sides = 2))[!is.na(tot), .(index = round(mean(tot), 2)), by = month][order(month)]
cat('Seasonal index of the monthly total (1 = the centred 12-month average):\n'); print(t(seas))
cat('Links with a non-zero value in a typical month:', median(colSums(V > 0)), ' of which published', median(colSums(B == 'p')), '\n')
shareflip <- mean(vapply(top, function(i) { r <- which(pairs$payee_sic == pairs$payer_sic[i] & pairs$payer_sic == pairs$payee_sic[i]); if (!length(r)) return(NA); mean(sign(V[i, ] - V[r, ]) != sign(sum(V[i, ]) - sum(V[r, ]))) }, 0), na.rm = TRUE)
cat('Top 500 links: share of months in which the pair\'s net direction is the opposite of its whole-period direction:', round(shareflip, 3), '\n')
