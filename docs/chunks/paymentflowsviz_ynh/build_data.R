# build_data.R
# Builds ynh_flows_data.js, the one data file index.html reads, from the three ONS zips.
# Run from the repo root:  Rscript docs/chunks/paymentflowsviz_ynh/build_data.R
#
# What it does (plan section 4):
#   1. extracts every monthly row touching Yorkshire and the Humber (cached in local/)
#   2. drops non-industries; keeps the four intermediary divisions, flagged
#   3. holds hidden months at the same cell's average published month
#   4. works out each industry's level in the supplier-to-buyer order, on the 77 industries and on all 81
#   5. finds the payment groups that hold across the years
#   6. sums payments to and from the rest of the UK for each industry and month
#   7. attaches names, sector groups and industrial strategy shares
#   8. writes the data file and prints checks against the test results
#
# Needs tests/00_lookups.R to have run (it writes tests/lookups/), and the R packages data.table,
# igraph and jsonlite. Units in the data file: GBP thousands (ONS rounds to 1,000) and payments (rounded to 100).
suppressPackageStartupMessages({ library(data.table); library(igraph); library(jsonlite) })
chunk <- 'docs/chunks/paymentflowsviz_ynh'
source(file.path(chunk, 'tests/functions_hierarchy.R'))          # hold_values(), flow_matrix(), trophic(), payment_groups()
set.seed(1)

small_threshold <- 0.5e6      # GBP a month of payments made plus received inside the region; below this an industry is "small"
# Payment group names are given by hand, keyed on one member each so they survive a re-run
group_names <- c('46' = 'Distribution', '25' = 'Engineering and metals', '43' = 'Construction',
                 '86' = 'Local and public services', '10' = 'Food')
sector_group <- function(section) fcase(section %in% c('A', 'B', 'D', 'E'), 'Primary and utilities', section == 'C', 'Manufacturing',
                                        section == 'F', 'Construction', section %in% c('G', 'H'), 'Trade and transport',
                                        section %in% c('J', 'K', 'L', 'M', 'N'), 'Business services',
                                        section %in% c('O', 'P', 'Q'), 'Public services', default = 'Hospitality, arts and other')
sector_groups <- c('Primary and utilities', 'Manufacturing', 'Construction', 'Trade and transport', 'Business services',
                   'Public services', 'Hospitality, arts and other')

# ---- 1 to 3. Extract, drop, hold ----------------------------------------------------------------
monthly_f <- file.path(local_dir, 'ynh_monthly.rds')
if (!file.exists(monthly_f)) source(file.path(chunk_dir, '01_extract_ynh_monthly.R'))
held_f <- file.path(local_dir, 'ynh_build_held.rds')
if (!file.exists(held_f) || file.mtime(held_f) < file.mtime(monthly_f)) {
  message('holding hidden months (about a minute)')
  x <- readRDS(monthly_f)
  drop <- c('00', not_industry)
  x <- x[!payer_sic %in% drop & !payee_sic %in% drop & payer_region != 'unknown' & payee_region != 'unknown']
  x <- hold_values(x, by = c('payer_sic', 'payer_region', 'payee_sic', 'payee_region'))
  saveRDS(x, held_f)
}
x <- readRDS(held_f)
x[, kind := fifelse(payer_region == 'TLE' & payee_region == 'TLE', 'inside', fifelse(payer_region == 'TLE', 'pays_out', 'paid_in'))]
months <- sort(unique(x$date)); nm <- length(months)
ins <- x[kind == 'inside']                                        # inside the region, intermediaries in, self-payments in
own <- ins[payer_sic != payee_sic]                                # between different industries: what the links show

# ---- 4. Industries and levels -------------------------------------------------------------------
all_sics <- sort(unique(c(own$payer_sic, own$payee_sic)))
centre <- function(tr, W) { diag(W) <- 0; w <- rowSums(W) + colSums(W); ok <- !is.na(tr$h); tr$h - weighted.mean(tr$h[ok], w[ok]) }   # 0 = the average industry
levels_for <- function(d, nodes) { W <- flow_matrix(d, 'value_held', nodes); tr <- trophic(W); list(h = centre(tr, W), F0 = tr$F0, up = tr$up_share) }
lv_all <- levels_for(own, all_sics)
ids <- names(lv_all$h)[!is.na(lv_all$h)]                          # industries in the connected network: the page's nodes
core <- setdiff(ids, intermediary)
own <- own[payer_sic %in% ids & payee_sic %in% ids]
own_core <- own[payer_sic %in% core & payee_sic %in% core]
lv_core <- levels_for(own_core, core); lv_all <- levels_for(own, ids)
# trailing 12-month levels, from the twelfth month on, for the detail panel
rolling <- function(d, nodes) {
  out <- matrix(NA_real_, length(nodes), nm, dimnames = list(nodes, NULL))
  for (m in 12:nm) out[, m] <- levels_for(d[date %in% months[(m - 11):m]], nodes)$h[nodes]
  out
}
roll_core <- rolling(own_core, core); roll_all <- rolling(own, ids)

Wc <- flow_matrix(own_core, 'value_held', core); per_month <- (rowSums(Wc) + colSums(Wc)) / nm
small <- names(per_month)[per_month < small_threshold]

# ---- 5. Payment groups --------------------------------------------------------------------------
set.seed(1)
pg <- payment_groups(own_core[, year := year(date)], core, small)$groups
pg[, name := { hit <- group_names[names(group_names) %in% sic2]; if (length(hit)) hit[[1]] else NA_character_ }, by = group]

# ---- 6. Rest of the UK, and payments within an industry ----------------------------------------
# Each series is split by whether the other side is an intermediary, so the page can add that part
# when intermediaries are switched in. London is kept separately for the detail panel.
series <- function(d, sic_col) {
  a <- d[get(sic_col) %in% ids, .(v = sum(value_held)), by = c(sic_col, 'date')]
  m <- matrix(0, length(ids), nm, dimnames = list(ids, NULL))
  m[cbind(match(a[[sic_col]], ids), match(a$date, months))] <- a$v
  round(m / 1e3)
}
out_pay <- x[kind == 'pays_out']; in_pay <- x[kind == 'paid_in']
ext <- list(
  buy_core = series(out_pay[!payee_sic %in% intermediary], 'payer_sic'), buy_int = series(out_pay[payee_sic %in% intermediary], 'payer_sic'),
  sell_core = series(in_pay[!payer_sic %in% intermediary], 'payee_sic'), sell_int = series(in_pay[payer_sic %in% intermediary], 'payee_sic'),
  buy_lon_core = series(out_pay[payee_region == 'TLI' & !payee_sic %in% intermediary], 'payer_sic'),
  buy_lon_int = series(out_pay[payee_region == 'TLI' & payee_sic %in% intermediary], 'payer_sic'),
  sell_lon_core = series(in_pay[payer_region == 'TLI' & !payer_sic %in% intermediary], 'payee_sic'),
  sell_lon_int = series(in_pay[payer_region == 'TLI' & payer_sic %in% intermediary], 'payee_sic'))
self <- series(ins[payer_sic == payee_sic], 'payer_sic')

# cells that exist but were never published in any month: links the page cannot show
cells <- own[, .(never = all(basis == 'never_published')), by = .(payer_sic, payee_sic)]
never_count <- function(s, set) cells[(payer_sic == s | payee_sic == s) & payer_sic %in% set & payee_sic %in% set, .(never = sum(never), cells = .N)]

# ---- 7. Nodes -----------------------------------------------------------------------------------
is8 <- fread(file.path(chunk_dir, 'lookups/indstrat_sic2.csv'), colClasses = list(character = 'sic2'))
is8_any <- fread(file.path(chunk_dir, 'lookups/indstrat_any_sic2.csv'), colClasses = list(character = 'sic2'))
r3 <- function(v) round(v, 3)
nodes <- lapply(ids, function(s) {
  lk <- sic2_lk[sic2 == s]; i8 <- is8[sic2 == s]; g <- pg[sic2 == s]; is_core <- s %in% core
  nc <- never_count(s, core); na <- never_count(s, ids)
  list(id = s, short = lk$short, name = lk$name, section = lk$section, section_name = lk$section_short, group = sector_group(lk$section),
       role = lk$role, small = s %in% small,
       level = if (is_core) r3(lv_core$h[[s]]) else NULL, level_all = r3(lv_all$h[[s]]),
       level_rolling = if (is_core) r3(unname(roll_core[s, ])) else NULL, level_all_rolling = r3(unname(roll_all[s, ])),
       pay_group = if (nrow(g) && g$status == 'core' && !is.na(g$name)) g$name else NULL,
       pay_group_loose = if (nrow(g) && !is.na(g$name)) g$name else NULL,
       pay_status = if (nrow(g)) g$status else 'not grouped', pay_group_years = if (nrow(g)) g$years_with_group else NULL,
       is8 = lapply(seq_len(nrow(i8)), function(k) list(name = i8$is8[k], share = i8$share_of_division_jobs[k], whole = i8$whole_division[k])),
       is8_any_share = if (s %in% is8_any$sic2) is8_any[sic2 == s, share_of_division_jobs_any] else 0,
       never_core = if (is_core) nc$never else NULL, cells_core = if (is_core) nc$cells else NULL, never_all = na$never, cells_all = na$cells,
       self = unname(self[s, ]),
       ext = lapply(ext, function(m) unname(m[s, ])))
})

# ---- Links: one row per ordered pair with any value, largest first ------------------------------
own[, m := match(date, months)]
pairs <- own[, .(total = sum(value_held)), by = .(payee_sic, payer_sic)][total > 0][order(-total)]
pairs[, k := .I]
own <- merge(own, pairs[, .(payee_sic, payer_sic, k)], by = c('payee_sic', 'payer_sic'))
V <- matrix(0, nrow(pairs), nm); N <- matrix(0, nrow(pairs), nm); B <- matrix('0', nrow(pairs), nm)
V[cbind(own$k, own$m)] <- round(own$value_held / 1e3); N[cbind(own$k, own$m)] <- round(own$n_held)
B[cbind(own$k, own$m)] <- fifelse(own$basis == 'published', 'p', fifelse(own$basis == 'never_published', 'x', 'h'))
links <- list(supplier = match(pairs$payee_sic, ids) - 1L, buyer = match(pairs$payer_sic, ids) - 1L,
              value = lapply(seq_len(nrow(V)), function(i) V[i, ]), count = lapply(seq_len(nrow(N)), function(i) N[i, ]),
              basis = apply(B, 1, paste, collapse = ''))

# ---- 8. Write -----------------------------------------------------------------------------------
dat <- list(built = format(Sys.Date()), source = 'ONS, Industry-to-industry payment flows by region, UK (editions of 17 September and 22 October 2026)',
            units = list(value = 'GBP thousands', count = 'payments'), months = format(months, '%Y-%m'),
            sector_groups = sector_groups, pay_groups = unname(group_names), is8_names = sort(unique(is8$is8)),
            nodes = nodes, links = links)
f <- file.path(chunk, 'ynh_flows_data.js')
writeLines(paste0('window.YNH_FLOWS = ', toJSON(dat, auto_unbox = TRUE, digits = NA, null = 'null', na = 'null'), ';'), f)
system(paste('gzip -k -9 -f', shQuote(f))); gz <- file.size(paste0(f, '.gz')); unlink(paste0(f, '.gz'))

# ---- Checks (plan section 4.3) ------------------------------------------------------------------
bn <- function(v) round(sum(v) / 1e9, 1)
core_x <- x[!payer_sic %in% intermediary & !payee_sic %in% intermediary]
io <- fread(file.path(chunk_dir, 'outputs/levels_payments_vs_io.csv'), colClasses = list(character = 'g'))
io_rho <- cor(lv_core$h[io$g], io$io, method = 'spearman', use = 'complete.obs')
chk <- data.table(
  check = c('industries (and of those, intermediaries)', 'links', 'months', 'inside total, GBP bn (77 industries, self-payments in)',
            'paid to the rest of the UK, GBP bn', 'received from the rest of the UK, GBP bn', 'incoherence score, 77 industries',
            'share of money running down the order', 'incoherence score, all 81', 'rank correlation with input-output levels',
            'payment groups', 'core divisions', 'small divisions', 'link-months with a value but no row', 'file size, MB (compressed)'),
  value = c(sprintf('%d (%d)', length(ids), sum(ids %in% intermediary)), nrow(pairs), nm, bn(core_x[kind == 'inside', value_held]),
            bn(core_x[kind == 'pays_out', value_held]), bn(core_x[kind == 'paid_in', value_held]), round(lv_core$F0, 3), round(lv_core$up, 3),
            round(lv_all$F0, 3), round(io_rho, 2), pg[status == 'core', uniqueN(name)], pg[status == 'core', .N], length(small),
            sum(V > 0 & B == '0'), sprintf('%.2f (%.2f)', file.size(f) / 1e6, gz / 1e6)),
  expected = c('81 (4)', '3627', '93', '79.0', '265.2', '230.2', '0.743', '0.712', '0.773', 'about 0.7', '5', '43', '11', '0', 'about 3'))
print(chk)
cat('\nCore members by payment group:\n')
for (g in unname(group_names)) cat(sprintf('  %s: %s\n', g, paste(sic2_lk[match(pg[name == g & status == 'core', sic2], sic2), short], collapse = ', ')))
cat('Small:', paste(sic2_lk[match(small, sic2), short], collapse = ', '), '\n')
