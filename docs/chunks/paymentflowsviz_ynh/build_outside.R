# build_outside.R
# Builds data/outside.js: what other sources say about the links the payments can't show, for the page's section
# "What the payments leave out", its side panel block and its dotted lines (plan/other_connections_plan.md, sections 7.1
# and 11). One small file for all regions, loaded with the page.
#
# It holds, for each region:
#   edge       ONS's estimates of goods and services crossing the region's edge, by industry group: goods to and from
#              other UK regions (2021) and other countries (2023), services to and from other countries (2023).
#              From tests/outputs/trade_at_the_edge.csv (tests/27).
#   accounts   what the accounts of employers of 1,000 or more registered there say, by industry group: how many were
#              read, the share of turnover sold outside the UK, how many say they buy from abroad, their main kinds of
#              customer. The same for all of Great Britain, to set beside it.
#   companies  those companies, one row each: name, Companies House number, division, turnover, share sold outside the
#              UK where it passed its checks, main kind of customer. Public accounts, named.
#   hidden     pairs of industries whose payments inside the region ONS hides in every month or most, where companies
#              registered there in the supplying industry name that kind of customer as their main one.
#   gov        divisions with companies whose main customer is government, whose payments have no region.
# The accounts part comes from local/paymentflowsviz_ynh/accounts_all_results.csv, which tests/30 writes. That file is
# not in the repo (it is the model's reading of each company), so this needs test 30 to have been run on this machine.
#
# Run from the repo root: Rscript docs/chunks/paymentflowsviz_ynh/build_outside.R
suppressPackageStartupMessages({library(data.table); library(jsonlite)})
chunk <- 'docs/chunks/paymentflowsviz_ynh'; local_dir <- 'local/paymentflowsviz_ynh'
edge_file <- file.path(chunk, 'tests/outputs/trade_at_the_edge.csv'); acc_file <- file.path(local_dir, 'accounts_all_results.csv'); own_file <- file.path(local_dir, 'all_regions_own_held.rds')
stopifnot(file.exists(edge_file), file.exists(acc_file), file.exists(own_file))
sic2_lk <- fread(file.path(chunk, 'tests/lookups/sic2_lookup.csv'), colClasses = 'character')
regions <- c('TLC', 'TLD', 'TLE', 'TLF', 'TLG', 'TLH', 'TLI', 'TLJ', 'TLK', 'TLL', 'TLM', 'TLN')

# ---- the nine industry groups ONS uses for regional trade; every division is in one
groups <- list(prod = c('Non-manufacturing production', 'farming, mining, energy, water, waste and construction'), manuf = c('Manufacturing', 'manufacturing'),
               whole = c('Wholesale and motor trades', 'wholesale and motor trades'), retail = c('Retail', 'retail'),
               transp = c('Transport, storage, accommodation and food', 'transport, storage, accommodation and food service'), info = c('Information and communication', 'information and communication'),
               fin = c('Finance and insurance', 'finance and insurance'), prof = c('Professional, scientific and technical', 'professional, scientific and technical services'), other = c('Other services', 'other services'))
group_of <- function(s) { n <- as.integer(s)
  fcase(is.na(n), NA_character_, n <= 9 | (n >= 35 & n <= 43), 'prod', n <= 33, 'manuf', n %in% 45:46, 'whole', n == 47, 'retail', n <= 56, 'transp', n <= 63, 'info', n <= 66, 'fin', n >= 69 & n <= 75, 'prof', default = 'other') }
r3 <- function(v) ifelse(is.finite(v), round(v, 3), NA)

# ---- 1. the region's edge, from ONS
e <- fread(edge_file)
goods_between <- c('prod', 'manuf', 'whole', 'retail', 'transp', 'info')          # groups defined the same way in both of ONS's sets of estimates
edge <- lapply(setNames(regions, regions), function(r) lapply(setNames(names(groups), names(groups)), function(g) { x <- e[region == r & group == g]; if (!nrow(x)) return(NULL)
  b <- g %in% goods_between
  list(gi_uk = if (b) r3(x$ons_rest_uk_buys_goods_2021) else NA, go_uk = if (b) r3(x$ons_rest_uk_sells_goods_2021) else NA, gi_ab = r3(x$imports_goods_2023), go_ab = r3(x$exports_goods_2023),
       si_ab = r3(x$imports_services_2023), so_ab = r3(x$exports_services_2023)) }))

# ---- 2. the accounts
a <- fread(acc_file, colClasses = list(character = c('company_number', 'sic', 'sic2')))
a[region == '', region := NA][sic2 == '', sic2 := NA][, group := group_of(sic2)]
kinds <- c('households and consumers', 'retailers and supermarkets', 'wholesalers and distributors', 'motor dealers', 'manufacturers', 'construction and housebuilders', 'farmers and growers',
           'energy, water and waste companies', 'transport and logistics companies', 'hotels, restaurants, pubs and caterers', 'banks, insurers and financial firms',
           'technology, media and telecoms companies', 'professional and business services firms', 'property owners and landlords', 'central or local government', 'health and social care bodies',
           'schools, colleges and universities', 'defence', 'own group companies', 'other or mixed businesses')
a[, kind := match(customer_1, kinds) - 1L]                                                   # 0-based, for the page
# a company's name as Companies House has it is in capitals; put it in title case
nice_name <- function(x) { s <- gsub("(^|[\\s()/&,-])([a-z])", '\\1\\U\\2', tolower(x), perl = TRUE)        # a capital at the start of each word
  s <- gsub('(?<=\\b[a-zA-Z]\\.)([a-z])', '\\U\\1', s, perl = TRUE); s <- gsub('\\b([a-z])\\.', '\\U\\1.', s, perl = TRUE)   # initials: t.j.smith to T.J.Smith
  s <- gsub('\\bMc([a-z])', 'Mc\\U\\1', s, perl = TRUE)
  # a short word with no vowels is almost always initials (FCC, DB, JCT), so it goes back to capitals
  m <- gregexpr('\\b[B-DF-HJ-NP-TV-Z][b-df-hj-np-tv-z]{1,3}\\b', s); regmatches(s, m) <- lapply(regmatches(s, m), function(v) ifelse(tolower(v) %in% c('ltd', 'plc', 'llp', 'st', 'dr', 'mr', 'mrs', 'ms'), v, toupper(v)))
  for (w in c('And', 'Of', 'The', 'For', 'In', 'On', 'At', 'By', 'De', 'La')) s <- gsub(sprintf('(?<=.)\\b%s\\b', w), tolower(w), s, perl = TRUE)
  for (p in list(c('\\bUk\\b', 'UK'), c('\\bPlc\\b', 'plc'), c('\\bLlp\\b', 'LLP'), c('\\bNhs\\b', 'NHS'), c('\\bGb\\b', 'GB'), c('\\bCic\\b', 'CIC'), c('\\bC\\.i\\.c\\b', 'C.I.C'), c('\\bIi\\b', 'II'), c('\\bBbc\\b', 'BBC'),
                 c('\\bIt\\b', 'IT'), c('\\bLtd\\b', 'Ltd'), c(',(?=\\S)', ', '))) s <- gsub(p[1], p[2], s, perl = TRUE)
  trimws(s) }
a[, name := nice_name(reg_name)]
agg <- function(d) { has <- !is.na(d$outside_uk); hw <- has & is.finite(d$w); n <- sum(has); k <- d[!is.na(kind), .N, by = kind][order(-N)][seq_len(min(3, .N))]
  list(n = nrow(d), n_share = n, turnover_bn = r3(sum(d$w[hw]) / 1e9), out_w = if (n >= 5) r3(sum((d$outside_uk * d$w)[hw]) / sum(d$w[hw])) else NA, out_med = if (n >= 5) r3(median(d$outside_uk[has])) else NA,
       buy = sum(d$buys_abroad == 'yes'), own = sum(d$mostly_own_group %in% TRUE), n_kind = sum(!is.na(d$kind)), cust = lapply(seq_len(nrow(k)), function(i) c(k$kind[i], k$N[i]))) }
by_group <- function(d) lapply(setNames(names(groups), names(groups)), function(g) if (d[group == g, .N]) agg(d[group == g]) else NULL)
uk_accounts <- by_group(a)
co_rows <- function(d) lapply(seq_len(nrow(d)), function(i) list(d$name[i], d$company_number[i], d$sic2[i], if (is.finite(d$w[i])) round(d$w[i] / 1e6) else NA, if (is.finite(d$outside_uk[i])) round(100 * d$outside_uk[i], 1) else NA,
                                                                 if (is.na(d$kind[i])) NA else d$kind[i], as.integer(d$customers_basis[i] == 'stated'), as.integer(d$mostly_own_group[i] %in% TRUE), as.integer(d$buys_abroad[i] == 'yes')))

# ---- 3. hidden links the accounts vouch for: a kind of customer that is one division, hidden inside the region in every month or most
kind_sic <- c('retailers and supermarkets' = '47', 'wholesalers and distributors' = '46', 'motor dealers' = '45', 'property owners and landlords' = '68', 'central or local government' = '84',
              'schools, colleges and universities' = '85')
cells <- readRDS(own_file)[, .(months = .N, published = sum(!suppressed)), by = .(region = payer_region, buyer = payer_sic, supplier = payee_sic)]
v <- a[!is.na(region) & !is.na(sic2) & customer_1 %in% names(kind_sic), .(n = .N, turnover_bn = r3(sum(w, na.rm = TRUE) / 1e9), names = list(head(name[order(-w)], 6))), by = .(region, supplier = sic2, kind = customer_1)]
v[, buyer := kind_sic[kind]]; v <- merge(v[buyer != supplier], cells, by = c('region', 'buyer', 'supplier'))
v[, every := published == 0]; v <- v[published / months < 0.25]
hidden <- function(r) { h <- v[region == r & buyer != '84' & n >= 2][order(-n)]
  lapply(seq_len(nrow(h)), function(i) list(s = h$supplier[i], b = h$buyer[i], kind = h$kind[i], n = h$n[i], turnover_bn = h$turnover_bn[i], every = h$every[i], published = h$published[i], months = h$months[i], names = h$names[[i]])) }
gov <- function(r) { g <- a[region == r & !is.na(sic2) & customer_1 %in% c('central or local government', 'defence'), .N, by = sic2]; as.list(setNames(g$N, g$sic2)) }

out <- list(built = format(Sys.Date()), accounts_read = nrow(a), accounts_with_share = a[!is.na(outside_uk), .N],
            groups = lapply(groups, function(g) list(name = g[1], words = g[2])), group_of = as.list(setNames(group_of(sic2_lk$sic2), sic2_lk$sic2)), kinds = kinds, uk = uk_accounts,
            regions = lapply(setNames(regions, regions), function(r) { d <- a[region %in% r][order(-w)]
              list(edge = edge[[r]], n = nrow(d), accounts = by_group(d), companies = co_rows(d), hidden = hidden(r), gov = gov(r)) }))
f <- file.path(chunk, 'data/outside.js')
writeLines(paste0('window.FLOWS_OUTSIDE = ', toJSON(out, auto_unbox = TRUE, na = 'null', null = 'null', digits = NA), ';'), f, useBytes = TRUE)
cat(sprintf('Wrote %s: %.0f KB. %d companies read, %d with a share of turnover outside the UK.\n', f, file.size(f) / 1024, nrow(a), a[!is.na(outside_uk), .N]))
cat('Companies by region:', paste(sprintf('%s %d', regions, vapply(regions, function(r) a[region %in% r, .N], 0L)), collapse = ', '), '\n')
cat('Hidden links the accounts vouch for (two or more companies, not counting government as the buyer):\n')
nm <- setNames(sic2_lk$short, sic2_lk$sic2); hv <- v[buyer != '84' & n >= 2][order(region, -n)]
print(hv[, .(region, supplier = nm[supplier], buyer = nm[buyer], companies = n, turnover_bn, published, months)])
cat('A sample of names as they will show:', paste(head(a[region %in% 'TLE'][order(-w), name], 8), collapse = '; '), '\n')
