# build_data.R
# Builds the data files index.html reads, from the three ONS zips:
#   data/flows_<region>.js   one for each ITL1 region built: its industries, links, levels, groups and quoted figures
#   data/regions.js          a small index of all twelve regions: data quality, how alike they are, chain or web
# Run from the repo root:
#   Rscript docs/chunks/paymentflowsviz_ynh/build_data.R                 the regions listed in `sample_regions`
#   Rscript docs/chunks/paymentflowsviz_ynh/build_data.R TLE TLD         named regions
#   Rscript docs/chunks/paymentflowsviz_ynh/build_data.R all             all twelve
#   Rscript docs/chunks/paymentflowsviz_ynh/build_data.R index           only the index
#   Rscript docs/chunks/paymentflowsviz_ynh/build_data.R stage TLF TLG   build without touching the site: files go to
#                                                                        local/paymentflowsviz_ynh/staged_data/, which ends up a complete
#                                                                        candidate for data/ (regions already on the site are copied in)
#
# For each region (plan sections 4 and 10.2):
#   1. extracts every monthly row touching it (cached in local/)       5. finds its payment groups
#   2. drops non-industries; keeps the four intermediaries, flagged     6. sums payments to and from the rest of the UK
#   3. holds hidden months at the cell's average published month        7. attaches names, sector groups, industrial strategy shares
#   4. works out levels in the supplier-to-buyer order                  8. works out the figures the page's text quotes
# Needs the R packages data.table, igraph and jsonlite, and tests/lookups/sic2_lookup.csv (from tests/00_lookups.R).
# Units in the data files: GBP thousands (ONS rounds to 1,000) and payments (rounded to 100).
suppressPackageStartupMessages({ library(data.table); library(igraph); library(jsonlite) })
chunk <- 'docs/chunks/paymentflowsviz_ynh'
source(file.path(chunk, 'tests/functions_hierarchy.R'))          # hold_values(), flow_matrix(), trophic(), null_F0(), payment_groups(), chain_measures(), flow_distance()

sample_regions <- c('TLE', 'TLD', 'TLC')      # built when no region is named: Y&H, a well-published region, and the most hidden one
small_threshold <- 0.5e6                      # GBP a month made plus received inside the region; below this an industry is "small"
thin_never_share <- 0.35                      # a region is "thin" when this share of its monthly figures sit in links never published
min_group_core <- 3                           # a payment group needs this many core industries to be shown as a group
count_unusable_share <- 0.25                  # above this share of value in links with no recorded payments, payment counts are not relied on
io_incoherence <- 0.71                        # the UK input-output table's score on the same method (tests/03_io_check.R)
regions <- data.table(
  code = c('TLC', 'TLD', 'TLE', 'TLF', 'TLG', 'TLH', 'TLI', 'TLJ', 'TLK', 'TLL', 'TLM', 'TLN'),
  name = c('North East', 'North West', 'Yorkshire and the Humber', 'East Midlands', 'West Midlands', 'East of England', 'London', 'South East',
           'South West', 'Wales', 'Scotland', 'Northern Ireland'),
  the_name = c('the North East', 'the North West', 'Yorkshire and the Humber', 'the East Midlands', 'the West Midlands', 'the East of England', 'London',
               'the South East', 'the South West', 'Wales', 'Scotland', 'Northern Ireland'),
  short = c('North East', 'North West', 'Yorkshire & Humber', 'East Midlands', 'West Midlands', 'East', 'London', 'South East', 'South West', 'Wales', 'Scotland', 'N. Ireland'))
# Payment group names given by hand, keyed on one member each so they survive a re-run. Only Yorkshire and the Humber's
# groups have been looked at and named; other regions' groups are labelled by their largest members.
hand_names <- list(TLE = c('46' = 'Distribution', '25' = 'Engineering and metals', '43' = 'Construction', '86' = 'Local and public services', '10' = 'Food'))
sector_group <- function(section) fcase(section %in% c('A', 'B', 'D', 'E'), 'Primary and utilities', section == 'C', 'Manufacturing',
                                        section == 'F', 'Construction', section %in% c('G', 'H'), 'Trade and transport',
                                        section %in% c('J', 'K', 'L', 'M', 'N'), 'Business services',
                                        section %in% c('O', 'P', 'Q'), 'Public services', default = 'Hospitality, arts and other')
sector_groups <- c('Primary and utilities', 'Manufacturing', 'Construction', 'Trade and transport', 'Business services', 'Public services', 'Hospitality, arts and other')

raw_dir <- path.expand('~/Code/claude/whole_economy_thinking/local/tradeable_jobs/data_raw/payments')
zips <- file.path(raw_dir, c('industryregionbusinesspayments1921dataset17092026.zip', 'industryregionbusinesspayments2224dataset17092026.zip',
                             'industryregionbusinesspayments2526dataset22102026.zip'))
ret <- path.expand('~/Code/RegionalEconomicTools')
data_dir <- file.path(chunk, 'data'); dir.create(data_dir, showWarnings = FALSE)      # what the site serves
out_dir <- data_dir                                                                  # where this run writes; changed by `stage`
dir.create(local_dir, recursive = TRUE, showWarnings = FALSE)
pct <- function(v, d = 0) paste0(formatC(100 * v, format = 'f', digits = d), '%')
num <- function(v, d = 2) formatC(v, format = 'f', digits = d)
rho <- function(a, b) suppressWarnings(cor(a, b, method = 'spearman', use = 'complete.obs'))

# ---- 1 to 3. Extract, drop, hold -------------------------------------------------------------------
monthly_for <- function(code) {
  f <- file.path(local_dir, sprintf('monthly_%s.rds', code))
  old <- file.path(local_dir, 'ynh_monthly.rds'); if (code == 'TLE' && !file.exists(f) && file.exists(old)) file.copy(old, f)
  if (!file.exists(f)) {
    stopifnot(all(file.exists(zips))); message(code, ': extracting from the ONS files (about 90 seconds)')
    x <- rbindlist(lapply(zips, function(z) {     # grep keeps only rows naming the region, before R sees them
      d <- fread(cmd = paste('unzip -p', shQuote(z), sprintf("| grep -F ',%s,'", code)), colClasses = 'character', header = FALSE, showProgress = FALSE)
      setnames(d, c('payer_sic', 'payer_region', 'payee_sic', 'payee_region', 'date', 'value', 'n'))
      d <- d[payer_region == code | payee_region == code]
      d[, suppressed := value == '[C]'][, `:=`(value = suppressWarnings(as.numeric(value)), n = suppressWarnings(as.numeric(n)))]
      d[, date := as.Date(date, format = '%d-%b-%y')][] }))
    stopifnot(!anyNA(x$date), !anyDuplicated(x[, .(payer_sic, payer_region, payee_sic, payee_region, date)]))
    saveRDS(x, f)
  }
  f
}
held_for <- function(code) {
  mf <- monthly_for(code); f <- file.path(local_dir, sprintf('held_%s.rds', code))
  old <- file.path(local_dir, 'ynh_build_held.rds'); if (code == 'TLE' && !file.exists(f) && file.exists(old)) file.copy(old, f)
  if (!file.exists(f) || file.mtime(f) < file.mtime(mf)) {
    message(code, ': holding hidden months (about a minute)')
    x <- readRDS(mf); drop <- c('00', not_industry)
    x <- x[!payer_sic %in% drop & !payee_sic %in% drop & payer_region != 'unknown' & payee_region != 'unknown']
    saveRDS(hold_values(x, by = c('payer_sic', 'payer_region', 'payee_sic', 'payee_region')), f)
  }
  readRDS(f)
}

# ---- What the page leaves out -----------------------------------------------------------------------
# The page shows payments where both sides have a known industry and a known region. Rows with an unknown industry (00)
# or an unknown region are dropped above, and the intermediaries are off unless switched in. Public administration
# almost never has a region, so nearly all of its payments go with the unknown-region rows. This measures, for the
# region's industries, how much of what they were paid and what they paid out sits in each of those.
# Published values only: hidden months of dropped rows are not held. (tests/19 has the same figures for Y&H.)
left_out <- function(code, core) {
  m <- readRDS(monthly_for(code)); here <- code
  other_side <- function(sic, reg) fifelse(sic == '00', 'unknown_industry', fifelse(reg == 'unknown', fifelse(sic == '84', 'gov', 'other'),
                  fifelse(sic %in% intermediary, 'intermediary', fifelse(sic %in% not_industry, 'other', fifelse(reg == here, 'inside', 'elsewhere')))))
  shares <- function(d) { s <- d[, .(v = sum(value, na.rm = TRUE)), by = side]; function(k) s[side %in% k, sum(v)] / sum(s$v) }
  rec <- shares(m[payee_region == here & payee_sic %in% core][, side := other_side(payer_sic, payer_region)])
  pay <- shares(m[payer_region == here & payer_sic %in% core][, side := other_side(payee_sic, payee_region)])
  ps <- shares(m[payee_region == here & payee_sic %in% intersect(core, c('85', '86', '87', '88'))][, side := other_side(payer_sic, payer_region)])
  gov <- m[payer_sic == '84' & payer_region == 'unknown' & payee_region == here & payee_sic %in% core, sum(value, na.rm = TRUE)] / 1e9
  list(shown_received = pct(rec(c('inside', 'elsewhere'))), inside_received = pct(rec('inside')), elsewhere_received = pct(rec('elsewhere')),
       unknown_received = pct(rec('unknown_industry')), inter_received = pct(rec('intermediary')), gov_received = pct(rec('gov')),
       shown_paid = pct(pay(c('inside', 'elsewhere'))), unknown_paid = pct(pay('unknown_industry')),
       gov_paid = sprintf('£%s billion', num(gov, if (gov >= 10) 0 else 1)),
       ps_shown = pct(ps(c('inside', 'elsewhere'))), ps_unknown = pct(ps('unknown_industry')), ps_gov = pct(ps('gov')))
}

# ---- Industrial strategy shares, region by region ---------------------------------------------------
# The strategy's list mixes 2 to 5-digit codes; each division gets the share of its jobs in the region that sits in the
# listed codes. Jobs: BRES employment by ITL3 area and five-digit SIC, latest year, summed to ITL1. Great Britain only,
# so Northern Ireland gets none and the page leaves the highlight out there.
is8_short <- c('Advanced Manufacturing' = 'Advanced manufacturing', 'Creative Industries' = 'Creative', 'Defence' = 'Defence',
               'Digital and Technologies' = 'Digital & tech', 'Financial Services' = 'Financial services', 'Life Sciences' = 'Life sciences',
               'Professional and Business Services' = 'Professional & business services', 'Foundational industries and inputs' = 'Foundational industries')
jobs_by_region <- local({ cache <- NULL; function() {
  if (is.null(cache)) {
    f <- file.path(local_dir, 'bres_itl1_sic5.rds')
    if (!file.exists(f)) {
      src <- list.files(file.path(ret, 'local/data/BRES/separate_SIC_types_summedfrom5digitSIC'), pattern = 'TYPE428.*4_Employment.*SIC_5DIGIT', full.names = TRUE)[1]
      b <- fread(src, select = c('DATE', 'GEOGRAPHY_CODE', 'SIC_5DIGIT_CODE', 'JOBCOUNT'), colClasses = list(character = 'SIC_5DIGIT_CODE'))[DATE == max(DATE)]
      saveRDS(b[, .(jobs = sum(JOBCOUNT, na.rm = TRUE)), by = .(year = DATE, itl1 = substr(GEOGRAPHY_CODE, 1, 3), sic5 = sprintf('%05d', as.integer(SIC_5DIGIT_CODE)))], f)
    }
    cache <<- readRDS(f)
  }
  cache } })
is8_for <- function(code) {
  defs <- fread(file.path(ret, 'data/industrialstrategy2025sectordefs.csv'), colClasses = 'character')
  defs <- defs[!is.na(sic_code) & sic_code != 'NA' & sic_code != ''][, is8 := is8_short[sector_name]]
  j5 <- jobs_by_region()[itl1 == code][, sic2 := substr(sic5, 1, 2)]
  if (!nrow(j5)) return(list(memb = data.table(sic2 = character(), is8 = character(), share = numeric(), whole = logical()), any = data.table(sic2 = character(), share = numeric()), year = NA))
  j2 <- j5[, .(div = sum(jobs)), by = sic2]
  under <- function(pre) j5[Reduce(`|`, lapply(pre, function(p) startsWith(sic5, p)))]
  memb <- rbindlist(lapply(unique(defs$is8), function(s) { pre <- unique(defs[is8 == s, sic_code]); under(pre)[, .(is8 = s, jobs = sum(jobs), whole = sic2[1] %in% pre), by = sic2] }))
  memb <- merge(memb, j2, by = 'sic2')[div > 0][, share := round(jobs / div, 3)]
  any <- merge(under(unique(defs$sic_code))[, .(jobs = sum(jobs)), by = sic2], j2, by = 'sic2')[div > 0][, share := round(jobs / div, 3)]
  list(memb = memb[, .(sic2, is8, share, whole)], any = any[, .(sic2, share)], year = j5$year[1])
}

# ---- Foundational economy zones, region by region -------------------------------------------------
# The Foundational Economy Collective's list places each five-digit code in a zone (tests/17 reads it; plan section 11).
# Each division gets the share of its jobs in the region that sits in each zone. Where the region has no jobs recorded
# for a division, or no jobs data at all (Northern Ireland), the Great Britain shares stand in: test 18 found they give
# a region's totals to within a point. A division with no jobs anywhere takes its share of the list's codes.
fe_for <- function(code) {
  fe <- fread(file.path(chunk_dir, 'lookups/foundational_sic5.csv'), colClasses = 'character')
  zs <- c('Material', 'Providential', 'Overlooked', 'Other')
  wide <- function(d, v) { w <- dcast(d, sic2 ~ zone, value.var = v, fun.aggregate = sum, fill = 0); for (z in zs) if (!z %in% names(w)) w[, (z) := 0]
    w[, tot := Material + Providential + Overlooked + Other][tot > 0, .(sic2, material = Material / tot, providential = Providential / tot, overlooked = Overlooked / tot)] }
  j <- merge(jobs_by_region(), fe[, .(sic5, zone)], by = 'sic5', all.x = TRUE)
  j[sic5 == '01000', zone := 'Material'][is.na(zone), zone := 'Other'][, sic2 := substr(sic5, 1, 2)]   # 01000 is BRES's farm total
  own <- wide(j[itl1 == code], 'jobs'); gb <- wide(j, 'jobs'); by_code <- wide(fe[, .(sic2 = substr(sic5, 1, 2), zone, n = 1)], 'n')
  share <- function(s) { for (t in list(own, gb, by_code)) if (s %in% t$sic2) return(round(unlist(t[sic2 == s, .(material, providential, overlooked)], use.names = FALSE), 3)); c(0, 0, 0) }
  tot <- j[itl1 == code, .(jobs = sum(jobs)), by = zone]
  list(share = share, regional = nrow(own) > 0, year = j$year[1], jobs = if (nrow(tot)) setNames(tot$jobs / sum(tot$jobs), tot$zone) else NULL)
}

# ---- One region ------------------------------------------------------------------------------------
build_region <- function(code) {
  set.seed(1); t0 <- Sys.time(); this <- code; reg <- regions[code == this]      # `this`, because `code` is also a column of regions
  x <- held_for(code)
  x[, kind := fifelse(payer_region == code & payee_region == code, 'inside', fifelse(payer_region == code, 'pays_out', 'paid_in'))]
  months <- sort(unique(x$date)); nm <- length(months)
  ins <- x[kind == 'inside']; own <- ins[payer_sic != payee_sic]                 # inside the region; and of that, between different industries

  # 4. Industries and levels. Levels are measured from the flow-weighted average industry.
  centre <- function(tr, W) { diag(W) <- 0; w <- rowSums(W) + colSums(W); ok <- !is.na(tr$h); tr$h - weighted.mean(tr$h[ok], w[ok]) }
  levels_for <- function(d, nodes, col = 'value_held') { W <- flow_matrix(d, col, nodes); tr <- trophic(W); list(h = centre(tr, W), F0 = tr$F0, up = tr$up_share, W = W) }
  all_sics <- sort(unique(c(own$payer_sic, own$payee_sic)))
  ids <- { h <- levels_for(own, all_sics)$h; names(h)[!is.na(h)] }             # industries in the connected network: the page's nodes
  core <- setdiff(ids, intermediary)
  own <- own[payer_sic %in% ids & payee_sic %in% ids]; own_core <- own[payer_sic %in% core & payee_sic %in% core]
  lv_core <- levels_for(own_core, core); lv_all <- levels_for(own, ids)
  rolling <- function(d, nodes) { out <- matrix(NA_real_, length(nodes), nm, dimnames = list(nodes, NULL))
    for (m in 12:nm) out[, m] <- levels_for(d[date %in% months[(m - 11):m]], nodes)$h[nodes]; out }
  roll_core <- rolling(own_core, core); roll_all <- rolling(own, ids)
  Wc <- lv_core$W; diag(Wc) <- 0; per_month <- (rowSums(Wc) + colSums(Wc)) / nm
  small <- names(per_month)[per_month < small_threshold]

  # 5. Payment groups, and what to call them
  set.seed(1)
  pg <- payment_groups(own_core[, year := year(date)], core, small)$groups
  pg[, flow := per_month[sic2]]
  tiny <- pg[status == 'core', .N, by = group][N < min_group_core, group]     # one or two industries are not a group: they float
  pg[group %in% tiny & status == 'core', status := 'floating']
  hn <- hand_names[[code]]
  label <- function(g) {
    members <- pg[group == g & status == 'core'][order(-flow)]; if (!nrow(members)) return(NA_character_)
    if (!is.null(hn)) { hit <- hn[names(hn) %in% members$sic2]; if (length(hit)) return(hit[[1]]) }
    nmz <- sic2_lk[match(members$sic2, sic2), short]
    if (length(nmz) <= 2) paste(nmz, collapse = ' and ') else sprintf('%s, %s and %d more', nmz[1], nmz[2], length(nmz) - 2)
  }
  pg[, name := label(group[1]), by = group]
  group_order <- pg[status == 'core', .(flow = sum(flow)), by = name][order(-flow), name]

  # 6. Rest of the UK, and payments within an industry. Each series is split by whether the other side is an
  # intermediary, so the page can add that part when intermediaries are switched in. One partner region is kept
  # separately for the side panel: London, or for London itself its largest partner.
  series <- function(d, sic_col) { a <- d[get(sic_col) %in% ids, .(v = sum(value_held)), by = c(sic_col, 'date')]
    m <- matrix(0, length(ids), nm, dimnames = list(ids, NULL)); m[cbind(match(a[[sic_col]], ids), match(a$date, months))] <- a$v; round(m / 1e3) }
  out_pay <- x[kind == 'pays_out']; in_pay <- x[kind == 'paid_in']
  partner <- if (code != 'TLI') 'TLI' else rbind(out_pay[, .(v = sum(value_held)), by = .(r = payee_region)], in_pay[, .(v = sum(value_held)), by = .(r = payer_region)])[, .(v = sum(v)), by = r][order(-v), r[1]]
  ext <- list(
    buy_core = series(out_pay[!payee_sic %in% intermediary], 'payer_sic'), buy_int = series(out_pay[payee_sic %in% intermediary], 'payer_sic'),
    sell_core = series(in_pay[!payer_sic %in% intermediary], 'payee_sic'), sell_int = series(in_pay[payer_sic %in% intermediary], 'payee_sic'),
    buy_lon_core = series(out_pay[payee_region == partner & !payee_sic %in% intermediary], 'payer_sic'),
    buy_lon_int = series(out_pay[payee_region == partner & payee_sic %in% intermediary], 'payer_sic'),
    sell_lon_core = series(in_pay[payer_region == partner & !payer_sic %in% intermediary], 'payee_sic'),
    sell_lon_int = series(in_pay[payer_region == partner & payer_sic %in% intermediary], 'payee_sic'))
  self <- series(ins[payer_sic == payee_sic], 'payer_sic')
  cells <- own[, .(never = all(basis == 'never_published')), by = .(payer_sic, payee_sic)]      # never published: links the page cannot show
  never_count <- function(s, set) cells[(payer_sic == s | payee_sic == s) & payer_sic %in% set & payee_sic %in% set, .(never = sum(never), cells = .N)]

  # 7. Nodes
  i8 <- is8_for(code); r3 <- function(v) round(v, 3); fz <- fe_for(code)
  nodes <- lapply(ids, function(s) {
    lk <- sic2_lk[sic2 == s]; mb <- i8$memb[sic2 == s]; g <- pg[sic2 == s]; is_core <- s %in% core
    nc <- never_count(s, core); na <- never_count(s, ids)
    list(id = s, short = lk$short, name = lk$name, section = lk$section, section_name = lk$section_short, group = sector_group(lk$section),
         role = lk$role, small = s %in% small,
         level = if (is_core) r3(lv_core$h[[s]]) else NULL, level_all = r3(lv_all$h[[s]]),
         level_rolling = if (is_core) r3(unname(roll_core[s, ])) else NULL, level_all_rolling = r3(unname(roll_all[s, ])),
         pay_group = if (nrow(g) && g$status == 'core' && !is.na(g$name)) g$name else NULL,
         pay_group_loose = if (nrow(g) && !is.na(g$name)) g$name else NULL,
         pay_status = if (nrow(g)) g$status else 'not grouped', pay_group_years = if (nrow(g)) g$years_with_group else NULL,
         is8 = lapply(seq_len(nrow(mb)), function(k) list(name = mb$is8[k], share = mb$share[k], whole = mb$whole[k])),
         is8_any_share = if (s %in% i8$any$sic2) i8$any[sic2 == s, share] else 0,
         fe = fz$share(s),                                  # share of its jobs in the material, providential and overlooked zones
         never_core = if (is_core) nc$never else NULL, cells_core = if (is_core) nc$cells else NULL, never_all = na$never, cells_all = na$cells,
         self = unname(self[s, ]), ext = lapply(ext, function(m) unname(m[s, ])))
  })

  # Links: one row per ordered pair with any value, largest first
  own[, m := match(date, months)]
  pairs <- own[, .(total = sum(value_held)), by = .(payee_sic, payer_sic)][total > 0][order(-total)][, k := .I]
  ownk <- merge(own, pairs[, .(payee_sic, payer_sic, k)], by = c('payee_sic', 'payer_sic'))
  V <- matrix(0, nrow(pairs), nm); N <- matrix(0, nrow(pairs), nm); B <- matrix('0', nrow(pairs), nm)
  V[cbind(ownk$k, ownk$m)] <- round(ownk$value_held / 1e3); N[cbind(ownk$k, ownk$m)] <- round(ownk$n_held)
  B[cbind(ownk$k, ownk$m)] <- fifelse(ownk$basis == 'published', 'p', fifelse(ownk$basis == 'never_published', 'x', 'h'))
  links <- list(supplier = match(pairs$payee_sic, ids) - 1L, buyer = match(pairs$payer_sic, ids) - 1L,
                value = lapply(seq_len(nrow(V)), function(i) V[i, ]), count = lapply(seq_len(nrow(N)), function(i) N[i, ]), basis = apply(B, 1, paste, collapse = ''))

  # 8. The figures the page's text quotes for this region (the same tests as tests/02, 03, 09 and 11, on this region)
  h <- lv_core$h; sname <- function(s) sic2_lk[match(s, sic2), short]
  yearly <- vapply(sort(unique(own_core$year)), function(y) rho(levels_for(own_core[year == y], core)$h, h), 0)
  mon <- lapply(months, function(m) levels_for(own_core[date == m], core)$h)
  monthly <- vapply(mon, rho, 0, b = h)
  own_core[, value_pub := fifelse(suppressed, 0, value)]
  monthly_pub <- vapply(months, function(m) rho(levels_for(own_core[date == m], core, 'value_pub')$h, levels_for(own_core, core, 'value_pub')$h), 0)
  wobble <- median(apply(do.call(cbind, mon), 1, sd, na.rm = TRUE), na.rm = TRUE)
  paid <- colSums(Wc); recv <- rowSums(Wc); h_count <- levels_for(own_core, core, 'n_held')$h
  io <- fread(file.path(chunk_dir, 'outputs/levels_payments_vs_io.csv'), colClasses = list(character = 'g'))
  core_ins <- ins[!payer_sic %in% intermediary & !payee_sic %in% intermediary]
  yr <- own_core[, .(v = sum(value_held) / uniqueN(date), n = sum(n_held) / uniqueN(date)), by = year]
  g_val <- yr[year == 2025, v] / yr[year == 2019, v] - 1; g_n <- yr[year == 2025, n] / yr[year == 2019, n] - 1
  g_avg <- (yr[year == 2025, v / n]) / (yr[year == 2019, v / n]) - 1
  no_count <- own_core[, sum(value_held[n_held < 50]) / sum(value_held)]          # value in link-months ONS records as having no payments (under 50)
  nv <- cells[never == TRUE & payer_sic %in% core & payee_sic %in% core][, big := pmin(per_month[payer_sic], per_month[payee_sic])][order(-big)]
  ends <- h[!is.na(h)]; top_small <- names(which.max(ends)) %in% small; bottom_small <- names(which.min(ends)) %in% small
  ex <- if (top_small) names(which.max(ends)) else if (bottom_small) names(which.min(ends)) else NA
  firm <- pg[status == 'core', .N]
  facts <- list(
    down_pct = pct(lv_core$up), incoherence = num(lv_core$F0), shuffled = num(mean(null_F0(lv_core$W, 200))), io_incoherence = num(io_incoherence),
    io_cor = num(rho(h[io$g], io$io)), yearly_cor = sprintf('%s to %s', num(min(yearly)), num(max(yearly))), monthly_cor = num(median(monthly)),
    monthly_cor_published = num(median(monthly_pub)), balance_cor = num(rho(h, -(recv - paid) / (recv + paid))),
    count_cor = num(rho(h, h_count)), count_io_cor = num(rho(h_count[io$g], io$io)), inter_cor = num(rho(lv_all$h[core], h)),
    span = num(diff(range(h, na.rm = TRUE)), 1), wobble = num(wobble),
    hidden_pct = pct(mean(core_ins$suppressed)), never_pct = pct(mean(core_ins$basis == 'never_published')),
    held_adds_pct = pct(core_ins[suppressed == TRUE, sum(value_held)] / core_ins[, sum(value, na.rm = TRUE)]),
    never_example = if (nrow(nv)) sprintf('%s paying %s', sname(nv$payer_sic[1]), sname(nv$payee_sic[1])) else NULL,
    growth = if (no_count >= count_unusable_share)
      sprintf('The value captured %s %s from 2019 to 2025. Whether that is more payments being counted cannot be said here: %s of the value sits in links with under 50 payments a month, which ONS records as none.',
              if (g_val >= 0) 'rose' else 'fell', pct(abs(g_val)), pct(no_count))
    else sprintf('The value captured %s %s from 2019 to 2025 and the number of payments %s %s, while the average payment %s.%s',
                 if (g_val >= 0) 'rose' else 'fell', pct(abs(g_val)), if (g_n >= 0) 'rose' else 'fell', pct(abs(g_n)),
                 if (abs(g_avg) < 0.1) 'stayed about the same' else sprintf('%s %s', if (g_avg > 0) 'rose' else 'fell', pct(abs(g_avg))),
                 if (g_n > 0.5 * g_val && g_val > 0.2) ' Much of the rise is more payments being counted, which is what wider coverage would look like.' else ''),
    no_count_pct = pct(no_count),
    small_n = length(small), industries = length(core),
    small_example = if (!is.na(ex)) sprintf('%s would otherwise sit at the very %s of the order on about %s in eight years.', sname(ex), if (top_small) 'top' else 'bottom',
                                            if (per_month[[ex]] * nm >= 1e6) sprintf('£%sm', num(per_month[[ex]] * nm / 1e6, 1)) else sprintf('£%s,000', round(per_month[[ex]] * nm / 1e3))) else NULL,
    groups_n = length(group_order), groups_firm = firm, groups_not_firm = sprintf('%d of the %d', length(core) - firm, length(core)),
    groups_named_by_hand = !is.null(hn), partner = regions[code == partner, name],
    is8_year = if (is.na(i8$year)) sprintf('%s (Great Britain only: Northern Ireland is not covered, so that highlight is left out here)', jobs_by_region()$year[1]) else i8$year)
  facts <- c(facts, left_out(code, core))
  facts <- c(facts, list(fe_year = fz$year, fe_regional = fz$regional,
                         fe_jobs = if (fz$regional) sprintf('%s of jobs here are in the material zone, %s in the providential and %s in the overlooked.', pct(fz$jobs[['Material']]), pct(fz$jobs[['Providential']]), pct(fz$jobs[['Overlooked']])) else 'There are no jobs figures for this region, so the wedges use the shares for Great Britain.'))

  dat <- list(region = code, name = reg$name, the_name = reg$the_name, built = format(Sys.Date()),
              source = 'ONS, Industry-to-industry payment flows by region, UK (editions of 17 September and 22 October 2026)',
              units = list(value = 'GBP thousands', count = 'payments'), months = format(months, '%Y-%m'),
              sector_groups = sector_groups, pay_groups = group_order, is8_names = sort(unique(i8$memb$is8)), facts = facts, nodes = nodes, links = links)
  f <- file.path(out_dir, sprintf('flows_%s.js', code))
  writeLines(paste0('window.FLOWS_DATA = window.FLOWS_DATA || {}; window.FLOWS_DATA.', code, ' = ',
                    toJSON(dat, auto_unbox = TRUE, digits = NA, null = 'null', na = 'null'), ';'), f)
  bn <- function(v) round(sum(v) / 1e9, 1)
  data.table(region = code, industries = sprintf('%d (%d)', length(ids), sum(ids %in% intermediary)), links = nrow(pairs), months = nm,
             inside_bn = bn(core_ins$value_held), paid_out_bn = bn(x[kind == 'pays_out' & !payer_sic %in% intermediary & !payee_sic %in% intermediary, value_held]),
             paid_in_bn = bn(x[kind == 'paid_in' & !payer_sic %in% intermediary & !payee_sic %in% intermediary, value_held]),
             incoherence = round(lv_core$F0, 3), down = round(lv_core$up, 3), incoherence_all = round(lv_all$F0, 3), io_cor = round(rho(h[io$g], io$io), 2),
             groups = length(group_order), core = firm, small = length(small), value_without_row = sum(V > 0 & B == '0'),
             file_mb = round(file.size(f) / 1e6, 2), seconds = round(as.numeric(Sys.time() - t0, units = 'secs')))
}

# ---- The index of all twelve regions (plan section 10.3) ---------------------------------------------
# Needs every region's inside flows: local/.../all_regions_own_monthly.rds, from tests/14_all_regions_extract.R.
build_index <- function(dir = out_dir) {
  set.seed(1)
  hf <- file.path(local_dir, 'all_regions_own_held.rds')
  if (!file.exists(hf)) {
    mf <- file.path(local_dir, 'all_regions_own_monthly.rds'); if (!file.exists(mf)) source(file.path(chunk_dir, '14_all_regions_extract.R'))
    x <- readRDS(mf); x <- x[!payer_sic %in% c('00', not_industry) & !payee_sic %in% c('00', not_industry)]
    saveRDS(hold_values(x, by = c('payer_sic', 'payer_region', 'payee_sic', 'payee_region')), hf)
  }
  x <- readRDS(hf)[payer_sic != payee_sic & !payer_sic %in% intermediary & !payee_sic %in% intermediary]
  nodes <- sic2_lk[role == 'industry', sic2]; nm <- uniqueN(x$date); codes <- regions$code
  W_of <- function(d) { W <- flow_matrix(d, 'value_held', nodes); diag(W) <- 0; W }
  W <- lapply(setNames(codes, codes), function(r) W_of(x[payer_region == r]))
  io <- fread(file.path(chunk_dir, 'outputs/levels_payments_vs_io.csv'), colClasses = list(character = 'g'))
  # A. overall distance, and each region against its own earlier self (2019 to 2022 against 2023 on)
  D <- sapply(codes, function(a) sapply(codes, function(b) flow_distance(W[[a]], W[[b]])))
  own_shift <- vapply(codes, function(r) flow_distance(W_of(x[payer_region == r & year(date) < 2023]), W_of(x[payer_region == r & year(date) >= 2023])), 0)
  # B. chain or web, whole period and each full year (for the year-to-year range)
  whole <- t(sapply(codes, function(r) chain_measures(W[[r]])))
  yearly <- rbindlist(lapply(codes, function(r) rbindlist(lapply(2019:2025, function(y) as.data.table(as.list(chain_measures(W_of(x[payer_region == r & year(date) == y]))))[, `:=`(code = r, year = y)]))))
  rng <- yearly[, .(down_lo = min(down), down_hi = max(down), spread_lo = min(spread), spread_hi = max(spread), down_sd = sd(down), spread_sd = sd(spread)), by = code]
  q <- rbindlist(lapply(codes, function(r) { d <- x[payer_region == r]; tr <- trophic(W[[r]]); pmn <- (rowSums(W[[r]]) + colSums(W[[r]])) / nm
    data.table(code = r, inside_bn = sum(W[[r]]) / 1e9, hidden = mean(d$suppressed), never = mean(d$basis == 'never_published'),
               held_adds = d[suppressed == TRUE, sum(value_held)] / d[, sum(value, na.rm = TRUE)], no_count = d[, sum(value_held[n_held < 50]) / sum(value_held)],
               industries = tr$n, small = sum(pmn > 0 & pmn < small_threshold),
               io_cor = rho(tr$h[io$g], io$io)) }))
  r2 <- function(v) round(v, 2); r3 <- function(v) round(v, 3)
  out <- lapply(codes, function(r) { rg <- regions[code == r]; qq <- q[code == r]; rr <- rng[code == r]; o <- sort(D[r, setdiff(codes, r)])
    list(code = r, name = rg$name, the_name = rg$the_name, short = rg$short, built = file.exists(file.path(dir, sprintf('flows_%s.js', r))),
         thin = qq$never >= thin_never_share,
         quality = list(inside_bn = round(qq$inside_bn, 1), hidden = r3(qq$hidden), never = r3(qq$never), held_adds = r3(qq$held_adds), no_count = r3(qq$no_count), industries = qq$industries, small = qq$small, io_cor = r2(qq$io_cor)),
         distances = lapply(names(o), function(b) list(code = b, d = r3(o[[b]]))), own_shift = r3(own_shift[[r]]),
         chain = list(down = list(v = r3(whole[r, 'down']), lo = r3(rr$down_lo), hi = r3(rr$down_hi)), spread = list(v = r2(whole[r, 'spread']), lo = r2(rr$spread_lo), hi = r2(rr$spread_hi)))) })
  between <- D[upper.tri(D)]
  idx <- list(built = format(Sys.Date()), default = 'TLE', regions = out,
              distance = list(lo = r2(min(between)), hi = r2(max(between)), median = r2(median(between)), own_lo = r2(min(own_shift)), own_hi = r2(max(own_shift))),
              chain = list(down_wobble = r3(mean(rng$down_sd)), spread_wobble = r2(mean(rng$spread_sd)),
                           down_cor_hidden = r2(rho(whole[, 'down'], q$hidden)), down_cor_size = r2(rho(whole[, 'down'], q$inside_bn)),
                           spread_cor_hidden = r2(rho(whole[, 'spread'], q$hidden)), spread_cor_size = r2(rho(whole[, 'spread'], q$inside_bn))))
  f <- file.path(dir, 'regions.js')
  writeLines(paste0('window.FLOWS_REGIONS = ', toJSON(idx, auto_unbox = TRUE, digits = NA, null = 'null'), ';'), f)
  cat('\nIndex of regions written:', round(file.size(f) / 1e3, 1), 'KB; built:', paste(codes[vapply(out, `[[`, TRUE, 'built')], collapse = ' '), '; thin:', paste(codes[vapply(out, `[[`, TRUE, 'thin')], collapse = ' '), '\n')
  cat('Distances between regions', idx$distance$lo, 'to', idx$distance$hi, '; a region against its own earlier self', idx$distance$own_lo, 'to', idx$distance$own_hi, '\n')
}

# ---- Run ---------------------------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
staging <- length(args) > 0 && args[1] == 'stage'
if (staging) { args <- args[-1]; out_dir <- file.path(local_dir, 'staged_data'); dir.create(out_dir, showWarnings = FALSE); message('staging: writing to ', out_dir, '; the site is not touched') }
todo <- if (!length(args)) sample_regions else if (args[1] == 'all') regions$code else if (args[1] == 'index') character() else args
stopifnot(all(todo %in% regions$code))
if (length(todo)) {
  chk <- rbindlist(lapply(todo, function(r) { message('building ', r); build_region(r) }))
  cat('\nBuilt:\n'); print(chk)
  # Checks against the tests, where there are test results to check against (Yorkshire and the Humber only)
  if ('TLE' %in% todo) { e <- chk[region == 'TLE']
    want <- list(industries = '81 (4)', links = 3627L, months = 93L, inside_bn = 79.0, paid_out_bn = 265.2, paid_in_bn = 230.2, incoherence = 0.743, down = 0.712,
                 incoherence_all = 0.773, io_cor = 0.71, groups = 5L, core = 43L, small = 11L, value_without_row = 0L)
    bad <- names(want)[!mapply(function(k, v) isTRUE(all.equal(e[[k]], v)), names(want), want)]
    cat('\nYorkshire and the Humber against the test results:', if (length(bad)) paste('DIFFERENT:', paste(bad, collapse = ', ')) else 'all 14 checks match', '\n')
  }
}
if (staging) for (f in list.files(data_dir, pattern = '^flows_.*\\.js$')) if (!file.exists(file.path(out_dir, f))) file.copy(file.path(data_dir, f), file.path(out_dir, f))   # regions already on the site
build_index()
