# 24_chains_across_the_edge.R
# Scoping. The page's chain measure (tests/23_middle_links.cjs) only sees payments where both sides are in the region.
# Wholesale sells a great deal to industries in its region but buys most of what it sells elsewhere, so it comes out as
# an end of the line, not the middle link it is. Would counting payments across the region's edge fix that?
# For each industry in a region this splits its payments three ways twice over:
#   inside     partners are the other industries in the region (what the page does now)
#   UK-wide    partners are the other industries in the region, plus every industry elsewhere in the UK, its own included
# and, for what is passed along UK-wide, says where it came from and where it went: bought here or elsewhere, sold here
# or elsewhere (shared in proportion). It also reports how much of the value across the edge is held, not published, and
# whether the UK-wide figures agree better with the UK input-output table than the inside ones do.
# Uses the held monthly data the build caches (local/paymentflowsviz_ynh/held_<region>.rds), so run build_data.R first,
# and outputs/middle_links_payments_vs_io.csv from 23b for the input-output comparison.
# Run from the repo root: Rscript docs/chunks/paymentflowsviz_ynh/tests/24_chains_across_the_edge.R [TLE,TLG,...]   (default: all twelve)
source('docs/chunks/paymentflowsviz_ynh/tests/functions_hierarchy.R')
out <- file.path(chunk_dir, 'outputs')
args <- commandArgs(trailingOnly = TRUE)
regions <- if (length(args)) strsplit(args[1], ',')[[1]] else c('TLC', 'TLD', 'TLE', 'TLF', 'TLG', 'TLH', 'TLI', 'TLJ', 'TLK', 'TLL', 'TLM', 'TLN')
pc <- function(v) sprintf('%.0f%%', 100 * v)
rho <- function(a, b) round(suppressWarnings(cor(a, b, method = 'spearman', use = 'complete.obs')), 2)

split3 <- function(r, p) {                    # r: what each partner pays the industry; p: what it pays each partner
  gross <- sum(r) + sum(p); net_in <- sum(pmax(r - p, 0)); net_out <- sum(pmax(p - r, 0))
  list(gross = gross, two_way = 2 * sum(pmin(r, p)) / gross, passed = 2 * min(net_in, net_out) / gross, end = abs(net_in - net_out) / gross,
       kind = if (net_out > net_in) 'buyer' else 'supplier', net_in = net_in, net_out = net_out)
}

one_region <- function(here) {
  f <- file.path(local_dir, sprintf('held_%s.rds', here)); if (!file.exists(f)) { cat(here, ': no held data cached, skipped\n'); return(NULL) }
  x <- readRDS(f); x <- x[!payer_sic %in% intermediary & !payee_sic %in% intermediary]            # intermediaries out, as the page opens
  x[, `:=`(payer_here = payer_region == here, payee_here = payee_region == here)]
  a <- x[, .(v = sum(value_held), pub = sum(value, na.rm = TRUE)), by = .(payer_sic, payer_here, payee_sic, payee_here)]
  held_share <- function(d) 1 - sum(d$pub) / sum(d$v)
  ins <- a[payer_here & payee_here & payer_sic != payee_sic]                                    # inside, between different industries
  w <- flow_matrix(x[payer_here & payee_here & payer_sic != payee_sic], 'value_held', sort(unique(c(ins$payer_sic, ins$payee_sic)))); diag(w) <- 0
  per_month <- (rowSums(w) + colSums(w)) / uniqueN(x$date); placed <- names(per_month)[per_month >= 0.5e6]     # the page's "too small" rule
  rbindlist(lapply(placed, function(i) {
    r_in <- ins[payee_sic == i, setNames(v, payer_sic)]; p_in <- ins[payer_sic == i, setNames(v, payee_sic)]
    r_out <- a[payee_sic == i & payee_here & !payer_here, setNames(v, payer_sic)]; p_out <- a[payer_sic == i & payer_here & !payee_here, setNames(v, payee_sic)]
    al <- function(u, v) { k <- union(names(u), names(v)); list(setNames(ifelse(is.na(u[k]), 0, u[k]), k), setNames(ifelse(is.na(v[k]), 0, v[k]), k)) }
    A <- al(r_in, p_in); B <- al(r_out, p_out)
    s_in <- split3(A[[1]], A[[2]]); s_uk <- split3(c(A[[1]], B[[1]]), c(A[[2]], B[[2]]))
    # of what is passed along UK-wide: where the net receipts come from (sold to) and where the net payments go (bought from)
    sold_here <- sum(pmax(A[[1]] - A[[2]], 0)) / s_uk$net_in; bought_here <- sum(pmax(A[[2]] - A[[1]], 0)) / s_uk$net_out
    data.table(region = here, sic2 = i, inside_gross = s_in$gross, inside_two_way = s_in$two_way, inside_passed = s_in$passed, inside_end = s_in$end, inside_kind = s_in$kind,
               uk_gross = s_uk$gross, uk_two_way = s_uk$two_way, uk_passed = s_uk$passed, uk_end = s_uk$end, uk_kind = s_uk$kind,
               bought_here_sold_here = bought_here * sold_here, bought_elsewhere_sold_here = (1 - bought_here) * sold_here,
               bought_here_sold_elsewhere = bought_here * (1 - sold_here), bought_elsewhere_sold_elsewhere = (1 - bought_here) * (1 - sold_here),
               held_inside = held_share(ins[payer_sic == i | payee_sic == i]), held_across = held_share(a[(payee_sic == i & payee_here & !payer_here) | (payer_sic == i & payer_here & !payee_here)]))
  }))
}

res <- rbindlist(lapply(regions, function(r) { cat('reading', r, '\n'); one_region(r) }))
res <- merge(res, sic2_lk[, .(sic2, short)], by = 'sic2'); setcolorder(res, c('region', 'sic2', 'short')); setorder(res, region, sic2)
io <- fread(file.path(out, 'middle_links_payments_vs_io.csv'), colClasses = list(character = 'sic2'))[, .(sic2, io_two_way, io_passed_along, io_end_of_line)]

cat('\n== Wholesale, inside the region and UK-wide ==\n')
print(res[sic2 == '46', .(region, inside = sprintf('%s two-way, %s passed along, %s %s end', pc(inside_two_way), pc(inside_passed), pc(inside_end), inside_kind),
                          uk_wide = sprintf('%s two-way, %s passed along, %s %s end', pc(uk_two_way), pc(uk_passed), pc(uk_end), uk_kind))])
cat('Input-output table, wholesale:', io[sic2 == '46', sprintf('%s two-way, %s passed along, %s end', pc(io_two_way), pc(io_passed_along), pc(io_end_of_line))], '\n')

cat('\n== By region: all payments, and how the two versions compare ==\n')
for (r in unique(res$region)) { d <- merge(res[region == r], io, by = 'sic2', all.x = TRUE); w <- function(col, g) sum(d[[col]] * d[[g]]) / sum(d[[g]])
  cat(sprintf('%s: inside %s two-way, %s passed along, %s end | UK-wide %s, %s, %s | across the edge is %.1f times inside | held: inside %s, across %s | passed along, inside against UK-wide %.2f | against input-output: inside %.2f, UK-wide %.2f\n',
              r, pc(w('inside_two_way', 'inside_gross')), pc(w('inside_passed', 'inside_gross')), pc(w('inside_end', 'inside_gross')),
              pc(w('uk_two_way', 'uk_gross')), pc(w('uk_passed', 'uk_gross')), pc(w('uk_end', 'uk_gross')), (sum(d$uk_gross) - sum(d$inside_gross)) / sum(d$inside_gross),
              pc(w('held_inside', 'inside_gross')), pc(sum(d$held_across * (d$uk_gross - d$inside_gross)) / sum(d$uk_gross - d$inside_gross)),
              rho(d$inside_passed, d$uk_passed), rho(d$inside_passed, d$io_passed_along), rho(d$uk_passed, d$io_passed_along))) }
mid <- res[, .(inside_passed = median(inside_passed), uk_passed = median(uk_passed), regions = .N), by = sic2]; mid <- merge(mid, io, by = 'sic2')
cat(sprintf('Each division at its middle value across regions, %d divisions, against the input-output table: inside %.2f, UK-wide %.2f\n', nrow(mid), rho(mid$inside_passed, mid$io_passed_along), rho(mid$uk_passed, mid$io_passed_along)))

home <- if ('TLE' %in% res$region) 'TLE' else res$region[1]; d <- res[region == home]
cat('\n==', home, ': who changes most ==\n')
show <- function(x) paste(sprintf('%s %s to %s', x$short, pc(x$inside_passed), pc(x$uk_passed)), collapse = '; ')
cat('passed along rises most:', show(d[order(-(uk_passed - inside_passed))][1:10]), '\n')
cat('falls most:', show(d[order(uk_passed - inside_passed)][1:6]), '\n')
cat('most passed along UK-wide:', paste(sprintf('%s %s', d[order(-uk_passed)][1:10]$short, pc(d[order(-uk_passed)][1:10]$uk_passed)), collapse = '; '), '\n')
cat('\nOf what is passed along UK-wide, where it was bought and sold (shared in proportion):\n')
print(d[sic2 %in% c('46', '10', '47', '32', '41', '43', '49', '25', '86', '69'), .(short, passed_along = pc(uk_passed), here_to_here = pc(bought_here_sold_here), brought_in = pc(bought_elsewhere_sold_here),
                                                                                     sent_out = pc(bought_here_sold_elsewhere), through = pc(bought_elsewhere_sold_elsewhere))])
num <- setdiff(names(res), c('region', 'sic2', 'short', 'inside_kind', 'uk_kind')); res[, (num) := lapply(.SD, function(v) signif(v, 4)), .SDcols = num]
fwrite(res, file.path(out, 'chains_across_the_edge.csv')); cat('\nWrote chains_across_the_edge.csv\n')
