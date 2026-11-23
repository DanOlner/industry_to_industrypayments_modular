# 12_consensus_groups.R
# Can the payment groups from 08_clusters.R be pinned down well enough to name on the page?
# Two candidates: (a) one set of groups, the most common over all years; (b) groups that change by year.
# For each year the grouping is run 2,000 times, and each pair of divisions scored by how often they land
# together ("togetherness"). A year's groups, and the all-years groups, are then found on those scores,
# which takes out the run-to-run luck that made single runs disagree.
# Run from the repo root: Rscript docs/chunks/paymentflowsviz_ynh/tests/12_consensus_groups.R
source('docs/chunks/paymentflowsviz_ynh/tests/functions_hierarchy.R')
suppressPackageStartupMessages(library(igraph))
set.seed(1)
out <- file.path(chunk_dir, 'outputs')
own <- readRDS(file.path(local_dir, 'ynh_own_held.rds'))
lev <- fread(file.path(out, 'levels_sic2.csv'), colClasses = list(character = 'sic2'))[!is.na(level_value)]
nodes <- lev$sic2; n <- length(nodes)
small <- lev[(received_bn + paid_bn) * 1000 / 93 < 0.5, sic2]          # under GBP 0.5m a month: too thin to place

# 2,000 runs a year: with 100 the result changed with the random seed (the Food group and Security moved in
# and out of the core); with 2,000 it came out identical on four seeds.
nodes <- sort(nodes)
pgr <- payment_groups(own, nodes, small, runs = 2000)      # in functions_hierarchy.R, shared with the page's build_data.R
m_year <- pgr$m_year; m_all <- pgr$m_all; years <- pgr$years

big <- !nodes %in% small
ari <- function(a, b) compare(a[big], b[big], method = 'adjusted.rand')
cat('Agreement between years once run-to-run luck is taken out (adjusted Rand index, 1 = identical; small divisions left out):\n')
cat('  each year against the all-years groups:', paste(sprintf('%d %.2f', years, vapply(m_year, ari, 0, b = m_all)), collapse = ' | '), '\n')
cat('  consecutive years:', paste(sprintf('%.2f', vapply(seq_along(years)[-1], function(i) ari(m_year[[i]], m_year[[i - 1]]), 0)), collapse = ' '), '\n')
cat('  groups per year:', paste(vapply(m_year, function(m) length(unique(m[big])), 0L), collapse = ' '), '\n')

# how firmly each division belongs: its average togetherness with the rest of its all-years group, and
# in how many of the eight years its year-group is mostly made of its all-years group
res <- merge(pgr$groups, lev[, .(sic2, short, section, level_value, per_month_m = round((received_bn + paid_bn) * 1000 / 93, 1))], by = 'sic2')
gs <- res[, .(size = .N, core = sum(status == 'core'), flow = sum(per_month_m)), by = group][order(-flow)]
gs[, label := LETTERS[seq_len(.N)]]; res <- merge(res, gs[, .(group, label)], by = 'group')
cat('\nAll-years groups (', nrow(gs), '), largest first by payments. Core: with the group in at least 6 of 8 years.\n', sep = '')
for (L in gs$label) { r <- res[label == L][order(-per_month_m)]
  cat(sprintf('\n  %s  (%d divisions, GBP %.0fm a month)\n     core:     %s\n     floating: %s\n     too small: %s\n', L, nrow(r), sum(r$per_month_m),
              paste(r[status == 'core', short], collapse = ', '), paste(r[status == 'floating', sprintf('%s (%d/8)', short, years_with_group)], collapse = ', '),
              paste(r[status == 'too small', short], collapse = ', '))) }
cat('\nCore:', res[status == 'core', .N], ' floating:', res[status == 'floating', .N], ' too small:', res[status == 'too small', .N],
    '; share of inside payments made or received by core divisions:', round(res[status == 'core', sum(per_month_m)] / sum(res$per_month_m), 2), '\n')
tab <- table(res[status == 'core', section], res[status == 'core', label])
cat('Core divisions whose group is the main one for their SIC section:', round(sum(apply(tab, 1, max)) / sum(tab), 2), '\n')
# how much would a per-year view move things? divisions that change group between consecutive years
moves <- vapply(seq_along(years)[-1], function(i) { a <- m_year[[i - 1]]; b <- m_year[[i]]
  # match this year's groups to last year's by best overlap, then count divisions not in the matched group
  map <- vapply(sort(unique(b)), function(gb) as.integer(names(which.max(table(a[b == gb])))), 0L); names(map) <- sort(unique(b))
  sum(map[as.character(b)][big] != a[big]) }, 0)
cat('Divisions (of', sum(big), 'large enough) that change group from one year to the next:', paste(moves, collapse = ' '), '\n')
fwrite(res[order(label, -per_month_m), .(sic2, short, section, group = label, status, firmness = round(firmness, 2), years_with_group, level_value, per_month_m)],
       file.path(out, 'payment_groups_consensus.csv'))
yearly <- as.data.table(do.call(cbind, lapply(m_year, as.integer))); setnames(yearly, paste0('y', years)); yearly[, sic2 := nodes]
fwrite(yearly, file.path(out, 'payment_groups_by_year.csv'))
