# 08_clusters.R
# For the "connections" view: do Y&H's divisions fall into tightly connected groups, or is it one
# ball with the big sectors in the middle? Compares two ways of weighting a link:
#   raw      the pair's two-way payments
#   affinity the pair's payments divided by what their sizes alone would predict
#            (payer's total x payee's total / all payments), so big sectors stop pulling everything in
# Run from the repo root: Rscript docs/chunks/paymentflowsviz_ynh/tests/08_clusters.R
source('docs/chunks/paymentflowsviz_ynh/tests/functions_hierarchy.R')
suppressPackageStartupMessages(library(igraph))
set.seed(1)
out <- file.path(chunk_dir, 'outputs')
own <- readRDS(file.path(local_dir, 'ynh_own_held.rds'))
lev <- fread(file.path(out, 'levels_sic2.csv'), colClasses = list(character = 'sic2'))[!is.na(level_value)]
nodes <- lev$sic2
W <- flow_matrix(own, 'value_held', nodes); diag(W) <- 0

cluster <- function(S, label) {
  g <- graph_from_adjacency_matrix(S, mode = 'undirected', weighted = TRUE)
  best <- NULL
  for (i in 1:20) { cl <- cluster_louvain(g); if (is.null(best) || modularity(cl) > modularity(best)) best <- cl }
  cat(sprintf('%-9s modularity %.3f, %d groups, sizes %s\n', label, modularity(best), length(best), paste(sort(sizes(best), decreasing = TRUE), collapse = ' ')))
  membership(best)
}
S_raw <- W + t(W)
expected <- outer(rowSums(W), colSums(W)) / sum(W)
A <- W / expected; A[!is.finite(A)] <- 0
S_aff <- pmin(A + t(A), 20)            # cap, so one tiny pair with a huge ratio cannot dominate
# keep only links that carry real money: pairs above GBP 1m over the period
S_aff[S_raw < 1e6] <- 0
m_raw <- cluster(S_raw, 'raw')
m_aff <- cluster(S_aff, 'affinity')

res <- data.table(sic2 = nodes, group_raw = as.integer(m_raw[nodes]), group_affinity = as.integer(m_aff[nodes]))
res <- merge(res, lev[, .(sic2, short, section, level_value)], by = 'sic2')
fwrite(res, file.path(out, 'clusters_sic2.csv'))
cat('\nAffinity groups (divisions that pay each other more than their sizes predict):\n')
for (gk in res[, .N, by = group_affinity][order(-N), group_affinity])
  cat(sprintf('  %d: %s\n', gk, paste(res[group_affinity == gk][order(section, sic2), short], collapse = ', ')))
# how far do the groups just repeat SIC sections?
tab <- table(res$section, res$group_affinity)
cat('\nShare of divisions whose affinity group is the main group of their SIC section:',
    round(sum(apply(tab, 1, max)) / sum(tab), 2), '\n')
cat('Largest raw link as a share of all inside payments:', round(max(S_raw) / sum(W), 3),
    '; top 3 divisions\' share of all payments made and received:', round(sum(sort(rowSums(S_raw), decreasing = TRUE)[1:3]) / sum(S_raw), 3), '\n')

# ---- Are the affinity groups stable? Re-run on each year and compare with the whole period -------
aff_groups <- function(d) {
  Wy <- flow_matrix(d, 'value_held', nodes); diag(Wy) <- 0
  Ay <- Wy / (outer(rowSums(Wy), colSums(Wy)) / sum(Wy)); Ay[!is.finite(Ay)] <- 0
  Sy <- pmin(Ay + t(Ay), 20); Sy[(Wy + t(Wy)) < 1e6 * nrow(unique(d[, .(date)])) / 93] <- 0
  g <- graph_from_adjacency_matrix(Sy, mode = 'undirected', weighted = TRUE)
  best <- NULL; for (i in 1:20) { cl <- cluster_louvain(g); if (is.null(best) || modularity(cl) > modularity(best)) best <- cl }
  best
}
cat('\nYear by year (adjusted Rand index against the whole-period groups; 1 = identical, 0 = no better than chance):\n')
for (y in sort(unique(own$year))) { cl <- aff_groups(own[year == y])
  cat(sprintf('  %d: modularity %.3f, %d groups, agreement %.2f\n', y, modularity(cl), length(cl), compare(membership(cl), m_aff, method = 'adjusted.rand'))) }
runs <- replicate(30, membership(cluster_louvain(graph_from_adjacency_matrix(S_aff, mode = 'undirected', weighted = TRUE))), simplify = FALSE)
cat('Same data, 30 repeat runs: agreement with the chosen grouping, median',
    round(median(vapply(runs, function(r) compare(r, m_aff, method = 'adjusted.rand'), 0)), 2), '\n')
