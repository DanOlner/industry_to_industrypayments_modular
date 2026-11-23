# functions_hierarchy.R
# Shared helpers for the hierarchy tests: held values for hidden cells, flow matrices, trophic levels
# and the direction-shuffle null.
suppressPackageStartupMessages(library(data.table))

chunk_dir <- 'docs/chunks/paymentflowsviz_ynh/tests'
local_dir <- 'local/paymentflowsviz_ynh'
sic2_lk <- fread(file.path(chunk_dir, 'lookups/sic2_lookup.csv'), colClasses = 'character')
not_industry <- sic2_lk[role == 'not_industry', sic2]      # 97, 98, 99 (00 is not in the lookup)
intermediary <- sic2_lk[role == 'intermediary', sic2]      # 63, 64, 66, 82

# ---- Held values ---------------------------------------------------------------------------------
# The handover's fill (companieshouseopen 15_directionality.R): each hidden month is held at the same
# cell's average published month, in the same year if it has any, otherwise pooled over the nearest
# years that together give at least `min_months` published months (earlier year first on ties).
# Cells never published stay at zero. Adds value_held, n_held and basis to a copy of d.
# d: one row per cell-month with payer_sic, payee_sic, date, value, n, suppressed (plus any `by` cols).
hold_values <- function(d, by = c('payer_sic', 'payee_sic'), min_months = 6) {
  d <- copy(d)
  d[, year := year(date)]
  cy <- d[, .(pub = sum(!suppressed), v = sum(value, na.rm = TRUE), n = sum(n, na.rm = TRUE)), by = c(by, 'year')]
  years <- sort(unique(d$year))
  fill <- cy[, {
    pubv <- setNames(integer(length(years)), years); vv <- nn <- setNames(numeric(length(years)), years)
    pubv[as.character(year)] <- pub; vv[as.character(year)] <- v; nn[as.character(year)] <- n
    res <- lapply(year, function(y) {
      if (pubv[as.character(y)] > 0)
        return(list(v_mean = vv[[as.character(y)]] / pubv[[as.character(y)]],
                    n_mean = nn[[as.character(y)]] / pubv[[as.character(y)]], basis = 'same_year'))
      ord <- years[order(abs(years - y), years)]; ord <- ord[ord != y]
      cp <- cumsum(pubv[as.character(ord)])
      if (cp[length(cp)] == 0) return(list(v_mean = 0, n_mean = 0, basis = 'never_published'))
      k <- if (any(cp >= min_months)) which(cp >= min_months)[1] else length(cp)
      use <- as.character(ord[seq_len(k)])
      list(v_mean = sum(vv[use]) / sum(pubv[use]), n_mean = sum(nn[use]) / sum(pubv[use]), basis = 'near_years')
    })
    .(year = year, v_mean = vapply(res, `[[`, 0, 'v_mean'), n_mean = vapply(res, `[[`, 0, 'n_mean'),
      fill_basis = vapply(res, `[[`, '', 'basis'))
  }, by = by]
  d <- merge(d, fill, by = c(by, 'year'), all.x = TRUE)
  d[, `:=`(value_held = fifelse(suppressed, v_mean, value), n_held = fifelse(suppressed, n_mean, n),
           basis = fifelse(suppressed, fill_basis, 'published'))]
  d[, c('v_mean', 'n_mean', 'fill_basis') := NULL]
  d[]
}

# ---- Flow matrix ---------------------------------------------------------------------------------
# W[supplier, buyer] = payments from buyer (payer) to supplier (payee). Goods and services run
# supplier -> buyer, so levels rise from suppliers to buyers. `col` is the measure to sum.
flow_matrix <- function(d, col, nodes, from = 'payee_sic', to = 'payer_sic') {
  a <- d[, .(w = sum(get(col), na.rm = TRUE)), by = c(from, to)]
  W <- matrix(0, length(nodes), length(nodes), dimnames = list(nodes, nodes))
  a <- a[get(from) %in% nodes & get(to) %in% nodes]
  W[cbind(match(a[[from]], nodes), match(a[[to]], nodes))] <- a$w
  W
}

# ---- Trophic levels (MacKay, Johnson and Sansom 2020, "How directed is a directed network?") -----
# Levels h solve (diag(u) - W - W') h = v, with u = in + out strength and v = in - out strength.
# F0 = sum_ij w_ij (h_j - h_i - 1)^2 / sum_ij w_ij: 0 = a perfect hierarchy (every link climbs exactly
# one level), 1 = no hierarchy at all. Self-payments are dropped (they carry no direction).
# Works on the largest connected component; other nodes get NA.
trophic <- function(W) {
  diag(W) <- 0
  nodes <- rownames(W); h <- setNames(rep(NA_real_, length(nodes)), nodes)
  live <- (rowSums(W) + colSums(W)) > 0
  if (sum(live) < 3) return(list(h = h, F0 = NA_real_, n = sum(live)))
  A <- (W + t(W)) > 0
  # connected components by flood fill (the matrices are small)
  comp <- rep(0L, length(nodes)); k <- 0L
  for (s in which(live)) if (comp[s] == 0L) {
    k <- k + 1L; front <- s
    while (length(front)) { comp[front] <- k; front <- setdiff(which(colSums(A[front, , drop = FALSE]) > 0 & comp == 0L), front) }
  }
  big <- comp == as.integer(names(which.max(table(comp[comp > 0]))))
  Wb <- W[big, big]
  u <- rowSums(Wb) + colSums(Wb); v <- colSums(Wb) - rowSums(Wb)
  L <- diag(u) - Wb - t(Wb)
  hb <- c(0, solve(L[-1, -1], v[-1]))            # pin the first node at zero, then shift
  hb <- hb - min(hb)
  dif <- outer(hb, hb, function(hi, hj) hj - hi)  # dif[i, j] = h_j - h_i
  h[big] <- hb
  list(h = h, F0 = sum(Wb * (dif - 1)^2) / sum(Wb), n = sum(big),
       # share of flow that climbs (buyer above supplier): 0.5 when direction is random
       up_share = sum(Wb[dif > 0]) / sum(Wb))
}

# ---- Net-flow share ------------------------------------------------------------------------------
# Of every pair's two-way total, the share left after netting the two directions off. 0 = every pair
# pays each other the same, 1 = every pair is one-way.
net_share <- function(W) { diag(W) <- 0; sum(abs(W - t(W))) / 2 / sum(W) }

# ---- Null: shuffle each pair's direction ---------------------------------------------------------
# Keeps who trades with whom and each pair's two amounts, but flips which way round they go with
# probability one half. If F0 on the real data is well below this, direction carries real structure.
null_F0 <- function(W, reps = 200) {
  n <- nrow(W); diag(W) <- 0
  vapply(seq_len(reps), function(i) {
    S <- matrix(0L, n, n); S[upper.tri(S)] <- rbinom(n * (n - 1) / 2, 1, 0.5); S <- S + t(S)
    Wn <- W; Wn[S == 1L] <- t(W)[S == 1L]
    trophic(Wn)$F0
  }, 0)
}

# ---- Payment groups (see 12_consensus_groups.R for the reasoning) -------------------------------
# Affinity: a pair's payments over what the two divisions' sizes alone would predict. Each year is
# grouped `runs` times and every pair scored on how often they land together; groups are then found
# from those scores for each year and for all years together. Returns the all-years groups with each
# division marked core (with its group in at least 6 of 8 years and together with its other members
# in at least 60% of runs), floating, or too small, plus the year-by-year groups.
# own: inside cell-months with payer_sic, payee_sic, date, year, value_held. Needs igraph.
payment_groups <- function(own, nodes, small, runs = 2000) {   # 2,000: fewer runs gave seed-dependent groups
  n <- length(nodes)
  affinity <- function(d, months) {
    W <- flow_matrix(d, 'value_held', nodes); diag(W) <- 0
    A <- W / (outer(rowSums(W), colSums(W)) / sum(W)); A[!is.finite(A)] <- 0
    S <- pmin(A + t(A), 20); S[(W + t(W)) < 1e6 * months / 93] <- 0; S   # capped; pairs under GBP 1m over the period dropped
  }
  together <- function(S) {
    g <- igraph::graph_from_adjacency_matrix(S, mode = 'undirected', weighted = TRUE); C <- matrix(0, n, n, dimnames = list(nodes, nodes))
    for (i in seq_len(runs)) { m <- igraph::membership(igraph::cluster_louvain(g)); C <- C + outer(m, m, `==`) }
    C / runs
  }
  consensus <- function(C) {      # keep pairs together more often than not, group, repeat until settled
    for (it in 1:20) {
      Ct <- C; Ct[Ct < 0.5] <- 0; diag(Ct) <- 0
      g <- igraph::graph_from_adjacency_matrix(Ct, mode = 'undirected', weighted = TRUE)
      ms <- replicate(50, igraph::membership(igraph::cluster_louvain(g)), simplify = FALSE)
      C2 <- Reduce(`+`, lapply(ms, function(m) outer(m, m, `==`))) / 50
      if (all(C2 %in% c(0, 1))) return(ms[[1]])
      C <- C2
    }
    ms[[1]]
  }
  years <- sort(unique(own$year))
  C_year <- lapply(years, function(y) together(affinity(own[year == y], uniqueN(own[year == y, date]))))
  m_year <- lapply(C_year, consensus)
  C_all <- Reduce(`+`, C_year) / length(years)
  m_all <- consensus(C_all)
  res <- data.table(sic2 = nodes, group = as.integer(m_all))
  res[, firmness := vapply(seq_len(n), function(i) { mates <- setdiff(which(m_all == m_all[i]), i); if (!length(mates)) NA_real_ else mean(C_all[i, mates]) }, 0)]
  res[, years_with_group := vapply(seq_len(n), function(i) sum(vapply(m_year, function(m) {
    mates <- setdiff(which(m == m[i]), i); length(mates) > 0 && mean(m_all[mates] == m_all[i]) > 0.5 }, TRUE)), 0)]
  res[, status := fifelse(sic2 %in% small, 'too small', fifelse(years_with_group >= 6 & firmness >= 0.6, 'core', 'floating'))]
  list(groups = res, m_year = m_year, m_all = m_all, years = years)
}

# ---- Comparing regions (see 16_region_similarity.R for the reasoning and the checks) ----------------
# effective_number: 1 if everything is in one place, n if spread evenly over n.
effective_number <- function(p) { p <- p[p > 0]; exp(-sum(p * log(p))) }
# For the typical pound spent, how many supplier industries its buyer's spending is in effect spread across.
suppliers_per_buyer <- function(M) { buy <- colSums(M); weighted.mean(vapply(which(buy > 0), function(j) effective_number(M[, j] / buy[j]), 0), buy[buy > 0]) }
# Chain or web: the share of money running down the order, and the spread of each buyer's spending.
chain_measures <- function(M) { diag(M) <- 0; tr <- trophic(M); c(down = tr$up_share, spread = suppliers_per_buyer(M), incoherence = tr$F0) }
# Distance between two regions' flows: each as shares of its own total, one per ordered pair of industries; half the sum
# of absolute differences. The share of one region's payments that would have to move to another pair to match the other.
flow_distance <- function(A, B) 0.5 * sum(abs(A / sum(A) - B / sum(B)))
