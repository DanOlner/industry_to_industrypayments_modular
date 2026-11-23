# 11_intermediaries.R
# What changes if the four intermediary divisions (63 information services, 64 banking and finance,
# 66 financial support services, 82 office and business support) are switched in?
# Run from the repo root: Rscript docs/chunks/paymentflowsviz_ynh/tests/11_intermediaries.R
source('docs/chunks/paymentflowsviz_ynh/tests/functions_hierarchy.R')
own <- readRDS(file.path(local_dir, 'ynh_own_held.rds'))      # non-industries out, intermediaries in
nodes_all <- sort(unique(c(own$payer_sic, own$payee_sic))); nodes_out <- setdiff(nodes_all, intermediary)
W_in <- flow_matrix(own, 'value_held', nodes_all); W_out <- flow_matrix(own, 'value_held', nodes_out)
t_in <- trophic(W_in); t_out <- trophic(W_out)
common <- nodes_out[!is.na(t_out$h[nodes_out])]
d <- (t_in$h[common] - mean(t_in$h[common])) - (t_out$h[common] - mean(t_out$h[common]))   # change in level once both are centred
cat('Other divisions\' levels, with intermediaries in against out: rank correlation',
    round(cor(t_in$h[common], t_out$h[common], method = 'spearman'), 3), '; median absolute change', round(median(abs(d)), 3),
    '; largest', round(max(abs(d)), 3), '(', sic2_lk[sic2 == common[which.max(abs(d))], short], ')\n')
cat('Incoherence score: out', round(t_out$F0, 3), ' in', round(t_in$F0, 3), '\n')
diag(W_in) <- 0
tot <- sum(W_in); span <- range(t_in$h, na.rm = TRUE)
res <- data.table(sic2 = intermediary, short = sic2_lk[match(intermediary, sic2), short], level = round(t_in$h[intermediary], 2),
                  received_m_month = round(rowSums(W_in)[intermediary] / 1e6 / 93, 1), paid_m_month = round(colSums(W_in)[intermediary] / 1e6 / 93, 1))
res[, rank_by_size := match(sic2, names(sort(rowSums(W_in) + colSums(W_in), decreasing = TRUE)))]
cat('\nThe four, with everything in (levels run from', round(span[1], 2), 'to', round(span[2], 2), '):\n'); print(res)
touch <- sum(W_in[intermediary, ]) + sum(W_in[, intermediary]) - sum(W_in[intermediary, intermediary])
cat('Share of all inside payments that involve one of them:', round(touch / tot, 3), '\n')
cat('Hidden share of their cell-months:', own[payer_sic %in% intermediary | payee_sic %in% intermediary, round(mean(suppressed), 3)],
    ' against', own[!payer_sic %in% intermediary & !payee_sic %in% intermediary, round(mean(suppressed), 3)], 'for the rest\n')
e <- as.data.table(as.table(W_in))[N > 0]; setnames(e, c('payee', 'payer', 'value')); e <- e[payee %in% intermediary | payer %in% intermediary][order(-value)][1:10]
e[, `:=`(payer = sic2_lk[match(payer, sic2), short], payee = sic2_lk[match(payee, sic2), short], m_month = round(value / 1e6 / 93, 1), value = NULL)]
cat('\nTheir ten largest links:\n'); print(e[, .(payer, payee, m_month)])
fwrite(data.table(sic2 = nodes_all, level_with_intermediaries = round(t_in$h[nodes_all], 3)), file.path(chunk_dir, 'outputs/levels_sic2_with_intermediaries.csv'))
