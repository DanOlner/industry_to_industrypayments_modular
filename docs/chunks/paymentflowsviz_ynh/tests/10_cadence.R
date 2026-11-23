# 10_cadence.R
# Can the page show how often payments are made, as well as how much? This measures what the
# number-of-payments column adds to value, and how many moving dots a "one dot per so many
# payments" rule would put on screen.
# Run from the repo root: Rscript docs/chunks/paymentflowsviz_ynh/tests/10_cadence.R
source('docs/chunks/paymentflowsviz_ynh/tests/functions_hierarchy.R')
x <- readRDS(file.path(local_dir, 'ynh_all_held.rds'))[kind == 'inside' & payer_sic != payee_sic]
x <- merge(x, sic2_lk[, .(payer_sic = sic2, payer = short)], by = 'payer_sic'); x <- merge(x, sic2_lk[, .(payee_sic = sic2, payee = short)], by = 'payee_sic')

# the latest 12 months, one row per link
last12 <- x[date > max(date) - 366, .(value_m = sum(value_held) / 1e6 / 12, payments = sum(n_held) / 12), by = .(payer, payee)]
setorder(last12, -value_m); last12[, rank := .I]; last12[, avg_payment := value_m * 1e6 / payments]
top <- last12[rank <= 430]                       # the links that carry about 80% of the value
cat('Top 430 links, average month over the latest 12 months\n')
cat('  payments per month: '); print(round(quantile(top$payments, c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1))))
cat('  average payment, GBP: '); print(round(quantile(top[payments > 0, avg_payment], c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1))))
cat('  links whose payments round to zero (under 50 a month):', top[payments < 50, .N], '\n')
cat('  rank correlation, value against number of payments:', round(cor(top$value_m, top$payments, method = 'spearman'), 2), '\n')
cat('  90th to 10th percentile ratio of average payment:', round(quantile(top[payments > 0, avg_payment], 0.9) / quantile(top[payments > 0, avg_payment], 0.1), 1), '\n')
cat('\nSame value, very different cadence. Links ranked 20 to 120 by value:\n')
mid <- top[rank %between% c(20, 120) & payments > 0]
cat('  fewest, largest payments:\n'); print(mid[order(-avg_payment)][1:6, .(payer, payee, value_m = round(value_m, 1), payments = round(payments), avg_payment = round(avg_payment))])
cat('  most, smallest payments:\n'); print(mid[order(avg_payment)][1:6, .(payer, payee, value_m = round(value_m, 1), payments = round(payments), avg_payment = round(avg_payment))])

cat('\nDots on screen if one dot stands for so many payments a month (top 430 links):\n')
for (u in c(100, 250, 500, 1000)) cat(sprintf('  one dot per %4d payments: %5d dots in all, %3d on the busiest link, %3d links with no dot\n',
                                              u, sum(round(top$payments / u)), max(round(top$payments / u)), sum(round(top$payments / u) == 0)))
cat('Busiest links by number of payments:\n'); print(top[order(-payments)][1:5, .(payer, payee, payments = round(payments), value_m = round(value_m, 1))])
fwrite(last12[, .(payer, payee, rank, value_m = round(value_m, 3), payments = round(payments), avg_payment = round(avg_payment))],
       file.path(chunk_dir, 'outputs/cadence_links_latest12.csv'))
