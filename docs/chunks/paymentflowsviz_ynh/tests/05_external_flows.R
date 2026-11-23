# 05_external_flows.R
# Question 2 (part): how big are Y&H's payments to and from the rest of the UK next to the payments
# inside the region, by division? This decides how the page can show outside links without drowning
# the inside ones.
# Run from the repo root: Rscript docs/chunks/paymentflowsviz_ynh/tests/05_external_flows.R
source('docs/chunks/paymentflowsviz_ynh/tests/functions_hierarchy.R')
out <- file.path(chunk_dir, 'outputs')

ynh <- readRDS(file.path(local_dir, 'ynh_monthly.rds'))
drop <- c('00', not_industry, intermediary)
x <- ynh[!payer_sic %in% drop & !payee_sic %in% drop & payer_region != 'unknown' & payee_region != 'unknown']
x[, kind := fifelse(payer_region == 'TLE' & payee_region == 'TLE', 'inside',
             fifelse(payer_region == 'TLE', 'ynh_buys_from_outside', 'outside_buys_from_ynh'))]
cat('Cell-months and hidden share by kind (intermediaries and non-industries out):\n')
print(x[, .(cell_months = .N, hidden = round(mean(suppressed), 3)), by = kind])

x <- hold_values(x, by = c('payer_sic', 'payer_region', 'payee_sic', 'payee_region'))
saveRDS(x, file.path(local_dir, 'ynh_all_held.rds'))
cat('\nHow hidden cell-months were valued, by kind:\n')
print(dcast(x[, .N, by = .(kind, basis)], kind ~ basis, value.var = 'N'))

tot <- x[, .(published_bn = sum(value, na.rm = TRUE) / 1e9, held_bn = sum(value_held) / 1e9,
             payments_m = sum(n_held) / 1e6), by = kind]
tot[, `:=`(held_adds_pct = round((held_bn / published_bn - 1) * 100, 1), avg_payment = round(held_bn * 1e9 / (payments_m * 1e6)))]
cat('\nTotals, Jan 2019 to Sep 2026:\n'); print(tot[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 1) else v)])

# ---- By division: the region's own share on each side -------------------------------------------
buy <- x[payer_region == 'TLE', .(inside = sum(value_held[kind == 'inside']), outside = sum(value_held[kind != 'inside'])), by = .(sic2 = payer_sic)]
sell <- x[payee_region == 'TLE', .(inside = sum(value_held[kind == 'inside']), outside = sum(value_held[kind != 'inside'])), by = .(sic2 = payee_sic)]
dv <- merge(buy, sell, by = 'sic2', all = TRUE, suffixes = c('_paid', '_received'))
for (j in names(dv)[-1]) set(dv, which(is.na(dv[[j]])), j, 0)
dv[, `:=`(own_share_spending = round(inside_paid / (inside_paid + outside_paid), 3),
          own_share_income = round(inside_received / (inside_received + outside_received), 3),
          outside_to_inside = round((outside_paid + outside_received) / (inside_paid + inside_received), 2))]
dv <- merge(dv, sic2_lk[, .(sic2, short, section)], by = 'sic2')
bn <- c('inside_paid', 'outside_paid', 'inside_received', 'outside_received'); dv[, (bn) := lapply(.SD, function(v) round(v / 1e9, 3)), .SDcols = bn]
fwrite(dv, file.path(out, 'external_by_division.csv'))
cat('\nOutside flows as a multiple of inside flows, across divisions (in + out):\n')
print(round(quantile(dv$outside_to_inside, c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1), na.rm = TRUE), 2))
cat('Whole region: outside is', round(sum(dv$outside_paid + dv$outside_received) / sum(dv$inside_paid + dv$inside_received), 2), 'times inside\n')
cat('Own-region share of spending: median', median(dv$own_share_spending, na.rm = TRUE), '; of income: median', median(dv$own_share_income, na.rm = TRUE), '\n')
cat('\nLargest divisions by outside flows (GBP bn, whole period):\n')
print(dv[order(-(outside_paid + outside_received))][1:12, .(sic2, short, inside_paid, outside_paid, inside_received, outside_received, own_share_spending, own_share_income)])

# ---- By partner region --------------------------------------------------------------------------
reg <- c(TLC = 'North East', TLD = 'North West', TLF = 'East Midlands', TLG = 'West Midlands', TLH = 'East of England',
         TLI = 'London', TLJ = 'South East', TLK = 'South West', TLL = 'Wales', TLM = 'Scotland', TLN = 'Northern Ireland')
pr <- rbind(x[kind == 'ynh_buys_from_outside', .(side = 'Y&H pays', value = sum(value_held)), by = .(region = payee_region)],
            x[kind == 'outside_buys_from_ynh', .(side = 'Y&H is paid', value = sum(value_held)), by = .(region = payer_region)])
pr <- dcast(pr, region ~ side, value.var = 'value')
pr[, `:=`(name = reg[region], pays_share = round(`Y&H pays` / sum(`Y&H pays`), 3), paid_share = round(`Y&H is paid` / sum(`Y&H is paid`), 3))]
cat('\nPartner regions (share of Y&H\'s outside flows):\n'); print(pr[order(-pays_share), .(name, pays_share, paid_share)])
fwrite(pr[, .(region, name, pays_share, paid_share)], file.path(out, 'external_by_partner_region.csv'))

# ---- Over time: does the inside share move? ------------------------------------------------------
ts <- x[, .(inside = sum(value_held[kind == 'inside']), out_pay = sum(value_held[kind == 'ynh_buys_from_outside']),
            in_pay = sum(value_held[kind == 'outside_buys_from_ynh'])), by = .(year = year(date))]
ts[, `:=`(inside_share = round(inside / (inside + out_pay + in_pay), 3), inside_bn = round(inside / 1e9, 1),
          out_bn = round(out_pay / 1e9, 1), in_bn = round(in_pay / 1e9, 1))]
cat('\nBy year (2026 is January to September):\n'); print(ts[, .(year, inside_bn, out_bn, in_bn, inside_share)])

# ---- Size of what the page would have to carry ---------------------------------------------------
cells_ext <- x[kind != 'inside', .(n = uniqueN(paste(payer_sic, payer_region, payee_sic, payee_region)))]
cat('\nDistinct outside cells (division x division x partner region x direction):', cells_ext$n, '\n')
cat('Collapsed to division x direction per month:', x[kind != 'inside', uniqueN(fifelse(kind == 'ynh_buys_from_outside', payer_sic, payee_sic))] * 2, 'series\n')
