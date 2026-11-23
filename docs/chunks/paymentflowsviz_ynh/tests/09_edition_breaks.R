# 09_edition_breaks.R
# The data come in three editions (2019-21, 2022-24, 2025-26). Playing through time will cross the
# joins at January 2022 and January 2025: is there a jump there that is not in other Januaries?
# Also: how the number of published cells and the inside total move over the whole period.
# Run from the repo root: Rscript docs/chunks/paymentflowsviz_ynh/tests/09_edition_breaks.R
source('docs/chunks/paymentflowsviz_ynh/tests/functions_hierarchy.R')
x <- readRDS(file.path(local_dir, 'ynh_all_held.rds'))[kind == 'inside' & payer_sic != payee_sic]
m <- x[, .(published_cells = sum(!suppressed), hidden_cells = sum(suppressed), published_m = sum(value, na.rm = TRUE) / 1e6,
           held_m = sum(value_held) / 1e6, payments_k = sum(n_held) / 1e3), by = date][order(date)]
m[, `:=`(chg_value = held_m / shift(held_m) - 1, chg_cells = published_cells / shift(published_cells) - 1, chg_n = payments_k / shift(payments_k) - 1)]
jan <- m[month(date) == 1 & year(date) > 2019, .(january = year(date), value_change_from_december = round(chg_value, 3),
                                                published_cells_change = round(chg_cells, 3), payments_change = round(chg_n, 3))]
jan[, edition_join := january %in% c(2022, 2025)]
cat('December to January changes (inside Y&H, intermediaries out, held values):\n'); print(jan)
yr <- m[, .(published_cells_per_month = round(mean(published_cells)), hidden_per_month = round(mean(hidden_cells)),
            value_m_per_month = round(mean(held_m)), payments_k_per_month = round(mean(payments_k)),
            avg_payment = round(sum(held_m) * 1e6 / (sum(payments_k) * 1e3))), by = .(year = year(date))]
cat('\nBy year:\n'); print(yr)
cat('\nGrowth 2019 to 2025: value', round(yr[year == 2025, value_m_per_month] / yr[year == 2019, value_m_per_month] - 1, 2),
    '; number of payments', round(yr[year == 2025, payments_k_per_month] / yr[year == 2019, payments_k_per_month] - 1, 2),
    '; published cells', round(yr[year == 2025, published_cells_per_month] / yr[year == 2019, published_cells_per_month] - 1, 2), '\n')
fwrite(m[, .(date, published_cells, hidden_cells, published_m = round(published_m, 1), held_m = round(held_m, 1), payments_k = round(payments_k, 1))],
       file.path(chunk_dir, 'outputs/inside_monthly_totals.csv'))
