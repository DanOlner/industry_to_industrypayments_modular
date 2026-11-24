# 28_accounts_geography.R
# Do large companies' accounts say how much of their turnover is sold abroad? Large companies must split turnover by
# geographical market in the notes to their accounts where the markets differ substantially, unless the directors judge
# that would be seriously prejudicial. If the split can be read, it gives a share of sales leaving the UK firm by firm,
# placed at the registered office exactly as the payments are, which no official regional trade figure is.
#
# A pilot on what already exists: the accounts of about 1,950 employers of 1,000 or more, read by OCR in the
# companieshouseopen project (jobs GEO-18 and GEO-19; its geography planner, section 10). For each set of accounts this
# looks for a turnover-by-geography note, reads the UK row, the other rows and the total, and keeps the reading only if
# the rows add up to the total. It reports how many accounts have a note that can be read this way, for employers
# registered in Yorkshire and the Humber and for all of them, and the share of turnover outside the UK by broad industry.
# It also counts accounts that say the split is withheld, and accounts that mention importing.
#
# This is pattern matching on OCR text, not a model reading the page, so it misses notes laid out unusually. The count
# of notes read is a floor. Company-level results name companies, so they go to local/ (not published); only counts and
# industry totals go to tests/outputs.
#
# Run from the repo root: Rscript docs/chunks/paymentflowsviz_ynh/tests/28_accounts_geography.R [path to paper_filers folder]
# Default path: ~/Code/companieshouseopen/local/reports/paper_filers (it needs data/employers_read_*.csv and ocr/*.txt).
source('docs/chunks/paymentflowsviz_ynh/tests/functions_hierarchy.R')
out <- file.path(chunk_dir, 'outputs')
args <- commandArgs(trailingOnly = TRUE); pf <- if (length(args)) args[1] else path.expand('~/Code/companieshouseopen/local/reports/paper_filers')
if (!dir.exists(file.path(pf, 'ocr'))) { cat('No OCR text found at', pf, '- nothing to do.\n'); quit(status = 0) }
chr <- list(character = c('company_number', 'sic'))
e <- rbind(fread(file.path(pf, 'data/employers_read_5000.csv'), colClasses = chr), fread(file.path(pf, 'data/employers_read_1000.csv'), colClasses = chr), fill = TRUE)
e <- unique(e, by = 'stem')
# Yorkshire and the Humber's local authorities, as Companies House names them
ynh <- c('Barnsley', 'Doncaster', 'Rotherham', 'Sheffield', 'Bradford', 'Calderdale', 'Kirklees', 'Leeds', 'Wakefield', 'Kingston upon Hull, City of', 'East Riding of Yorkshire',
         'North East Lincolnshire', 'North Lincolnshire', 'York', 'North Yorkshire')
e[, `:=`(here = reg_la %in% ynh, sic2 = substr(sic, 1, 2))]
e[, broad := fcase(sic2 >= '10' & sic2 <= '33', 'Manufacturing', sic2 %in% c('45', '46'), 'Wholesale and motor trades', sic2 == '47', 'Retail',
                   sic2 >= '49' & sic2 <= '56', 'Transport, storage, accommodation and food', sic2 >= '58' & sic2 <= '63', 'Information and communication',
                   sic2 >= '64' & sic2 <= '66', 'Finance and insurance', sic2 >= '69' & sic2 <= '75', 'Professional, scientific and technical',
                   sic2 %in% c('84', '85', '86', '87', '88'), 'Public services, education, health and care', sic2 < '10' | sic2 %in% c('35', '36', '37', '38', '39', '41', '42', '43'), 'Non-manufacturing production', default = 'Other services')]

# ---- reading one set of accounts
nums <- function(s) { m <- regmatches(s, gregexpr('\\(?[0-9][0-9,]*(\\.[0-9]+)?\\)?', s))[[1]]; m <- m[!grepl('^\\(?(19|20)[0-9]{2}\\)?$', m)]   # leaves out years
  as.numeric(gsub('[(),]', '', m)) }
header_re <- 'geograph\\w*\\s+(market|area|region|destination|analysis|segment|location|split|origin)|by\\s+(geograph|destination)|(turnover|revenue|sales)\\s+by\\s+(geograph|destination|origin|market|region|country)|geographical analysis'
uk_re <- '^[^A-Za-z0-9]*(within the |in the )?(united kingdom|u\\.?k\\.?|great britain|uk and (the )?(republic of )?ireland|united kingdom and (the )?(republic of )?ireland|british isles)\\b'
read_geo <- function(stem) {
  f <- file.path(pf, 'ocr', paste0(stem, '.txt')); if (!file.exists(f)) return(list(note = NA, read = FALSE))
  L <- readLines(f, warn = FALSE, encoding = 'UTF-8'); low <- tolower(L)
  res <- list(note = FALSE, read = FALSE, withheld = any(grepl('prejudicial', low) & grepl('geograph|turnover|market', low)) ||
                any(grepl('geograph', low) & grepl('not (been )?(presented|disclosed|given|provided)', low)),
              mentions_import = sum(grepl('\\bimport(s|ed|ing|er|ers)?\\b', low)), mentions_export = sum(grepl('\\bexport(s|ed|ing|er|ers)?\\b', low)))
  hs <- grep(header_re, low, perl = TRUE); if (!length(hs)) return(res)
  res$note <- TRUE
  for (h in hs) {
    win <- seq(h, min(length(L), h + 16)); uk <- win[grepl(uk_re, low[win], perl = TRUE)]
    uk <- uk[vapply(uk, function(i) length(nums(L[i])) >= 1, TRUE)]; if (!length(uk)) next
    u <- uk[1]; uk_val <- nums(L[u])[1]; rows <- data.table(label = 'UK', value = uk_val); total <- NA_real_
    for (i in seq(u + 1, min(length(L), u + 12))) { v <- nums(L[i]); if (!length(v)) next
      lab <- trimws(gsub('[^A-Za-z &/-]', ' ', sub('[0-9(].*$', '', L[i])))
      if (nchar(gsub('[^A-Za-z]', '', lab)) < 3 || grepl('^total', tolower(lab))) { total <- v[1]; break }       # a line of figures with no label, or "Total": the total
      rows <- rbind(rows, data.table(label = lab, value = v[1])) }
    if (!is.finite(total) || total <= 0 || uk_val < 0) next
    adds_up <- abs(sum(rows$value) - total) <= 0.011 * total
    if (!adds_up) next
    return(modifyList(res, list(read = TRUE, uk = uk_val, total = total, uk_share = uk_val / total, rows = paste(sprintf('%s %s', rows$label, format(rows$value, big.mark = ',', trim = TRUE)), collapse = '; '),
                                wording = trimws(substr(L[h], 1, 140)), by_origin = grepl('origin', low[h]) && !grepl('destination', low[h]), line = h)))
  }
  res
}
cat('reading', nrow(e), 'sets of accounts\n')
got <- lapply(e$stem, read_geo)
pick <- function(n, na = NA) vapply(got, function(g) if (is.null(g[[n]])) na else g[[n]], na)
e[, `:=`(note = pick('note', NA), read = pick('read', FALSE), withheld = pick('withheld', FALSE), uk_share = pick('uk_share', NA_real_), geo_total = pick('total', NA_real_),
         rows = pick('rows', NA_character_), wording = pick('wording', NA_character_), by_origin = pick('by_origin', NA), mentions_import = pick('mentions_import', 0L), mentions_export = pick('mentions_export', 0L))]
# does the note's total match the turnover already read from the same accounts? (figures are printed in pounds, thousands or millions)
e[, matches_turnover := read & !is.na(turnover) & vapply(seq_len(.N), function(i) any(abs(geo_total[i] * c(1, 1e3, 1e6) - turnover[i]) <= 0.02 * abs(turnover[i])), TRUE)]
e[, abroad := 1 - uk_share]

e[, ok := matches_turnover]                      # the reading used below: rows add up, and the total is the turnover read earlier from the same accounts
pc <- function(v) ifelse(is.finite(v), sprintf('%.0f%%', 100 * v), '')
tab <- function(d) d[, .(accounts = .N, wording_found = sum(note, na.rm = TRUE), rows_add_up = sum(read), read_and_checked = sum(ok), say_withheld = sum(withheld & !ok),
                         turnover_checked_gbp_bn = round(sum(turnover[ok]) / 1e9, 1), outside_uk_weighted = pc(sum((abroad * turnover)[ok]) / sum(turnover[ok])), outside_uk_median = pc(median(abroad[ok])))]
cat('\n== Turnover by geography in large employers\' accounts ==\n')
cat('wording_found: words like "turnover by geographical market" are in the text. rows_add_up: a UK row and a total were read, and the rows add up to the total.\n')
cat('read_and_checked: as well as that, the total is the turnover already read from the same accounts. Only these are used for the shares.\n')
cat('outside_uk_weighted: share of turnover outside the UK over the accounts read and checked, weighted by turnover.\n\n')
print(rbind(cbind(set = 'Registered in Yorkshire and the Humber', tab(e[here == TRUE])), cbind(set = 'All employers read', tab(e))))
cat('\nBy broad industry, all employers read:\n'); print(e[, tab(.SD), by = broad][order(-accounts)])
cat('\nBy broad industry, registered in Yorkshire and the Humber:\n'); print(e[here == TRUE, tab(.SD), by = broad][order(-accounts)])
# A company whose sales are all in the UK has nothing to split, so it gives no note. If every company with no note (and no
# statement that it is withheld) sells only in the UK, the share outside the UK across all of them is this. A guess at a floor.
floor_tab <- e[here == TRUE & !is.na(turnover) & !(withheld & !ok), .(companies = .N, turnover_gbp_bn = round(sum(turnover) / 1e9, 1), read_and_checked = sum(ok),
                                                                      outside_uk_if_no_note_means_uk_only = pc(sum((abroad * turnover)[ok]) / sum(turnover))), by = broad][order(-turnover_gbp_bn)]
cat('\nYorkshire and the Humber: share of turnover outside the UK if a company with no note sells only in the UK (those saying the split is withheld left out):\n'); print(floor_tab)
cat('\nAccounts that mention importing or exporting at least once:\n')
print(e[, .(accounts = .N, mention_import = pc(mean(mentions_import > 0)), mention_export = pc(mean(mentions_export > 0))), by = .(set = fifelse(here, 'Registered in Yorkshire and the Humber', 'Elsewhere'))])

# company-level results stay out of the published folder
firm <- e[, .(company_number, reg_name, sic, broad, reg_la, here, made_up, turnover, note, read, withheld, matches_turnover, uk_share, abroad, by_origin, rows, wording, mentions_import, mentions_export, stem)]
fwrite(firm[order(-here, -turnover)], file.path(local_dir, 'accounts_geography_companies.csv'))
cat('\n== Yorkshire and the Humber: the notes read, largest turnover first (company-level; also in local/, not published) ==\n')
print(e[here == TRUE & read == TRUE][order(-turnover)][, .(company = substr(reg_name, 1, 34), sic, turnover_gbp_m = round(turnover / 1e6), outside_uk = pc(abroad), checked = ok, rows = substr(rows, 1, 95))], nrows = 80)
cat('\nYorkshire and the Humber: accounts with a geography wording that could not be read, or that say it is withheld (for reading by hand or by a model):\n')
print(e[here == TRUE & read == FALSE & (note == TRUE | withheld == TRUE)][order(-turnover)][, .(company = substr(reg_name, 1, 34), sic, turnover_gbp_m = round(turnover / 1e6), withheld)], nrows = 80)

# The reading set for the next step (the plan's section 5.4: a model reads each note, and the notes on purchases and currencies):
# companies registered in Yorkshire and the Humber in the industries that make, move or sell goods. Also kept in local/.
goods_side <- c('Manufacturing', 'Wholesale and motor trades', 'Retail', 'Transport, storage, accommodation and food', 'Non-manufacturing production')
rs <- e[here == TRUE & broad %in% goods_side & !is.na(turnover)][, status := fcase(ok, 'read and checked here', withheld, 'says the split is withheld', read, 'read, total not matched', note %in% TRUE, 'wording found, not read', default = 'no note found')]
fwrite(rs[order(broad, -turnover), .(company_number, reg_name, sic, broad, reg_la, made_up, turnover, status, uk_share, stem)], file.path(local_dir, 'accounts_reading_set.csv'))
cat('\n== The reading set for the next step: Yorkshire and the Humber companies that make, move or sell goods ==\n')
print(dcast(rs[, .N, by = .(broad, status)], broad ~ status, value.var = 'N', fill = 0)); cat(nrow(rs), 'companies, turnover GBP', round(sum(rs$turnover) / 1e9, 1), 'bn. Wrote accounts_reading_set.csv in local/\n')

agg <- rbind(e[, cbind(set = 'All employers read', tab(.SD)), by = broad], e[here == TRUE, cbind(set = 'Registered in Yorkshire and the Humber', tab(.SD)), by = broad])
fwrite(agg[order(set, -accounts)], file.path(out, 'accounts_geography_by_industry.csv'))
cat('\nWrote accounts_geography_by_industry.csv (counts and industry totals) and, in local/, accounts_geography_companies.csv (one row a company)\n')
