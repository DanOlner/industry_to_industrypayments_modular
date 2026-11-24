# 29_accounts_reading.R
# What can be read from large companies' accounts about where they sell and where they buy? The follow-up to test 28,
# set out in section 5.4 of plan/other_connections_plan.md. Test 28 found the turnover-by-geography note by pattern
# matching. Here a model reads the pages that bear on trade and fills in a form: the turnover split and whether it is
# by destination or origin, any statement that all sales are in the UK or that the split is withheld, what the accounts
# say about where goods are bought, the currencies purchases are made in, sales to and purchases from group companies,
# and who the customers and suppliers are.
#
# The set is the one test 28 writes: companies registered in Yorkshire and the Humber that make, move or sell goods
# (local/paymentflowsviz_ynh/accounts_reading_set.csv). The text is the OCR already made in the companieshouseopen
# project, and the method is that project's (analysis/paper_filers/02_read.R): the pages are sent to Claude Haiku
# through Claude Code's `claude -p`, signed in as you and on your plan, six at a time, with a JSON schema. Answers are
# kept in local/paymentflowsviz_ynh/accounts_read/, one file a company, and a company is only asked again if the text
# or the form changes.
#
# Checks, as that project's: every figure returned must appear in the text sent; the rows of the split must add up to
# its total; the total must be the company's turnover; and every quote must be found in the text. The split is also
# set against test 28's reading where it has one.
#
# Company-level results name companies, so they stay in local/. Counts by industry go to tests/outputs.
# Run from the repo root: Rscript docs/chunks/paymentflowsviz_ynh/tests/29_accounts_reading.R [--only stem] [--dry] [--path paper_filers folder]
#   --dry   choose the pages and say how much would be sent, without asking the model
source('docs/chunks/paymentflowsviz_ynh/tests/functions_hierarchy.R')
suppressPackageStartupMessages({library(jsonlite); library(processx)})
out <- file.path(chunk_dir, 'outputs'); LLM <- file.path(local_dir, 'accounts_read'); dir.create(LLM, showWarnings = FALSE, recursive = TRUE)
args <- commandArgs(trailingOnly = TRUE); arg <- function(name, default) if (name %in% args) args[which(args == name) + 1] else default
pf <- arg('--path', path.expand('~/Code/companieshouseopen/local/reports/paper_filers')); ONLY <- arg('--only', NA_character_); DRY <- '--dry' %in% args
MODEL <- 'haiku'; WORKERS <- 6; TIMEOUT <- 600; MAX_CHARS <- 110000
VERSION <- 'edge-v2'                             # change when the prompt or the form changes, so kept answers are asked again
t0 <- Sys.time(); mins <- function() sprintf('%.1f min', as.numeric(Sys.time() - t0, units = 'mins'))
set_file <- file.path(local_dir, 'accounts_reading_set.csv')
if (!file.exists(set_file) || !dir.exists(file.path(pf, 'ocr'))) { cat('Needs', set_file, '(run test 28) and the OCR text at', pf, '\n'); quit(status = 0) }
set <- fread(set_file, colClasses = list(character = c('company_number', 'sic')))
if (!is.na(ONLY)) set <- set[stem == ONLY]

# ---- 1. the pages to send: those that bear on sales by place, purchases, currencies, group trade, customers and suppliers
pages_of <- function(f) {
  txt <- sub('=== OCR PAGES \\d+ OF \\d+ ===\\s*$', '', paste(readLines(f, warn = FALSE, encoding = 'UTF-8'), collapse = '\n'))
  pg <- strsplit(txt, '=== PAGE (?=\\d+ ===)', perl = TRUE)[[1]][-1]
  data.table(page = as.integer(regmatches(pg, regexpr('^\\d+', pg))), text = sub('^\\d+ ===\\s*', '', pg))
}
KEEP <- paste0('turnover|revenue|geograph|destination|by origin|export|import|overseas|currenc|foreign exchange|denominated|supplier|customer|sourc(e|ed|ing)\\b|procure|',
               'related part|group undertaking|fellow subsidiar|parent (company|undertaking)|principal activit|cost of sales|raw materials|profit and loss|income statement')
CORE <- '(turnover|revenue|sales)[^\\n]{0,80}(geograph|destination|origin|market|segment)|(geograph|destination|segment)[^\\n]{0,80}(turnover|revenue|sales)|profit and loss account|income statement|related part|currency risk|foreign (exchange|currency) risk'
select_text <- function(f) {
  p <- pages_of(f); p <- p[grepl(KEEP, text, ignore.case = TRUE, perl = TRUE) & nchar(gsub('\\s+', ' ', text)) > 200]
  p[, core := grepl(CORE, text, ignore.case = TRUE, perl = TRUE)]
  p <- p[order(-core, page)][, cum := cumsum(nchar(text))][cum <= MAX_CHARS][order(page)]
  paste(sprintf('=== PAGE %d ===\n%s', p$page, p$text), collapse = '\n')
}

# ---- 2. the form
num <- list(type = c('number', 'null')); str_or_null <- list(type = c('string', 'null'))
geo_row <- list(type = 'object', additionalProperties = FALSE, properties = list(label = list(type = 'string'), this_year = num, prior_year = num), required = c('label', 'this_year', 'prior_year'))
props <- list(
  currency = list(type = 'string', enum = list('GBP', 'EUR', 'USD', 'other')), scale = list(type = 'string', enum = list('units', 'thousands', 'millions')),
  accounts_of = list(type = 'string', enum = list('company', 'group')), turnover = num, cost_of_sales = num,
  geography = list(type = 'string', enum = list('table', 'statement', 'withheld', 'none')), geography_basis = list(type = 'string', enum = list('destination', 'origin', 'not stated')),
  geography_rows = list(type = 'array', items = geo_row), geography_total = num, geography_page = list(type = c('integer', 'null')), geography_quote = str_or_null,
  all_in_uk = list(type = c('boolean', 'null')), exports_quote = str_or_null, purchases_quote = str_or_null,
  purchase_currencies = list(type = 'array', items = list(type = 'string')), currency_quote = str_or_null,
  group_sales = num, group_purchases = num, group_quote = str_or_null, customers_quote = str_or_null, suppliers_quote = str_or_null, principal_activity = str_or_null)
SCHEMA <- list(type = 'object', additionalProperties = FALSE, properties = props, required = names(props))
SYSTEM <- paste(
  'You read pages from a UK company\'s annual report and financial statements (often OCR of a scan) and fill in a form about where it sells and where it buys.',
  'Copy numbers exactly as printed; never compute, add up, convert or guess. Use null (or an empty list) when something is not given. Every quote must be copied word for word from the text, up to 300 characters, and must not include the names of people.',
  'scale is the unit of the money columns ("£\'000" is thousands, "£m" is millions). currency is the reporting currency.',
  'accounts_of: if the document has both a consolidated (group) profit and loss account and the company\'s own, use the group\'s figures throughout and say "group"; otherwise "company".',
  'turnover is the total turnover or revenue for the latest year, and cost_of_sales the cost of sales line for the latest year, both from the profit and loss account or income statement.',
  'geography: "table" if the notes analyse turnover or revenue by geographical market, area, region, destination or origin in a table of figures; "statement" if there is no table but a sentence says where turnover arises (for example that all turnover arises in the United Kingdom); "withheld" if the accounts say the geographical analysis is not given (for example because it would be seriously prejudicial); "none" if nothing is said.',
  'geography_basis: "destination" if the analysis is by where customers are (by destination, by geographical market, by location of customer); "origin" if it is by where the selling business is (by origin); "not stated" otherwise. If both are given, use the analysis by destination.',
  'geography_rows: each row of that table with its label as printed and the figures for the latest year (this_year) and the year before (prior_year). Leave out the total row. geography_total is the total as printed under the rows. An empty list and null if there is no table.',
  'geography_page is the page number (from the === PAGE n === markers) of the analysis or statement. geography_quote quotes the heading or sentence that introduces the analysis, or the statement, or the sentence saying it is withheld.',
  'all_in_uk: true if the accounts say that all, or substantially all, turnover arises in or is to customers in the United Kingdom (or the UK and Ireland); false if they show or say that some is elsewhere; null if they do not say.',
  'exports_quote: a sentence that is specifically about exporting or selling to customers in other countries; null if there is none (a general description of the business does not count). purchases_quote: a sentence that says where the company buys or sources its goods, materials or services from (named countries or regions, imports, overseas or UK suppliers); null if there is none.',
  'purchase_currencies: the currencies, other than the reporting currency, in which the accounts say the company buys goods or has exposure on purchases or trade payables, as three-letter codes (EUR, USD, CNY and so on). currency_quote: the sentence that says so.',
  'group_sales is the total sales to parent, fellow subsidiary or other group companies in the latest year, and group_purchases the total purchases from them, only if a figure is printed (for example in a related party note). group_quote: the sentence or table heading they come from.',
  'customers_quote: a sentence saying who the customers are (kinds of customer, sectors, or how much of turnover the largest customers account for); null if there is none. suppliers_quote: a sentence saying who or where the suppliers are (kinds of supplier, what is bought from them, group companies, or how much the largest account for); null if there is none. General statements about valuing or paying suppliers do not count.',
  'principal_activity quotes the principal activity sentence.')

ask <- function(stem, prompt) {
  dir <- tempfile('accounts_'); dir.create(dir); input <- file.path(dir, 'prompt.txt'); writeLines(prompt, input, useBytes = TRUE)
  a <- c('-p', '--model', MODEL, '--effort', 'low', '--tools', '', '--strict-mcp-config', '--setting-sources', 'project', '--no-session-persistence', '--output-format', 'json',
         '--system-prompt', SYSTEM, '--json-schema', toJSON(SCHEMA, auto_unbox = TRUE))
  e <- Sys.getenv(); keep <- names(e) %in% c('CLAUDE_CONFIG_DIR', 'CLAUDE_CODE_OAUTH_TOKEN') | !grepl('^(CLAUDE|VSCODE_)', names(e))   # the environment claude runs in: this one, less anything tying it to the session that starts it
  list(stem = stem, dir = dir, proc = process$new('claude', a, stdin = input, stdout = file.path(dir, 'out.json'), stderr = file.path(dir, 'err.txt'), wd = dir, env = setNames(as.character(e[keep]), names(e)[keep])))
}
finish <- function(job, text_hash) {
  f <- file.path(job$dir, 'out.json'); raw <- if (file.exists(f)) paste(readLines(f, warn = FALSE), collapse = '\n') else ''
  err <- file.path(job$dir, 'err.txt'); msg <- if (file.exists(err)) paste(readLines(err, warn = FALSE), collapse = ' ') else ''
  unlink(job$dir, recursive = TRUE)
  r <- tryCatch(fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(r) || isTRUE(r$is_error) || is.null(r$structured_output)) { cat('  no answer for', job$stem, ':', substr(paste(msg, if (!is.null(r$result)) r$result else substr(raw, 1, 200)), 1, 300), '\n'); return(FALSE) }
  writeLines(toJSON(list(answer = r$structured_output, text_hash = text_hash, version = VERSION, cost_usd = r$total_cost_usd, asked_at = format(Sys.time())), auto_unbox = TRUE, null = 'null', digits = NA),
             file.path(LLM, paste0(job$stem, '.json')))
  TRUE
}

set[, text := vapply(file.path(pf, 'ocr', paste0(stem, '.txt')), function(f) if (file.exists(f)) select_text(f) else '', '')]
set[, prompt := sprintf('Accounts of: %s (company number %s), made up to %s\n\n%s', reg_name, company_number, made_up, text)]
set[, text_hash := vapply(paste(VERSION, prompt), function(s) { f <- tempfile(); writeLines(s, f, useBytes = TRUE); h <- unname(tools::md5sum(f)); unlink(f); h }, '')]
set[, cached := vapply(stem, function(s) { f <- file.path(LLM, paste0(s, '.json')); if (file.exists(f)) { h <- fromJSON(f)$text_hash; if (is.null(h)) '' else h } else '' }, '')]
todo <- set[nchar(text) > 0 & cached != text_hash]
cat(sprintf('%d companies in the set, %d with text, %d to ask %s (%d at a time); %s characters to send, %s the longest\n', nrow(set), set[nchar(text) > 0, .N], nrow(todo), MODEL, WORKERS,
            format(sum(nchar(todo$prompt)), big.mark = ','), format(max(c(0, nchar(todo$prompt))), big.mark = ',')))
if (DRY) quit(status = 0)

running <- list(); queue <- seq_len(nrow(todo)); failed <- character(0); tries <- integer(0); n_done <- 0
while (length(queue) || length(running)) {
  while (length(running) < WORKERS && length(queue)) { i <- queue[1]; queue <- queue[-1]; running[[length(running) + 1]] <- c(ask(todo$stem[i], todo$prompt[i]), list(i = i)) }
  Sys.sleep(1)
  done <- vapply(running, function(j) !j$proc$is_alive() || as.numeric(Sys.time() - j$proc$get_start_time(), units = 'secs') > TIMEOUT, TRUE)
  for (j in running[done]) {
    if (j$proc$is_alive()) j$proc$kill()
    ok <- finish(j, todo$text_hash[j$i]); n_done <- n_done + ok
    if (ok && n_done %% 20 == 0) cat(sprintf('  %d asked (%s)\n', n_done, mins()))
    if (!ok) { s <- todo$stem[j$i]; tries[s] <- if (is.na(tries[s])) 1L else tries[s] + 1L; if (tries[s] < 2) queue <- c(queue, j$i) else failed <- c(failed, s) }   # one more go after a failure
  }
  running <- running[!done]
}
if (nrow(todo)) cat(sprintf('  asked in %s; %d failed%s\n', mins(), length(failed), if (length(failed)) paste0(': ', paste(failed, collapse = ', ')) else ''))

# ---- 3. tidy the answers and check them against the text sent
got <- set[file.exists(file.path(LLM, paste0(stem, '.json')))]
ans <- lapply(setNames(got$stem, got$stem), function(s) fromJSON(file.path(LLM, paste0(s, '.json')), simplifyVector = FALSE))
cost <- sum(vapply(ans, function(a) if (is.null(a$cost_usd)) 0 else a$cost_usd, 0))
`%||%` <- function(a, b) if (is.null(a)) b else a
flat_text <- setNames(gsub('(?<=\\d),(?=\\d{3}\\b)', '', got$text, perl = TRUE), got$stem)          # thousands separators out, so 3,107,891 is found as 3107891
has <- function(t, d) grepl(paste0('(?<![0-9])', gsub('.', '\\.', d, fixed = TRUE), '(?![0-9])'), t, perl = TRUE)
found_how <- function(t, v) { d <- format(abs(v), scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
  if (has(t, d)) return('as printed')
  if (grepl('.', d, fixed = TRUE) && (has(t, sub('.', ' ', d, fixed = TRUE)) || has(t, sub('.', '', d, fixed = TRUE)))) return('point lost')   # OCR often drops the decimal point of figures in millions
  'not found' }
squash <- function(s) gsub('[^a-z0-9]+', ' ', tolower(s))
quote_found <- function(t, q) { if (is.null(q) || !nzchar(q)) return(NA); tt <- squash(t); qq <- trimws(squash(q)); grepl(qq, tt, fixed = TRUE) || grepl(substr(qq, 1, 60), tt, fixed = TRUE) }
is_uk <- function(l) grepl('united kingdom|\\bu ?k\\b|great britain|british isles', l, perl = TRUE) & !grepl('outside|excluding|\\bnon|rest of|other than|overseas|europe', l, perl = TRUE)
mult <- c(units = 1, thousands = 1e3, millions = 1e6)
quotes <- c('geography_quote', 'exports_quote', 'purchases_quote', 'currency_quote', 'group_quote', 'customers_quote', 'suppliers_quote', 'principal_activity')
res <- rbindlist(lapply(names(ans), function(s) { a <- ans[[s]]$answer; t <- flat_text[[s]]; rows <- a$geography_rows
  rv <- vapply(rows, function(r) r$this_year %||% NA_real_, 0); rl <- vapply(rows, function(r) r$label, ''); m <- mult[[a$scale]]
  figs <- c(rv, vapply(rows, function(r) r$prior_year %||% NA_real_, 0), a$geography_total %||% NA_real_, a$turnover %||% NA_real_, a$cost_of_sales %||% NA_real_, a$group_sales %||% NA_real_, a$group_purchases %||% NA_real_)
  figs <- figs[!is.na(figs)]; how <- vapply(figs, function(v) found_how(t, v), '')
  # the total: as printed under the rows, or the rows added up where no total is printed. A row with no figure (lost in the OCR, or a dash) counts as nil.
  total <- a$geography_total %||% NA_real_; base <- if (is.finite(total)) total else if (length(rv) && any(is.finite(rv))) sum(rv, na.rm = TRUE) else NA_real_
  uk <- is_uk(tolower(rl)) & is.finite(rv); qf <- vapply(quotes, function(q) quote_found(t, a[[q]]), NA)
  data.table(stem = s, currency = a$currency, scale = a$scale, accounts_of = a$accounts_of, turnover_read = (a$turnover %||% NA_real_) * m, cost_of_sales = (a$cost_of_sales %||% NA_real_) * m,
             geography = a$geography, basis = a$geography_basis, n_rows = length(rows), rows = paste(sprintf('%s %s', rl, format(rv, big.mark = ',', trim = TRUE, scientific = FALSE)), collapse = '; '),
             geo_total = base * m, total_printed = is.finite(total), rows_blank = sum(!is.finite(rv)), rows_add_up = length(rows) > 0 && is.finite(base) && abs(sum(rv, na.rm = TRUE) - base) <= 0.011 * abs(base),
             uk_value = if (any(uk)) sum(rv[uk]) * m else NA_real_, uk_share_model = if (any(uk) && is.finite(base) && base > 0) sum(rv[uk]) / base else NA_real_,
             all_in_uk = a$all_in_uk %||% NA, geography_page = a$geography_page %||% NA_integer_, geography_quote = a$geography_quote %||% NA_character_,
             exports_quote = a$exports_quote %||% NA_character_, purchases_quote = a$purchases_quote %||% NA_character_, purchase_currencies = paste(unlist(a$purchase_currencies), collapse = ' '),
             currency_quote = a$currency_quote %||% NA_character_, group_sales = (a$group_sales %||% NA_real_) * m, group_purchases = (a$group_purchases %||% NA_real_) * m, group_quote = a$group_quote %||% NA_character_,
             customers_quote = a$customers_quote %||% NA_character_, suppliers_quote = a$suppliers_quote %||% NA_character_, principal_activity = a$principal_activity %||% NA_character_,
             figures = length(figs), figures_found = sum(how != 'not found'), quotes_given = sum(!is.na(qf)), quotes_found = sum(qf, na.rm = TRUE)) }))
res <- merge(got[, .(stem, company_number, reg_name, sic, broad, turnover, status28 = status, uk_share28 = uk_share)], res, by = 'stem')
near <- function(a, b, tol = 0.02) is.finite(a) & is.finite(b) & abs(a - b) <= tol * abs(b)
res[, `:=`(turnover_agrees = near(turnover_read, turnover), total_is_turnover = near(geo_total, turnover) | near(geo_total, turnover_read))]
res[, split_ok := geography == 'table' & rows_add_up & total_is_turnover & is.finite(uk_share_model)]
# A statement in words. "All turnover arises in the UK" is taken as nil outside the UK. "Principally" or "substantially" in the
# UK is kept apart: it gives no figure, so it is not counted as nil.
gq <- tolower(res$geography_quote); says_uk <- grepl('united kingdom|\\buk\\b', gq, perl = TRUE)
res[, mostly := geography == 'statement' & says_uk & grepl('principally|substantially|mainly|predominantly|primarily|majority', gq)]
res[, wholly := geography == 'statement' & says_uk & !mostly & all_in_uk %in% TRUE]
res[, outside_uk := fifelse(split_ok, 1 - uk_share_model, fifelse(wholly, 0, NA_real_))]
res[, source_of_share := fcase(split_ok, 'table', wholly, 'statement: all in the UK', mostly, 'statement: mostly in the UK', geography == 'withheld', 'withheld', geography == 'table', 'table, not checked', default = 'not given')]
# Two things read off the customer sentence by pattern, for counting only: does it say the main customers are retailers or
# supermarkets, and does it say the main customer is the company's own group (or a named parent)?
cq <- tolower(paste(res$customers_quote, res$principal_activity))
res[, `:=`(sells_to_retailers = !is.na(customers_quote) & grepl('retailers|supermarket|grocer', tolower(customers_quote)),
           sells_to_own_group = !is.na(customers_quote) & grepl('(majority|principal|predominantly|supplying)[^.]{0,60}(group|asda)|derived from group|other group companies', tolower(customers_quote)))]
fwrite(res[order(broad, -turnover)], file.path(local_dir, 'accounts_reading_results.csv'))

pc <- function(v) ifelse(is.finite(v), sprintf('%.0f%%', 100 * v), '')
cat(sprintf('\n== %d companies read. Notional cost $%.2f ==\n', nrow(res), cost))
cat(sprintf('Figures returned: %d, found in the text sent: %d (%s). Quotes returned: %d, found in the text: %d (%s).\n', sum(res$figures), sum(res$figures_found), pc(sum(res$figures_found) / sum(res$figures)),
            sum(res$quotes_given), sum(res$quotes_found), pc(sum(res$quotes_found) / sum(res$quotes_given))))
cat(sprintf('Turnover read agrees with the turnover read earlier from the same accounts (within 2%%): %d of %d.\n', sum(res$turnover_agrees), res[is.finite(turnover_read), .N]))

cat('\n== Against test 28\'s pattern matching ==\n')
both <- res[status28 == 'read and checked here']
cat(sprintf('Of the %d that test 28 read and checked, the model gives a checked table for %d; the UK share is within one point for %d. Largest gap: %.1f points.\n', nrow(both), both[split_ok == TRUE, .N],
            both[split_ok == TRUE & abs(uk_share_model - uk_share28) <= 0.01, .N], 100 * both[split_ok == TRUE, max(abs(uk_share_model - uk_share28))]))
print(dcast(res[, .N, by = .(test_28 = status28, model = source_of_share)], test_28 ~ model, value.var = 'N', fill = 0))

summ <- function(d) d[, .(companies = .N, turnover_gbp_bn = round(sum(turnover) / 1e9, 1), split_from_table = sum(split_ok), say_all_in_uk = sum(wholly), say_mostly_in_uk = sum(mostly), withheld = sum(geography == 'withheld'),
                          not_given = sum(source_of_share %in% c('not given', 'table, not checked')), turnover_with_a_share_gbp_bn = round(sum(turnover[!is.na(outside_uk)]) / 1e9, 1),
                          outside_uk_weighted = pc(sum((outside_uk * turnover)[!is.na(outside_uk)]) / sum(turnover[!is.na(outside_uk)])), by_destination = sum(split_ok & basis == 'destination'), by_origin = sum(split_ok & basis == 'origin'),
                          say_where_they_buy = sum(!is.na(purchases_quote)), name_purchase_currencies = sum(nzchar(purchase_currencies)), give_group_purchases = sum(is.finite(group_purchases)), give_group_sales = sum(is.finite(group_sales)),
                          say_who_customers_are = sum(!is.na(customers_quote)), say_who_suppliers_are = sum(!is.na(suppliers_quote)),
                          main_customers_retailers = sum(sells_to_retailers), their_turnover_gbp_bn = round(sum(turnover[sells_to_retailers]) / 1e9, 1), main_customer_own_group = sum(sells_to_own_group), its_turnover_gbp_bn = round(sum(turnover[sells_to_own_group]) / 1e9, 1))]
by_ind <- rbind(res[, summ(.SD), by = broad][order(-turnover_gbp_bn)], cbind(broad = 'All', summ(res)))
cat('\n== What the accounts give, by broad industry ==\n'); print(by_ind)
cat('\nCurrencies named for purchases (companies naming each):\n'); print(sort(table(unlist(strsplit(res$purchase_currencies[nzchar(res$purchase_currencies)], ' '))), decreasing = TRUE))
gp <- res[is.finite(group_purchases) | is.finite(group_sales)]
if (nrow(gp)) { cat('\nTrade with group companies, where a figure is printed, as a share of turnover and of cost of sales:\n')
  print(gp[, .(broad, sales_to_group_over_turnover = pc(group_sales / turnover_read), purchases_from_group_over_cost_of_sales = pc(group_purchases / cost_of_sales))][order(broad)]) }
fwrite(by_ind, file.path(out, 'accounts_reading_summary.csv'))

cat('\n== Company by company (also in local/, not published) ==\n')
print(res[order(broad, -turnover), .(company = substr(reg_name, 1, 30), sic, turnover_m = round(turnover / 1e6), of = accounts_of, geography, basis = substr(basis, 1, 5), outside_uk = pc(outside_uk), src = substr(source_of_share, 1, 12),
                                     t28 = pc(1 - uk_share28), buy_ccy = purchase_currencies, grp_buy_m = round(group_purchases / 1e6), figs = sprintf('%d/%d', figures_found, figures), quotes = sprintf('%d/%d', quotes_found, quotes_given))], nrows = 100)
cat('\nWrote accounts_reading_summary.csv (counts by industry) and, in local/, accounts_reading_results.csv (one row a company)\n')
