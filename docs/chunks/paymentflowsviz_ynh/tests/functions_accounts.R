# functions_accounts.R
# Shared by tests 29 and 30: choosing the pages of a company's OCR'd accounts to send, asking Claude Haiku about them
# through Claude Code's `claude -p` with a JSON schema, keeping each answer, and checking the figures and quotes that
# come back against the text sent. The method is the companieshouseopen project's (analysis/paper_filers/02_read.R).
suppressPackageStartupMessages({library(data.table); library(jsonlite); library(processx)})
`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- the text: OCR files hold one "=== PAGE n ===" marker a page
pages_of <- function(f) {
  txt <- sub('=== OCR PAGES \\d+ OF \\d+ ===\\s*$', '', paste(readLines(f, warn = FALSE, encoding = 'UTF-8'), collapse = '\n'))
  pg <- strsplit(txt, '=== PAGE (?=\\d+ ===)', perl = TRUE)[[1]][-1]
  data.table(page = as.integer(regmatches(pg, regexpr('^\\d+', pg))), text = sub('^\\d+ ===\\s*', '', pg))
}
# The pages to send: those that bear on sales by place, purchases, currencies, group trade, customers and suppliers. The
# pages most likely to hold the turnover note, the profit and loss account and the related party and currency notes go
# first, so a long narrative report can't push them past the limit; then the rest in page order.
ACC_KEEP <- paste0('turnover|revenue|geograph|destination|by origin|export|import|overseas|currenc|foreign exchange|denominated|supplier|customer|sourc(e|ed|ing)\\b|procure|',
                   'related part|group undertaking|fellow subsidiar|parent (company|undertaking)|principal activit|cost of sales|raw materials|profit and loss|income statement')
ACC_CORE <- '(turnover|revenue|sales)[^\\n]{0,80}(geograph|destination|origin|market|segment)|(geograph|destination|segment)[^\\n]{0,80}(turnover|revenue|sales)|profit and loss account|income statement|related part|currency risk|foreign (exchange|currency) risk'
select_text <- function(f, max_chars = 110000) {
  p <- pages_of(f); p <- p[grepl(ACC_KEEP, text, ignore.case = TRUE, perl = TRUE) & nchar(gsub('\\s+', ' ', text)) > 200]
  p[, core := grepl(ACC_CORE, text, ignore.case = TRUE, perl = TRUE)]
  p <- p[order(-core, page)][, cum := cumsum(nchar(text))][cum <= max_chars][order(page)]
  paste(sprintf('=== PAGE %d ===\n%s', p$page, p$text), collapse = '\n')
}

# ---- asking, and keeping the answers
hash_of <- function(s) { f <- tempfile(); writeLines(s, f, useBytes = TRUE); h <- unname(tools::md5sum(f)); unlink(f); h }
kept_hash <- function(dir, stem) vapply(stem, function(s) { f <- file.path(dir, paste0(s, '.json')); if (file.exists(f)) fromJSON(f)$text_hash %||% '' else '' }, '')
# Ask about each row of todo (columns stem, prompt, text_hash), `workers` at a time, and keep each answer as
# <dir>/<stem>.json with the hash of what was sent, so it is only asked again if the text or the form changes. One more
# go after a failure. Stops early if `stop_after` fail in a row, which is what running out of allowance looks like.
ask_all <- function(todo, system, schema, version, dir, model = 'haiku', workers = 6, timeout = 600, stop_after = 12, every = 20) {
  t0 <- Sys.time(); mins <- function() sprintf('%.1f min', as.numeric(Sys.time() - t0, units = 'mins'))
  ask <- function(stem, prompt) {
    d <- tempfile('accounts_'); dir.create(d); input <- file.path(d, 'prompt.txt'); writeLines(prompt, input, useBytes = TRUE)
    a <- c('-p', '--model', model, '--effort', 'low', '--tools', '', '--strict-mcp-config', '--setting-sources', 'project', '--no-session-persistence', '--output-format', 'json',
           '--system-prompt', system, '--json-schema', toJSON(schema, auto_unbox = TRUE))
    e <- Sys.getenv(); keep <- names(e) %in% c('CLAUDE_CONFIG_DIR', 'CLAUDE_CODE_OAUTH_TOKEN') | !grepl('^(CLAUDE|VSCODE_)', names(e))   # the environment claude runs in: this one, less anything tying it to the session that starts it
    list(stem = stem, dir = d, proc = process$new('claude', a, stdin = input, stdout = file.path(d, 'out.json'), stderr = file.path(d, 'err.txt'), wd = d, env = setNames(as.character(e[keep]), names(e)[keep])))
  }
  finish <- function(job, text_hash) {
    f <- file.path(job$dir, 'out.json'); raw <- if (file.exists(f)) paste(readLines(f, warn = FALSE), collapse = '\n') else ''
    err <- file.path(job$dir, 'err.txt'); msg <- if (file.exists(err)) paste(readLines(err, warn = FALSE), collapse = ' ') else ''
    unlink(job$dir, recursive = TRUE)
    r <- tryCatch(fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(r) || isTRUE(r$is_error) || is.null(r$structured_output)) { cat('  no answer for', job$stem, ':', substr(paste(msg, if (!is.null(r$result)) r$result else substr(raw, 1, 200)), 1, 300), '\n'); return(FALSE) }
    writeLines(toJSON(list(answer = r$structured_output, text_hash = text_hash, version = version, cost_usd = r$total_cost_usd, asked_at = format(Sys.time())), auto_unbox = TRUE, null = 'null', digits = NA),
               file.path(dir, paste0(job$stem, '.json')))
    TRUE
  }
  running <- list(); queue <- seq_len(nrow(todo)); failed <- character(0); tries <- integer(0); n_done <- 0; in_a_row <- 0; stopped <- FALSE
  while (length(queue) || length(running)) {
    while (!stopped && length(running) < workers && length(queue)) { i <- queue[1]; queue <- queue[-1]; running[[length(running) + 1]] <- c(ask(todo$stem[i], todo$prompt[i]), list(i = i)) }
    Sys.sleep(1)
    done <- vapply(running, function(j) !j$proc$is_alive() || as.numeric(Sys.time() - j$proc$get_start_time(), units = 'secs') > timeout, TRUE)
    for (j in running[done]) {
      if (j$proc$is_alive()) j$proc$kill()
      ok <- finish(j, todo$text_hash[j$i]); n_done <- n_done + ok; in_a_row <- if (ok) 0 else in_a_row + 1
      if (ok && n_done %% every == 0) cat(sprintf('  %d asked (%s)\n', n_done, mins()))
      if (!ok) { s <- todo$stem[j$i]; tries[s] <- if (is.na(tries[s])) 1L else tries[s] + 1L; if (tries[s] < 2 && !stopped) queue <- c(queue, j$i) else failed <- c(failed, s) }
    }
    running <- running[!done]
    if (!stopped && in_a_row >= stop_after) { stopped <- TRUE; cat(sprintf('  %d failures in a row: stopping. %d not asked. Run again later to carry on from here.\n', in_a_row, length(queue))); queue <- integer(0) }
  }
  if (nrow(todo)) cat(sprintf('  asked %d in %s; %d failed%s\n', n_done, mins(), length(failed), if (length(failed) && length(failed) <= 12) paste0(': ', paste(failed, collapse = ', ')) else ''))
  invisible(list(failed = failed, stopped = stopped))
}

# ---- checking what comes back against the text sent
flat_numbers <- function(text) gsub('(?<=\\d),(?=\\d{3}\\b)', '', text, perl = TRUE)            # thousands separators out, so 3,107,891 is found as 3107891
has_number <- function(t, d) grepl(paste0('(?<![0-9])', gsub('.', '\\.', d, fixed = TRUE), '(?![0-9])'), t, perl = TRUE)
found_how <- function(t, v) { d <- format(abs(v), scientific = FALSE, trim = TRUE, drop0trailing = TRUE)   # t: text with separators out
  if (has_number(t, d)) return('as printed')
  if (grepl('.', d, fixed = TRUE) && (has_number(t, sub('.', ' ', d, fixed = TRUE)) || has_number(t, sub('.', '', d, fixed = TRUE)))) return('point lost')   # OCR often drops the decimal point of figures in millions
  'not found' }
squash <- function(s) gsub('[^a-z0-9]+', ' ', tolower(s))
quote_found <- function(t, q) { if (is.null(q) || !nzchar(q)) return(NA); tt <- squash(t); qq <- trimws(squash(q)); grepl(qq, tt, fixed = TRUE) || grepl(substr(qq, 1, 60), tt, fixed = TRUE) }
# a row of a turnover table that is the UK's: the UK named anywhere in the label, unless the label is about somewhere else
is_uk <- function(l) grepl('united kingdom|\\bu ?k\\b|great britain|british isles', l, perl = TRUE) & !grepl('outside|excluding|\\bnon|rest of|other than|overseas|europe', l, perl = TRUE)
near <- function(a, b, tol = 0.02) is.finite(a) & is.finite(b) & abs(a - b) <= tol * abs(b)
money_scale <- c(units = 1, thousands = 1e3, millions = 1e6)

# The fields both forms share, and the instructions for them
acc_num <- list(type = c('number', 'null')); acc_str <- list(type = c('string', 'null'))
acc_geo_row <- list(type = 'object', additionalProperties = FALSE, properties = list(label = list(type = 'string'), this_year = acc_num, prior_year = acc_num), required = c('label', 'this_year', 'prior_year'))
acc_props <- list(
  currency = list(type = 'string', enum = list('GBP', 'EUR', 'USD', 'other')), scale = list(type = 'string', enum = list('units', 'thousands', 'millions')),
  accounts_of = list(type = 'string', enum = list('company', 'group')), turnover = acc_num, cost_of_sales = acc_num,
  geography = list(type = 'string', enum = list('table', 'statement', 'withheld', 'none')), geography_basis = list(type = 'string', enum = list('destination', 'origin', 'not stated')),
  geography_rows = list(type = 'array', items = acc_geo_row), geography_total = acc_num, geography_page = list(type = c('integer', 'null')), geography_quote = acc_str,
  all_in_uk = list(type = c('boolean', 'null')), exports_quote = acc_str, purchases_quote = acc_str,
  purchase_currencies = list(type = 'array', items = list(type = 'string')), currency_quote = acc_str,
  group_sales = acc_num, group_purchases = acc_num, group_quote = acc_str, customers_quote = acc_str, suppliers_quote = acc_str, principal_activity = acc_str)
acc_system <- c(
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

# One company's answer on the shared fields, tidied and checked. a: the answer; t: the text sent, separators out; turnover: read earlier from the same accounts
acc_quotes <- c('geography_quote', 'exports_quote', 'purchases_quote', 'currency_quote', 'group_quote', 'customers_quote', 'suppliers_quote', 'principal_activity')
tidy_answer <- function(s, a, t) {
  rows <- a$geography_rows; rv <- vapply(rows, function(r) r$this_year %||% NA_real_, 0); rl <- vapply(rows, function(r) r$label, ''); m <- money_scale[[a$scale]]
  figs <- c(rv, vapply(rows, function(r) r$prior_year %||% NA_real_, 0), a$geography_total %||% NA_real_, a$turnover %||% NA_real_, a$cost_of_sales %||% NA_real_, a$group_sales %||% NA_real_, a$group_purchases %||% NA_real_)
  figs <- figs[!is.na(figs)]; how <- vapply(figs, function(v) found_how(t, v), '')
  # the total: as printed under the rows, or the rows added up where no total is printed. A row with no figure (lost in the OCR, or a dash) counts as nil.
  total <- a$geography_total %||% NA_real_; base <- if (is.finite(total)) total else if (length(rv) && any(is.finite(rv))) sum(rv, na.rm = TRUE) else NA_real_
  uk <- is_uk(tolower(rl)) & is.finite(rv); qf <- vapply(acc_quotes, function(q) quote_found(t, a[[q]]), NA)
  data.table(stem = s, currency = a$currency, scale = a$scale, accounts_of = a$accounts_of, turnover_read = (a$turnover %||% NA_real_) * m, cost_of_sales = (a$cost_of_sales %||% NA_real_) * m,
             geography = a$geography, basis = a$geography_basis, n_rows = length(rows), rows = paste(sprintf('%s %s', rl, format(rv, big.mark = ',', trim = TRUE, scientific = FALSE)), collapse = '; '),
             geo_total = base * m, total_printed = is.finite(total), rows_blank = sum(!is.finite(rv)), rows_add_up = length(rows) > 0 && is.finite(base) && abs(sum(rv, na.rm = TRUE) - base) <= 0.011 * abs(base),
             uk_value = if (any(uk)) sum(rv[uk]) * m else NA_real_, uk_share_model = if (any(uk) && is.finite(base) && base > 0) sum(rv[uk]) / base else NA_real_,
             all_in_uk = a$all_in_uk %||% NA, geography_page = a$geography_page %||% NA_integer_, geography_quote = a$geography_quote %||% NA_character_,
             exports_quote = a$exports_quote %||% NA_character_, purchases_quote = a$purchases_quote %||% NA_character_, purchase_currencies = paste(unlist(a$purchase_currencies), collapse = ' '),
             currency_quote = a$currency_quote %||% NA_character_, group_sales = (a$group_sales %||% NA_real_) * m, group_purchases = (a$group_purchases %||% NA_real_) * m, group_quote = a$group_quote %||% NA_character_,
             customers_quote = a$customers_quote %||% NA_character_, suppliers_quote = a$suppliers_quote %||% NA_character_, principal_activity = a$principal_activity %||% NA_character_,
             figures = length(figs), figures_found = sum(how != 'not found'), quotes_given = sum(!is.na(qf)), quotes_found = sum(qf, na.rm = TRUE))
}
# The share of turnover outside the UK, and where it comes from. res needs the tidied fields and `turnover` (read earlier from the same accounts).
# A checked table: its rows add up and its total is the company's turnover. A sentence saying all turnover is in the UK is
# taken as nil outside it. "Principally" or "substantially" in the UK gives no figure, so it is kept apart and not counted as nil.
add_shares <- function(res) {
  res[, `:=`(turnover_agrees = near(turnover_read, turnover), total_is_turnover = near(geo_total, turnover) | near(geo_total, turnover_read))]
  res[, split_ok := geography == 'table' & rows_add_up & total_is_turnover & is.finite(uk_share_model)]
  gq <- tolower(res$geography_quote); says_uk <- grepl('united kingdom|\\buk\\b', gq, perl = TRUE)
  res[, mostly := geography == 'statement' & says_uk & grepl('principally|substantially|mainly|predominantly|primarily|majority', gq)]
  res[, wholly := geography == 'statement' & says_uk & !mostly & all_in_uk %in% TRUE]
  res[, outside_uk := fifelse(split_ok, 1 - uk_share_model, fifelse(wholly, 0, NA_real_))]
  res[, source_of_share := fcase(split_ok, 'table', wholly, 'statement: all in the UK', mostly, 'statement: mostly in the UK', geography == 'withheld', 'withheld', geography == 'table', 'table, not checked', default = 'not given')]
  res[]
}
