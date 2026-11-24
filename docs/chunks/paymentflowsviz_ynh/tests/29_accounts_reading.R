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
source('docs/chunks/paymentflowsviz_ynh/tests/functions_accounts.R')      # choosing pages, asking the model, checking the answers
out <- file.path(chunk_dir, 'outputs'); LLM <- file.path(local_dir, 'accounts_read'); dir.create(LLM, showWarnings = FALSE, recursive = TRUE)
args <- commandArgs(trailingOnly = TRUE); arg <- function(name, default) if (name %in% args) args[which(args == name) + 1] else default
pf <- arg('--path', path.expand('~/Code/companieshouseopen/local/reports/paper_filers')); ONLY <- arg('--only', NA_character_); DRY <- '--dry' %in% args
MODEL <- 'haiku'; WORKERS <- 6
VERSION <- 'edge-v2'                             # change when the prompt or the form changes, so kept answers are asked again
set_file <- file.path(local_dir, 'accounts_reading_set.csv')
if (!file.exists(set_file) || !dir.exists(file.path(pf, 'ocr'))) { cat('Needs', set_file, '(run test 28) and the OCR text at', pf, '\n'); quit(status = 0) }
set <- fread(set_file, colClasses = list(character = c('company_number', 'sic')))
if (!is.na(ONLY)) set <- set[stem == ONLY]

# ---- 1. the form: the fields and instructions tests 29 and 30 share (functions_accounts.R)
SCHEMA <- list(type = 'object', additionalProperties = FALSE, properties = acc_props, required = names(acc_props))
SYSTEM <- paste(acc_system, collapse = ' ')

# ---- 2. choose the pages, and ask about any company not already asked with this text and form
set[, text := vapply(file.path(pf, 'ocr', paste0(stem, '.txt')), function(f) if (file.exists(f)) select_text(f) else '', '')]
set[, prompt := sprintf('Accounts of: %s (company number %s), made up to %s\n\n%s', reg_name, company_number, made_up, text)]
set[, text_hash := vapply(paste(VERSION, prompt), hash_of, '')]
set[, cached := kept_hash(LLM, stem)]
todo <- set[nchar(text) > 0 & cached != text_hash]
cat(sprintf('%d companies in the set, %d with text, %d to ask %s (%d at a time); %s characters to send, %s the longest\n', nrow(set), set[nchar(text) > 0, .N], nrow(todo), MODEL, WORKERS,
            format(sum(nchar(todo$prompt)), big.mark = ','), format(max(c(0, nchar(todo$prompt))), big.mark = ',')))
if (DRY) quit(status = 0)
ask_all(todo, SYSTEM, SCHEMA, VERSION, LLM, model = MODEL, workers = WORKERS)

# ---- 3. tidy the answers and check them against the text sent
got <- set[file.exists(file.path(LLM, paste0(stem, '.json')))]
ans <- lapply(setNames(got$stem, got$stem), function(s) fromJSON(file.path(LLM, paste0(s, '.json')), simplifyVector = FALSE))
cost <- sum(vapply(ans, function(a) a$cost_usd %||% 0, 0))
flat_text <- setNames(flat_numbers(got$text), got$stem)
res <- rbindlist(lapply(names(ans), function(s) tidy_answer(s, ans[[s]]$answer, flat_text[[s]])))
res <- add_shares(merge(got[, .(stem, company_number, reg_name, sic, broad, turnover, status28 = status, uk_share28 = uk_share)], res, by = 'stem'))
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
