# 30_accounts_reading_all.R
# The reading of test 29, run over every large employer's accounts that the companieshouseopen project has read by OCR
# (about 1,950, registered all over the UK), with two things added to the form: the kinds of customer and of supplier
# each company names, and whether it says it buys from abroad. Sections 5.6 and 7.1 of plan/other_connections_plan.md.
#
# What it is for:
#   - the share of turnover sold outside the UK, by industry group and by region of registration, on the same footing
#     as the payments (a company's whole turnover at its registered office);
#   - a rough table of who sells to whom, from what companies say about their customers. It doesn't depend on the payments;
#   - which pairs of industries the payments hide in every month in a region, where companies registered there name that
#     kind of customer as their main one ("hidden links the accounts vouch for").
#
# Method and checks are test 29's (functions_accounts.R): Claude Haiku through `claude -p`, signed in as you and on your
# plan, six at a time. Answers are kept in local/paymentflowsviz_ynh/accounts_read_all/, one file a company, and a company
# is only asked again if its text or the form changes. If twelve in a row fail (which is what running out of allowance
# looks like) it stops, and running it again carries on from there.
#
# Company-level results name companies, so they stay in local/. What goes to tests/outputs is counts and totals, with
# shares and money left blank for any cell of fewer than five companies (three for the customer tables).
#
# Run from the repo root: Rscript docs/chunks/paymentflowsviz_ynh/tests/30_accounts_reading_all.R [--dry] [--sample n] [--tidy] [--path paper_filers folder]
#   --dry       choose the pages and say how much would be sent, without asking the model
#   --sample n  ask only n companies, spread across regions (a trial of the form)
#   --tidy      don't ask anything: tidy and report on the answers already kept
source('docs/chunks/paymentflowsviz_ynh/tests/functions_hierarchy.R')
source('docs/chunks/paymentflowsviz_ynh/tests/functions_accounts.R')
out <- file.path(chunk_dir, 'outputs'); LLM <- file.path(local_dir, 'accounts_read_all'); dir.create(LLM, showWarnings = FALSE, recursive = TRUE)
args <- commandArgs(trailingOnly = TRUE); arg <- function(name, default) if (name %in% args) args[which(args == name) + 1] else default
pf <- arg('--path', path.expand('~/Code/companieshouseopen/local/reports/paper_filers')); DRY <- '--dry' %in% args; TIDY <- '--tidy' %in% args; SAMPLE <- as.integer(arg('--sample', NA))
MODEL <- 'haiku'; WORKERS <- 6
VERSION <- 'edge-v3'                             # change when the prompt or the form changes, so kept answers are asked again
if (!dir.exists(file.path(pf, 'ocr'))) { cat('No OCR text found at', pf, '- nothing to do.\n'); quit(status = 0) }
pc <- function(v) ifelse(is.finite(v), sprintf('%.0f%%', 100 * v), '')

# ---- 1. the set: every employer read, with its region of registration and broad industry group
chr <- list(character = c('company_number', 'sic'))
e <- unique(rbind(fread(file.path(pf, 'data/employers_read_5000.csv'), colClasses = chr), fread(file.path(pf, 'data/employers_read_1000.csv'), colClasses = chr), fill = TRUE), by = 'stem')
la_file <- file.path(chunk_dir, 'lookups/la_itl1.csv')                           # local authority to ITL1 region, Great Britain
if (!file.exists(la_file)) {                                                     # made once from a postcode lookup kept in companieshouseopen: the region most of an authority's postcodes are in
  lk <- fread(path.expand('~/Code/companieshouseopen/local/lookups/postcode_localauthority_itl2_lookup_2025-03-19.csv'), select = c('localauthority_code', 'localauthority_name', 'ITL221CD'))
  la <- lk[!is.na(ITL221CD) & nzchar(ITL221CD), .N, by = .(la_code = localauthority_code, la_name = localauthority_name, itl1 = substr(ITL221CD, 1, 3))][order(la_name, -N)][, .SD[1], by = la_name]
  fwrite(la[order(itl1, la_name), .(la_name, la_code, itl1)], la_file)
}
la <- fread(la_file)
e <- merge(e, la[, .(reg_la = la_name, region = itl1)], by = 'reg_la', all.x = TRUE)
e[, sic2 := substr(sic, 1, 2)][!grepl('^[0-9]{2}$', sic2), sic2 := NA_character_]
# the nine groups ONS uses for regional trade (test 27), so the two can sit side by side
groups <- c(prod = 'Non-manufacturing production', manuf = 'Manufacturing', whole = 'Wholesale and motor trades', retail = 'Retail', transp = 'Transport, storage, accommodation and food',
            info = 'Information and communication', fin = 'Finance and insurance', prof = 'Professional, scientific and technical', other = 'Other services')
e[, n2 := as.integer(sic2)]
e[, group := fcase(is.na(n2), NA_character_, n2 <= 9 | (n2 >= 35 & n2 <= 43), 'prod', n2 <= 33, 'manuf', n2 %in% 45:46, 'whole', n2 == 47, 'retail', n2 <= 56, 'transp', n2 <= 63, 'info', n2 <= 66, 'fin',
                   n2 >= 69 & n2 <= 75, 'prof', default = 'other')]
set <- e[, .(stem, company_number, reg_name, sic, sic2, group, reg_la, region, made_up, turnover = as.numeric(turnover), employees = as.numeric(employees))]
cat(sprintf('%d sets of accounts. Registered in a known region: %d. With an industry code: %d.\n', nrow(set), set[!is.na(region), .N], set[!is.na(sic2), .N]))

# ---- 2. the form: test 29's fields, and the kinds of customer and supplier
cust <- c('households and consumers', 'retailers and supermarkets', 'wholesalers and distributors', 'motor dealers', 'manufacturers', 'construction and housebuilders', 'farmers and growers',
          'energy, water and waste companies', 'transport and logistics companies', 'hotels, restaurants, pubs and caterers', 'banks, insurers and financial firms',
          'technology, media and telecoms companies', 'professional and business services firms', 'property owners and landlords', 'central or local government', 'health and social care bodies',
          'schools, colleges and universities', 'defence', 'own group companies', 'other or mixed businesses')
supp <- c('farmers and growers', 'manufacturers', 'wholesalers and distributors', 'energy and utilities', 'transport and logistics companies', 'subcontractors and building trades',
          'professional and business services firms', 'technology suppliers', 'staffing agencies', 'own group companies', 'other or mixed')
props <- c(acc_props, list(
  customer_kinds = list(type = 'array', items = list(type = 'string', enum = as.list(cust))), customer_kinds_basis = list(type = 'string', enum = list('stated', 'inferred from the description of the business', 'not stated')),
  mostly_own_group = list(type = c('boolean', 'null')), largest_customer_share = acc_num,
  supplier_kinds = list(type = 'array', items = list(type = 'string', enum = as.list(supp))), buys_abroad = list(type = 'string', enum = list('yes', 'no', 'not stated'))))
SCHEMA <- list(type = 'object', additionalProperties = FALSE, properties = props, required = names(props))
SYSTEM <- paste(c(acc_system,
  'customer_kinds: the kinds of customer the company mainly sells to, most important first, at most three, chosen from the list. Use only what the text says. If the text names or describes the customers, set customer_kinds_basis to "stated". If it does not, but its description of the business makes the customers plain (a supermarket sells to households; a housebuilder sells homes to households; a bus company carries the public), choose from that and set customer_kinds_basis to "inferred from the description of the business". If neither, give an empty list and "not stated". Do not use anything you know about the company from outside the text.',
  'mostly_own_group: true if the accounts say that most of the turnover comes from other companies in the same group (for example a company that makes goods for, or provides services to, its parent or fellow subsidiaries); false if they say or show otherwise; null if they do not say.',
  'largest_customer_share: the percentage of turnover that the single largest customer accounts for, only if a percentage is printed; null otherwise.',
  'supplier_kinds: the kinds of supplier the company mainly buys from, most important first, at most three, chosen from the list, only where the text says or makes plain what it buys and from whom; an empty list otherwise.',
  'buys_abroad: "yes" if the text says the company buys goods, materials or services from suppliers in other countries, imports, or makes purchases in foreign currencies; "no" if it says its purchases are all or almost all from UK suppliers; "not stated" otherwise.'), collapse = ' ')

# ---- 3. choose the pages, and ask about any company not already asked with this text and form
if (!TIDY) {
  cat('choosing pages\n')
  set[, text := vapply(file.path(pf, 'ocr', paste0(stem, '.txt')), function(f) if (file.exists(f)) select_text(f) else '', '')]
  set[, prompt := sprintf('Accounts of: %s (company number %s), made up to %s\n\n%s', reg_name, company_number, made_up, text)]
  set[, text_hash := vapply(paste(VERSION, prompt), hash_of, '')]
  set[, cached := kept_hash(LLM, stem)]
  todo <- set[nchar(text) > 0 & cached != text_hash]
  if (!is.na(SAMPLE)) { set.seed(30); todo <- todo[!is.na(region) & !is.na(group)][, .SD[sample(.N, min(.N, ceiling(SAMPLE / 11)))], by = region][seq_len(min(.N, SAMPLE))] }
  cat(sprintf('%d companies, %d with text, %d already asked, %d to ask %s (%d at a time); %s characters to send, %s the longest\n', nrow(set), set[nchar(text) > 0, .N], set[nchar(text) > 0 & cached == text_hash, .N],
              nrow(todo), MODEL, WORKERS, format(sum(nchar(todo$prompt)), big.mark = ','), format(max(c(0, nchar(todo$prompt))), big.mark = ',')))
  if (DRY) quit(status = 0)
  ask_all(todo, SYSTEM, SCHEMA, VERSION, LLM, model = MODEL, workers = WORKERS, every = 100)
  saveRDS(set[, .(stem, text)], file.path(local_dir, 'accounts_read_all_text.rds'))          # the text sent, kept so --tidy can check answers without choosing pages again
} else {
  tx <- readRDS(file.path(local_dir, 'accounts_read_all_text.rds')); set <- merge(set, tx, by = 'stem', all.x = TRUE)
}

# ---- 4. tidy the answers and check them against the text sent
got <- set[file.exists(file.path(LLM, paste0(stem, '.json'))) & !is.na(text)]
ans <- lapply(setNames(got$stem, got$stem), function(s) fromJSON(file.path(LLM, paste0(s, '.json')), simplifyVector = FALSE))
ans <- ans[vapply(ans, function(a) identical(a$version, VERSION), TRUE)]; got <- got[stem %in% names(ans)]
cost <- sum(vapply(ans, function(a) a$cost_usd %||% 0, 0)); flat_text <- setNames(flat_numbers(got$text), got$stem)
res <- rbindlist(lapply(names(ans), function(s) { a <- ans[[s]]$answer; t <- flat_text[[s]]; k <- unlist(a$customer_kinds); sk <- unlist(a$supplier_kinds); lc <- a$largest_customer_share %||% NA_real_
  cbind(tidy_answer(s, a, t), data.table(customer_1 = k[1] %||% NA_character_, customer_2 = if (length(k) > 1) k[2] else NA_character_, customer_3 = if (length(k) > 2) k[3] else NA_character_,
        customers_basis = a$customer_kinds_basis, mostly_own_group = a$mostly_own_group %||% NA, largest_customer_share = lc, largest_found = if (is.finite(lc)) found_how(t, lc) != 'not found' else NA,
        supplier_1 = sk[1] %||% NA_character_, supplier_2 = if (length(sk) > 1) sk[2] else NA_character_, buys_abroad = a$buys_abroad)) }))
res <- add_shares(merge(got[, .(stem, company_number, reg_name, sic, sic2, group, reg_la, region, made_up, turnover, employees)], res, by = 'stem'))
# the money to weight by: turnover in pounds. Where the accounts are in another currency the share still counts, but not in weighted figures
res[, w := fifelse(currency == 'GBP', fifelse(is.finite(turnover_read), turnover_read, turnover), NA_real_)]
res[, stated := customers_basis == 'stated']
fwrite(res[order(region, group, -w)], file.path(local_dir, 'accounts_all_results.csv'))

cat(sprintf('\n== %d companies read with this form. Notional cost $%.2f ==\n', nrow(res), cost))
cat(sprintf('Figures returned: %d, found in the text sent: %d (%s). Quotes returned: %d, found in the text: %d (%s).\n', sum(res$figures), sum(res$figures_found), pc(sum(res$figures_found) / sum(res$figures)),
            sum(res$quotes_given), sum(res$quotes_found), pc(sum(res$quotes_found) / sum(res$quotes_given))))
cat(sprintf('Turnover read agrees with the turnover read earlier from the same accounts (within 2%%): %d of the %d where both were read.\n', sum(res$turnover_agrees), res[is.finite(turnover_read) & is.finite(turnover), .N]))
cat(sprintf('Accounts in pounds: %d. In another currency: %d.\n', res[currency == 'GBP', .N], res[currency != 'GBP', .N]))
old <- file.path(local_dir, 'accounts_reading_results.csv')
if (file.exists(old)) { o <- merge(fread(old)[, .(stem, old_share = outside_uk)], res[, .(stem, outside_uk)], by = 'stem')
  cat(sprintf('Against test 29 (an earlier form, 66 companies): %d read by both; both give a share for %d, and it is within one point for %d.\n', nrow(o), o[is.finite(old_share) & is.finite(outside_uk), .N],
              o[is.finite(old_share) & is.finite(outside_uk) & abs(old_share - outside_uk) <= 0.01, .N])) }

cat('\n== Where the share of turnover outside the UK comes from ==\n'); print(res[, .N, by = source_of_share][order(-N)])
cat('\n== Customers and suppliers ==\n')
cat(sprintf('Kinds of customer: stated %d, inferred from the description of the business %d, not stated %d. Largest customer\'s share given: %d (found in the text: %d).\n', res[customers_basis == 'stated', .N],
            res[customers_basis %like% 'inferred', .N], res[customers_basis == 'not stated', .N], res[is.finite(largest_customer_share), .N], sum(res$largest_found, na.rm = TRUE)))
cat(sprintf('Say most turnover is from their own group: %d. Kinds of supplier given: %d. Buys from abroad: yes %d, no %d, not stated %d.\n', sum(res$mostly_own_group %in% TRUE), res[!is.na(supplier_1), .N],
            res[buys_abroad == 'yes', .N], res[buys_abroad == 'no', .N], res[buys_abroad == 'not stated', .N]))

# ---- 5. sales abroad, by region of registration and industry group
few <- function(v, n, floor = 5) fifelse(n >= floor, as.numeric(v), NA_real_)                 # nothing for a cell of fewer than five companies
agg <- function(d) d[, { has <- !is.na(outside_uk); hw <- has & is.finite(w); n <- sum(has)
  .(companies = .N, with_a_share = n, turnover_with_a_share_gbp_bn = few(round(sum(w[hw]) / 1e9, 2), n), outside_uk_weighted = few(round(sum((outside_uk * w)[hw]) / sum(w[hw]), 3), n),
    outside_uk_median = few(round(median(outside_uk[has]), 3), n), sell_over_a_quarter_abroad = few(sum(outside_uk[has] > 0.25), n),
    largest_company_share_of_turnover = few(round(max(c(0, w[hw])) / sum(w[hw]), 2), n), say_they_buy_abroad = few(sum(buys_abroad == 'yes'), .N), customers_stated = few(sum(stated), .N)) }]
by_rg <- res[!is.na(region) & !is.na(group), agg(.SD), by = .(region, group)][, group_name := groups[group]][order(region, match(group, names(groups)))]
by_g <- res[!is.na(group), agg(.SD), by = group][, `:=`(region = 'UK', group_name = groups[group])][order(match(group, names(groups)))]
by_r <- res[!is.na(region), agg(.SD), by = region][, `:=`(group = 'all', group_name = 'All industries')]
fwrite(rbind(by_g, by_r, by_rg, use.names = TRUE)[, c('region', 'group', 'group_name', setdiff(names(by_rg), c('region', 'group', 'group_name'))), with = FALSE], file.path(out, 'accounts_all_by_region_group.csv'))
cat('\n== Share of turnover outside the UK, by industry group, all regions (weighted by turnover; median) ==\n')
print(by_g[, .(group = group_name, companies, with_a_share, turnover_gbp_bn = turnover_with_a_share_gbp_bn, weighted = pc(outside_uk_weighted), median = pc(outside_uk_median), over_a_quarter = sell_over_a_quarter_abroad, say_they_buy_abroad)])
cat('\nBy region of registration, all industries:\n'); print(by_r[order(region), .(region, companies, with_a_share, turnover_gbp_bn = turnover_with_a_share_gbp_bn, weighted = pc(outside_uk_weighted), median = pc(outside_uk_median))])
cat('\nMedian share outside the UK, region by group (blank: fewer than five companies with a share):\n')
print(dcast(by_rg, region ~ factor(group, names(groups)), value.var = 'outside_uk_median')[, lapply(.SD, function(v) if (is.numeric(v)) pc(v) else v)])
cat('\nCompanies with a share, region by group:\n'); print(dcast(by_rg, region ~ factor(group, names(groups)), value.var = 'with_a_share', fill = 0))
by_div <- res[!is.na(sic2), agg(.SD), by = sic2][, industry := setNames(sic2_lk$short, sic2_lk$sic2)[sic2]][order(sic2)]
fwrite(by_div[, c('sic2', 'industry', setdiff(names(by_div), c('sic2', 'industry'))), with = FALSE], file.path(out, 'accounts_all_by_division.csv'))
cat('\nDivisions with five or more companies giving a share, by the median sold outside the UK (top and bottom):\n')
dv <- by_div[with_a_share >= 5][order(-outside_uk_median), .(industry, with_a_share, weighted = pc(outside_uk_weighted), median = pc(outside_uk_median), say_they_buy_abroad)]; print(dv[1:min(14, .N)]); cat('  ...\n'); print(tail(dv, 6))

# ---- 6. who sells to whom: each company's main kind of customer, by the seller's group and division
cu <- res[!is.na(customer_1) & !is.na(group)]
ck <- cu[, .(companies = .N, stated = sum(stated), turnover_gbp_bn = round(sum(w, na.rm = TRUE) / 1e9, 2)), by = .(group, customer = customer_1)][, turnover_gbp_bn := few(turnover_gbp_bn, companies, 3)]
ck[, share_of_group := round(companies / sum(companies), 3), by = group][, group_name := groups[group]]
fwrite(ck[order(match(group, names(groups)), -companies), .(group, group_name, main_customer = customer, companies, of_which_stated = stated, share_of_group, turnover_gbp_bn)], file.path(out, 'accounts_all_customers_by_group.csv'))
cat('\n== Main kind of customer, by the seller\'s industry group: share of companies (three largest kinds) ==\n')
print(ck[order(match(group, names(groups)), -companies), .(top = paste(sprintf('%s %s', customer, pc(share_of_group))[1:min(3, .N)], collapse = '; '), companies = sum(companies)), by = .(group = group_name)])
cd <- cu[!is.na(sic2), .(companies = .N, stated = sum(stated), turnover_gbp_bn = round(sum(w, na.rm = TRUE) / 1e9, 2)), by = .(sic2, customer = customer_1)][, turnover_gbp_bn := few(turnover_gbp_bn, companies, 3)]
cd[, industry := setNames(sic2_lk$short, sic2_lk$sic2)[sic2]]
fwrite(cd[order(sic2, -companies), .(sic2, industry, main_customer = customer, companies, of_which_stated = stated, turnover_gbp_bn)], file.path(out, 'accounts_all_customers_by_division.csv'))

# ---- 7. hidden links the accounts vouch for: a pair of industries hidden in every month inside a region, where companies
# registered there in the supplying industry name that kind of customer as their main one
kind_sic <- list('retailers and supermarkets' = '47', 'wholesalers and distributors' = '46', 'motor dealers' = '45', 'manufacturers' = sprintf('%02d', 10:33), 'construction and housebuilders' = c('41', '42', '43'),
                 'farmers and growers' = c('01', '02', '03'), 'energy, water and waste companies' = sprintf('%02d', 35:39), 'transport and logistics companies' = sprintf('%02d', 49:53),
                 'hotels, restaurants, pubs and caterers' = c('55', '56'), 'banks, insurers and financial firms' = c('64', '65', '66'), 'technology, media and telecoms companies' = sprintf('%02d', 58:63),
                 'professional and business services firms' = sprintf('%02d', c(69:75, 77:82)), 'property owners and landlords' = '68', 'central or local government' = '84',
                 'health and social care bodies' = c('86', '87', '88'), 'schools, colleges and universities' = '85', 'defence' = '84')
own <- file.path(local_dir, 'all_regions_own_held.rds')
if (file.exists(own)) {
  cells <- readRDS(own)[, .(months = .N, published = sum(!suppressed), held_gbp_m_month = sum(value_held) / .N / 1e6), by = .(region = payer_region, buyer = payer_sic, supplier = payee_sic)]
  v <- cu[!is.na(region) & !is.na(sic2) & customer_1 %in% names(kind_sic), .(companies = .N, stated = sum(stated), turnover_gbp_bn = round(sum(w, na.rm = TRUE) / 1e9, 2)), by = .(region, supplier = sic2, customer = customer_1)]
  v <- v[, .(buyer = kind_sic[[customer]], divisions_in_kind = length(kind_sic[[customer]])), by = .(region, supplier, customer, companies, stated, turnover_gbp_bn)][buyer != supplier]
  v <- merge(v, cells, by = c('region', 'buyer', 'supplier'), all.x = TRUE)
  v[, status := fcase(is.na(months), 'no row in the data', published == 0, 'hidden in every month', published / months < 0.25, 'hidden in most months', default = 'published')]
  nm <- setNames(sic2_lk$short, sic2_lk$sic2); v[, `:=`(supplier_name = nm[supplier], buyer_name = nm[buyer], turnover_gbp_bn = few(turnover_gbp_bn, companies, 3))]
  cat('\n== Links the accounts point to, against what the payments publish inside each region ==\n')
  cat('A link: companies registered in a region, in one division, whose main kind of customer maps to another division. Status is that pair\'s payments inside the region.\n')
  print(dcast(v[, .N, by = .(kind_covers = fifelse(divisions_in_kind == 1, 'one division', 'several divisions'), status)], kind_covers ~ status, value.var = 'N', fill = 0))
  hid <- v[status %in% c('hidden in every month', 'hidden in most months')][order(-companies, region)]
  fwrite(hid[, .(region, supplier, supplier_name, buyer, buyer_name, customer_kind = customer, divisions_in_kind, companies, of_which_stated = stated, turnover_gbp_bn, status, months_published = published, months)],
         file.path(out, 'accounts_all_hidden_links.csv'))
  cat(sprintf('\nHidden pairs the accounts point to: %d, in %d regions. Those where the kind of customer is one division, two or more companies:\n', nrow(hid), uniqueN(hid$region)))
  print(hid[divisions_in_kind == 1 & companies >= 2, .(region, supplier = supplier_name, buyer = buyer_name, companies, stated, turnover_gbp_bn, status)][order(-companies)][1:min(40, .N)])
  cat('\nRetail paying food manufacturing inside each region (the pair that prompted this), with the food makers registered there that name retailers as their main customer:\n')
  fr <- merge(cells[buyer == '47' & supplier == '10', .(region, months, published)], v[supplier == '10' & buyer == '47', .(region, companies, turnover_gbp_bn)], by = 'region', all.x = TRUE); print(fr[order(region)])
} else cat('\nNo inside-region flows cached (tests/14), so hidden links were not looked for.\n')
cat('\nWrote accounts_all_by_region_group.csv, accounts_all_by_division.csv, accounts_all_customers_by_group.csv, accounts_all_customers_by_division.csv and accounts_all_hidden_links.csv; and in local/, accounts_all_results.csv (one row a company)\n')
