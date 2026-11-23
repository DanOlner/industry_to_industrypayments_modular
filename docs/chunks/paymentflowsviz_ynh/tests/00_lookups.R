# 00_lookups.R
# Build the small lookup tables the tests (and later the page) need:
#   lookups/sic2_lookup.csv     division code, full name, short label, SIC section letter and name
#   lookups/indstrat_sic2.csv   industrial strategy (IS-8) membership of each division, with the share
#                               of the division's Y&H jobs that sits in the listed SIC codes
#   lookups/indstrat_any_sic2.csv   the same share for any listed code, whichever IS-8 sector
# Run from the repo root: Rscript docs/chunks/paymentflowsviz_ynh/tests/00_lookups.R
suppressPackageStartupMessages(library(data.table))

ret <- path.expand('~/Code/RegionalEconomicTools')
out <- 'docs/chunks/paymentflowsviz_ynh/tests/lookups'
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# ---- 1. Divisions: names, short labels, sections -------------------------------------------------
lk <- fread(file.path(ret, 'data/SIClookup_forBRES_REGIONALGVA_2023_JOIN.csv'), colClasses = 'character')
sic2 <- unique(lk[, .(sic2 = SIC_2DIGIT_CODE, name = sub('^[0-9]+ : ', '', SIC_2DIGIT_NAME),
                      section = SIC_SECTION_LETTER, section_name = SIC_SECTION_NAME)])
stopifnot(nrow(sic2) == 88, !anyDuplicated(sic2$sic2))

short <- c(
  '01' = 'Farming', '02' = 'Forestry', '03' = 'Fishing', '05' = 'Coal mining', '06' = 'Oil & gas extraction',
  '07' = 'Metal ore mining', '08' = 'Quarrying', '09' = 'Mining support', '10' = 'Food manufacturing',
  '11' = 'Drinks manufacturing', '12' = 'Tobacco', '13' = 'Textiles', '14' = 'Clothing', '15' = 'Leather',
  '16' = 'Wood products', '17' = 'Paper', '18' = 'Printing', '19' = 'Coke & refined petroleum',
  '20' = 'Chemicals', '21' = 'Pharmaceuticals', '22' = 'Rubber & plastics', '23' = 'Glass, cement & ceramics',
  '24' = 'Basic metals', '25' = 'Fabricated metal', '26' = 'Electronics', '27' = 'Electrical equipment',
  '28' = 'Machinery', '29' = 'Motor vehicles', '30' = 'Other transport equipment', '31' = 'Furniture',
  '32' = 'Other manufacturing', '33' = 'Machinery repair & install', '35' = 'Energy supply',
  '36' = 'Water supply', '37' = 'Sewerage', '38' = 'Waste', '39' = 'Remediation',
  '41' = 'Building construction', '42' = 'Civil engineering', '43' = 'Building trades',
  '45' = 'Motor trades', '46' = 'Wholesale', '47' = 'Retail', '49' = 'Land transport',
  '50' = 'Water transport', '51' = 'Air transport', '52' = 'Warehousing & transport support',
  '53' = 'Post & courier', '55' = 'Accommodation', '56' = 'Food & drink service', '58' = 'Publishing',
  '59' = 'Film, TV & music', '60' = 'Broadcasting', '61' = 'Telecoms', '62' = 'IT & software',
  '63' = 'Information services', '64' = 'Banking & finance', '65' = 'Insurance & pensions',
  '66' = 'Financial support services', '68' = 'Real estate', '69' = 'Legal & accounting',
  '70' = 'Head offices & consultancy', '71' = 'Architecture & engineering', '72' = 'Scientific R&D',
  '73' = 'Advertising & market research', '74' = 'Other professional', '75' = 'Veterinary',
  '77' = 'Rental & leasing', '78' = 'Employment agencies', '79' = 'Travel agencies', '80' = 'Security',
  '81' = 'Building services & landscaping', '82' = 'Office & business support',
  '84' = 'Public administration', '85' = 'Education', '86' = 'Health', '87' = 'Residential care',
  '88' = 'Social work', '90' = 'Arts & entertainment', '91' = 'Libraries & museums', '92' = 'Gambling',
  '93' = 'Sport & recreation', '94' = 'Membership organisations', '95' = 'Repair of goods',
  '96' = 'Personal services', '97' = 'Households as employers', '98' = 'Households own use',
  '99' = 'Extraterritorial')
stopifnot(setequal(names(short), sic2$sic2))
sic2[, short := short[sic2]]

section_short <- c(A = 'Agriculture', B = 'Mining', C = 'Manufacturing', D = 'Energy', E = 'Water & waste',
                   F = 'Construction', G = 'Wholesale & retail', H = 'Transport & storage',
                   I = 'Hospitality', J = 'Information & comms', K = 'Finance & insurance', L = 'Real estate',
                   M = 'Professional services', N = 'Admin & support', O = 'Public administration',
                   P = 'Education', Q = 'Health & care', R = 'Arts & recreation', S = 'Other services',
                   T = 'Households', U = 'Extraterritorial')
sic2[, section_short := section_short[section]]
# The flags every script uses
sic2[, role := fifelse(sic2 %in% c('00', '97', '98', '99'), 'not_industry',
                fifelse(sic2 %in% c('63', '64', '66', '82'), 'intermediary', 'industry'))]
setcolorder(sic2, c('sic2', 'short', 'name', 'section', 'section_short', 'section_name', 'role'))
fwrite(sic2, file.path(out, 'sic2_lookup.csv'))

# ---- 2. Industrial strategy (IS-8) membership at division level ----------------------------------
# The strategy's list mixes 2, 3, 4 and 5-digit codes. The regional payments data only has divisions,
# so each division gets, for each IS-8 sector, the share of its Y&H jobs in the listed codes (BRES 2024,
# employment, summed over Y&H's 2023 county and unitary authorities).
is8 <- fread(file.path(ret, 'data/industrialstrategy2025sectordefs.csv'), colClasses = 'character')
is8 <- is8[!is.na(sic_code) & sic_code != 'NA' & sic_code != '']
is8_short <- c('Advanced Manufacturing' = 'Advanced manufacturing', 'Creative Industries' = 'Creative',
               'Defence' = 'Defence', 'Digital and Technologies' = 'Digital & tech',
               'Financial Services' = 'Financial services', 'Life Sciences' = 'Life sciences',
               'Professional and Business Services' = 'Professional & business services',
               'Foundational industries and inputs' = 'Foundational industries')
stopifnot(all(is8$sector_name %in% names(is8_short)))
is8[, is8 := is8_short[sector_name]]
cat('IS-8 sectors with SIC codes:', paste(unique(is8$is8), collapse = '; '), '\n(Clean energy has none.)\n')

ynh_la <- c(sprintf('E080000%02d', c(16:19, 32:36)), sprintf('E060000%02d', 10:14), 'E06000065')
bres_f <- file.path(ret, 'local/data/BRES/separate_SIC_types_summedfrom5digitSIC',
                    'BRES_ALLYEARSWITHDATA_TYPE423_localauthoritiescountyunitaryasofApril2023_4_Employment_2015_2024_SIC_5DIGIT.csv')
bres <- fread(bres_f, colClasses = list(character = 'SIC_5DIGIT_CODE'))
bres <- bres[DATE == max(DATE) & GEOGRAPHY_CODE %in% ynh_la]
stopifnot(uniqueN(bres$GEOGRAPHY_CODE) == length(ynh_la))
bres[, sic5 := sprintf('%05d', as.integer(SIC_5DIGIT_CODE))]
jobs5 <- bres[, .(jobs = sum(JOBCOUNT, na.rm = TRUE)), by = sic5][, sic2 := substr(sic5, 1, 2)]   # a few cells are NA
jobs2 <- jobs5[, .(div_jobs = sum(jobs)), by = sic2]

# For each IS-8 sector: which five-digit codes fall under any of its listed prefixes
memb <- rbindlist(lapply(unique(is8$is8), function(s) {
  pre <- unique(is8[is8 == s, sic_code])
  hit <- jobs5[Reduce(`|`, lapply(pre, function(p) startsWith(sic5, p)))]
  hit[, .(is8 = s, jobs_in_is8 = sum(jobs)), by = sic2][
    , whole_division := sic2 %in% pre][
    , listed_codes := vapply(sic2, function(d) paste(sort(pre[startsWith(pre, d)]), collapse = ' '), '')]
}))
memb <- merge(memb, jobs2, by = 'sic2')[, share_of_division_jobs := round(jobs_in_is8 / div_jobs, 3)]
memb <- merge(memb, sic2[, .(sic2, short, section)], by = 'sic2')
setorder(memb, is8, sic2)
fwrite(memb, file.path(out, 'indstrat_sic2.csv'))

# Union across IS-8 sectors: share of each division's jobs in ANY listed code
all_pre <- unique(is8$sic_code)
any5 <- jobs5[Reduce(`|`, lapply(all_pre, function(p) startsWith(sic5, p)))]
any2 <- merge(jobs2, any5[, .(jobs_any = sum(jobs)), by = sic2], by = 'sic2', all.x = TRUE)[is.na(jobs_any), jobs_any := 0]
any2[, share_any := jobs_any / div_jobs]
fwrite(any2[jobs_any > 0, .(sic2, share_of_division_jobs_any = round(share_any, 3))], file.path(out, 'indstrat_any_sic2.csv'))

cat('\nDivisions touched by the industrial strategy list:', memb[, uniqueN(sic2)], 'of 88\n')
cat('  listed as whole divisions:', memb[whole_division == TRUE, uniqueN(sic2)], '\n')
cat('  only partly covered (a 3 to 5-digit code inside them):', memb[, .(w = any(whole_division)), by = sic2][w == FALSE, .N], '\n')
cat('  in more than one IS-8 sector:', memb[, .N, by = sic2][N > 1, .N], '\n')
cat('Share of Y&H jobs (BRES 2024) in any listed code:', round(sum(any2$jobs_any) / sum(any2$div_jobs), 3), '\n')
cat('Share of Y&H jobs in a division the list touches at all:',
    round(any2[jobs_any > 0, sum(div_jobs)] / sum(any2$div_jobs), 3), '\n\n')
print(memb[, .(divisions = .N, whole = sum(whole_division), part = sum(!whole_division),
               jobs = sum(jobs_in_is8)), by = is8])
cat('\nPartly covered divisions (share of the division\'s Y&H jobs in the listed codes):\n')
print(memb[whole_division == FALSE, .(sic2, short, is8, listed_codes, share_of_division_jobs)])
