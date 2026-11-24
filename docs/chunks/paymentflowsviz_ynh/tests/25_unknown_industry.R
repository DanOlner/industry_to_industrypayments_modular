# 25_unknown_industry.R
# Who are the payers and payees with no industry? ONS's methodology note says an account gets both its industry and its
# address by matching its name to Companies House, or to lists of public bodies. If that is so, an account that can't be
# matched should have neither. This checks it in the regional data: of the money moving between a region's organisations
# and those of unknown industry (SIC 00), how much is with organisations that have no region either? It also reports how
# much of a region's own organisations have no industry, what the regionless organisations are, and how far public
# administration is regionless. Published values only (no held values), so it describes the data as ONS issued it.
# Uses the monthly data the build caches (local/paymentflowsviz_ynh/monthly_<region>.rds), so run build_data.R first.
# Run from the repo root: Rscript docs/chunks/paymentflowsviz_ynh/tests/25_unknown_industry.R [TLE,TLG,...]   (default: all twelve)
source('docs/chunks/paymentflowsviz_ynh/tests/functions_hierarchy.R')
out <- file.path(chunk_dir, 'outputs')
args <- commandArgs(trailingOnly = TRUE)
regions <- if (length(args)) strsplit(args[1], ',')[[1]] else c('TLC', 'TLD', 'TLE', 'TLF', 'TLG', 'TLH', 'TLI', 'TLJ', 'TLK', 'TLL', 'TLM', 'TLN')
pc <- function(v) sprintf('%.1f%%', 100 * v)

one_region <- function(here) {
  f <- file.path(local_dir, sprintf('monthly_%s.rds', here)); if (!file.exists(f)) { cat(here, ': no monthly data cached, skipped\n'); return(NULL) }
  x <- readRDS(f); s <- function(i) x[i, sum(value, na.rm = TRUE)]
  to_here <- x$payee_region == here; from_here <- x$payer_region == here; p00 <- x$payer_sic == '00'; e00 <- x$payee_sic == '00'
  p_none <- x$payer_region == 'unknown'; e_none <- x$payee_region == 'unknown'
  data.table(region = here,
    # money between the region and organisations of unknown industry: the share where that organisation has no region either
    unknown_payers_no_region = s(to_here & p00 & p_none) / s(to_here & p00), unknown_payees_no_region = s(from_here & e00 & e_none) / s(from_here & e00),
    # the region's own organisations: how much of what they pay and are paid is by those with no industry
    own_paid_by_unknown_industry = s(from_here & p00) / s(from_here), own_received_by_unknown_industry = s(to_here & e00) / s(to_here),
    # organisations with no region that pay the region: what industry they have
    regionless_payers_unknown_industry = s(to_here & p_none & p00) / s(to_here & p_none), regionless_payers_public_admin = s(to_here & p_none & x$payer_sic == '84') / s(to_here & p_none),
    # public administration paying the region: the share where the public administration account (the payer) has no region.
    # And the other way: of what the region pays to public administration, the share where that account (the payee) has none.
    # The other party is always an organisation in the region, with a region by definition.
    public_admin_no_region = s(to_here & x$payer_sic == '84' & p_none) / s(to_here & x$payer_sic == '84'),
    public_admin_payee_no_region = s(from_here & x$payee_sic == '84' & e_none) / s(from_here & x$payee_sic == '84'),
    regionless_public_admin_pays_region_bn = s(to_here & x$payer_sic == '84' & p_none) / 1e9, region_pays_regionless_public_admin_bn = s(from_here & x$payee_sic == '84' & e_none) / 1e9,
    own_public_admin_pays_bn = s(from_here & x$payer_sic == '84') / 1e9, own_public_admin_is_paid_bn = s(to_here & x$payee_sic == '84') / 1e9,
    # everything paid to the region's organisations: the share from payers of unknown industry, and from those with no region
    received_from_unknown_industry = s(to_here & p00) / s(to_here), received_from_no_region = s(to_here & p_none) / s(to_here),
    received_bn = s(to_here) / 1e9)
}
res <- rbindlist(lapply(regions, function(r) { cat('reading', r, '\n'); one_region(r) }))
cat('\nOf the money paid to each region by payers of unknown industry, the share where the payer has no region either; and the same for payees:\n')
print(res[, .(region, payers = pc(unknown_payers_no_region), payees = pc(unknown_payees_no_region))])
cat('\nThe region\'s own organisations (those ONS could place): share of what they pay, and are paid, that is by organisations of unknown industry:\n')
print(res[, .(region, pay = pc(own_paid_by_unknown_industry), are_paid = pc(own_received_by_unknown_industry))])
cat('\nPayers with no region: share of unknown industry, share public administration; and public administration\'s payments to the region with no region:\n')
print(res[, .(region, unknown_industry = pc(regionless_payers_unknown_industry), public_admin = pc(regionless_payers_public_admin), public_admin_regionless = pc(public_admin_no_region))])
cat('\nPublic administration, GBP billions over the period. Accounts with no region: what they pay into the region and are paid by it. Accounts placed in the region: what they pay and are paid, to and from anywhere:\n')
print(res[, .(region, no_region_pays_in = round(regionless_public_admin_pays_region_bn, 1), no_region_is_paid = round(region_pays_regionless_public_admin_bn, 1), share_of_payees_regionless = pc(public_admin_payee_no_region),
              in_region_pays = round(own_public_admin_pays_bn, 2), in_region_is_paid = round(own_public_admin_is_paid_bn, 2))])
cat('\nOf everything paid to a region\'s organisations: from payers of unknown industry, and from payers with no region:\n')
print(res[, .(region, unknown_industry = pc(received_from_unknown_industry), no_region = pc(received_from_no_region))])
num <- setdiff(names(res), 'region'); res[, (num) := lapply(.SD, function(v) signif(v, 4)), .SDcols = num]
fwrite(res, file.path(out, 'unknown_industry_by_region.csv')); cat('\nWrote unknown_industry_by_region.csv\n')
