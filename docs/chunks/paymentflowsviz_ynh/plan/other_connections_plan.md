# Sources for other connections: plan

*Written 24 November 2026. A side plan to `paymentflowsviz_plan.md`, picking up its section 13.10: how to show an industry's links inside its region without missing its wider role. This asks what other sources could fill in the links the payments can't see. National input-output tables stay a separate comparison, as agreed. Anything here that reached the page would be marked as an estimate from another source.*

## 1. Summary

Three tests were written and run (`tests/26`, `27` and `28`), and a list of other sources drawn up.

- **Card takings are in the payments, but only a sliver of them.** Finance firms paid UK retail £18bn in 2025, in 11 million payments averaging £1,600. These behave like household spending: a December peak, and in the first lockdown restaurants' takings halved while retail's held. But they are a few per cent of what retail takes by card (a rough figure, section 3.5), and they grew nearly sevenfold from 2019 as more came into view. They mark where household money enters. They can't measure it.
- **The flow first noticed, retail paying London finance, is something else.** In Yorkshire and the Humber it is £2.6bn a year in only 58,000 payments averaging £44,700, and it barely existed before 2023. That is a few very large retailers' financing, not shoppers.
- **ONS already publishes imports and exports by industry group for each region.** Wholesale and motor trades in Yorkshire and the Humber imported £10.2bn of goods in 2023 and exported £3.0bn. ONS also estimates trade between regions: the same group bought £37bn of goods from other UK regions in 2021 and sold them £28bn. So about a fifth of the goods it brings into the region come from abroad. This answers section 13.10's question about wholesale, at the level of nine industry groups.
- **The payments record only part of that trade between regions, mostly a tenth to a half, and disagree with it on balance.** They see 10% of wholesale's purchases from other regions. Whether an industry group in a region sells more to the rest of the UK than it buys is no better than a coin toss between the two sources.
- **Large companies' accounts say where they sell, and it can be read.** A model read the accounts of 66 large companies registered in Yorkshire and the Humber for 40 cents (test 29, section 5.5). 55 give a share of turnover outside the UK that passes every check: 12% for the manufacturers, 2% for the wholesalers and motor traders, nil for the retailers.
- **They don't give imports as figures, but many say where they buy.** 20 of the 66 say something about where their goods come from and 15 name the currencies they buy in, mostly dollars and euros.
- **The same reading was then run over all 1,956 accounts, across Great Britain** (test 30, section 5.6), for $12. 1,206 give a share of sales abroad. Most large companies sell nothing abroad: the median is nil in seven of nine industry groups, and 5% for manufacturers. About half of manufacturers and wholesalers, and 63% of retailers, say they buy from abroad.
- **They also say who the customers are, and that turned up a chain the page can't show.** Manufacturers with £7.6bn of turnover, nearly all food makers, say their customers are mainly retailers and supermarkets, and three more with £1.8bn say their main customer is their own supermarket group. ONS has never published a single month of retail paying food manufacturing inside the region. The UK-wide reading shows Yorkshire and the Humber is the only region where that is so.
- **The best untested sources** are ONS's regional Annual Business Survey (turnover and purchases by division and region, placed at sites), regional household spending, Visa card spending by where shoppers live and shop, and HMRC's lists of importers and exporters by name.

**My recommendation:** put the official trade figures beside the payments first, because they exist, cover every region and need no guessing. Section 8 has the order I'd work in, and section 9 the decisions.

## 2. What is missing, and what "other connections" means

The payments show what businesses in one industry pay businesses in another, placed at each business's registered office. Four kinds of link are missing or bent.

| Missing link | Why | Why it matters on the page |
|----|----|----|
| **Households** | The data are payments between organisations. Payments from people's own accounts are not in them | Retail, food and drink, and personal services look as if they sell to almost nobody |
| **Other countries** | The data are UK payments only | Wholesale and manufacturing look like ends of the line, when their goods come from or go abroad |
| **Public bodies** | Public administration's accounts have no region (section 14.1) | Health, education and construction lose their main customer |
| **Where the activity is** | A whole business is placed at its registered office | A shop in Leeds owned by a London company is London's |

"Other connections" means any source that puts a figure, or a marked guess, on one of these.

## 3. Idea 1: card takings inside the payments

*Test `tests/26_card_takings.R`. Outputs `card_takings_by_payer.csv` and `card_takings_retail_by_region.csv`. Log in `local/paymentflowsviz_ynh/26_card_takings.log`. Published values only.*

### 3.1 What earlier work found

`RegionalEconomicTools/claude/payment_flows_method_and_caveats.md` worked this through for hotels and restaurants, not retail.

- **77% of hospitality's income outside London came "from London".** It was £20.2bn in 15 million payments averaging £1,347.
- **The payers were intermediaries:** finance (SIC 64) £10.9bn, read as card acquirers; information services (63) £3.5bn, read as booking platforms; office support (82) £3.3bn, read as booking agents.
- **The mark was the size of the payments:** many small ones from London, fewer and larger ones from everywhere else.
- **For retail, the note found something different.** Retail had the lowest share of its *spending* inside its own region, 19%. That is the "retail payments going to London" remembered. Section 3.4 below looks at it.

When a shopper pays by card, the shop is paid later by its card acquirer. So the direction to look in is finance paying the shop, not the shop paying finance.

### 3.2 Who pays the consumer-facing industries

Share of what each industry was paid in 2025, all twelve regions together. The first six sell mostly to households, the last four to businesses.

| Industry | Other industries | Finance (64) | Information services (63) | Office support (82) | Public administration | Unknown industry |
|----|----|----|----|----|----|----|
| Retail | 20% | 12% | 0% | 9% | 7% | 53% |
| Food and drink service | 15% | 26% | 8% | 6% | 2% | 43% |
| Accommodation | 12% | 4% | 0% | 8% | 6% | 71% |
| Personal services | 37% | 7% | 0% | 5% | 10% | 41% |
| Motor trades | 28% | 20% | 0% | 4% | 1% | 47% |
| Sport and recreation | 21% | 8% | 0% | 6% | 7% | 57% |
| Wholesale | 57% | 3% | 0% | 3% | 2% | 34% |
| Fabricated metal | 49% | 3% | 0% | 3% | 3% | 42% |
| Building construction | 32% | 4% | 0% | 2% | 15% | 47% |
| Legal and accounting | 29% | 7% | 0% | 3% | 5% | 55% |

- **Finance pays the consumer-facing industries more:** 12% of retail's receipts and 26% of food and drink's, against 3% for wholesale and fabricated metal.
- **Its payments are smaller.** Finance's average payment to retail is £1,600, against £3,110 from other industries. To wholesale the averages are £3,260 and £5,380.
- **Yorkshire and the Humber is close to the UK:** 8% of retail's receipts and 31% of food and drink's come from finance.
- **Half of everything comes from payers with no industry.** Section 14.1 of the main plan found these are accounts ONS couldn't match to a company or public body. That is the largest single source of income for every industry in the table bar wholesale and fabricated metal.

### 3.3 Do finance's payments behave like household spending?

Two marks, each worked out on cells published in every month compared.

| Paid to | First lockdown: finance | Other industries | Unknown industry | December: finance | Other industries | Unknown industry |
|----|----|----|----|----|----|----|
| Retail | 106% | 80% | 92% | 129% | 114% | 122% |
| Food and drink service | 51% | 64% | 42% | 128% | 114% | 112% |
| Accommodation | 38% | 49% | 24% | 119% | 106% | 97% |
| Personal services | 79% | 88% | 81% | 116% | 108% | 106% |
| Motor trades | 36% | 56% | 54% | 87% | 97% | 93% |
| Sport and recreation | 57% | 54% | 53% | 114% | 106% | 85% |
| Wholesale | 80% | 91% | 90% | 107% | 110% | 108% |
| Fabricated metal | 111% | 83% | 83% | 109% | 101% | 104% |
| Building construction | 92% | 82% | 78% | 120% | 114% | 112% |
| Legal and accounting | 80% | 101% | 91% | 106% | 105% | 103% |

*First lockdown: April to June 2020 as a share of April to June 2019. December: December as a share of the average month, 2019 and 2021 to 2024.*

- **Yes.** Finance's payments to retail and to food and drink peak in December at 128% to 129%, well above what other industries pay them (114%).
- **The lockdown pattern is the right one.** Finance's payments to hotels fell to 38% and to restaurants to 51%, while those to retail held at 106%. Card use also rose in 2020, which lifts all three.
- **December alone is a weak mark.** Building construction's receipts peak then too (114% to 120%), as year-end payments do. The lockdown pattern is the stronger evidence.
- **Three other signs fit.** Information services' payments to food and drink more than doubled in the lockdown (233%), as delivery platforms' would. Office support's payments to hotels fell to 9%, as booking agents' would. Public administration's payments to every consumer-facing industry rose to between two and five times, which is furlough and grants.
- **The payers of unknown industry are a mix.** Their payments to retail peak in December (122%) and their payments to restaurants fell further than finance's. Some of this is surely card and platform money from acquirers ONS couldn't match. It can't be separated from the rest.

### 3.4 Retail's payments to London finance

This is the flow first noticed. It is not card spending.

| Where the retailer is registered | Paid to finance in London, 2025 | Share of all retail pays | Payments | Average payment |
|----|----|----|----|----|
| Yorkshire and the Humber | £2.59bn | 11% | 58,000 | £44,700 |
| East of England | £2.03bn | 6% | 74,000 | £27,300 |
| East Midlands | £1.06bn | 8% | 55,000 | £19,100 |
| The other eight regions with figures | £0.04bn to £0.73bn | 1% to 4% | | £1,900 to £4,300 |

- **It is a few very large payments,** seven times the size of what retail in the region pays anyone else on average (£6,200).
- **It began in 2023.** Yorkshire and the Humber retail paid London finance about £0.2bn a year from 2019 to 2022, then £2.2bn in 2023. Some months are hidden in both 2022 and 2023, so the true step may be a little earlier or larger.
- **It is in three regions only,** all of them home to the registered offices of some of the UK's largest retailers.
- **So it reads as financing by a handful of large companies:** payments to lenders, to a group's own finance company, or to a bank that has paid the retailer's suppliers early. Holding companies are also classed as SIC 64. I can't tell which from the data, and no named company can be read off it.
- **Motor trades and rental and leasing** in the region also pay 11% of everything to London finance, in payments averaging £11,500 to £16,500. Those are more likely vehicle finance.

### 3.5 Is it useful for estimating how much of retail is final demand?

**Not as a figure.** Four reasons.

- **Too little of it is there.** Finance paid UK retail £13.1bn in 2023, about £1.1bn a month. UK Finance put all UK card purchases at £84bn in June 2023 alone (quoted by ONS), and retail is a large part of that. If retail sales are roughly £500bn a year and most are paid by card, £13bn is about 3%. The £500bn is from memory of ONS's retail sales figures and was not checked here.
- **Retail's receipts from every payer in the data were £156bn in 2025,** so less than a third of its sales are in the data in any form.
- **Company accounts show the same gap.** The largest retailer registered in Yorkshire and the Humber reports £24bn of turnover on its own. The payments show the region's whole retail industry receiving £13.8bn in 2025.
- **It grew for reasons that aren't spending.** Finance's payments to retail were £2.7bn in 2019 and £18.0bn in 2025. Card spending did not grow nearly sevenfold. More of the settlements came into view, most sharply in 2022 and 2023.
- **It is in the wrong place.** London has 31% of what UK retail was paid in 2023 and 15% of retail jobs. Scotland has 3% and 9%. Yorkshire and the Humber has 7% and 8%.

**As a share it gives a floor that is far too low.** Setting finance's payments to retail against other industries' gives 38% from households (12 against 20). The national input-output table has households at 83% of retail's sales.

**It is useful as a marker.** For the consumer-facing industries, what finance, booking platforms and delivery platforms pay is household money. The page drops all of it when intermediaries are switched out, which is right for geography and wrong for "who is the customer". Section 7 suggests how to show that.

## 4. Idea 2: official figures for trade across the region's edge

*Test `tests/27_trade_at_the_edge.R`. Output `trade_at_the_edge.csv`, every region. Log in `local/paymentflowsviz_ynh/27_trade_at_the_edge.log`. The three ONS workbooks are in `local/paymentflowsviz_ynh/downloads/`, each in its own folder.*

### 4.1 What exists

| Source | What it gives | Industry detail | How trade is placed | Status |
|----|----|----|----|----|
| ONS subnational trade in goods, and in services, 2023 (published August 2025; series from 2016) | Each region's imports from and exports to other countries, by partner country | Nine groups at ITL1: non-manufacturing production; manufacturing; wholesale and motor trades; retail; transport, storage, accommodation and food; information and communication; finance and insurance; professional services; other services | Shared out to a business's sites by employment | Official statistics in development |
| ONS interregional trade in goods and services, 2019, 2020 and 2021 (the dataset page is dated February 2026) | What each ITL1 region buys from and sells to each other ITL1 region | Eight groups: as above without finance, and with real estate joined to professional services | Wales, Scotland and Northern Ireland from their own trade surveys. England by a residual, split using road freight for goods and **these same payments data** for services | Official statistics in development. Not benchmarked to anything |

- **Imports and exports by industry and region don't need estimating at this level.** ONS publishes them.
- **The industry is the trader's.** A wholesaler's imports are counted under wholesale whatever the goods are.
- **Divisions are not available.** Other manufacturing can't be told from the rest of manufacturing, nor wholesale from motor trades.
- **The interregional services figures are partly built from the payments,** so they can't check the payments. The goods figures can, with care.

### 4.2 What they say about Yorkshire and the Humber

Goods crossing the region's edge, £ billion a year, by the group that buys or sells them. Other regions are 2021, other countries 2023, so the shares are rough.

| Group | In from other UK regions | In from abroad | Share from abroad | Out to other UK regions | Out to abroad | Share to abroad |
|----|----|----|----|----|----|----|
| Wholesale and motor trades | 37.4 | 10.2 | 21% | 27.6 | 3.0 | 10% |
| Manufacturing | 14.2 | 16.5 | 54% | 25.2 | 14.4 | 36% |
| Retail | 19.6 | 2.4 | 11% | 7.8 | hidden | |

- **Wholesale is a middle link on these figures.** It brings in £48bn of goods a year from outside the region and sends out £31bn. The payments show it buying £4.3bn from industries in the rest of the UK and £0.7bn inside the region (2021).
- **Most of what it brings in is from the rest of the UK, not abroad.** Section 13.10 put imports first among the things missing. On these figures they are a fifth. But goods imported by a business registered in London or the South East and sent on to Yorkshire count here as trade between regions.
- **Manufacturing in the region imports more than it exports:** £16.5bn against £14.4bn.
- **Services:** the region's finance and insurance businesses exported £8.0bn in 2023, and transport, storage, accommodation and food £3.6bn.
- **Wholesale's share from abroad differs a lot by region:** 57% in London and 37% in the South East, against 16% to 17% in the East Midlands, East of England and South West.

### 4.3 The payments against the interregional figures

| Group | Payments as a share of ONS's sales to other regions | Of ONS's purchases from other regions |
|----|----|----|
| Non-manufacturing production | 22% | 60% |
| Manufacturing | 28% | 20% |
| Wholesale and motor trades | 16% | 10% |
| Retail | 7% | 24% |
| Transport, storage, accommodation and food | 25% | 52% |
| Information and communication | 36% | 79% |
| Real estate and professional services | 41% | 57% |
| Other services | 47% | 31% |

*2021, all regions together. Payments to and from industries in the rest of the UK, with intermediaries and payers of unknown industry left out, held values.*

- **The payments see mostly between a tenth and a half** of the trade between regions that ONS estimates. The range is 7% to 79%. Earlier work found a median of 16% against the national input-output table (the handover, section 3), which is the same order.
- **They don't agree on balance.** For each group, across the twelve regions, the rank correlation between the payments' balance with the rest of the UK and ONS's goods balance runs from -0.47 to 0.34. The two have the same sign in 3 to 7 regions out of 12.
- **They agree better on which regions a group trades with.** For wholesale's purchases the correlation across the eleven partner regions has a median of 0.80 over the English regions, for retail's 0.72 and for manufacturing 0.56.
- **London is far larger in the payments.** For the other English regions it has 18% to 30% of manufacturing's and retail's trade with the rest of the UK in the payments, against 4% to 10% in ONS's goods figures. That gap is a measure of how far registered offices pull the payments towards London. Wholesale's purchases are the exception: ONS has London at 32% and the payments at 19%.

**Cautions on ONS's figures.** They are experimental and have oddities of their own.

- **London is shown selling £317bn of goods to other regions and buying £63bn.** That looks like head offices, not lorries.
- **Yorkshire and the Humber is shown selling £52bn of services to other regions and buying £7bn.** I have not found why.
- **ONS warns** that the estimates may differ a lot from the devolved administrations' own, that trade between regions need not balance, and that goods are assumed to be used by the same industry that sent them.

### 4.4 Could imports and exports be estimated for divisions?

Yes, as a guess with two known totals behind it. Not built.

- **The two totals.** ONS's regional figure for the group (say manufacturing in Yorkshire and the Humber, £14.4bn of goods exports), and a national figure for each division inside it. ONS publishes national trade in goods by industry, country and commodity, and trade in services by industry (both experimental; I have not opened them, so the level of industry detail is unconfirmed).
- **The method.** Start from each division's national trade for each job, times its jobs in the region. Scale so the divisions add up to the regional group total. That is one step of the fitting used to build regional tables, and it is an assumption that a division trades the same for each job everywhere.
- **What would improve it:** HMRC's regional trade by commodity, and the company accounts in section 5, both of which say something about particular divisions in this region.
- **What it would give:** a marked guess such as "other manufacturing here exports about £x a month", to set beside the £9m a month the payments show it selling to industries in the region.

## 5. Idea 3: Companies House and company accounts

*Test `tests/28_accounts_geography.R`. Output `accounts_geography_by_industry.csv` (counts and totals only). Company-level results are in `local/paymentflowsviz_ynh/accounts_geography_companies.csv` and `accounts_reading_set.csv`, not published. Log in `local/paymentflowsviz_ynh/28_accounts_geography.log`.*

### 5.1 What Companies House holds

From the `companieshouseopen` project's own notes (`claude/uk_company_data_guide.md`, the geography planner and the jobs list).

- **The open register** has each company's name, registered office, SIC codes, status and accounts category. Nothing on trade.
- **The bulk accounts data** are the accounts filed electronically, mostly by small and micro companies. They carry tagged figures: turnover, staff, profit where given. Small companies need not state turnover at all.
- **Large companies mostly file PDFs,** so their accounts are not in the bulk data. About 27,000 full and 7,000 group accounts are missing from it for that reason. This closes from 1 April 2028, when all accounts must be filed in tagged form.
- **About 1,950 of the largest employers' accounts have been read by OCR** (jobs GEO-18 and GEO-19): every employer of 1,000 or more on the gender pay gap list whose employees weren't already held. The text of every page is kept. None of it is in the database yet.
- **Tagged accounts can carry segment tags.** The guide notes that 12% of group accounts tag turnover only with dimensions. I have not checked whether any are geographic.

### 5.2 What accounts say about trade

- **Turnover by geographical market.** Large and medium companies must state the turnover from each market that differs substantially from the others, where "market" means one bounded by geography. They may withhold it if disclosure would be seriously prejudicial, and must then say so. (The Large and Medium-sized Companies and Groups (Accounts and Reports) Regulations 2008, Schedule 1, paragraph 68, as summarised on legislation.gov.uk. I have not read the current text in full.)
- **A company that sells only in the UK has nothing to split,** so it gives no note.
- **Nothing on imports.** No rule asks for purchases by origin. 12% to 13% of the accounts read mention importing at all, usually in passing.
- **Other notes that sometimes help:** the strategic report on main customers and suppliers, the note on currency risk (which currencies purchases are made in), and sales to and purchases from group companies.

### 5.3 The pilot: reading the geography note from the OCR text

The test finds wording such as "turnover by geographical market", reads the UK row, the other rows and the total, and keeps a reading only if the rows add up to the total and the total is the turnover already read from the same accounts. It is pattern matching, so the counts are a floor.

| | Accounts | Wording found | Read and checked | Say it is withheld | Share of turnover outside the UK, weighted | Median |
|----|----|----|----|----|----|----|
| Registered in Yorkshire and the Humber | 125 | 54 | 26 | 1 | 11% | 5% |
| All employers read | 1,956 | 822 | 324 | 28 | 30% | 5% |

By broad industry, all employers read:

| Industry | Accounts | Read and checked | Outside the UK, weighted | Median |
|----|----|----|----|----|
| Manufacturing | 231 | 106 | 25% | 7% |
| Wholesale and motor trades | 121 | 31 | 4% | 2% |
| Retail | 131 | 26 | 40% | 5% |
| Information and communication | 111 | 33 | 57% | 11% |
| Professional, scientific and technical | 140 | 27 | 26% | 17% |
| Transport, storage, accommodation and food | 221 | 13 | 45% | 9% |

- **It works.** A sixth of the accounts give a split that passes both checks, and nearly half of manufacturers'.
- **Yorkshire and the Humber manufacturing:** 14 of 25 accounts read, covering £13.7bn of turnover, of which 14% is outside the UK. If the companies with no note sell only in the UK, the share across all 25 is 10%.
- **That is well below ONS's figure for the region.** ONS has manufacturing in the region exporting £14.4bn of goods. The 25 companies' turnover is £19.6bn in all. The two aren't the same thing: the companies are those registered here with 1,000 or more staff, and several of the largest are food producers selling to UK supermarkets.
- **The weighted figures are moved by a few very large groups.** The medians are the safer reading: most large companies sell 5% or less abroad, and a minority sell most of what they make abroad.
- **One other manufacturing company is in the set.** A maker of modular buildings registered in York reports 28% of turnover overseas. The national table has other manufacturing exporting 58% of what it makes.

Some of the companies read, from their filed accounts:

| Company | Industry | Turnover | Outside the UK |
|----|----|----|----|
| Arla Foods | Dairy | £3,270m | 5% |
| Cranswick Country Foods | Meat processing | £2,680m | 4% |
| Nestlé UK | Confectionery | £2,512m | 17% |
| T.J. Smith and Nephew | Chemical products | £653m | 71% |
| Portakabin | Other manufacturing | £516m | 28% |
| Ardagh Glass | Glass | £449m | 5% |
| Polypipe | Plastics | £448m | 7% |
| Barrett Steel | Metals wholesale | £422m | 8% |

*These are public accounts of large companies, named because the request was for prominent firms to test on. Say if you'd rather the plan named none.*

### 5.4 The model reading: how it was set up

*Run on 24 November 2026: the results are in section 5.5.* A model reads the notes, as GEO-18 did for employees, on a set chosen to cover the gaps the pattern matching left.

- **The set:** the 66 companies registered in Yorkshire and the Humber that make, move or sell goods and whose accounts are already read by OCR. Together they turn over £95bn. The list is `local/paymentflowsviz_ynh/accounts_reading_set.csv`.

| Industry | Read and checked here | Wording found, not read | No note found | Withheld, or total not matched |
|----|----|----|----|----|
| Manufacturing | 14 | 4 | 6 | 1 |
| Wholesale and motor trades | 2 | 5 | 0 | 1 |
| Retail | 1 | 2 | 7 | 0 |
| Transport, storage, accommodation and food | 2 | 1 | 7 | 0 |
| Non-manufacturing production | 3 | 5 | 5 | 0 |

- **It includes the region's two largest supermarkets,** a fruit and vegetable wholesaler, two motor groups, an electronics wholesaler that says its split is withheld, a clothing and footwear wholesaler and a metals wholesaler. Those are the wholesale and retail cases the page most needs.
- **The form** asks for: turnover by market (by destination or by origin, and which); whether the accounts are the company's or a group's; any statement on where goods are bought; the currencies purchases are made in; sales to and purchases from group companies; and named kinds of customer.
- **The checks** are GEO-18's: every figure must appear in the text sent, and the rows must add up.
- **It passes if** the model agrees with the 22 readings already checked here to within a point, and reads at least half of the 17 where wording was found but not read.
- **Cost:** no downloading or OCR is needed. GEO-18 ran at about a cent a call, so under £1.
- **What it could then be set beside:** each division's payments in the region. For example, do the retailers that state their purchases confirm that most of what they buy comes from outside the region?

### 5.5 The model reading: what came back

*Test `tests/29_accounts_reading.R`. Output `accounts_reading_summary.csv` (counts by industry). Company-level results are in `local/paymentflowsviz_ynh/accounts_reading_results.csv`, and each answer in `local/paymentflowsviz_ynh/accounts_read/`. Log in `local/paymentflowsviz_ynh/29_accounts_reading.log`.*

**The run.** All 66 were read by Claude Haiku in under three minutes, at a notional cost of $0.40. About 72,000 characters of each set of accounts were sent: the pages that mention turnover, exports, imports, currencies, suppliers, customers or group companies.

**The checks.**

- **Figures:** 322 of the 323 returned are in the text sent.
- **Quotes:** 256 of 277 are found in the text (92%). The rest differ by a mis-joined word or a dropped line break.
- **Turnover:** the model's figure agrees with the one read earlier from the same accounts for all 66.
- **Against test 28:** for all 22 that the pattern matching had read and checked, the model gives the same UK share.
- **Both pass marks in section 5.4 were met.** Of the 17 where wording was found but not read, 14 now have a share.
- **By eye:** five were compared with the scanned pages (two tables, a one-row table and two statements). All five match.

**Where they sell.**

| Industry | Companies | Share from a checked table | Say all turnover is in the UK | Say mostly, withheld, or nothing | Turnover with a share | Outside the UK |
|----|----|----|----|----|----|----|
| Manufacturing | 25 | 18 | 4 | 3 | £17.4bn of £19.6bn | 12% |
| Wholesale and motor trades | 8 | 5 | 2 | 1 | £5.0bn of £5.6bn | 2% |
| Retail | 10 | 2 | 6 | 2 | £18.4bn of £42.6bn | 0% |
| Transport, storage, accommodation and food | 10 | 3 | 6 | 1 | £4.4bn of £11.9bn | 2% |
| Non-manufacturing production | 13 | 6 | 3 | 4 | £5.9bn of £15.3bn | 0% |
| All | 66 | 34 | 21 | 11 | £51.1bn of £95.0bn | 4% |

- **55 of 66 give a usable share.** 34 from a table whose rows add up to the company's turnover, and 21 from a sentence such as "all revenue is derived in the UK".
- **Most are by destination.** 26 of the 34 tables say they are by where the customer is, one is by where the seller is, and 7 don't say.
- **The largest companies are the ones that say nothing.** The region's largest retailer, with £24bn of turnover, gives no note. An airline group (£7.5bn) says its revenue is "principally" from the UK. A power company and a housebuilder give nothing. All four sell mainly or wholly in the UK on any reading, which is why only about half the turnover has a share against five sixths of the companies.
- **Large manufacturers registered here sell little abroad.** 12% of £17.4bn is about £2bn. ONS has manufacturing in the region exporting £14.4bn of goods. So most of the region's exports come from companies outside this set: smaller ones, and sites of companies registered elsewhere.
- **The spread is wide.** Four manufacturers sell 35% to 71% abroad (chemical products, engineering, steel, bedding). Most food manufacturers sell 5% or less.

**Where they buy.** No company gives purchases by origin as a figure. What there is:

- **20 of the 66 say something about where they buy.** A pork processor buys charcuterie and fresh pork from continental Europe. A steelmaker's raw materials come from the Americas, Europe and Australasia. A power company buys biomass from North America, South America and Europe. A card retailer's suppliers are "predominantly based in China", and a clothing and footwear wholesaler has a sourcing team there. A metals wholesaler "holds significant port space to allow bulk stockholding and steel imports".
- **Two speak of buying locally.** A water company says nearly a third of its 1,100 suppliers are based in Yorkshire. A tea and coffee company buys "both locally in Yorkshire and globally".
- **15 name the currencies they buy in:** US dollars 12, euros 11.
- **Two give amounts that put a floor under their imports:** forward contracts to buy $40m (a motor parts wholesaler turning over £409m) and $66m (a bedding maker turning over £319m).
- **Trade with group companies** is given as a figure by five. It is small next to turnover, or is rent.

**Who they sell to.** 55 of the 66 say something about their customers. Two patterns stand out among the manufacturers.

- **Their customers are mainly retailers and supermarkets.** Seven manufacturers, six of them food makers, with £7.6bn of turnover, say so in words like "the majority of our customer sales are to large UK based household name grocery retailers".
- **Some are a supermarket's own suppliers.** Three meat and cooked-meat processors (£1.8bn) and a fruit and vegetable wholesaler (£0.9bn) say their main customer is their own group, or name the supermarket. Each shares an ultimate parent, as stated in its accounts, with one of the region's two large supermarket groups, and both groups have such companies registered here.
- **None of this is on the page.** Retail paying food manufacturing inside Yorkshire and the Humber has a row in ONS's data for all 93 months, and every one is hidden. So is retail paying farming. With no published month to hold a value at, the page draws no link at all.
- **So one of the region's largest real chains is invisible in the payments:** farms, to food manufacturers, to supermarkets. It may also be that some of this trade never passes through the banks, where a group settles between its own companies on its books.

**What this means for the plan.**

- **Sales abroad can be had cheaply and reliably,** on the same footing as the payments. Reading all 1,956 accounts would cost about $12.
- **Imports can't be had as figures from accounts.** The statements and currencies are evidence that a company imports, not how much. HMRC's lists of importers (section 6) are still the better route.
- **Who sells to whom can be read, roughly, and that is the most useful surprise.** A form that asked the model to put each company's main customers into a short list (retailers, wholesalers, manufacturers, builders, public bodies, households, its own group) would give a coarse table of who supplies whom from the accounts themselves. That is independent of the payments, and it reaches cells ONS hides.

**Limits.**

- **The set is large employers registered here,** those with 1,000 or more staff. It is not the region's businesses.
- **A company's turnover is all placed at its registered office,** as in the payments.
- **The two customer patterns were counted by matching words** in the sentence the model quoted, so the counts are a floor.
- **One table failed its check** (the rows don't add up to the turnover) and was left out.

### 5.6 The UK-wide reading

*Test `tests/30_accounts_reading_all.R`, run 24 November 2026. Outputs `accounts_all_by_region_group.csv`, `accounts_all_by_division.csv`, `accounts_all_customers_by_group.csv`, `accounts_all_customers_by_division.csv` and `accounts_all_hidden_links.csv`. Company-level results are in `local/paymentflowsviz_ynh/accounts_all_results.csv`. Logs `30_accounts_reading_all.log` and `30_tidy.log` in `local/paymentflowsviz_ynh/`.*

**What was read.** All 1,956 sets of accounts: every employer of 1,000 or more that the `companieshouseopen` project had read by OCR, registered all over Great Britain. The form is section 5.5's with three things added: the kinds of customer each company names, the kinds of supplier, and whether it says it buys from abroad.

**The run.** 87 minutes, six at a time, no failures. Notional cost $12.45.

**The checks.**

- **Figures:** 7,263 of the 7,364 returned are in the text sent (99%).
- **Quotes:** 6,901 of 7,476 are found in the text (92%).
- **Turnover:** agrees with the figure read earlier from the same accounts for 1,644 of 1,660.
- **Against section 5.5:** all 55 shares that the earlier form gave for the Yorkshire and the Humber set come out the same.
- **By eye:** three more tables were compared with the scanned pages, making eight. All match.
- **Customer kinds:** I read 22 at random beside the sentence each came from. 21 fit. One is arguable: a housebuilder put under households whose sentence speaks of housing associations.

**Where they sell.** 1,206 of the 1,956 give a usable share: 537 from a checked table and 669 from a sentence saying all turnover is in the UK. Of the rest, 573 say nothing, 70 have a table that fails a check, 65 say "mostly" in the UK and 42 say the split is withheld.

| Industry group | Companies | With a share | Their turnover | Outside the UK, weighted | Median | Sell over a quarter abroad |
|----|----|----|----|----|----|----|
| Manufacturing | 231 | 177 | £205bn | 36% | 5% | 45 |
| Information and communication | 111 | 69 | £50bn | 32% | 6% | 15 |
| Professional, scientific and technical | 140 | 88 | £55bn | 18% | 0% | 12 |
| Transport, storage, accommodation and food | 221 | 173 | £71bn | 17% | 0% | 5 |
| Retail | 131 | 103 | £190bn | 7% | 0% | 7 |
| Other services | 694 | 368 | £128bn | 7% | 0% | 12 |
| Non-manufacturing production | 127 | 80 | £111bn | 6% | 0% | 2 |
| Finance and insurance | 131 | 39 | £48bn | 3% | 0% | 3 |
| Wholesale and motor trades | 121 | 89 | £87bn | 2% | 0% | 4 |

- **Most large companies sell nothing abroad.** The median is nil in seven of the nine groups.
- **The weighted figures are carried by a minority.** A quarter of manufacturers sell over a quarter of their turnover abroad, and they are the largest. One services company checked by eye books 89% of £12bn outside the UK.
- **Manufacturers by region of registration** (median, where five or more give a share): West Midlands 14%, Scotland 12%, South East 6%, Yorkshire and the Humber and the East of England 5%, North West and South West 4%, London 3%, East Midlands 2%.
- **By division:** machinery has a median of 70%, other transport equipment 27%, electronics 23%, chemicals 17%, fabricated metal 16%. Other manufacturing has 12% across 14 companies (29% weighted), against 58% in the national table.
- **Region by group is thin.** With a floor of five companies, Wales has no group to show and the North East three. London has all nine.

**Whether they buy from abroad.** 488 say they do (a quarter of all), 5 say they don't, and the rest don't say.

- **Retail 63%, wholesale and motor trades 50%, manufacturing 49%.** Then information and communication 31%, and 10% to 21% for the others.
- **This is a floor,** since silence isn't "no". But it is the first figure in this plan on who imports, by industry, on the payments' footing.

**Who they sell to.** A main kind of customer was given for 1,867: stated in the accounts for 1,281, and plain from the description of the business for 586.

| Seller's group | Main kinds of customer, by share of companies |
|----|----|
| Manufacturing | Retailers and supermarkets 25%, manufacturers 14%, own group 14% |
| Wholesale and motor trades | Households 40%, retailers and supermarkets 16%, construction 10% |
| Retail | Households 90% |
| Non-manufacturing production | Households 27%, government 19%, energy, water and waste companies 18% |
| Transport, storage, accommodation and food | Households 58%, government 8% |
| Information and communication | Mixed businesses 22%, households 19%, government 17% |
| Finance and insurance | Households 49%, own group 21%, financial firms 13% |
| Professional, scientific and technical | Own group 28%, households 25%, government 15% |
| Other services | Households 30%, government 28%, own group 16% |

- **Food manufacturers sell to supermarkets:** 44 of 63 across Great Britain name retailers as their main customer.
- **Other manufacturing sells to health.** Of 19 companies, health and social care bodies are the main customer for four, level with "own group". That matches the link the page shows in Yorkshire and the Humber (section 13).
- **One in nine say most of their turnover comes from their own group** (222). Those are companies whose payments, if they are paid at all, are inside a group.
- **Households are the main customer for 40% of wholesale and motor trades.** Most of those are motor dealers, who share the group (33 of the 47). Among wholesalers alone, retailers and supermarkets come first (19 of 75), then households and construction.

**Hidden links the accounts vouch for.** A pair counts where companies registered in a region, in one division, name a kind of customer that maps to another division, and the payments between the two inside that region are hidden in every month or in most.

- **The one that started this is real, and it is Yorkshire and the Humber's alone.** Nine food manufacturers registered there, with £12.0bn of turnover, name retailers as their main customer. Retail paying food manufacturing there is hidden in all 93 months.
- **In every other region that link is published:** in all 93 months in eight regions, 90 in Wales and 71 in the East of England. The West Midlands has it in only 18.
- **Two more of the same kind:** farming selling to retail in Yorkshire and the Humber (three companies, £1.2bn, hidden in every month), and drinks manufacturing selling to retail in the West Midlands (two companies, hidden in most months).
- **Nearly all the rest have public administration as the buyer.** Schools, care homes and bus and rail companies whose main customer is government: education in the South West (15 companies) and Yorkshire and the Humber (10), residential care in the South East (9). That is not ONS hiding a link. It is section 14.1's problem again: public administration's accounts have no region, so they are not inside any region to pay.
- **Where a kind of customer covers several divisions** ("manufacturers", "construction") there are 736 hidden pairs, far too loose to draw.

**What this means for the scoped feature (section 7.1).**

- **The table works for the UK and for the larger regions.** For small regions it would mostly say "too few to show". Showing the UK's row beside the region's would keep it useful everywhere.
- **The dotted line is worth having but will be rare.** It marks three links in two regions. Yorkshire and the Humber is the region the page opens on, and the link is among its largest, so it still earns its place.
- **The public administration pairs belong in words, not lines.** A side panel sentence for those industries: "Its main customer is government, whose payments have no region, so they are not on this chart."
- **Two things the reading adds that weren't planned:** a "buys from abroad" figure for each group, and the share of companies whose main customer is their own group.

**Limits.**

- **Employers of 1,000 or more only,** and only those whose accounts were read by OCR. Not the economy.
- **Great Britain.** The lookup from local authority to region has no Northern Ireland, and 21 companies have no region.
- **A group's accounts include its companies abroad,** which is what lifts some weighted shares. The medians are the safer reading.
- **Customer kinds are a model's reading of a sentence or two.** They are right most of the time, not every time.
- **"Not given" is 573 companies.** Many are schools, charities and care providers, which sell only in the UK and have no reason to say so.

## 6. Idea 4: other sources

Ordered by how soon each could be used. "Checked" means I read ONS's or the publisher's own page or file today. "From a search" means a search summary only.

| Source | What it would add | Basis | How checked |
|----|----|----|----|
| **ONS regional Annual Business Survey** (2024 results, June 2026) | Turnover, purchases and approximate GVA by region and two-digit division, for the non-financial business economy | Sites, not registered offices | From a search. Datasets not opened |
| **ONS regional household final consumption expenditure**, 2009 to 2023 (February 2026) | What households in each ITL1 and ITL2 area spend, by 12 kinds of spending, with finer detail in the dataset. Yorkshire and the Humber: £120.0bn in 2023, £21,448 a head | Mainly residents' spending wherever it happens. A second set counts spending inside the region | Bulletin read by a helper. Dataset not opened |
| **ONS and Visa consumer card spending** (flows article, March 2024; data on Nomis) | Where cardholders live against where they spend face to face, by postal district, with a region-to-region table. 74% of face-to-face spending is in the cardholder's own postal area | The shop's location, not the retailer's office. Shares and indexes, not pounds. Visa cards only. 13% of spending has no merchant location | Articles read by a helper. Data not opened |
| **HMRC importers and exporters lists** (monthly since 2016) | Names, addresses, postcodes and commodity codes of businesses that import or export. No values | Named businesses, so they can be matched to Companies House. Businesses can opt out | From a search |
| **HMRC regional trade in goods statistics** (quarterly) | Imports and exports by region, product group and partner country | Shared out by employees since 2016 | From a search |
| **DfT road freight statistics** (tables RFS0122 and RFS0123) | Goods lifted and moved between regions of origin and destination | Where lorries load and unload | From a search. ONS already uses this survey for its interregional goods figures |
| **Exports Statistics Scotland, the Trade Survey for Wales, Northern Ireland Economic Trade Statistics** | Each nation's sales to the rest of the UK, by sector. Wales and Northern Ireland also give purchases | Business surveys | From a search. An earlier test (T4 in `whole_economy_thinking`) set the payments against Scotland's figures and found no agreement |
| **Public bodies' spending records** | Who councils pay, payment by payment over £500, with supplier names (England, quarterly). Central government and NHS bodies publish similar lists | Named payers and payees | Councils' from a search. The others not checked |
| **Tagged accounts from April 2028** | Every company's accounts in tagged form, large ones included | Registered office | From the `companieshouseopen` notes |
| **Multi-site work in `companieshouseopen`** | Which sites a multi-site company has, so its payments could be shared out across them | Sites | From that project's planner. Not built for this |
| **SEIM-UK** (City-REDI, Birmingham) | A modelled table of trade between the 12 regions for 30 sectors | Modelled. Not public | From a search |

**What each would be for.**

- **The Annual Business Survey is the strongest untested check.** It has divisions, regions and sites. Payments received by a division in a region, over its survey turnover, would say how much of each industry the payments see and how far registered offices move it. It leaves out finance, farming and the public sector.
- **Household spending and the Visa data together cover households.** The first gives pounds by kind of spending and where people live. The second gives where they shop. Neither is by industry, but food, clothing, restaurants and hotels map onto divisions well enough for a marked guess.
- **The HMRC lists are the only firm-level evidence on imports.** Matched to the Companies House database, they would give, for each division and region, how many businesses import and what kinds of goods. That is the gap company accounts can't fill.
- **Public bodies' spending records bear on section 11.11,** bringing public administration's payments in. They are real payments with real names, but scattered across hundreds of publishers.
- **The devolved surveys are an independent check for three of the twelve regions.** The earlier test used a tradability index, not the inside and outside shares the page now has. It could be rerun on those.

**Not pursued:** card schemes' and banks' own data (not open), and business surveys of who exports (shares of firms, not values).

## 7. How any of this could reach the page

The principle from section 13.10 holds: keep local links and wider role apart, and show them side by side. None of these sources can be added to the payments, because they are placed differently and cover different amounts.

- **An "at the edge" block in the side panel.** For the pinned industry's group: goods in and out, from other UK regions and from abroad, from ONS. One line, with the source and the year. For wholesale in Yorkshire and the Humber: "ONS estimates this group brings in £48bn of goods a year from outside the region, a fifth of it from abroad, and sends out £31bn."
- **It would say "group".** A reader pinning other manufacturing would be shown manufacturing as a whole, and told so.
- **A households line for consumer-facing industries.** "Nationally, 83% of retail's sales are to households. They are not in this data, except where a card company pays the shop."
- **A marker on the intermediaries.** When intermediaries are switched in, finance's links to consumer-facing industries could be labelled as card takings in the tooltip.
- **Stubs on the trace** (already in section 13.10): where a chain reaches an industry whose goods mostly cross the edge, a short stub labelled "other regions and abroad".
- **A coverage note by industry,** if the Annual Business Survey check is done: how much of each industry's sales the payments see.

**What I would not do.**

- **Add the sources together** into one set of flows.
- **Draw abroad or households as circles on the chart.** They would dwarf everything and their links would be on a different footing.
- **Use card takings as a measure of household spending.**

### 7.1 Scoped: company accounts on the page

*Scoped 24 November 2026, on request. Not built. It depended on the UK-wide reading of accounts, which is now done: section 5.6 ends with what that reading means for this feature.*

**The decision.** Mostly a separate part of the page, as the national accounts are, with two small links into the main view.

- **Why separate.** The accounts give shares and statements for a few dozen large companies in a region. They are not monthly flows between pairs of industries, so they can't become links with a width.
- **Why not wholly separate.** They are closer to the page than the national table is: the same region, and placed at registered offices exactly as the payments are. And in one case they correct something the chart gets wrong, where it shows no link because ONS hides one.

**The three parts.**

1. **A section under the chart, "What the payments leave out".** It holds the outside sources together, each with its source and what it covers: ONS's trade figures at the region's edge (section 4), the accounts, and later households. For the accounts it is one small table for the region shown, a row for each broad industry group:
    - how many large companies registered there were read, and their turnover;
    - the share of that turnover sold outside the UK;
    - the kinds of customer they name most;
    - how many say they buy from abroad.
2. **A short block in the side panel when an industry is pinned.** Two or three sentences from its group's row. For example: "From the accounts of 22 large manufacturers registered here: 12% of turnover is sold abroad. Their customers are mainly retailers and supermarkets."
3. **A marker for hidden links the accounts vouch for.** A thin dotted line with no width between two industries, where both of these hold:
    - ONS has that pair's payments hidden in every month in that region, so the chart draws nothing;
    - companies registered there, in the supplying industry, name that kind of customer as their main one.

   It shows when either end is pinned, and its tooltip says what it is: "Hidden by ONS in every month. Accounts of 6 large food makers registered here name supermarkets as their main customers."

**Rules.**

- **Groups, not divisions.** The nine broad groups ONS uses for regional trade, so the two sources line up.
- **A floor on numbers.** No figure for a group with fewer than five companies read, or where one company is most of the turnover. It says "too few to show".
- **No company is named on the page.**
- **Every figure carries its footing:** "from the accounts of N employers of 1,000 or more registered here", and the years the accounts cover.
- **No widths and no adding up.** Nothing from the accounts is put on the payments' scale.
- **The dotted line means "there is a link", not "this much".**

**What it adds to the data files.** A small block in each region's file, built from the reading's outputs: the table's rows, and a short list of vouched hidden links (supplying division, buying division, how many companies, their turnover). A few kilobytes a region.

**Controls.** One switch, "Hidden links that company accounts point to", on by default when an industry is pinned. It goes in the address like the others.

**What has to be true first.**

- **The UK-wide reading is done and checked** (section 5.6), with customer kinds on the form.
- **Customer kinds map onto divisions well enough.** "Retailers" is division 47. "Public sector" or "construction" is several. Where a kind covers several divisions the dotted line goes to each that has a hidden pair, or the marker is left out and the side panel says it in words.
- **Enough companies in each region.** The North East, Wales and Northern Ireland have few large employers registered there. They may get the table with most rows blank.
- **A hand check** of a sample of the customer kinds against the pages.

**Checks when built.**

- **The table's figures match the reading's output** for each region.
- **Every dotted line is a pair hidden in every month** in that region's data.
- **Screenshots** of the section, the side panel block and a pinned industry with a dotted line, light and dark.
- **No page or console errors, and no loss of speed.**

**Open questions.**

- **Group accounts.** A group's turnover can include companies registered elsewhere, and two companies in one group can both be in the set. The table should count each group once where it can.
- **Whether a hidden link that the accounts don't vouch for should be marked at all.** About a quarter of cells are never published. Most are small. Marking them all would bury the chart, which is why this uses the accounts to pick.
- **How to word the buying side.** Accounts say that a company imports, not how much.

## 8. The order I'd work in

1. **Put ONS's regional trade figures in the build** as a small lookup by region and group, and draft the side panel block. Small, and it changes what the page can say about wholesale.
2. **The model reading: done,** on the 66 (section 5.5) and then on all 1,956 with customer kinds added (section 5.6). Next for this strand is building section 7.1, if wanted.
3. **The Annual Business Survey check.** Open the regional datasets, and set turnover and purchases by division and region against the payments.
4. **Households:** open the regional household spending dataset and the Visa data on Nomis, and see how well kinds of spending map onto divisions.
5. **HMRC lists against Companies House:** a count of importing and exporting businesses by division in Yorkshire and the Humber. This needs the `companieshouseopen` database and a name and postcode match.
6. **Division-level trade guesses** (section 4.4), once 3 and 5 say how far they can be trusted.

## 9. Decisions for you

1. **Is the side panel block (step 1) wanted,** and at group level only? My recommendation: yes, with the group named.
2. **Build the accounts feature (section 7.1)?** The reading it needed is done (section 5.6). My recommendation: yes, with the UK's row shown beside the region's, the dotted line kept, and the public administration cases put in words.
3. **Should the page say that the food manufacturing to retail link is hidden?** My recommendation: yes, in the notes for Yorkshire and the Humber, since it is one of the region's largest chains. The check is done: it is the only region where the link is never published. The West Midlands has it in 18 months of 93.
4. **Named companies in this plan** (section 5.3): keep, or take out before it is published?
5. **Should the interregional figures be shown on the page at all,** given the oddities in 4.3? My recommendation: the goods figures yes, as a rough guide; the services figures no.
6. **Which of steps 3 to 5 matters most to you?** I'd take the Annual Business Survey first.

## 10. Files, checks, and what isn't done

| Path | What |
|----|----|
| `tests/26_card_takings.R` | Who pays the consumer-facing industries, marks of household spending, size, place, and retail's payments to London finance |
| `tests/27_trade_at_the_edge.R` | ONS's regional and interregional trade figures beside the payments, by industry group |
| `tests/28_accounts_geography.R` | Turnover by geography read from large employers' OCR'd accounts |
| `tests/29_accounts_reading.R` | A model reading of 66 Yorkshire and the Humber companies' accounts: where they sell, where they buy, and who their customers are |
| `tests/30_accounts_reading_all.R`, `tests/functions_accounts.R` | The same reading over all 1,956 accounts, with customer and supplier kinds; and the mechanics the two tests share |
| `tests/lookups/la_itl1.csv` | Local authority to region, Great Britain, made by test 30 from a postcode lookup in `companieshouseopen` |
| `tests/outputs/card_takings_by_payer.csv`, `card_takings_retail_by_region.csv`, `trade_at_the_edge.csv`, `accounts_geography_by_industry.csv`, `accounts_reading_summary.csv`, and five files beginning `accounts_all_` | Their outputs |
| `local/paymentflowsviz_ynh/downloads/` | The three ONS workbooks, each in its own folder (not published) |
| `local/paymentflowsviz_ynh/accounts_geography_companies.csv`, `accounts_reading_set.csv`, `accounts_reading_results.csv`, `accounts_all_results.csv`, `accounts_read/`, `accounts_read_all/` | Company-level results, the reading set, and each answer from the model (not published) |

**Checked.**

- **All five tests run from a clean start** with no errors or warnings. Test 30's report can be re-made from the kept answers with `--tidy`.
- **Test 28's readings pass two checks each:** the rows add up, and the total is the turnover read earlier from the same accounts.
- **Figures in this plan were taken from the test logs.**

**Not done, or not checked.**

- **Nothing here is on the page.**
- **Retail sales of about £500bn a year** is from memory.
- **The 2008 Regulations' current wording** on turnover by market was not read in full.
- **Several sources in section 6 were found by search only,** as marked.
- **The services side of ONS's interregional figures** was not looked into beyond noticing it is lopsided.
- **Test 28 was not checked by hand** against page images. Tests 29 and 30 were, for eight companies in all.
- **Customer kinds were checked only by reading 22 of them** beside their sentences.
- **The lockdown and December marks are for the UK,** not for one region.

## Sources

- ONS, International trade in UK nations, regions and cities: 2023: <https://www.ons.gov.uk/businessindustryandtrade/internationaltrade/bulletins/internationaltradeinuknationsregionsandcities/2023>
- ONS, Subnational trade in goods: <https://www.ons.gov.uk/businessindustryandtrade/internationaltrade/datasets/subnationaltradeingoods>
- ONS, Subnational trade in services: <https://www.ons.gov.uk/businessindustryandtrade/internationaltrade/datasets/subnationaltradeinservices>
- ONS, Interregional trade in goods and services, UK: <https://www.ons.gov.uk/businessindustryandtrade/internationaltrade/datasets/interregionaltradeingoodsandservicesuk>, and its methodology note: <https://www.ons.gov.uk/businessindustryandtrade/internationaltrade/methodologies/interregionaltradeingoodsandservicesukqmi>
- ONS, Regional household final consumption expenditure, UK: 2009 to 2023: <https://www.ons.gov.uk/economy/regionalaccounts/grossdisposablehouseholdincome/bulletins/regionalhouseholdfinalconsumptionexpenditureuk/2009to2023>
- ONS, Consumer card spending, flow of spending across the UK: 2019 to 2023: <https://www.ons.gov.uk/economy/economicoutputandproductivity/output/articles/consumercardspendingflowofspendingacrosstheuk/2019to2023>
- ONS, Non-financial business economy, regional (Annual Business Survey): 2024 results: <https://www.ons.gov.uk/businessindustryandtrade/business/businessservices/bulletins/nonfinancialbusinesseconomyukandregionalannualbusinesssurvey/2024results>
- HMRC, bulk data sets including importers and exporters: <https://www.uktradeinfo.com/trade-data/latest-bulk-data-sets/>
- HMRC, Regional trade in goods statistics methodology: <https://www.gov.uk/government/statistics/overseas-trade-statistics-methodologies/regional-trade-in-goods-statistics-methodology>
- DfT, Domestic road freight activity (RFS01): <https://www.gov.uk/government/statistical-data-sets/domestic-road-freight-activity-rfs01>
- Scottish Government, Exports Statistics Scotland 2023: <https://www.gov.scot/publications/exports-statistics-scotland-2023/>
- Welsh Government, Trade Survey for Wales 2022: <https://www.gov.wales/trade-survey-wales-tsw-2022-official-statistics-development-html>
- NISRA, Northern Ireland Economic Trade Statistics 2024: <https://datavis.nisra.gov.uk/economy-and-labour-market/northern-ireland-economic-trade-statistics-2024.html>
- The Large and Medium-sized Companies and Groups (Accounts and Reports) Regulations 2008, Schedule 1, paragraph 68: <https://legislation.gov.uk/uksi/2008/410/schedule/1/paragraph/68>
- Local Government Transparency Code 2015: <https://www.gov.uk/government/publications/local-government-transparency-code-2015/local-government-transparency-code-2015>
- City-REDI, SEIM-UK: <https://www.birmingham.ac.uk/research/centres-institutes/city-region-economic-development-institute/projects/seim-uk>
