# Replication package for Adachi, Kawaguchi, and Saito (2022)

Please cite Adachi, Kawaguchi, and Saito (2022) "Robots and Employment: Evidence from Japan, 1978-2017" when using our data.

## How to replicate

After preparing data, please open `aks_robots.Rproj` and run `codes/MASTER_replicate.R`. This master code automatically calls a series of sub-codes under `codes/sub/` to generate figures and tables in `output_final` folder.

## Data

### Main robot data 

`data/by-industry_by-application_1978-2017_labelled_prices.csv` is the main data file that summarizes the number and value of robot shipments by industry, application, and year. It contains the following variables:

- code_union: industry code
- code_union_app: application code
- year: calendar year
- ind_union: industry name in Japanese
- ind_union_eng: industry name in English
- name_union: application name in Japanese
- app_code_name_eng: application name in English
- quantity: quantity
- sales.m: sales in current million JPY
- quantity_t: quantity in thousand units
- sales_b: sales in current billion JPY 
- unitval_m: unit value in current million JPY (sales.m/quantity)
- q_indagg: quantity aggregated at the industry-year level
- pq_indagg: sales aggregated at the industry-year level	
- q_indagg_loo: leave-one-application-out quantity aggregated at the industry-year level 
- pq_indagg_loo: leave-one-application-out sales aggregated at the industry-year level 	
- p_indagg_loo: leave-one-application-out price (unit value) aggregated at the industry-year level

### Other data

In addition to these, we have included the following datasets under `data/` that are called in the replication files.

- `by-industry_by-application_1978-2017.csv`: Raw Japanese robot adoption data file between 1978 and 2017.
- `by-industry_by-application_1978-2017_labelled_prices_5year.csv`: 5-year aggregated robot data file.
-  `JARA_ait.csv`: Robot data file ready for main analysis.
- `JARA_it_by_a_quality.csv`: Robot data with quality adjustment variables.
- `price_output_export.csv`: Industrial output price, output, export data from the [JIP database](https://www.rieti.go.jp/en/database/jip.html).
- `bojprice/raw/revision/nme_R031.24549.20210330072035.02_utf.csv`: The Bank of Japan price series.
- `by-industry_by-type_1974-2000_noGHI_noNonManuf.csv`: Robot data file containing early periods 1974-1977.
- `by-application-by-type_1982-1991_labelled.csv`: Robot data file classified by robot types.
- `alpha.csv`: Robot application expenditure share data across industries.
- `oecd/ALFS_SUMTAB_23042021012546383.csv`: [OECD annual employment data](https://stats.oecd.org/#).
- `oecd/china/Employment.xls`: Chinese employment data available at the [National Bureau of Statistics China](https://data.stats.gov.cn/easyquery.htm?cn=C01).
- `jip/jip_import.csv`: Industrial import statistics obtained from the JIP database.
- `jip/jip_capial.csv`: Industrial capital stock statistics obtained from the JIP database.
- `industry_codes_within_jara_v2_to_essCompatibleJARA2002.xlsx`: Industry codes concordance.
- `comtrade_847950.csv`: The [Comtrade](https://comtrade.un.org/) data of industrial robots (HS code = 847950).
- `exrate/AEXJPUS.csv`: The exchange rate trend taken from the [Federal Reserve Economic Data](https://fred.stlouisfed.org/) (FRED).

Furthermore, the following datasets based on proprietary data are needed to run the codes.

- `JARA_ESS_it_by_a.csv`: Data that match the above JARA data and employment statistics. Employment statistics are available from the [Employment Status Survey](https://www.stat.go.jp/english/data/shugyou/index.html) (ESS).
- `JARA_ESS_ct.csv`, `ESS_cit_d.csv`, `ESS_cit.csv`: Commuting zone level data of robots and employment. The employment data are based on the ESS.
- `ifr/industry_000 - All Industries.xlsx`: Robot operational stock data from the [International Federation of Robotics](https://ifr.org/) (IFR).
- `soba/MP_by_industry.csv`: Multinational production (MP) data compiled from the [Basic Survey on Overseas Business Activities](https://www.meti.go.jp/english/statistics/tyo/kaigaizi/index.html).

## Contact

If you have any problems in running the code, please report [issues](https://github.com/daisukeadachi/aks_robots/issues).
