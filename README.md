# Replication package for Adachi, Kawaguchi, and Saito (2022) "Robots and Employment: Evidence from Japan, 1978-2017."

## How to replicate

After preparing data, please open `aks_robots.Rproj` and run `codes/MASTER_replicate.R`. This master code automatically calls a series of sub-codes under `codes/sub/` to generate figures and tables in `output_final` folder.

## Data

We have included the following datasets under `data/`

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

In addition to these data sources, the following datasets based on proprietary data are needed to run the codes.

- `JARA_ESS_it_by_a.csv`: Data that match the above JARA data and employment statistics. Employment statistics are available from the [Employment Status Survey](https://www.stat.go.jp/english/data/shugyou/index.html) (ESS).
- `JARA_ESS_ct.csv`, `ESS_cit_d.csv`, `ESS_cit.csv`: Commuting zone level data of robots and employment. The employment data are based on the ESS.
- `ifr/industry_000 - All Industries.xlsx`: Robot operational stock data from the [International Federation of Robotics](https://ifr.org/) (IFR).
- `soba/MP_by_industry.csv`: Multinational production (MP) data compiled from the [Basic Survey on Overseas Business Activities](https://www.meti.go.jp/english/statistics/tyo/kaigaizi/index.html).

## Contact

If you have any problems in running the code, please report [issues](https://github.com/daisukeadachi/aks_robots/issues).
