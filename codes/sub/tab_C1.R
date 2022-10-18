# this file generates JARA dataset (by application) in ONE FILE! for clarity.
# note the order of variable generating. This order creates variables in the most efficient way.

# housekeeping ------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(readxl)
library(lfe)
library(stargazer)

# parameters
rho <- 0.188 # five-year external rate of return following Oberfield and Raval
delta_abr <- 0.1
delta_nm <- 0.18


# read --------------------------------------------------------------------

df <- read_csv('data/jara/generated.data/by-industry_by-application_1978-2017.csv')


# industry concordance with ESS -------------------------------------------

conc <- read_excel('data/jara/industry_codes_within_jara_v2_to_essCompatibleJARA2002.xlsx', sheet = 'jara_to_compatible')

# aggregate to ESS industries
df %>% left_join(conc, by = 'code_union') %>% ungroup %>% group_by(code_union_ess, code_union_app, variable, year) %>% summarize(value = sum(value)) -> df

# add export (code 0)
df %>% ungroup %>% mutate(code_union_ess = ifelse(is.na(code_union_ess), 0, code_union_ess)) -> df


# stock measures ----------------------------------------------------------

# measure 1: PIM
# take our data year length
nyear <- max(df$year) - min(df$year)

# generate lags with depreciation based on ABR18 and NM08. https://gist.github.com/drsimonj/2038ff9f9c67063f384f10fac95de566
lags <- seq(nyear)

lag_names_abr <- paste("value_l", formatC(lags, width = nchar(max(lags)), flag = "0"), "depr_abr", sep = "_")
lag_functions_abr <- setNames(paste("dplyr::lag(., ", lags, ") * (1-0.1)^", lags), lag_names_abr)
df %>% ungroup %>% group_by(code_union_ess, code_union_app, variable) %>% mutate_at(vars(value), funs_(lag_functions_abr)) -> df

lag_names_nm <- paste("value_l", formatC(lags, width = nchar(max(lags)), flag = "0"), "depr_nm", sep = "_")
lag_functions_nm <- setNames(paste("dplyr::lag(., ", lags, ") * (1-0.18)^", lags), lag_names_nm)
df %>% ungroup %>% group_by(code_union_ess, code_union_app, variable) %>% mutate_at(vars(value), funs_(lag_functions_nm)) -> df

# fill NAs with zeros
df[is.na(df)] <- 0

# generate stock variables based on PIM
df %>% ungroup %>% mutate(
  stock_abr = rowSums(select(., 'value', ends_with('_abr'))), # measure based on artuc, bastos, rijkers 2018
  stock_nm = rowSums(select(., 'value', ends_with('_nm'))) # measure based on nomura and momose 2008
) %>% select(-ends_with('_depr_abr'), -ends_with('_depr_nm')) -> df



# measure 2: simple linear depreciation as in IFR

# generate lags. https://gist.github.com/drsimonj/2038ff9f9c67063f384f10fac95de566
lags <- seq(14)
lag_names <- paste("value_l", formatC(lags, width = nchar(max(lags)), flag = "0"), sep = "_")
lag_functions <- setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)
df %>% ungroup %>% group_by(code_union_ess, code_union_app, variable) %>% mutate_at(vars(value), funs_(lag_functions)) -> df

# fill NAs with zeros
df[is.na(df)] <- 0

# generate stock variables based on JARA method.  https://stackoverflow.com/questions/27354734/dplyr-mutate-rowsums-calculations-or-custom-functions
df %>% ungroup %>% mutate(
  stock_15 = rowSums(select(., contains('value')))
) %>% select(-ends_with('12'), -ends_with('13'), -ends_with('14')) %>% mutate(
  stock_12 = rowSums(select(., contains('value')))
) %>% select(-ends_with('10'), -ends_with('11')) %>% mutate(
  stock_10 = rowSums(select(., contains('value')))
) -> df



# ESS comparable years ----------------------------------------------------

# generate 5 year sums and averages
df %>% select(-ends_with('05'), -ends_with('06'), -ends_with('07'), -ends_with('08'), -ends_with('09')) %>% mutate(
  value_5_sum = rowSums(select(., contains('value'))),
  value_5_avg = rowMeans(select(., contains('value'))),
) %>% select(-contains('_l_')) -> df

# keep only relevant years
set_years <- seq(1982, 2017, by = 5)
df %>% filter(year %in% set_years) -> df

# take initial value for lagged alphas used in `generate_JARA_it_by_a.R`
df %>% filter(variable == "sales.m", year == 1978) -> df_1978
#write_csv(df_1978, "data/JARA_ait_1978.csv")

# unit values -------------------------------------------------------------

# spread by sales and quantity
df %>% gather(concept, value, -code_union_ess, -code_union_app, -variable, -year) %>% spread(variable, value) -> df

# omit irrelevant value variable
df %>% filter(concept != 'value') -> df

# individual price
df %>% mutate(unitval_t = sales.m/quantity) -> df

# aggregate price
df %>% ungroup %>% group_by(code_union_app, concept, year) %>% mutate(unitval_t_indagg = sum(sales.m)/sum(quantity)) -> df

# export price
df %>% ungroup %>% group_by(code_union_app, concept, year) %>% mutate(unitval_t_exp = sum(sales.m * (code_union_ess != 0)/sum(quantity * (code_union_ess != 0)))) -> df

# leave-one-out price
df %>% ungroup %>% group_by(code_union_app, concept, year) %>% mutate(
  sales.m_indtot = sum(sales.m),
  quantity_indtot = sum(quantity),
  unitval_t_indloo = (sales.m_indtot - sales.m)/(quantity_indtot - quantity)
) %>% ungroup %>% group_by(code_union_ess, concept, year) %>% mutate(
  unitval_t_apploo = (sum(sales.m_indtot) - sales.m_indtot)/(sum(quantity_indtot) - quantity_indtot)
) -> df

# spread back to variable-concept level
df %>% gather(variable, value, -code_union_ess, -code_union_app, -concept, -year) %>% 
  mutate(variable_concept = paste(variable, concept, sep = '_')) %>% ungroup %>% select(-variable, -concept) %>% 
  spread(variable_concept, value) -> df


# user costs --------------------------------------------------------------

# version without capital gains. otherwise the user cost becomes negative, which gives the problem in genearting divisia index
df %>% ungroup %>% group_by(code_union_ess, code_union_app) %>% mutate(
  uc_t_abr = rho*lag(unitval_t_indagg_value_5_avg, 1) + delta_abr*unitval_t_indagg_value_5_avg,
  uc_t_nm = rho*lag(unitval_t_indagg_value_5_avg, 1) + delta_nm*unitval_t_indagg_value_5_avg
) -> df


# unit efficiency adjustment ----------------------------------------------


# generate a-i group and i-t groups
df %>% ungroup %>% group_by(code_union_app, code_union_ess) %>% group_indices() -> df$ai
df %>% ungroup %>% group_by(code_union_ess, year) %>% group_indices() -> df$it
df %>% ungroup %>% group_by(code_union_app, year) %>% group_indices() -> df$at

# keep only finite values for the purpose of regression
df %>% filter(quantity_stock_12 > 0 & unitval_t_indagg_stock_12 > 0) -> df

# regression to estimate sigma, quantity-based
m_ols <- felm(log(quantity_stock_12) ~ log(unitval_t_indagg_stock_12) | ai + it, data = df)
summary(m_ols)
m_fs <- felm(log(unitval_t_indagg_stock_12) ~ log(unitval_t_indloo_stock_12) | ai + it, data = df)
summary(m_fs)
f_m_fs <- sprintf("%.3f", round(lfe::waldtest(m_fs, 'log(unitval_t_indloo_stock_12)')['F'], 3))
m_iv <- felm(log(quantity_stock_12) ~ 1 | ai + it | (log(unitval_t_indagg_stock_12) ~ log(unitval_t_indloo_stock_12)), data = df)
summary(m_iv)


# Table C2 ----------------------------------------------------------------


# regression to estimate sigma, sales-based
m_ols_s <- felm(log(sales.m_stock_12) ~ log(unitval_t_indagg_stock_12) | ai + it, data = df)
summary(m_ols_s)
m_fs_s <- felm(log(unitval_t_indagg_stock_12) ~ log(unitval_t_indloo_stock_12) | ai + it, data = df)
summary(m_fs_s)
f_m_fs_s <- sprintf("%.3f", round(lfe::waldtest(m_fs_s, 'log(unitval_t_indloo_stock_12)')['F'], 3))
m_iv_s <- felm(log(sales.m_stock_12) ~ 1 | ai + it | (log(unitval_t_indagg_stock_12) ~ log(unitval_t_indloo_stock_12)), data = df)
summary(m_iv_s)


# global constants https://stackoverflow.com/questions/1236620/global-variables-in-r
sigma_iv_s <<- 1 - m_iv_s$beta[1,1]


# table adjustment
rownames(m_iv[["beta"]])[1] <- "log(unitval_t_indagg_stock_12)"
rownames(m_iv[["coefficients"]])[1] <- "log(unitval_t_indagg_stock_12)"
m_ols[["beta"]] <- - m_ols[["beta"]]
m_iv[["beta"]] <- - m_iv[["beta"]]

rownames(m_iv_s[["beta"]])[1] <- "log(unitval_t_indagg_stock_12)"
rownames(m_iv_s[["coefficients"]])[1] <- "log(unitval_t_indagg_stock_12)"
m_ols_s[["beta"]] <- 1 - m_ols_s[["beta"]]
m_iv_s[["beta"]] <- 1 - m_iv_s[["beta"]]

# reporting
added_line <- list(
  c('Application-Industry FE', '\\checkmark', '\\checkmark'),
  c('Industry-Year FE', '\\checkmark', '\\checkmark'),
  c('Estimator', 'FE', 'FE-IV')
)
stargazer(
  m_ols, m_iv,
  dep.var.labels = c('$\\ln(R_{ait})$', '$\\ln((rR)_{ait})$'),
  covariate.labels = '$\\ln(r_{at})$',
  omit.stat = c('adj.rsq', 'ser'),
  add.lines = added_line,
  notes.label = '',
  omit.table.layout = "n",
  title = "Robot Demand Elasticity Estimate",
  table.placement = '!t',
  label = 'rde',
  out = 'output_final/tab_C1.tex'
)

