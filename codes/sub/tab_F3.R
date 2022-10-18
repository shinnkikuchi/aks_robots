
# housekeeping ------------------------------------------------------------

rm(list=ls())
library(tidyverse)
library(lfe)
library(stargazer)
library(boot)

set.seed(123)
n_bs <- 100

# take the bootstrap standard error from `regression_it_by_a_cb.R`
sd_b_m_main <- 0.071
f_m_fs_main <- 27.527

# read raw data ------------------------------------------------------

df_ait <- read_csv("data/JARA_ait.csv")

df_ess <- read_csv('data/ESS_cit.csv')
# aggregate ESS to industry level
df_ess %>% ungroup %>% group_by(code_union_ess, year) %>% 
  summarize_at(
    vars(c('count', starts_with('weight'), starts_with('hours'), starts_with('income'))),
    sum
  ) -> df_ess

# prep industry name vector and functions
list_ind_name <- df_ait %>% select(name_ind_short) %>% unique %>% unname %>% unlist 
df <- df_ait
source("codes/sub/functions_final.R")


# compute a-dropped aggregates ---------------------------------------


list_a <- df_ait %>% select(code_union_app, name_app) %>% unique
list_a


# aggregate ait-level robot data to it-level aggregates
make_data_it <- function(df){
  
  # collapse across applications, following `generate_JARA_it_by_a.R` -------
  
  # calculate alpha based on initial year
  df_alpha <- df %>% 
    filter(year == 1982) %>% 
    ungroup %>% group_by(code_union_ess) %>% 
    mutate(alpha = sales.m_value_5_avg/sum(sales.m_value_5_avg)) %>% 
    select(code_union_app, code_union_ess, alpha)
  
  # match alpha and data
  df <- df %>% 
    left_join(df_alpha, by = c('code_union_app', 'code_union_ess')) 
  
  # generate log industry-average and industry-specific prices
  df <- df %>% 
    ungroup %>% group_by(code_union_app, year) %>% 
    mutate_at(vars(starts_with('unitval'), starts_with('quantity'), starts_with('uc')), funs(log = log(.)))
  
  # drop zero-weight applications because those incorporate NaN in r_ait (zero sales/zero quantity).
  df %>% filter(alpha > 0) -> df
  
  # aggregate to prices and quantity aggregates. see also below section `generatling tornqvist indices`
  df %>% ungroup %>% group_by(code_union_ess, name_ind, name_ind_short, year) %>% 
    summarize_at(
      vars(ends_with('_log')), 
      funs(alpha = sum(alpha * .))
    ) -> df_it
  
  # generate levels
  df_it %>%
    mutate_at(
      vars(starts_with('quantity')),
      funs(level = exp(.))
    ) -> df_it
  
  # generate sales aggregates
  df %>% ungroup %>% group_by(code_union_ess, name_ind, name_ind_short, year) %>% 
    summarize_at(
      vars(starts_with('sales.m'), -contains('indagg')), 
      sum, na.rm = T
    ) -> df_it_sales
  
  # attach sales aggregates to quantity-price data
  df_it %>% left_join(df_it_sales, by = c('code_union_ess', 'name_ind', 'name_ind_short', 'year')) -> df_it
  
  return(df_it)
  
}

# read it-aggregated robot data and return regression coefficients
match_data <- function(df_jara){
  # add labor and control variables -----------------------------------------
  
  # match
  df <- left_join(df_jara, df_ess, by = c('code_union_ess', 'year'))
  
  # generate relevant variables
  df %>% 
    # demography shares
    mutate_at(vars(starts_with('weight')), funs(share = ./weight)) %>% 
    # per capita values
    mutate(
      hours_week_pc = (hours_year_1982/hours_answer_working) / 52,
      income_pc = income_1982/weight,
    ) %>% 
    # log outcome variables
    mutate_at(
      vars(starts_with('weight'), ends_with('pc')),
      funs(log = log(.))
    ) -> df
  
  # drop robot export
  df %>% filter(name_ind_short != '(export)') -> df
  
  
  # take offshoring data from SOBA
  df_soba <- read_csv('data/soba/MP_by_industry.csv')
  df_soba %>% mutate_at(vars(-code_union_ess, -year), funs(log = log(.))) -> df_soba # NaNs will be produced because some profits are negative.
  colnames(df_soba) <- paste(colnames(df_soba), 'soba', sep = '_')
  
  # take import and capital stock data from JIP
  df_jip_import <- read_csv('data/jip/jip_import.csv')
  df_jip_capital <- read_csv('data/jip/jip_capital.csv')
  # match JIP data
  df_jip <- left_join(df_jip_import, df_jip_capital, by = c('code_union_ess', 'year_ess'))
  df_jip %>% mutate_at(vars(-'code_union_ess', -'year_ess'), funs(log = log(.))) -> df_jip
  
  
  # match to the original data
  df %>% left_join(df_soba, by = c('code_union_ess' = 'code_union_ess_soba', 'year' = 'year_soba')) %>% 
    left_join(df_jip, by = c('code_union_ess', 'year' = 'year_ess')) -> df
  
  # generate 15 year-lagged differences and lags
  df %>% ungroup %>% group_by(code_union_ess, name_ind, name_ind_short) %>% 
    mutate_at(
      vars(-group_cols()), 
      funs(
        d15 = . - lag(., 3),
        l15 = lag(., 3),
      )
    ) -> df
  
  
  
  
}


# check these functions with the main specification
df_main <- make_data_it(df_ait)
df <- match_data(df_main)
m_main <- felm(weight_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
varname <- 'unitval_t_indagg_stock_12_log_alpha'
m_main$se[varname] <- sd_b_m_main
m_main$tval[varname] <- m_main$coefficients[varname,]/sd_b_m_main
m_main$pval[varname] <- 2*(1-pt(abs(m_main$tval[varname]), df = m_main$df))

# do the above with dropping one application
leave_out <- function(a){
  df <- df_ait %>% filter(code_union_app!=a)
  df_jara <- make_data_it(df)
  df <- match_data(df_jara)
  # regression
  # below is 2sls specification
  #m <- felm(weight_log ~ weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year | (quantity_stock_12_log_alpha ~ unitval_t_indagg_stock_12_log_alpha), data = df, weights = df$sales.m_value_5_avg)
  #summary(m)
  #varname <- '`quantity_stock_12_log_alpha(fit)`'
  # bootstrap
  m <- felm(weight_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
  #summary(m)
  varname <- 'unitval_t_indagg_stock_12_log_alpha'
  fmla <- formula("weight_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year")
  sd_b <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
  m$se[varname] <- sd_b
  m$tval[varname] <- m$coefficients[varname,]/sd_b
  m$pval[varname] <- 2*(1-pt(abs(m$tval[varname]), df = m$df))
  return(m)
}

# loop with applications
m_list <- list_a$code_union_app %>% map(leave_out)




# output ------------------------------------------------------------------

stargazer(
  m_main, m_list,
  dep.var.labels = '$\\ln(L_{it})$',
  covariate.labels = '$\\ln(p^{R}_{it})$',
  keep = 'unitval_t_indagg_stock_12_log_alpha',
  omit.stat = c('adj.rsq', 'ser'),
  add.lines = list(
    c('Dropped Applications', '(None)', 'Tending', 'Welding', 'Dispensing', 'Processing', 'Assembling', 'Others')
  ),
  notes.label = '',
  title = 'Alternative Set of Applications',
  omit.table.layout = "n",
  table.placement = '!t',
  label = 'industry_alternative_set_of_applications_exhaustive_rf',
  out = 'output_final/tab_F3.tex'
)

