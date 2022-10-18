# generate quality data for submission

# housekeeping ------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(lfe)
library(stargazer)
library(sandwich) # needed for robust standard error. https://www.jakeruss.com/cheatsheets/stargazer/#robust-standard-errors-replicating-statas-robust-option

# read --------------------------------------------------------------------

df <- read_csv("data/JARA_it_by_a_quality.csv")


# define useful functions -------------------------------------------------

source("codes/sub/functions_final.R")

set.seed(123)
n_bs <- 100 # number of bootstrap sampling


# regression --------------------------------------------------------------

varname <- 'price'

# sigma = 0.3
df <- df %>% mutate(price = unitval_t_indagg_stock_12_ea_30_log_alpha)
m_rf_dic_eff_3 <- felm(weight_log ~ price + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
#summary(m_rf_dic_eff_3)
fmla <- formula("weight_log ~ price + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year")
sd_b_rf_dic <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
m_rf_dic_eff_3$se[varname] <- sd_b_rf_dic
m_rf_dic_eff_3$tval[varname] <- m_rf_dic_eff_3$coefficients[varname,]/sd_b_rf_dic
m_rf_dic_eff_3$pval[varname] <- 2*(1-pt(abs(m_rf_dic_eff_3$tval[varname]), df = m_rf_dic_eff_3$df))
summary(m_rf_dic_eff_3)

# sigma = 0.5
df <- df %>% mutate(price = unitval_t_indagg_stock_12_ea_50_log_alpha)
m_rf_dic_eff_5 <- felm(weight_log ~ price + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
#summary(m_rf_dic_eff_5)
fmla <- formula("weight_log ~ price + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year")
sd_b_rf_dic <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
m_rf_dic_eff_5$se[varname] <- sd_b_rf_dic
m_rf_dic_eff_5$tval[varname] <- m_rf_dic_eff_5$coefficients[varname,]/sd_b_rf_dic
m_rf_dic_eff_5$pval[varname] <- 2*(1-pt(abs(m_rf_dic_eff_5$tval[varname]), df = m_rf_dic_eff_5$df))
summary(m_rf_dic_eff_5)



# output ------------------------------------------------------------------


stargazer(
  m_rf_dic_eff_3, m_rf_dic_eff_5,
  dep.var.labels = '$\\ln(L_{it})$',
  covariate.labels = '$\\ln(p^{R,QA}_{it})$',
  keep = "price",
  omit.stat = c('adj.rsq', 'ser'),
  add.lines = list(c('EoS $\\sigma$', '0.3', '0.5')),
  notes.label = '',
  omit.table.layout = "n",
  title = "Employment effect of Quality-adjusted Prices",
  table.placement = '!t',
  label = 'quality_eos',
  out = 'output_final/tab_C2.tex'
)
