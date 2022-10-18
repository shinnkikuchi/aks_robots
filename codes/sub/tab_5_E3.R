# regression analysis about robot price and output, export, and output price, with concise table

# housekeeping ------------------------------------------------------------

rm(list = ls())

library(data.table)
library(tidyverse)
library(lfe)
library(stargazer)
library(sandwich) 
library(readxl)

# read --------------------------------------------------------------------

dt <- fread("data/price_output_export.csv")

dt_reg <- dt[year<=2012] # JIP for year >= 2013 is not available


# Table F3 ----------------------------------------------------------------




# up to three decimal points
dt_exp <- dt_reg[, sh_exp := round(export_total/value_nominal, 3)][, .(code_union_ess, year, sh_exp)]

# wide table for reporting
dt_exp <- dcast(dt_exp, code_union_ess ~ year, value.var = "sh_exp")

# take industry name
list <- data.table(read_excel("data/industry_codes_within_jara_v2_to_essCompatibleJARA2002.xlsx", sheet = "compatible_list"))
dt_exp <- list[dt_exp, on = "code_union_ess"]

# report only readable part
dt_exp <- dt_exp[, c("code_union_ess", "name") := NULL]
colnames(dt_exp)[1] <- "Industry"

fwrite(dt_exp, "output_final/tab_E3.csv")


# Table 7 -----------------------------------------------------------------



m_fs_dic_out <- felm(ln_output ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = dt_reg, weights = dt_reg$sales.m_value_5_avg)
summary(m_fs_dic_out)

m_fs_dic_out_n <- felm(ln_output_nominal ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = dt_reg, weights = dt_reg$sales.m_value_5_avg)
summary(m_fs_dic_out_n)

m_fs_dic_exp <- felm(ln_export_total ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = dt_reg, weights = dt_reg$sales.m_value_5_avg)
summary(m_fs_dic_exp)

m_fs_dic_abs <- felm(ln_absorption ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = dt_reg, weights = dt_reg$sales.m_value_5_avg)
summary(m_fs_dic_abs)

m_fs_dic <- felm(ln_price ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_intangible_log | code_union_ess + year, data = dt, weights = dt$sales.m_value_5_avg)
summary(m_fs_dic)

# drop electronics since it's price trend is outlier as can be seen in `price_trend_size_2.png`
dt_price_noelec <- dt[code_union_ess != 5]

m_fs_dic_noelec <- felm(ln_price ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_intangible_log | code_union_ess + year, data = dt_price_noelec, weights = dt_price_noelec$sales.m_value_5_avg)
summary(m_fs_dic_noelec)

added_line <- list(
  c('Drop Electrics', '', '', '', '', '', '\\checkmark')
)
stargazer(
  m_fs_dic_out, m_fs_dic_out_n, m_fs_dic_exp, m_fs_dic_abs, m_fs_dic, m_fs_dic_noelec,
  dep.var.labels = c('$\\ln(Y_{it})$', '$\\ln(PY_{it})$', '$\\ln(EX_{it})$', '$\\ln(AB_{it})$', '$\\ln(P_{it})$', '$\\ln(P_{it})$'),
  covariate.labels = '$\\ln(r^{Z}_{it})$',
  keep = 'unitval_t_indagg_stock_12_log_alpha',
  omit.stat = c('adj.rsq', 'ser'),
  add.lines = added_line,
  notes.label = '',
  title = 'Robot Price, Output Quantity, and Output Prices',
  table.placement = '!t',
  label = 'output_price_concise',
  out = paste0('output_final/tab_5.tex')
)



