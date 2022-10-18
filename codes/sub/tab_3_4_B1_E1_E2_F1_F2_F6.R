# regression at the industry level. 
# NB: because of seeds in bootstrap, all codes need to be run at a time (under `MASTER_replicate.R`) to get the final regression tables. 


# housekeeping ------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(lfe)
library(stargazer)
library(boot)

set.seed(123)
n_bs <- 100 # number of bootstrap sampling

ptm <- proc.time()


# read --------------------------------------------------------------------


df <- read_csv('data/JARA_ESS_it_by_a.csv')

# prep industry name vector
list_ind_name <- df %>% select(name_ind_short) %>% unique %>% unname %>% unlist 

source("codes/sub/functions_final.R")



# table 3, panel A  -------------------------------------------------------


print("working on robot demand analysis ...")

varname <- 'unitval_t_indagg_stock_12_log_alpha'

# no control
m_fs <- felm(quantity_stock_12_log_alpha ~ unitval_t_indagg_stock_12_log_alpha | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
summary(m_fs)
fmla <- formula("quantity_stock_12_log_alpha ~ unitval_t_indagg_stock_12_log_alpha | code_union_ess + year")
sd_b_fs <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
f_m_fs <- round((m_fs$coefficients[varname,]/sd_b_fs)^2, 3)
m_fs$se[varname] <- sd_b_fs
m_fs$tval[varname] <- m_fs$coefficients[varname,]/sd_b_fs
m_fs$pval[varname] <- 2*(1-pt(abs(m_fs$tval[varname]), df = m_fs$df))

# demography
m_fs_d <- felm(quantity_stock_12_log_alpha ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
summary(m_fs_d)
fmla <- formula("quantity_stock_12_log_alpha ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share | code_union_ess + year")
sd_b_fs_d <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
f_m_fs_d <- round((m_fs_d$coefficients[varname,]/sd_b_fs_d)^2, 3)
m_fs_d$se[varname] <- sd_b_fs_d
m_fs_d$tval[varname] <- m_fs_d$coefficients[varname,]/sd_b_fs_d
m_fs_d$pval[varname] <- 2*(1-pt(abs(m_fs_d$tval[varname]), df = m_fs_d$df))

# trade variables
m_fs_di <- felm(quantity_stock_12_log_alpha ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
summary(m_fs_di)
fmla <- formula("quantity_stock_12_log_alpha ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log | code_union_ess + year")
sd_b_fs_di <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
f_m_fs_di <- round((m_fs_di$coefficients['unitval_t_indagg_stock_12_log_alpha',]/sd_b_fs_di)^2, 3)
m_fs_di$se[varname] <- sd_b_fs_di
m_fs_di$tval[varname] <- m_fs_di$coefficients[varname,]/sd_b_fs_di
m_fs_di$pval[varname] <- 2*(1-pt(abs(m_fs_di$tval[varname]), df = m_fs_di$df))


# capitals. BASELINE
m_fs_dic <- felm(quantity_stock_12_log_alpha ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
summary(m_fs_dic)
fmla <- formula("quantity_stock_12_log_alpha ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year")
sd_b_fs_dic <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
f_m_fs_dic <- round((m_fs_dic$coefficients['unitval_t_indagg_stock_12_log_alpha',]/sd_b_fs_dic)^2, 3)
m_fs_dic$se[varname] <- sd_b_fs_dic
m_fs_dic$tval[varname] <- m_fs_dic$coefficients[varname,]/sd_b_fs_dic
m_fs_dic$pval[varname] <- 2*(1-pt(abs(m_fs_dic$tval[varname]), df = m_fs_dic$df))

# output
added_line_baseline <- list(
  c('Industry FE', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Year FE', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Demographic Controls', '', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Globalization Controls', '', '', '\\checkmark', '\\checkmark'),
  c('Technology Controls', '', '', '', '\\checkmark')
)

m_list <- list(m_fs, m_fs_d, m_fs_di, m_fs_dic)
output_slides(
  m_list = m_list, 
  title = 'Industry-level, First Stage', 
  filename = 'tab_3_panel_A', 
  dep_var_label = '$\\ln(R_{it})$', 
  cov_label = '$\\ln(p^{R}_{it})$',
  keep_var = 'unitval_t_indagg_stock_12_log_alpha',
  added_line = added_line_baseline
)






# table 3, panel B (columns 1- 4) and table E2 ----------------------------




print("working on labor demand analysis...")

varname <- 'unitval_t_indagg_stock_12_log_alpha'

# no control
m_rf <- felm(weight_log ~ unitval_t_indagg_stock_12_log_alpha | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
summary(m_rf)
fmla <- formula("weight_log ~ unitval_t_indagg_stock_12_log_alpha | code_union_ess + year")
sd_b_rf <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
m_rf$se[varname] <- sd_b_rf
m_rf$tval[varname] <- m_rf$coefficients[varname,]/sd_b_rf
m_rf$pval[varname] <- 2*(1-pt(abs(m_rf$tval[varname]), df = m_rf$df))

m_rf_nrp <- felm(weight_net_rp_log ~ unitval_t_indagg_stock_12_log_alpha | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
summary(m_rf_nrp)
fmla <- formula("weight_net_rp_log ~ unitval_t_indagg_stock_12_log_alpha | code_union_ess + year")
sd_b_rf_nrp <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
m_rf_nrp$se[varname] <- sd_b_rf_nrp
m_rf_nrp$tval[varname] <- m_rf_nrp$coefficients[varname,]/sd_b_rf_nrp
m_rf_nrp$pval[varname] <- 2*(1-pt(abs(m_rf_nrp$tval[varname]), df = m_rf_nrp$df))


# demography
m_rf_d <- felm(weight_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
summary(m_rf_d)
fmla <- formula("weight_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share | code_union_ess + year")
sd_b_rf_d <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
m_rf_d$se[varname] <- sd_b_rf_d
m_rf_d$tval[varname] <- m_rf_d$coefficients[varname,]/sd_b_rf_d
m_rf_d$pval[varname] <- 2*(1-pt(abs(m_rf_d$tval[varname]), df = m_rf_d$df))

m_rf_nrp_d <- felm(weight_net_rp_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
summary(m_rf_nrp_d)
fmla <- formula("weight_net_rp_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share | code_union_ess + year")
sd_b_rf_nrp_d <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
m_rf_nrp_d$se[varname] <- sd_b_rf_nrp_d
m_rf_nrp_d$tval[varname] <- m_rf_nrp_d$coefficients[varname,]/sd_b_rf_nrp_d
m_rf_nrp_d$pval[varname] <- 2*(1-pt(abs(m_rf_nrp_d$tval[varname]), df = m_rf_nrp_d$df))


# international variables
m_rf_di <- felm(weight_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
summary(m_rf_di)
fmla <- formula("weight_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log | code_union_ess + year")
sd_b_rf_di <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
m_rf_di$se[varname] <- sd_b_rf_di
m_rf_di$tval[varname] <- m_rf_di$coefficients[varname,]/sd_b_rf_di
m_rf_di$pval[varname] <- 2*(1-pt(abs(m_rf_di$tval[varname]), df = m_rf_di$df))

m_rf_nrp_di <- felm(weight_net_rp_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
summary(m_rf_nrp_di)
fmla <- formula("weight_net_rp_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log | code_union_ess + year")
sd_b_rf_nrp_di <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
m_rf_nrp_di$se[varname] <- sd_b_rf_nrp_di
m_rf_nrp_di$tval[varname] <- m_rf_nrp_di$coefficients[varname,]/sd_b_rf_nrp_di
m_rf_nrp_di$pval[varname] <- 2*(1-pt(abs(m_rf_nrp_di$tval[varname]), df = m_rf_nrp_di$df))


# capitals. BASELINE
m_rf_dic <- felm(weight_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
summary(m_rf_dic)
fmla <- formula("weight_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year")
sd_b_rf_dic <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
m_rf_dic$se[varname] <- sd_b_rf_dic
m_rf_dic$tval[varname] <- m_rf_dic$coefficients[varname,]/sd_b_rf_dic
m_rf_dic$pval[varname] <- 2*(1-pt(abs(m_rf_dic$tval[varname]), df = m_rf_dic$df))

m_rf_nrp_dic <- felm(weight_net_rp_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
summary(m_rf_nrp_dic)
fmla <- formula("weight_net_rp_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year")
sd_b_rf_nrp_dic <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
m_rf_nrp_dic$se[varname] <- sd_b_rf_nrp_dic
m_rf_nrp_dic$tval[varname] <- m_rf_nrp_dic$coefficients[varname,]/sd_b_rf_nrp_dic
m_rf_nrp_dic$pval[varname] <- 2*(1-pt(abs(m_rf_nrp_dic$tval[varname]), df = m_rf_nrp_dic$df))



# output
m_list <- list(m_rf, m_rf_d, m_rf_di, m_rf_dic)
output_slides(
  m_list = m_list, 
  title = 'Industry-level, Reduced Form', 
  filename = 'tab_3_panel_B_column_1_4', 
  dep_var_label = '$\\ln(L_{it})$', 
  cov_label = '$\\ln(p^{R}_{it})$',
  keep_var = 'unitval_t_indagg_stock_12_log_alpha',
  added_line = added_line_baseline
)

m_list <- list(m_rf_nrp, m_rf_nrp_d, m_rf_nrp_di, m_rf_nrp_dic)
output_slides(
  m_list = m_list, 
  title = 'Industry-level, Reduced Form, Removing Robot-Producing Workers', 
  filename = 'tab_E2', 
  dep_var_label = '$\\ln(L^{NRP}_{it})$', 
  cov_label = '$\\ln(p^{R}_{it})$',
  keep_var = 'unitval_t_indagg_stock_12_log_alpha',
  added_line = added_line_baseline
)







# table 3, panel B (columns 5-6) ------------------------------------------



print("working on different prices analysis ...")

# retrieve the main reduced-form specification
m_rf_dic_4a <- m_rf_dic
rownames(m_rf_dic_4a$coefficients)[1] <- "unitval_t_indagg_stock_12_log_alpha_reg"
rownames(m_rf_dic_4a$beta)[1] <- "unitval_t_indagg_stock_12_log_alpha_reg"

# leave-one-out prices
df_reg <- df %>% mutate(unitval_t_indagg_stock_12_log_alpha_reg = unitval_t_indagg_stock_abr_log_alpha)
m_rf_dic_looprice <- felm(weight_log ~ unitval_t_indagg_stock_12_log_alpha_reg + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df_reg, weights = df_reg$sales.m_value_5_avg)
summary(m_rf_dic_looprice)
varname <- "unitval_t_indagg_stock_12_log_alpha_reg"
fmla <- formula("weight_log ~ unitval_t_indagg_stock_12_log_alpha_reg + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year")
sd_b_rf_dic_looprice <- 1:n_bs %>% map(reg_b, df_reg, fmla, varname) %>% unlist %>% sd
m_rf_dic_looprice$se[varname] <- sd_b_rf_dic_looprice
m_rf_dic_looprice$tval[varname] <- m_rf_dic_looprice$coefficients[varname,]/sd_b_rf_dic_looprice
m_rf_dic_looprice$pval[varname] <- 2*(1-pt(abs(m_rf_dic_looprice$tval[varname]), df = m_rf_dic_looprice$df))


# export prices
df_reg <-  df %>% mutate(unitval_t_indagg_stock_12_log_alpha_reg = unitval_t_exp_stock_abr_log_alpha)
m_rf_dic_expprice <- felm(weight_log ~ unitval_t_indagg_stock_12_log_alpha_reg + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df_reg, weights = df_reg$sales.m_value_5_avg)
summary(m_rf_dic_expprice)
varname <- "unitval_t_indagg_stock_12_log_alpha_reg"
fmla <- formula("weight_log ~ unitval_t_indagg_stock_12_log_alpha_reg + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year")
sd_b_rf_dic_expprice <- 1:n_bs %>% map(reg_b, df_reg, fmla, varname) %>% unlist %>% sd
m_rf_dic_expprice$se[varname] <- sd_b_rf_dic_expprice
m_rf_dic_expprice$tval[varname] <- m_rf_dic_expprice$coefficients[varname,]/sd_b_rf_dic_expprice
m_rf_dic_expprice$pval[varname] <- 2*(1-pt(abs(m_rf_dic_expprice$tval[varname]), df = m_rf_dic_expprice$df))

# output
added_line_differentprice_rf <- list(
  c('Industry FE', '\\checkmark', '\\checkmark'),
  c('Year FE', '\\checkmark', '\\checkmark'),
  c('Demographic Controls', '\\checkmark', '\\checkmark'),
  c('Globalization Controls', '\\checkmark', '\\checkmark'),
  c('Technology Controls', '\\checkmark', '\\checkmark'),
  c('Price Measure', 'Leave-one-out', 'Export')
)
m_list_price <- list(m_rf_dic_looprice, m_rf_dic_expprice)
output_slides(
  m_list = m_list_price, 
  title = 'Alternative Price Measures', 
  filename = 'tab_3_panel_B_column_5_6', 
  dep_var_label = '$\\ln(L_{it})$', 
  cov_label = '$\\ln(p^R_{it})$',
  keep_var = 'unitval_t_indagg_stock_12_log_alpha_reg',
  added_line = added_line_differentprice_rf
)



# table F2 ----------------------------------------------------------------



print("working on dropping-industry analysis ...")

# drop electronics
df %>% filter(code_union_ess != 5) -> df_reg
m_rf_dic_noelec <- felm(weight_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df_reg, weights = df_reg$sales.m_value_5_avg)
summary(m_rf_dic_noelec)
varname <- "unitval_t_indagg_stock_12_log_alpha"
fmla <- formula("weight_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year")
sd_b_rf_dic_noelec <- 1:n_bs %>% map(reg_b, df_reg, fmla, varname) %>% unlist %>% sd
m_rf_dic_noelec$se[varname] <- sd_b_rf_dic_noelec
m_rf_dic_noelec$tval[varname] <- m_rf_dic_noelec$coefficients[varname,]/sd_b_rf_dic_noelec
m_rf_dic_noelec$pval[varname] <- 2*(1-pt(abs(m_rf_dic_noelec$tval[varname]), df = m_rf_dic_noelec$df))


# drop automobiles
df %>% filter(code_union_ess != 7) -> df_reg
m_rf_dic_noauto <- felm(weight_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df_reg, weights = df_reg$sales.m_value_5_avg)
summary(m_rf_dic_noauto)
varname <- "unitval_t_indagg_stock_12_log_alpha"
fmla <- formula("weight_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year")
sd_b_rf_dic_noauto <- 1:n_bs %>% map(reg_b, df_reg, fmla, varname) %>% unlist %>% sd
m_rf_dic_noauto$se[varname] <- sd_b_rf_dic_noauto
m_rf_dic_noauto$tval[varname] <- m_rf_dic_noauto$coefficients[varname,]/sd_b_rf_dic_noauto
m_rf_dic_noauto$pval[varname] <- 2*(1-pt(abs(m_rf_dic_noauto$tval[varname]), df = m_rf_dic_noauto$df))


# drop both
df %>% filter(code_union_ess != 5 & code_union_ess != 7) -> df_reg
m_rf_dic_noelec_noauto <- felm(weight_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df_reg, weights = df_reg$sales.m_value_5_avg)
summary(m_rf_dic_noelec_noauto)
varname <- "unitval_t_indagg_stock_12_log_alpha"
fmla <- formula("weight_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year")
sd_b_rf_dic_noelec_noauto <- 1:n_bs %>% map(reg_b, df_reg, fmla, varname) %>% unlist %>% sd
m_rf_dic_noelec_noauto$se[varname] <- sd_b_rf_dic_noelec_noauto
m_rf_dic_noelec_noauto$tval[varname] <- m_rf_dic_noelec_noauto$coefficients[varname,]/sd_b_rf_dic_noelec_noauto
m_rf_dic_noelec_noauto$pval[varname] <- 2*(1-pt(abs(m_rf_dic_noelec_noauto$tval[varname]), df = m_rf_dic_noelec_noauto$df))

# output
added_line_dropindustries <- list(
  c('Industry FE', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Year FE', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Demographic Controls', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Globalization Controls', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Technology Controls', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Dropped Industries', 'Elec.', 'Transp.', 'E\\&T')
)

m_list_price <- list(m_rf_dic_noelec, m_rf_dic_noauto, m_rf_dic_noelec_noauto)
output_slides(
  m_list = m_list_price, 
  title = 'Dropping Major Industries', 
  filename = 'tab_F2', 
  dep_var_label = '$\\ln(L_{it})$', 
  cov_label = '$\\ln(p^R_{it})$',
  keep_var = 'unitval_t_indagg_stock_12_log_alpha',
  added_line = added_line_dropindustries
)


# table 4 -----------------------------------------------------------------

print("working on group-heterogeneous effect analysis ...")

# low-skill
m_rf_dic_hs <- felm(weight_hs_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
summary(m_rf_dic_hs)
fmla <- formula("weight_hs_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year")
varname <- 'unitval_t_indagg_stock_12_log_alpha'
sd_b_rf_dic_hs <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
m_rf_dic_hs$se[varname] <- sd_b_rf_dic_hs
m_rf_dic_hs$tval[varname] <- m_rf_dic_hs$coefficients[varname,]/sd_b_rf_dic_hs
m_rf_dic_hs$pval[varname] <- 2*(1-pt(abs(m_rf_dic_hs$tval[varname]), df = m_rf_dic_hs$df))


# high-skill
m_rf_dic_cg <- felm(weight_cg_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
summary(m_rf_dic_cg)
fmla <- formula("weight_cg_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year")
varname <- 'unitval_t_indagg_stock_12_log_alpha'
sd_b_rf_dic_cg <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
m_rf_dic_cg$se[varname] <- sd_b_rf_dic_cg
m_rf_dic_cg$tval[varname] <- m_rf_dic_cg$coefficients[varname,]/sd_b_rf_dic_cg
m_rf_dic_cg$pval[varname] <- 2*(1-pt(abs(m_rf_dic_cg$tval[varname]), df = m_rf_dic_cg$df))

# female
m_rf_dic_fem <- felm(weight_fem_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
summary(m_rf_dic_fem)
fmla <- formula("weight_fem_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year")
varname <- 'unitval_t_indagg_stock_12_log_alpha'
sd_b_rf_dic_fem <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
m_rf_dic_fem$se[varname] <- sd_b_rf_dic_fem
m_rf_dic_fem$tval[varname] <- m_rf_dic_fem$coefficients[varname,]/sd_b_rf_dic_fem
m_rf_dic_fem$pval[varname] <- 2*(1-pt(abs(m_rf_dic_fem$tval[varname]), df = m_rf_dic_fem$df))

# young
m_rf_dic_u35 <- felm(weight_u35_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
summary(m_rf_dic_u35)
fmla <- formula("weight_u35_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year")
varname <- 'unitval_t_indagg_stock_12_log_alpha'
sd_b_rf_dic_u35 <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
m_rf_dic_u35$se[varname] <- sd_b_rf_dic_u35
m_rf_dic_u35$tval[varname] <- m_rf_dic_u35$coefficients[varname,]/sd_b_rf_dic_u35
m_rf_dic_u35$pval[varname] <- 2*(1-pt(abs(m_rf_dic_u35$tval[varname]), df = m_rf_dic_u35$df))

# old
m_rf_dic_o50 <- felm(weight_o50_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
summary(m_rf_dic_o50)
fmla <- formula("weight_o50_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year")
varname <- 'unitval_t_indagg_stock_12_log_alpha'
sd_b_rf_dic_o50 <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
m_rf_dic_o50$se[varname] <- sd_b_rf_dic_o50
m_rf_dic_o50$tval[varname] <- m_rf_dic_o50$coefficients[varname,]/sd_b_rf_dic_o50
m_rf_dic_o50$pval[varname] <- 2*(1-pt(abs(m_rf_dic_o50$tval[varname]), df = m_rf_dic_o50$df))

added_line_othergroup <- list(
  c('Industry FE', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Year FE', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Demographic Controls', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Globalization Controls', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Technology Controls', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Group of Worker', 'All', 'High School', '4-year Univ.', 'Female', 'Age $\\leq$ 35', 'Age $\\ge$ 50')
)


m_list_price <- list(m_rf_dic, m_rf_dic_hs, m_rf_dic_cg, m_rf_dic_fem, m_rf_dic_u35, m_rf_dic_o50)
output_slides(
  m_list = m_list_price, 
  title = 'Subgroup Analysis', 
  filename = 'tab_4', 
  dep_var_label = c('$\\ln(L_{it})$', '$\\ln(L_{it}^{HS})$', '$\\ln(L_{it}^{CG})$', '$\\ln(L_{it}^{Fem})$', '$\\ln(L_{it}^{U35})$', '$\\ln(L_{it}^{O50})$'), 
  cov_label = '$\\ln(p^R_{it})$',
  keep_var = 'unitval_t_indagg_stock_12_log_alpha',
  added_line = added_line_othergroup
)


# table B1 ----------------------------------------------------------------


df_summary <- df %>% 
  mutate(sales.b_value_5_avg = sales.m_value_5_avg/1000) %>% 
  mutate(
    quantity_stock_12_alpha = exp(quantity_stock_12_log_alpha),
    unitval_t_indagg_stock_12_alpha = exp(unitval_t_indagg_stock_12_log_alpha),
    weight_net_rp = exp(weight_net_rp_log)/1000, 
    va_soba = (exp(va_log_soba) - 1)/1000, 
    import_total = (exp(import_total_log) - 1)/1000, 
    asset_IT = (exp(asset_IT_log) - 1)/1000, 
    asset_innovation = (exp(asset_innovation_log) - 1)/1000, 
    asset_competitive = (exp(asset_competitive_log) - 1)/1000
  ) %>% 
  select(quantity_stock_12_alpha, unitval_t_indagg_stock_12_alpha, sales.m_value_5_avg, weight_net_rp, weight_hs_share, weight_cg_share, weight_fem_share, weight_u35_share, weight_o50_share, va_soba, import_total, asset_IT, asset_innovation, asset_competitive)
varnames <- c('Robot Stock', 'Robot Price (million JPY)', 'Robot Purchases (billion JPY)', 'Employment (thousand)', 'High School Graduate Share', 'College Graduate Share', 'Female Share', 'Age $\\leq$ 35 Share', 'Age $\\geq$ 50 Share', 'Foreign Value Added (billion JPY)', 'Import (billion JPY)', 'IT Asset (billion JPY)', 'Innovation Asset (billion JPY)', 'Competitive Asset (billion JPY)')  

#summary(df_summary)
df_summary %>% 
  as.data.frame() %>% 
  stargazer(
    covariate.labels = varnames,
    title = "Summary Statistics",
    label = "summaryStats",
    out = "output_final/tab_B1.tex"
  )



# table E1 ----------------------------------------------------------------


print("working on alternative stock measure analysis ...")



# 10 years
stockmeasure <- 'quantity_stock_10_log_alpha'
df <- df %>% mutate(pricemeasure = unitval_t_indagg_stock_10_log_alpha)
m_rf_dic_s10 <- felm(weight_log ~ pricemeasure + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
varname <- "pricemeasure"
fmla <- formula("weight_log ~ pricemeasure + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year")
sd_b_rf_dic_s10 <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
m_rf_dic_s10$se[varname] <- sd_b_rf_dic_s10
m_rf_dic_s10$tval[varname] <- m_rf_dic_s10$coefficients[varname,]/sd_b_rf_dic_s10
m_rf_dic_s10$pval[varname] <- 2*(1-pt(abs(m_rf_dic_s10$tval[varname]), df = m_rf_dic_s10$df))


# 15 years
stockmeasure <- 'quantity_stock_15_log_alpha'
df <- df %>% mutate(pricemeasure = unitval_t_indagg_stock_15_log_alpha)
m_rf_dic_s15 <- felm(weight_log ~ pricemeasure + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
varname <- "pricemeasure"
fmla <- formula("weight_log ~ pricemeasure + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year")
sd_b_rf_dic_s15 <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
m_rf_dic_s15$se[varname] <- sd_b_rf_dic_s15
m_rf_dic_s15$tval[varname] <- m_rf_dic_s15$coefficients[varname,]/sd_b_rf_dic_s15
m_rf_dic_s15$pval[varname] <- 2*(1-pt(abs(m_rf_dic_s15$tval[varname]), df = m_rf_dic_s15$df))


# exponential depreciation by 0.10. Artuc, Bastos, Rijkers (2018)
stockmeasure <- 'quantity_stock_abr_log_alpha'
df <- df %>% mutate(pricemeasure = unitval_t_indagg_stock_abr_log_alpha)
m_rf_dic_sabr <- felm(weight_log ~ pricemeasure + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
varname <- "pricemeasure"
fmla <- formula("weight_log ~ pricemeasure + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year")
sd_b_rf_dic_sabr <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
m_rf_dic_sabr$se[varname] <- sd_b_rf_dic_sabr
m_rf_dic_sabr$tval[varname] <- m_rf_dic_sabr$coefficients[varname,]/sd_b_rf_dic_sabr
m_rf_dic_sabr$pval[varname] <- 2*(1-pt(abs(m_rf_dic_sabr$tval[varname]), df = m_rf_dic_sabr$df))

# exponential depreciation by 0.18. Nomura and Momose (2008)
stockmeasure <- 'quantity_stock_nm_log_alpha'
df <- df %>% mutate(pricemeasure = unitval_t_indagg_stock_nm_log_alpha)
m_rf_dic_snm <- felm(weight_log ~ pricemeasure + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
varname <- "pricemeasure"
fmla <- formula("weight_log ~ pricemeasure + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year")
sd_b_rf_dic_snm <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
m_rf_dic_snm$se[varname] <- sd_b_rf_dic_snm
m_rf_dic_snm$tval[varname] <- m_rf_dic_snm$coefficients[varname,]/sd_b_rf_dic_snm
m_rf_dic_snm$pval[varname] <- 2*(1-pt(abs(m_rf_dic_snm$tval[varname]), df = m_rf_dic_snm$df))


# output
m_list <- list(m_rf_dic_s10, m_rf_dic_s15, m_rf_dic_sabr, m_rf_dic_snm)
added_line_differentstock <- list(
  c('Stock Measurement', '10 Years IWM', '15 Years IWM', '$\\delta = 0.1$', '$\\delta = 0.18$'),
  c('Industry FE', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Year FE', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Demographic Controls', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Globalization Controls', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Technology Controls', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark')
)
output_slides(
  m_list = m_list, 
  title = 'Industry-level Analysis with Different Stock Measures', 
  filename = 'tab_E1', 
  dep_var_label = '$\\ln(L_{it})$', 
  cov_label = '$\\ln(p^{R}_{it})$',
  keep_var = 'pricemeasure',
  added_line = added_line_differentstock
)


# table F1 ----------------------------------------------------------------

print("working on pre-period analysis ...")

df %>% ungroup %>% group_by(code_union_ess) %>% 
  mutate(weight_log_lag = lag(weight_log, 3)) -> df

# no control
# m_fs_pre <- felm(quantity_stock_12_log_alpha ~ unitval_t_indagg_stock_12_log_alpha | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
# #f_m_fs_pre <- sprintf("%.3f", round(lfe::waldtest(m_fs_pre, 'unitval_t_indagg_stock_12_log_alpha')['F'], 3))
# varname <- "unitval_t_indagg_stock_12_log_alpha"
# fmla <- formula("quantity_stock_12_log_alpha ~ unitval_t_indagg_stock_12_log_alpha | code_union_ess + year")
# sd_b_fs_pre <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
# f_m_fs_pre <- round((m_fs_pre$coefficients[varname,]/sd_b_fs_pre)^2, 3)
m_rf_pre <- felm(weight_log_lag ~ unitval_t_indagg_stock_12_log_alpha | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
summary(m_rf_pre)


# demography
# m_fs_d_pre <- felm(quantity_stock_12_log_alpha ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
# #f_m_fs_d_pre <- sprintf("%.3f", round(lfe::waldtest(m_fs_d_pre, 'unitval_t_indagg_stock_12_log_alpha')['F'], 3))
# varname <- "unitval_t_indagg_stock_12_log_alpha"
# fmla <- formula("quantity_stock_12_log_alpha ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share | code_union_ess + year")
# sd_b_fs_d_pre <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
# f_m_fs_d_pre <- round((m_fs_d_pre$coefficients[varname,]/sd_b_fs_d_pre)^2, 3)
m_rf_d_pre <- felm(weight_log_lag ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
summary(m_rf_d_pre)


# trade variables
# m_fs_di_pre <- felm(quantity_stock_12_log_alpha ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + import_total_log | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
# #f_m_fs_di_pre <- sprintf("%.3f", round(lfe::waldtest(m_fs_di_pre, 'unitval_t_indagg_stock_12_log_alpha')['F'], 3))
# varname <- "unitval_t_indagg_stock_12_log_alpha"
# fmla <- formula("quantity_stock_12_log_alpha ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + import_total_log | code_union_ess + year")
# sd_b_fs_di_pre <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
# f_m_fs_di_pre <- round((m_fs_di_pre$coefficients[varname,]/sd_b_fs_di_pre)^2, 3)
m_rf_di_pre <- felm(weight_log_lag ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + import_total_log | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
summary(m_rf_di_pre)

# capitals. BASELINE
# m_fs_dic_pre <- felm(quantity_stock_12_log_alpha ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
# #f_m_fs_dic_pre <- sprintf("%.3f", round(lfe::waldtest(m_fs_dic_pre, 'unitval_t_indagg_stock_12_log_alpha')['F'], 3))
# varname <- "unitval_t_indagg_stock_12_log_alpha"
# fmla <- formula("quantity_stock_12_log_alpha ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year")
# sd_b_fs_dic_pre <- 1:n_bs %>% map(reg_b, df, fmla, varname) %>% unlist %>% sd
# f_m_fs_dic_pre <- round((m_fs_dic_pre$coefficients[varname,]/sd_b_fs_dic_pre)^2, 3)
m_rf_dic_pre <- felm(weight_log_lag ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df, weights = df$sales.m_value_5_avg)
summary(m_rf_dic_pre)

added_line_pre <- list(
  c('Industry FE', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Year FE', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Demographic Controls', '', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Globalization Controls', '', '', '\\checkmark', '\\checkmark'),
  c('Technology Controls', '', '', '', '\\checkmark')
)
m_list <- list(m_rf_pre, m_rf_d_pre, m_rf_di_pre, m_rf_dic_pre)
output_slides(
  m_list = m_list, 
  title = 'Pre-period Analysis at the Industry Level', 
  filename = 'tab_F1', 
  dep_var_label = '$\\ln(L_{i,t-15})$', 
  cov_label = '$\\ln(p^{R}_{it})$', 
  keep_var = 'unitval_t_indagg_stock_12_log_alpha',
  added_line = added_line_pre
)



# table F6 ----------------------------------------------------------------


print("working on different years analysis ...")


# dropping one year
df %>% filter(year <= 2012) -> df_reg

m_rf_dic_1_1 <- felm(weight_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df_reg, weights = df_reg$sales.m_value_5_avg)
summary(m_rf_dic_1_1)
varname <- "unitval_t_indagg_stock_12_log_alpha"
fmla <- formula("weight_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year")
sd_b_rf_dic_1_1 <- 1:n_bs %>% map(reg_b, df_reg, fmla, varname) %>% unlist %>% sd
f_m_rf_dic_1_1 <- round((m_rf_dic_1_1$coefficients[varname,]/sd_b_rf_dic_1_1)^2, 3)


df %>% filter(year >= 1987) -> df_reg

m_rf_dic_1_2 <- felm(weight_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df_reg, weights = df_reg$sales.m_value_5_avg)
summary(m_rf_dic_1_2)
varname <- "unitval_t_indagg_stock_12_log_alpha"
fmla <- formula("weight_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year")
sd_b_rf_dic_1_2 <- 1:n_bs %>% map(reg_b, df_reg, fmla, varname) %>% unlist %>% sd
f_m_rf_dic_1_2 <- round((m_rf_dic_1_2$coefficients[varname,]/sd_b_rf_dic_1_2)^2, 3)



# dropping two years 
df %>% filter(year <= 2007) -> df_reg

m_rf_dic_2_1 <- felm(weight_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df_reg, weights = df_reg$sales.m_value_5_avg)
summary(m_rf_dic_2_1)
varname <- "unitval_t_indagg_stock_12_log_alpha"
fmla <- formula("weight_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year")
sd_b_rf_dic_2_1 <- 1:n_bs %>% map(reg_b, df_reg, fmla, varname) %>% unlist %>% sd
f_m_rf_dic_2_1 <- round((m_rf_dic_2_1$coefficients[varname,]/sd_b_rf_dic_2_1)^2, 3)


df %>% filter(year >= 1987 & year <= 2012) -> df_reg

m_rf_dic_2_2 <- felm(weight_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df_reg, weights = df_reg$sales.m_value_5_avg)
summary(m_rf_dic_2_2)
varname <- "unitval_t_indagg_stock_12_log_alpha"
fmla <- formula("weight_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year")
sd_b_rf_dic_2_2 <- 1:n_bs %>% map(reg_b, df_reg, fmla, varname) %>% unlist %>% sd
f_m_rf_dic_2_2 <- round((m_rf_dic_2_2$coefficients[varname,]/sd_b_rf_dic_2_2)^2, 3)



df %>% filter(year >= 1992) -> df_reg
m_rf_dic_2_3 <- felm(weight_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = df_reg, weights = df_reg$sales.m_value_5_avg)
summary(m_rf_dic_2_3)
varname <- "unitval_t_indagg_stock_12_log_alpha"
fmla <- formula("weight_log ~ unitval_t_indagg_stock_12_log_alpha + weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year")
sd_b_rf_dic_2_3 <- 1:n_bs %>% map(reg_b, df_reg, fmla, varname) %>% unlist %>% sd
f_m_rf_dic_2_3 <- round((m_rf_dic_2_3$coefficients[varname,]/sd_b_rf_dic_2_3)^2, 3)




# report
m_list <- list(m_rf_dic, m_rf_dic_1_1, m_rf_dic_1_2, m_rf_dic_2_1, m_rf_dic_2_2, m_rf_dic_2_3)
added_line_sampleperiod <- list(
  c('Sample Period', '1978-2017', '1978-2012', '1983-2017', '1978-2007', '1983-2012', '1988-2017')
)
output_slides(
  m_list = m_list, 
  title = 'Alternative Sample Periods', 
  filename = 'tab_F6', 
  dep_var_label = '$\\ln(L_{it})$', 
  cov_label = '$\\ln(p^{R}_{it})$', 
  keep_var = 'unitval_t_indagg_stock_12_log_alpha',
  added_line = added_line_sampleperiod
)



