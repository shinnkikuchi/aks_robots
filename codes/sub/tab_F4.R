# check rotemberg weights following goldsmith-pinkham, sorkin, swift (gpss)
# goal is to make figure-1 correspondent of gpss


# housekeeping ------------------------------------------------------------

rm(list=ls())
library(tidyverse)
library(data.table)
library(lfe)

# GPSS package does not work with an installing issue. posted on paul's github issue
#install.packages("devtools")
#devtools::install_github("paulgp/bartik-weight/R-code/pkg") 

# small tolerance value for sanity checks that allow rounding errors
tol <- 10^(-5)

# explanatory variable
#ev <- 'quantity'
ev <- 'price'

# prepare application-industry-level data ---------------------------------

dt_ait <- fread("data/JARA_ait.csv")

# to have application-level regression, fill all missing to zeros
list_a <- dt_ait %>% select(code_union_app, name_app) %>% unique 
list_i <- dt_ait %>% select(code_union_ess) %>% unique
list_t <- dt_ait %>% select(year) %>% unique
list_ait <- list_a %>% expand_grid(list_i) %>% expand_grid(list_t)

# attach unit values
list_unitval <- dt_ait %>% select(code_union_app, year, unitval_t_indagg_stock_12) %>% unique  
list_ait <- list_ait %>% left_join(list_unitval, by = c("code_union_app", "year")) %>% rename(unitval_t_indagg_stock_12_full = unitval_t_indagg_stock_12)

dt_ait <- list_ait %>% left_join(dt_ait, by = c("code_union_app", "name_app", "code_union_ess", "year"))
dt_ait <- dt_ait %>% mutate(sales.m_value_5_avg = ifelse(is.na(sales.m_value_5_avg), 0, sales.m_value_5_avg))

# below taken from `generate_JARA_it_by_a.R`. 

# calculate alpha based on initial year 
dt_alpha <- dt_ait %>% 
  filter(year == 1982) %>% 
  ungroup %>% group_by(code_union_ess) %>% 
  mutate(alpha = sales.m_value_5_avg/sum(sales.m_value_5_avg)) %>% 
  select(code_union_app, code_union_ess, alpha)

# to use time-series variation, consider lagged alphas too (copied from `generate_JARA_it_by_a.R`)
dt_alpha_lag <- dt_ait %>% 
  ungroup %>% group_by(code_union_ess, year) %>% 
  mutate(alpha = sales.m_value_5_avg/sum(sales.m_value_5_avg)) %>% 
  ungroup %>% group_by(code_union_ess, code_union_app) %>% 
  mutate(
    alpha_lag = lag(alpha),
    alpha_lag2 = lag(alpha, n = 2),
    alpha_lag3 = lag(alpha, n = 3),
    alpha_lag4 = lag(alpha, n = 4),
    alpha_lag5 = lag(alpha, n = 5),
    alpha_lag6 = lag(alpha, n = 6),
    alpha_lag7 = lag(alpha, n = 7)
  ) %>% 
  select(code_union_app, code_union_ess, year, starts_with("alpha_lag"))

# fill by initial-year value
dt_alpha_lag <- dt_alpha_lag %>% 
  left_join(dt_alpha, by = c("code_union_app", "code_union_ess")) %>% 
  mutate(
    alpha = ifelse(is.na(alpha), 0, alpha), # need to manually fill zero for GPSS purpose: 0 is also an observation as opposed to zero weight.
    alpha_lag = ifelse(is.na(alpha_lag), alpha, alpha_lag),
    alpha_lag2 = ifelse(is.na(alpha_lag2), alpha, alpha_lag2),
    alpha_lag3 = ifelse(is.na(alpha_lag3), alpha, alpha_lag3),
    alpha_lag4 = ifelse(is.na(alpha_lag4), alpha, alpha_lag4),
    alpha_lag5 = ifelse(is.na(alpha_lag5), alpha, alpha_lag5),
    alpha_lag6 = ifelse(is.na(alpha_lag6), alpha, alpha_lag6),
    alpha_lag7 = ifelse(is.na(alpha_lag7), alpha, alpha_lag7)
  )


# industry-year-level data ------------------------------------------------


dt_it <- fread("data/JARA_ESS_it_by_a.csv")

# annihilate data
# recall the main specification from `regress_it_by_a.R`:
#m_2sls_dic <- felm(weight_log ~ weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year | (quantity_stock_12_log_alpha ~ unitval_t_indagg_stock_12_log_alpha), data = dt_it, weights = dt_it$sales.m_value_5_avg)
#summary(m_2sls_dic)
if (ev == 'quantity'){
  m_2sls_dic_x <- felm(quantity_stock_12_log_alpha ~ weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = dt_it, weights = dt_it$sales.m_value_5_avg)
} else if (ev == 'price'){
  m_2sls_dic_x <- felm(unitval_t_indagg_stock_12_log_alpha ~ weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = dt_it, weights = dt_it$sales.m_value_5_avg)
} else {
  stop('Please set variable "ev" correctly')
}

dt_it$res_x <- m_2sls_dic_x$residuals
m_2sls_dic_y <- felm(weight_log ~ weight_hs_share + weight_cg_share + weight_fem_share + weight_u35_share + weight_o50_share + va_log_soba + import_total_log + asset_IT_log + asset_innovation_log + asset_competitive_log | code_union_ess + year, data = dt_it, weights = dt_it$sales.m_value_5_avg)
dt_it$res_y <- m_2sls_dic_y$residuals

# sanity check
m_sc_c <- felm(res_y ~ 1 | 0 | (res_x ~ unitval_t_indagg_stock_12_log_alpha), data = dt_it, weights = dt_it$sales.m_value_5_avg)
summary(m_sc_c)

# the point estimate is <W,B'Y>/<W,B'X>
beta_bartik_c <- sum(dt_it$sales.m_value_5_avg * dt_it$unitval_t_indagg_stock_12_log_alpha * dt_it$res_y)/sum(dt_it$sales.m_value_5_avg * dt_it$unitval_t_indagg_stock_12_log_alpha * dt_it$res_x)

# save annihilated data
dt_it_ann <- dt_it[,.(code_union_ess, name_ind, name_ind_short, year, res_y, res_x, weight_log, quantity_stock_12_log_alpha, weight_hs_share, weight_cg_share, weight_fem_share, weight_u35_share, weight_o50_share, va_log_soba, import_total_log, asset_IT_log, asset_innovation_log, asset_competitive_log, unitval_t_indagg_stock_12_log_alpha, sales.m_value_5_avg)]
#fwrite(dt_it_ann, "data/JARA_ESS_it_by_a_annihilated.csv")

# GPSS computations, regression -------------------------------------------

# expand dt_it to ait level for application-specific regression
dt_it_expand <- dt_it %>% left_join(dt_alpha_lag, by = c('code_union_ess', 'year')) 
list_a_vec <- list_a %>% select(code_union_app) %>% unlist %>% unname

# functionalize the IV-specific regressions
i <- 1
t <- 1982
iv_coef <- function(i, t){
  
  # slice data
  dt_it_reg <- dt_it_expand %>% filter(code_union_app == list_a_vec[i], year == t)
  
  # annihilated regression
  m_2sls_dic_res_c <- felm(res_y ~ 1 | 0 | (res_x ~ alpha), data = dt_it_reg, weights = dt_it_reg$sales.m_value_5_avg)
  #summary(m_2sls_dic_res_c)
  
  # sanity check: the point estimate must be <W,Z_k'Y>/<W,Z_k'X>
  coef <- summary(m_2sls_dic_res_c)$coefficients["`res_x(fit)`","Estimate"]
  wzywzx <- sum(dt_it_reg$sales.m_value_5_avg * dt_it_reg$alpha * dt_it_reg$res_y)/sum(dt_it_reg$sales.m_value_5_avg * dt_it_reg$alpha * dt_it_reg$res_x)
  stopifnot(abs(coef - wzywzx) < tol)
  
  return(data.frame(
    code_union_app = list_a_vec[i],
    year = t,
    coef = coef,
    fstat = lfe::waldtest(m_2sls_dic_res_c, 'res_x')['F']
  ))
}

# loop the regression
reglist <- data.frame(expand.grid(
  1:length(list_a_vec),
  seq(1982, 2017, 5)
))
colnames(reglist) <- c("i","t")
coef_gpss <- reglist %>% pmap_df(iv_coef)

# GPSS computations, rotemberg weight -------------------------------------


# take alpha and residual (from dt_it) in dt_ait
dt_ait <- dt_ait %>% 
  left_join(dt_alpha_lag, by = c("code_union_app", "code_union_ess", "year")) %>% 
  left_join(dt_it %>% select(code_union_ess, year, res_x, sales.m_value_5_avg) %>% rename(sales.m_value_5_avg_it = sales.m_value_5_avg), by = c("code_union_ess", "year"))

# drop export
dt_ait <- dt_ait %>% filter(code_union_ess != 0)

# GO!
dt_ait_rw <- dt_ait %>% 
  ungroup %>% group_by(code_union_app, name_app, year) %>% 
  mutate(summand=log(unitval_t_indagg_stock_12_full)*alpha*sales.m_value_5_avg_it*res_x) %>% #select(sales.m_value_5_avg, unitval_t_indagg_stock_12_full, alpha, res_x, summand) %>% view
  summarize(num=sum(summand, na.rm = T)) %>% 
  ungroup %>% 
  mutate(
    denom = sum(num),
    rw = num/denom
  )

# match with coefficient
dt_ait_rw <- dt_ait_rw %>% left_join(coef_gpss, by = c("code_union_app", "year"))

# sanity check: rotemberg decomposition 
stopifnot(beta_bartik_c-sum(dt_ait_rw$coef * dt_ait_rw$rw)<tol)

# application aggregtation following GPSS III.C. (page 2602)
dt_ait_rw_agg <- dt_ait_rw %>% group_by(code_union_app, name_app) %>% 
  summarize(
    alpha = sum(rw),
    beta = weighted.mean(coef, rw)
  )




# Table G.2 ---------------------------------------------------------------



dt_ait_rw_agg_export <- dt_ait_rw_agg %>% 
  mutate(
    alpha = sprintf("%.3f", round(alpha, 3)),
    beta = sprintf("%.3f", round(beta, 3)),
  )
colnames(dt_ait_rw_agg_export) <- c("Application Code", "Application", "Rotemberg Weight", "Coefficient")

file <- paste0("output_final/tab_F4.csv")
write_csv(dt_ait_rw_agg_export, file)

