# this file takes `JARA_ESS_ct.csv` and conduct CZ-level analysis

# housekeeping ------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(lfe)
library(stargazer)
library(sandwich)


output_slides <- function(m_list, title, filename, dep_var_label, cov_label, keep_var, added_line){
  stargazer(
    m_list,
    dep.var.labels = dep_var_label,
    covariate.labels = cov_label,
    keep = keep_var,
    omit.stat = c('adj.rsq', 'ser'),
    add.lines = added_line,
    notes.label = '',
    omit.table.layout = "n",
    title = title,
    table.placement = '!t',
    label = paste0('cz_', filename),
    out = paste0('output_final/', filename, '.tex')
  )
  
}


# read --------------------------------------------------------------------

nrow <- nrow(read_csv('data/JARA_ESS_ct.csv'))
df <- read_csv('data/JARA_ESS_ct.csv', guess_max = nrow)

# only focus on 1997 and later, because anyways we take 15 year differences.
df %>% filter(year >= 1997) -> df


# keep the sample consistent across analysis
df %>% filter(
  is.finite(weight_manuf_educ__sex__age__log_d15),
  is.finite(weight_emp_educ__sex__age__log_d15), 
  is.finite(weight_emp_educ_2_sex__age__log_d15), 
  is.finite(weight_emp_educ_4_sex__age__log_d15),
  is.finite(weight_emp_educ__sex_2_age__log_d15), 
  is.finite(weight_emp_educ__sex__age_1_log_d15),
  is.finite(weight_emp_educ__sex__age_3_log_d15),
  is.finite(weight_pop_educ_4_sex__age__log_l15), 
  is.finite(weight_pop_educ__sex_2_age__log_l15), 
  is.finite(weight_pop_educ__sex__age_1_log_l15), 
  is.finite(weight_pop_educ__sex__age_3_log_l15), 
  is.finite(weight_pop_educ__sex__age_4_log_l15),
  is.finite(weight_pop_educ_4_sex__age__log_l15),
  is.finite(hours_week_emp_educ__sex__age__log_d15), 
  is.finite(income_ph_emp_educ__sex__age__log_d15), 
  is.finite(income_ph_emp_educ_1_sex__age__log_d15), 
  is.finite(income_ph_emp_educ_2_sex__age__log_d15), 
  is.finite(income_ph_emp_educ_3_sex__age__log_d15), 
  is.finite(income_ph_emp_educ_4_sex__age__log_d15),
  is.finite(income_ph_emp_educ__sex_2_age__log_d15), 
  is.finite(income_ph_emp_educ__sex__age_1_log_d15), 
  is.finite(income_ph_emp_educ__sex__age_3_log_d15),
  is.finite(hours_week_emp_educ_1_sex__age__log_d15), 
  is.finite(hours_week_emp_educ_2_sex__age__log_d15), 
  is.finite(hours_week_emp_educ_3_sex__age__log_d15), 
  is.finite(hours_week_emp_educ_4_sex__age__log_d15),
  is.finite(hours_week_emp_educ__sex_2_age__log_d15), 
  is.finite(hours_week_emp_educ__sex__age_1_log_d15), 
  is.finite(hours_week_emp_educ__sex__age_3_log_d15),
) %>% ungroup %>% group_by(cluster) %>% 
  mutate(obs = n()) %>% filter(obs==5) -> df_reg



# Table F7 ----------------------------------------------------------------

m_fs <- felm(quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha_d15 | cluster + year | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
summary(m_fs)
f_m_fs <- round(lfe::waldtest(m_fs, 'unitval_t_indagg_stock_12_log_alpha_d15')['F'], 3)

m_fs_d <- felm(quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha_d15 + weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 | cluster + year | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
summary(m_fs_d)
f_m_fs_d <- round(lfe::waldtest(m_fs_d, 'unitval_t_indagg_stock_12_log_alpha_d15')['F'], 3)

m_fs_di <- felm(quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha_d15 + weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 | cluster + year | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
summary(m_fs_di)
f_m_fs_di <- round(lfe::waldtest(m_fs_di, 'unitval_t_indagg_stock_12_log_alpha_d15')['F'], 3)

m_fs_dic <- felm(quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha_d15 + weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_competitive_d15 + asset_innovation_d15 | cluster + year | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
summary(m_fs_dic)
f_m_fs_dic <- round(lfe::waldtest(m_fs_dic, 'unitval_t_indagg_stock_12_log_alpha_d15')['F'], 3)


# standard deviation of IV
df_reg$unitval_t_indagg_stock_12_log_alpha_d15 %>% sd
# residualize
m_fs_dic_res <- felm(unitval_t_indagg_stock_12_log_alpha_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_competitive_d15 + asset_innovation_d15 | cluster + year, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
m_fs_dic_res$residuals %>% sd

added_line_fs <- list(
  c('CZ FE', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Year FE', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Demographic Controls', '', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Globalization Controls', '', '', '\\checkmark', '\\checkmark'),
  c('Technology Controls', '', '', '', '\\checkmark'),
  c('IV F-statistics', f_m_fs, f_m_fs_d, f_m_fs_di, f_m_fs_dic)
)
output_slides(
  m_list = list(m_fs, m_fs_d, m_fs_di, m_fs_dic), 
  title = 'CZ-level, First Stage', 
  filename = 'tab_F7', 
  dep_var_label = '$\\Delta R_{ct}$',
  cov_label = '$\\Delta p^{R}_{ct}$',
  keep_var = 'unitval_t_indagg_stock_12_log_alpha_d15',
  added_line = added_line_fs
)



# Table 6 -----------------------------------------------------------------



# employment
#df %>% filter(is.finite(weight_emp_educ__sex__age__log_d15), is.finite(weight_pop_educ_4_sex__age__log_l15), is.finite(weight_pop_educ__sex_2_age__log_l15), is.finite(weight_pop_educ__sex__age_1_log_l15), is.finite(weight_pop_educ__sex__age_3_log_l15), is.finite(weight_pop_educ__sex__age_4_log_l15)) -> df_reg 
m_2sls_emp <- felm(weight_emp_educ__sex__age__log_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_competitive_d15 + asset_innovation_d15 | cluster + year | (quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha) | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
summary(m_2sls_emp)

# population
#df %>% filter(is.finite(weight_pop_educ__sex__age__log_d15), is.finite(weight_pop_educ_4_sex__age__log_l15), is.finite(weight_pop_educ__sex_2_age__log_l15), is.finite(weight_pop_educ__sex__age_1_log_l15), is.finite(weight_pop_educ__sex__age_3_log_l15), is.finite(weight_pop_educ__sex__age_4_log_l15)) -> df_reg 
m_2sls_pop <- felm(weight_pop_educ__sex__age__log_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_competitive_d15 + asset_innovation_d15 | cluster + year | (quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha) | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
summary(m_2sls_pop)

# emp-pop ratio (Acemoglu Restrepo)
#df %>% filter(is.finite(emp_pop_d15), is.finite(weight_pop_educ_4_sex__age__log_l15), is.finite(weight_pop_educ__sex_2_age__log_l15), is.finite(weight_pop_educ__sex__age_1_log_l15), is.finite(weight_pop_educ__sex__age_3_log_l15), is.finite(weight_pop_educ__sex__age_4_log_l15)) -> df_reg 
m_2sls_AR <- felm(emp_pop_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_competitive_d15 + asset_innovation_d15 | cluster + year | (quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha) | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
summary(m_2sls_AR)

# manufacturing
#df %>% filter(is.finite(weight_manuf_educ__sex__age__log_d15), is.finite(weight_pop_educ_4_sex__age__log_l15), is.finite(weight_pop_educ__sex_2_age__log_l15), is.finite(weight_pop_educ__sex__age_1_log_l15), is.finite(weight_pop_educ__sex__age_3_log_l15), is.finite(weight_pop_educ__sex__age_4_log_l15)) -> df_reg 
m_2sls_manuf <- felm(weight_manuf_educ__sex__age__log_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_competitive_d15 + asset_innovation_d15 | cluster + year | (quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha) | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
summary(m_2sls_manuf)

# non-manufacturing
#df %>% filter(is.finite(weight_service_educ__sex__age__log_d15), is.finite(weight_pop_educ_4_sex__age__log_l15), is.finite(weight_pop_educ__sex_2_age__log_l15), is.finite(weight_pop_educ__sex__age_1_log_l15), is.finite(weight_pop_educ__sex__age_3_log_l15), is.finite(weight_pop_educ__sex__age_4_log_l15)) -> df_reg 
m_2sls_service <- felm(weight_service_educ__sex__age__log_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_competitive_d15 + asset_innovation_d15 | cluster + year | (quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha) | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
summary(m_2sls_service)

# non-employment
#df %>% filter(is.finite(weight_depend_educ__sex__age__log_d15), is.finite(weight_pop_educ_4_sex__age__log_l15), is.finite(weight_pop_educ__sex_2_age__log_l15), is.finite(weight_pop_educ__sex__age_1_log_l15), is.finite(weight_pop_educ__sex__age_3_log_l15), is.finite(weight_pop_educ__sex__age_4_log_l15)) -> df_reg 
m_2sls_depend <- felm(weight_depend_educ__sex__age__log_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_competitive_d15 + asset_innovation_d15 | cluster + year | (quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha) | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
summary(m_2sls_depend)

# output
added_line_main_spillover <- list(
  c('CZ FE', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Year FE', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Demographic Controls', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Globalization Controls', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Technology Controls', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark')
)
output_slides(
  m_list = list(m_2sls_emp, m_2sls_pop, m_2sls_AR, m_2sls_manuf, m_2sls_service, m_2sls_depend), 
  title = 'CZ-level 2SLS Regression', 
  filename = 'tab_6', 
  dep_var_label = c('$\\Delta \\ln(L_{ct})$', '$\\Delta \\ln(Pop_{ct})$', '$\\Delta \\frac{L_{ct}}{Pop_{ct}}$', '$\\Delta \\ln(L^{MAN.}_{ct})$', '$\\Delta \\ln(L^{SER.}_{ct})$', '$\\Delta \\ln(Pop^{DEP}_{ct})$'), 
  cov_label = '$\\Delta R_{ct}$',
  keep_var = 'quantity_stock_12_log_alpha',
  added_line = added_line_main_spillover
)



# Table 7 -----------------------------------------------------------------


# hours
#df %>% filter(is.finite(hours_week_emp_educ__sex__age__log_d15), is.finite(weight_pop_educ_4_sex__age__log_l15), is.finite(weight_pop_educ__sex_2_age__log_l15), is.finite(weight_pop_educ__sex__age_1_log_l15), is.finite(weight_pop_educ__sex__age_3_log_l15), is.finite(weight_pop_educ__sex__age_4_log_l15)) -> df_reg 
m_2sls_hours <- felm(hours_week_emp_educ__sex__age__log_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_intangible_d15 + asset_innovation_d15 | cluster + year | (quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha) | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
summary(m_2sls_hours)

# income per hour (wage)
#df %>% filter(is.finite(income_ph_emp_educ__sex__age__log_d15), is.finite(weight_pop_educ_4_sex__age__log_l15), is.finite(weight_pop_educ__sex_2_age__log_l15), is.finite(weight_pop_educ__sex__age_1_log_l15), is.finite(weight_pop_educ__sex__age_3_log_l15), is.finite(weight_pop_educ__sex__age_4_log_l15)) -> df_reg 
m_2sls_income <- felm(income_ph_emp_educ__sex__age__log_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_intangible_d15 + asset_innovation_d15 | cluster + year | (quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha) | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
summary(m_2sls_income)

# output for other variables
added_line_variable <- list(
  c('CZ FE', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Year FE', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Demographic Controls', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Globalization Controls', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Technology Controls', '\\checkmark', '\\checkmark', '\\checkmark'),
  c('Variable', '\\# Workers', 'Average Hours', 'Average Hourly Wage')
)
output_slides(
  m_list = list(m_2sls_emp, m_2sls_hours, m_2sls_income), 
  title = 'CZ-level, 2SLS', 
  filename = 'tab_7', 
  dep_var_label = c('$\\Delta \\ln(L_{ct})$', '$\\Delta \\ln(h_{ct})$', '$\\Delta \\ln(w_{ct})$'), 
  cov_label = '$\\Delta R_{ct}$',
  keep_var = 'quantity_stock_12_log_alpha',
  added_line = added_line_variable
)



# Table 8, panel A -------------------------------------------------------


# educ == 2 (high school)
m_2sls_emp_educ_2 <- felm(weight_emp_educ_2_sex__age__log_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_intangible_d15 + asset_innovation_d15 | cluster + year | (quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha) | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
summary(m_2sls_emp_educ_2)

# educ == 4 (4-year university)
m_2sls_emp_educ_4 <- felm(weight_emp_educ_4_sex__age__log_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_intangible_d15 + asset_innovation_d15 | cluster + year | (quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha) | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
summary(m_2sls_emp_educ_4)

# sex == 2 (female)
m_2sls_emp_sex_2 <- felm(weight_emp_educ__sex_2_age__log_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_intangible_d15 + asset_innovation_d15 | cluster + year | (quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha) | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
summary(m_2sls_emp_sex_2)

# age <= 35
m_2sls_emp_age_1 <- felm(weight_emp_educ__sex__age_1_log_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_intangible_d15 + asset_innovation_d15 | cluster + year | (quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha) | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
summary(m_2sls_emp_age_1)

# age >= 50
m_2sls_emp_age_3 <- felm(weight_emp_educ__sex__age_3_log_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_intangible_d15 + asset_innovation_d15 | cluster + year | (quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha) | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
summary(m_2sls_emp_age_3)


# added_line_educ_emp <- list(
#   c('CZ FE', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
#   c('Year FE', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
#   c('Demographic Controls', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
#   c('Globalization Controls', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
#   c('Technology Controls', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
#   c('Group', 'High School', '4-year Univ.', 'Female', 'Age \\leq 35', 'Age \\geq 50') 
# )
added_line_educ_emp <- list(
  c('Group', 'High School', '4-year Univ.', 'Female', 'Age $\\leq$ 35', 'Age $\\geq$ 50') 
)
output_slides(
  m_list = list(m_2sls_emp_educ_2, m_2sls_emp_educ_4, m_2sls_emp_sex_2, m_2sls_emp_age_1, m_2sls_emp_age_3), 
  title = 'CZ-level Regression of Employment By Demographic Group', 
  filename = 'tab_8_panel_A', 
  dep_var_label = c('$\\Delta \\ln(L^{HS}_{ct})$', '$\\Delta \\ln(L^{CG}_{ct})$', '$\\Delta \\ln(L^{Fem}_{ct})$', '$\\Delta \\ln(L^{U35}_{ct})$', '$\\Delta \\ln(L^{O50}_{ct})$'),
  cov_label = '$\\Delta R_{ct}$',
  keep_var = 'quantity_stock_12_log_alpha',
  added_line = added_line_educ_emp
)


# Table 8, panel B -------------------------------------------------------


# educ == 1 (less than high school)
#df %>% filter(is.finite(hours_week_emp_educ_1_sex__age__log_d15), is.finite(hours_week_emp_educ_2_sex__age__log_d15), is.finite(hours_week_emp_educ_3_sex__age__log_d15), is.finite(hours_week_emp_educ_4_sex__age__log_d15), is.finite(weight_pop_educ_4_sex__age__log_l15), is.finite(weight_pop_educ__sex_2_age__log_l15), is.finite(weight_pop_educ__sex__age_1_log_l15), is.finite(weight_pop_educ__sex__age_3_log_l15), is.finite(weight_pop_educ__sex__age_4_log_l15)) -> df_reg 
#m_2sls_hours_educ_1 <- felm(hours_week_emp_educ_1_sex__age__log_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_intangible_d15 + asset_innovation_d15 | cluster + year | (quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha) | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
#summary(m_2sls_hours_educ_1)

# educ == 2 (high school)
#df %>% filter(is.finite(hours_week_emp_educ_1_sex__age__log_d15), is.finite(hours_week_emp_educ_2_sex__age__log_d15), is.finite(hours_week_emp_educ_3_sex__age__log_d15), is.finite(hours_week_emp_educ_4_sex__age__log_d15), is.finite(weight_pop_educ_4_sex__age__log_l15), is.finite(weight_pop_educ__sex_2_age__log_l15), is.finite(weight_pop_educ__sex__age_1_log_l15), is.finite(weight_pop_educ__sex__age_3_log_l15), is.finite(weight_pop_educ__sex__age_4_log_l15)) -> df_reg 
m_2sls_hours_educ_2 <- felm(hours_week_emp_educ_2_sex__age__log_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_intangible_d15 + asset_innovation_d15 | cluster + year | (quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha) | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
summary(m_2sls_hours_educ_2)

# educ == 3 (technical college)
#df %>% filter(is.finite(hours_week_emp_educ_1_sex__age__log_d15), is.finite(hours_week_emp_educ_2_sex__age__log_d15), is.finite(hours_week_emp_educ_3_sex__age__log_d15), is.finite(hours_week_emp_educ_4_sex__age__log_d15), is.finite(weight_pop_educ_4_sex__age__log_l15), is.finite(weight_pop_educ__sex_2_age__log_l15), is.finite(weight_pop_educ__sex__age_1_log_l15), is.finite(weight_pop_educ__sex__age_3_log_l15), is.finite(weight_pop_educ__sex__age_4_log_l15)) -> df_reg 
#m_2sls_hours_educ_3 <- felm(hours_week_emp_educ_3_sex__age__log_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_intangible_d15 + asset_innovation_d15 | cluster + year | (quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha) | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
#summary(m_2sls_hours_educ_3)

# educ == 4 (4-year university)
#df %>% filter(is.finite(hours_week_emp_educ_1_sex__age__log_d15), is.finite(hours_week_emp_educ_2_sex__age__log_d15), is.finite(hours_week_emp_educ_3_sex__age__log_d15), is.finite(hours_week_emp_educ_4_sex__age__log_d15), is.finite(weight_pop_educ_4_sex__age__log_l15), is.finite(weight_pop_educ__sex_2_age__log_l15), is.finite(weight_pop_educ__sex__age_1_log_l15), is.finite(weight_pop_educ__sex__age_3_log_l15), is.finite(weight_pop_educ__sex__age_4_log_l15)) -> df_reg 
m_2sls_hours_educ_4 <- felm(hours_week_emp_educ_4_sex__age__log_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_intangible_d15 + asset_innovation_d15 | cluster + year | (quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha) | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
summary(m_2sls_hours_educ_4)

# sex == 2 (female)
#df %>% filter(is.finite(hours_week_emp_educ_1_sex__age__log_d15), is.finite(hours_week_emp_educ_2_sex__age__log_d15), is.finite(hours_week_emp_educ_3_sex__age__log_d15), is.finite(hours_week_emp_educ_4_sex__age__log_d15), is.finite(weight_pop_educ_4_sex__age__log_l15), is.finite(weight_pop_educ__sex_2_age__log_l15), is.finite(weight_pop_educ__sex__age_1_log_l15), is.finite(weight_pop_educ__sex__age_3_log_l15), is.finite(weight_pop_educ__sex__age_4_log_l15)) -> df_reg 
m_2sls_hours_sex_2 <- felm(hours_week_emp_educ__sex_2_age__log_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_intangible_d15 + asset_innovation_d15 | cluster + year | (quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha) | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
summary(m_2sls_hours_sex_2)

# age <= 35
#df %>% filter(is.finite(hours_week_emp_educ_1_sex__age__log_d15), is.finite(hours_week_emp_educ_2_sex__age__log_d15), is.finite(hours_week_emp_educ_3_sex__age__log_d15), is.finite(hours_week_emp_educ_4_sex__age__log_d15), is.finite(weight_pop_educ_4_sex__age__log_l15), is.finite(weight_pop_educ__sex_2_age__log_l15), is.finite(weight_pop_educ__sex__age_1_log_l15), is.finite(weight_pop_educ__sex__age_3_log_l15), is.finite(weight_pop_educ__sex__age_4_log_l15)) -> df_reg 
m_2sls_hours_age_1 <- felm(hours_week_emp_educ__sex__age_1_log_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_intangible_d15 + asset_innovation_d15 | cluster + year | (quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha) | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
summary(m_2sls_hours_age_1)

# age >= 50
#df %>% filter(is.finite(hours_week_emp_educ_1_sex__age__log_d15), is.finite(hours_week_emp_educ_2_sex__age__log_d15), is.finite(hours_week_emp_educ_3_sex__age__log_d15), is.finite(hours_week_emp_educ_4_sex__age__log_d15), is.finite(weight_pop_educ_4_sex__age__log_l15), is.finite(weight_pop_educ__sex_2_age__log_l15), is.finite(weight_pop_educ__sex__age_1_log_l15), is.finite(weight_pop_educ__sex__age_3_log_l15), is.finite(weight_pop_educ__sex__age_4_log_l15)) -> df_reg 
m_2sls_hours_age_3 <- felm(hours_week_emp_educ__sex__age_3_log_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_intangible_d15 + asset_innovation_d15 | cluster + year | (quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha) | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
summary(m_2sls_hours_age_3)

# added_line_hours_wage <- list(
#   c('CZ FE', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
#   c('Year FE', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
#   c('Demographic Controls', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
#   c('Globalization Controls', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
#   c('Technology Controls', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
#   c('Group', 'Middle School Grad.', 'High School Grad.', 'Technical College Grad.', '4-year Univ. Grad.')
# )
# output_slides(
#   m_list = list(m_2sls_hours_educ_1, m_2sls_hours_educ_2, m_2sls_hours_educ_3, m_2sls_hours_educ_4), 
#   title = 'CZ-level, 2SLS, Hours Effects By Education Level', 
#   filename = '2sls_hours_educ_cb', 
#   dep_var_label = c('$\\Delta \\ln(h^{MS}_{ct})$', '$\\Delta \\ln(h^{HS}_{ct})$', '$\\Delta \\ln(h^{TC}_{ct})$', '$\\Delta \\ln(h^{4U}_{ct})$'), 
#   cov_label = '$\\Delta R_{ct}$',
#   keep_var = 'quantity_stock_12_log_alpha',
#   added_line = added_line_hours_wage
# )

# added_line_educ_hours <- list(
#   c('CZ FE', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
#   c('Year FE', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
#   c('Demographic Controls', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
#   c('Globalization Controls', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
#   c('Technology Controls', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
#   c('Group', 'High School', '4-year Univ.', 'Female', 'Age \\leq 35', 'Age \\geq 50') 
# )
added_line_educ_hours <- list(
  c('Group', 'High School', '4-year Univ.', 'Female', 'Age $\\leq$ 35', 'Age $\\geq$ 50') 
)
output_slides(
  m_list = list(m_2sls_hours_educ_2, m_2sls_hours_educ_4, m_2sls_hours_sex_2, m_2sls_hours_age_1, m_2sls_hours_age_3), 
  title = 'CZ-level Regression of Hours By Demographic Group', 
  filename = 'tab_8_panel_B', 
  dep_var_label = c('$\\Delta \\ln(h^{HS}_{ct})$', '$\\Delta \\ln(h^{CG}_{ct})$', '$\\Delta \\ln(h^{Fem}_{ct})$', '$\\Delta \\ln(h^{U35}_{ct})$', '$\\Delta \\ln(h^{O50}_{ct})$'),
  cov_label = '$\\Delta R_{ct}$',
  keep_var = 'quantity_stock_12_log_alpha',
  added_line = added_line_educ_hours
)


# Table 8, panel C -------------------------------------------------------

# educ == 1 (less than high school)
#df %>% filter(is.finite(income_ph_emp_educ_1_sex__age__log_d15), is.finite(income_ph_emp_educ_2_sex__age__log_d15), is.finite(income_ph_emp_educ_3_sex__age__log_d15), is.finite(income_ph_emp_educ_4_sex__age__log_d15), is.finite(weight_pop_educ_4_sex__age__log_l15), is.finite(weight_pop_educ__sex_2_age__log_l15), is.finite(weight_pop_educ__sex__age_1_log_l15), is.finite(weight_pop_educ__sex__age_3_log_l15), is.finite(weight_pop_educ__sex__age_4_log_l15)) -> df_reg
#m_2sls_income_ph_educ_1 <- felm(income_ph_emp_educ_1_sex__age__log_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_intangible_d15 + asset_innovation_d15 | cluster + year | (quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha) | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
#summary(m_2sls_income_ph_educ_1)


# educ == 2 (high school)
#df %>% filter(is.finite(income_ph_emp_educ_1_sex__age__log_d15), is.finite(income_ph_emp_educ_2_sex__age__log_d15), is.finite(income_ph_emp_educ_3_sex__age__log_d15), is.finite(income_ph_emp_educ_4_sex__age__log_d15), is.finite(weight_pop_educ_4_sex__age__log_l15), is.finite(weight_pop_educ__sex_2_age__log_l15), is.finite(weight_pop_educ__sex__age_1_log_l15), is.finite(weight_pop_educ__sex__age_3_log_l15), is.finite(weight_pop_educ__sex__age_4_log_l15)) -> df_reg 
m_2sls_income_ph_educ_2 <- felm(income_ph_emp_educ_2_sex__age__log_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_intangible_d15 + asset_innovation_d15 | cluster + year | (quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha) | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
summary(m_2sls_income_ph_educ_2)

# educ == 3 (technical college)
#df %>% filter(is.finite(income_ph_emp_educ_1_sex__age__log_d15), is.finite(income_ph_emp_educ_2_sex__age__log_d15), is.finite(income_ph_emp_educ_3_sex__age__log_d15), is.finite(income_ph_emp_educ_4_sex__age__log_d15), is.finite(weight_pop_educ_4_sex__age__log_l15), is.finite(weight_pop_educ__sex_2_age__log_l15), is.finite(weight_pop_educ__sex__age_1_log_l15), is.finite(weight_pop_educ__sex__age_3_log_l15), is.finite(weight_pop_educ__sex__age_4_log_l15)) -> df_reg 
#m_2sls_income_ph_educ_3 <- felm(income_ph_emp_educ_3_sex__age__log_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_intangible_d15 + asset_innovation_d15 | cluster + year | (quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha) | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
#summary(m_2sls_income_ph_educ_3)

# educ == 4 (4-year university)
#df %>% filter(is.finite(income_ph_emp_educ_1_sex__age__log_d15), is.finite(income_ph_emp_educ_2_sex__age__log_d15), is.finite(income_ph_emp_educ_3_sex__age__log_d15), is.finite(income_ph_emp_educ_4_sex__age__log_d15), is.finite(weight_pop_educ_4_sex__age__log_l15), is.finite(weight_pop_educ__sex_2_age__log_l15), is.finite(weight_pop_educ__sex__age_1_log_l15), is.finite(weight_pop_educ__sex__age_3_log_l15), is.finite(weight_pop_educ__sex__age_4_log_l15)) -> df_reg 
m_2sls_income_ph_educ_4 <- felm(income_ph_emp_educ_4_sex__age__log_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_intangible_d15 + asset_innovation_d15 | cluster + year | (quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha) | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
summary(m_2sls_income_ph_educ_4)

# sex == 2 (female)
#df %>% filter(is.finite(income_ph_emp_educ_1_sex__age__log_d15), is.finite(income_ph_emp_educ_2_sex__age__log_d15), is.finite(income_ph_emp_educ_3_sex__age__log_d15), is.finite(income_ph_emp_educ_4_sex__age__log_d15), is.finite(weight_pop_educ_4_sex__age__log_l15), is.finite(weight_pop_educ__sex_2_age__log_l15), is.finite(weight_pop_educ__sex__age_1_log_l15), is.finite(weight_pop_educ__sex__age_3_log_l15), is.finite(weight_pop_educ__sex__age_4_log_l15)) -> df_reg 
m_2sls_income_ph_sex_2 <- felm(income_ph_emp_educ__sex_2_age__log_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_intangible_d15 + asset_innovation_d15 | cluster + year | (quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha) | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
summary(m_2sls_income_ph_educ_4)

# age <= 35
#df %>% filter(is.finite(income_ph_emp_educ_1_sex__age__log_d15), is.finite(income_ph_emp_educ_2_sex__age__log_d15), is.finite(income_ph_emp_educ_3_sex__age__log_d15), is.finite(income_ph_emp_educ_4_sex__age__log_d15), is.finite(weight_pop_educ_4_sex__age__log_l15), is.finite(weight_pop_educ__sex_2_age__log_l15), is.finite(weight_pop_educ__sex__age_1_log_l15), is.finite(weight_pop_educ__sex__age_3_log_l15), is.finite(weight_pop_educ__sex__age_4_log_l15)) -> df_reg 
m_2sls_income_ph_age_1 <- felm(income_ph_emp_educ__sex__age_1_log_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_intangible_d15 + asset_innovation_d15 | cluster + year | (quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha) | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
summary(m_2sls_income_ph_age_1)

# age >= 50
#df %>% filter(is.finite(income_ph_emp_educ_1_sex__age__log_d15), is.finite(income_ph_emp_educ_2_sex__age__log_d15), is.finite(income_ph_emp_educ_3_sex__age__log_d15), is.finite(income_ph_emp_educ_4_sex__age__log_d15), is.finite(weight_pop_educ_4_sex__age__log_l15), is.finite(weight_pop_educ__sex_2_age__log_l15), is.finite(weight_pop_educ__sex__age_1_log_l15), is.finite(weight_pop_educ__sex__age_3_log_l15), is.finite(weight_pop_educ__sex__age_4_log_l15)) -> df_reg 
m_2sls_income_ph_age_3 <- felm(income_ph_emp_educ__sex__age_3_log_d15 ~ weight_pop_educ_4_sex__age__log_l15 + weight_pop_educ__sex_2_age__log_l15 + weight_pop_educ__sex__age_1_log_l15 + weight_pop_educ__sex__age_3_log_l15 + weight_pop_educ__sex__age_4_log_l15 + va_soba_d15 + import_total_d15 + asset_IT_d15 + asset_intangible_d15 + asset_innovation_d15 | cluster + year | (quantity_stock_12_log_alpha_level_d15 ~ unitval_t_indagg_stock_12_log_alpha) | 0 | cluster, data = df_reg, weights = df_reg$weight_pop_educ__sex__age__l15)
summary(m_2sls_income_ph_age_3)

# added_line_educ_wages <- list(
#   c('CZ FE', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
#   c('Year FE', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
#   c('Demographic Controls', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
#   c('Globalization Controls', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
#   c('Technology Controls', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
#   c('Group', 'Middle School Grad.', 'High School Grad.', 'Technical College Grad.', '4-year Univ. Grad.')
# )
# output_slides(
#   m_list = list(m_2sls_income_ph_educ_1, m_2sls_income_ph_educ_2, m_2sls_income_ph_educ_3, m_2sls_income_ph_educ_4), 
#   title = 'CZ-level, 2SLS, Wage Effects By Education Level', 
#   filename = '2sls_wage_educ_cb', 
#   dep_var_label = c('$\\Delta \\ln(w^{MS}_{ct})$', '$\\Delta \\ln(w^{HS}_{ct})$', '$\\Delta \\ln(w^{TC}_{ct})$', '$\\Delta \\ln(w^{4U}_{ct})$'), 
#   cov_label = '$\\Delta R_{ct}$',
#   keep_var = 'quantity_stock_12_log_alpha',
#   added_line = added_line_educ_wages
# )

# added_line_educ_wages <- list(
#   c('CZ FE', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
#   c('Year FE', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
#   c('Demographic Controls', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
#   c('Globalization Controls', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
#   c('Technology Controls', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
#   c('Group', 'High School', '4-year Univ.', 'Female', 'Age \\leq 35', 'Age \\geq 50') 
# )
added_line_educ_wages <- list(
  c('Group', 'High School', '4-year Univ.', 'Female', 'Age $\\leq$ 35', 'Age $\\geq$ 50') 
)
output_slides(
  m_list = list(m_2sls_income_ph_educ_2, m_2sls_income_ph_educ_4, m_2sls_income_ph_sex_2, m_2sls_income_ph_age_1, m_2sls_income_ph_age_3), 
  title = 'CZ-level Regression of Wages By Demographic Group', 
  filename = 'tab_8_panel_C', 
  dep_var_label = c('$\\Delta \\ln(w^{HS}_{ct})$', '$\\Delta \\ln(w^{CG}_{ct})$', '$\\Delta \\ln(w^{Fem}_{ct})$', '$\\Delta \\ln(w^{U35}_{ct})$', '$\\Delta \\ln(w^{O50}_{ct})$'),
  cov_label = '$\\Delta R_{ct}$',
  keep_var = 'quantity_stock_12_log_alpha',
  added_line = added_line_educ_wages
)
