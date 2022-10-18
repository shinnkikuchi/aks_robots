# check the correlates of application shares and initial period characteristics, in the spirit of to Table 2 of GPSS

# housekeeping ------------------------------------------------------------

rm(list=ls())
library(tidyverse)
library(data.table)
library(lfe)
library(stargazer)
library(readstata13)

# read --------------------------------------------------------------------

# initial application shares
alpha <- fread("data/alpha.csv")

# make wide
alpha_wide <- dcast(alpha, code_union_ess ~ code_union_app, value.var = "alpha")

# fill NA with zeros
alpha_wide[is.na(alpha_wide)] <- 0

# initial shares
dt_full <- fread("data/JARA_ESS_it_by_a.csv")
dt_init <- dt_full[year==1982,.(code_union_ess, name_ind_short, weight_fem_share, weight_cg_share, weight_u35_share, weight_o50_share, sales.m_value_5_avg)]

# match
dt <- alpha_wide[dt_init, on = "code_union_ess"]


# regress -----------------------------------------------------------------

a <- 160
reg <- function(a){
  fmla <- formula(paste0("`", a, "` ~ weight_fem_share + weight_cg_share + weight_u35_share + weight_o50_share"))
  #fmla <- formula(paste0("`", a, "` ~ weight_fem_share + weight_cg_share + weight_u35_share + weight_o50_share + weight_fem_share*weight_cg_share + weight_fem_share*weight_o50_share"))
  m <- lm(fmla, data = dt, weights = dt$sales.m_value_5_avg)
  #summary(m)
  
}

a_list <- unique(alpha, by="code_union_app")[,code_union_app]
m_list <- a_list %>% map(reg)

# take name of applications
name_app <- fread("data/JARA_ait.csv")
name_app <- unique(name_app, by = "name_app_short")[,name_app]
# even shorter
name_app[1] <- "Tending"
name_app[2] <- "Welding"
name_app[5] <- "Assembling"

# stargazer(
#   m_list,
#   dep.var.caption = "Application share $s_{ai}$",
#   dep.var.labels = name_app,
#   covariate.labels = c("Female", "College", "Age $\\leq$ 34", "Age $\\geq$ 50"),
#   keep = c("weight_fem_share", "weight_cg_share", "weight_u35_share", "weight_o50_share"),
#   omit.stat = c('adj.rsq', 'ser', 'f'),
#   notes.label = '',
#   omit.table.layout = "n",
#   title = "Application Shares and Initial Industrial Characteristics",
#   table.placement = '!t',
#   label = "initial_share_correlates",
#   out = 'output_final/initial_share_correlates_slide.tex'
# )


# correlation with pre-trend -----------------------------------------------

df_ess <- fread('data/ESS_cit_d.csv')

# take only the first two periods
df_ess <- df_ess %>% 
  filter(year <= 1982)

# aggregate to industry-year level
df_ess <- df_ess %>% 
  group_by(code_union_ess, year) %>% 
  summarize(emp = sum(weight))

# compute growths
df_ess <- df_ess %>% 
  spread(key = year, value = emp) %>% 
  mutate(g_emp_79_82 = (`1982`-`1979`)/`1979`)

# match with alpha and regress
dt <- dt %>% left_join(df_ess, by = "code_union_ess")



# regress 

dt <- dt[, c("g_emp_79_82_110", "app_share") := .(g_emp_79_82, `110`)]
m_110 <- lm(g_emp_79_82_110 ~ app_share + weight_fem_share + weight_cg_share + weight_u35_share + weight_o50_share + weight_fem_share*weight_cg_share, data = dt, weights = dt$sales.m_value_5_avg)
dt <- dt[, c("g_emp_79_82_160", "app_share") := .(g_emp_79_82, `160`)]
m_160 <- lm(g_emp_79_82_160 ~ app_share + weight_fem_share + weight_cg_share + weight_u35_share + weight_o50_share + weight_fem_share*weight_cg_share, data = dt, weights = dt$sales.m_value_5_avg)
dt <- dt[, c("g_emp_79_82_170", "app_share") := .(g_emp_79_82, `170`)]
m_170 <- lm(g_emp_79_82_170 ~ app_share + weight_fem_share + weight_cg_share + weight_u35_share + weight_o50_share + weight_fem_share*weight_cg_share, data = dt, weights = dt$sales.m_value_5_avg)
dt <- dt[, c("g_emp_79_82_190", "app_share") := .(g_emp_79_82, `190`)]
m_190 <- lm(g_emp_79_82_190 ~ app_share + weight_fem_share + weight_cg_share + weight_u35_share + weight_o50_share + weight_fem_share*weight_cg_share, data = dt, weights = dt$sales.m_value_5_avg)
dt <- dt[, c("g_emp_79_82_200", "app_share") := .(g_emp_79_82, `200`)]
m_200 <- lm(g_emp_79_82_200 ~ app_share + weight_fem_share + weight_cg_share + weight_u35_share + weight_o50_share + weight_fem_share*weight_cg_share, data = dt, weights = dt$sales.m_value_5_avg)
dt <- dt[, c("g_emp_79_82_900", "app_share") := .(g_emp_79_82, `900`)]
m_900 <- lm(g_emp_79_82_900 ~ app_share + weight_fem_share + weight_cg_share + weight_u35_share + weight_o50_share + weight_fem_share*weight_cg_share, data = dt, weights = dt$sales.m_value_5_avg)


stargazer(
  m_110, m_160, m_170, m_190, m_200, m_900,
  dep.var.caption = "Employment Growth",
  dep.var.labels = name_app,
  keep = "app_share",
  covariate.labels = "Application Share",
  omit.stat = c('adj.rsq', 'ser', 'f'),
  notes.label = '',
  omit.table.layout = "n",
  title = "Application Shares and 1979-1982 Employment Growth",
  table.placement = '!t',
  label = "initial_share_correlates_pretrend",
  out = 'output_final/tab_F5.tex'
)
