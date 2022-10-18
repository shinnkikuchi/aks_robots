# this file plots 5-year average data from `generate_5-year-averages.R`
# inherits from `plot_jara_1978_2017_app.R` and `plot_jara_1978_2000_type.R`

# housekeeping ------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(readxl)
library(scales)

options(scipen=10000)

# read --------------------------------------------------------------------

# need to guess correctly the column types
df <- read_csv('data/by-industry_by-application_1978-2017_labelled_prices_5year.csv')
nrow <- nrow(df)
df <- read_csv('data/by-industry_by-application_1978-2017_labelled_prices_5year.csv', guess_max = nrow)


# order of the factor should be fixed--this time at 2017 quantity of robots (total, because total value is at the bottom any way.)
# 2017 data is taken from plot_jara_1978_2017
df2017 <- read_csv('data/by-industry_by-application_1978-2017_labelled_prices.csv')
nrow2017 <- nrow(df2017)
df2017 <- read_csv('data/by-industry_by-application_1978-2017_labelled_prices.csv', guess_max = nrow2017) %>% 
  filter(year == 2017, app_code_name_eng == 'TOTAL') %>% 
  select(code_union, quantity_t) %>% 
  rename(quantity_t_2017 = quantity_t)
df <- 
  df %>% 
  ungroup %>% 
  left_join(df2017, by = 'code_union') %>% 
  mutate(ind_union_eng = fct_reorder(ind_union_eng, quantity_t_2017, tail, n = 1, .desc = TRUE)) # https://stackoverflow.com/questions/54536094/ggplot-order-legend-using-last-values-on-x-axis

# prepare many numbers of linetypeshttps://stackoverflow.com/questions/34711942/shapes-and-linetypes-in-ggplot
linetypes = c("solid", 
              apply(expand.grid(c(2,4), c(1,2,4,8,"A")), 1, paste, collapse=""), 
              apply(expand.grid(c(2,4,8), c(2,4), c(5,"F"), 2), 1, paste, collapse=""),
              "4284B4F4", "228F61A4")


# plot industry -----------------------------------------------------------


df %>% filter(app_code_name_eng == 'TOTAL') %>% 
  mutate(destination = ifelse(code_union != 'export', 'JPN', 'export')) %>% 
  ungroup() %>% group_by(destination, year_5) %>% 
  summarize(sales.m_sum = sum(sales.m_sum)) %>% 
  ungroup() %>% group_by(year_5) %>% 
  mutate(
    share = sales.m_sum/sum(sales.m_sum),
    sales.t_sum = sales.m_sum/1000000
  ) -> df_share_exp

# level
df_share_exp %>% mutate(destination_fct = factor(destination)) -> df_share_exp
p <- ggplot(df_share_exp, aes(x = year_5, y = sales.t_sum, fill = destination_fct, alpha = destination_fct)) +
  geom_area() +
  scale_x_continuous(breaks = seq(1982, 2017, by = 5)) +
  scale_y_continuous(limits = c(0,3.5)) +
  scale_linetype_manual(values = linetypes) +
  xlab('') + ylab('total sales (trillion current JPY)') +
  theme_classic() +
  scale_alpha_manual(values=c(0.5,1)) +
  guides(fill = guide_legend(title = 'destination'), alpha = guide_legend(title = 'destination')) 
print(p)
size <- 1
file <- paste0('output_final/fig_E6.png')
ggsave(file, p, width = 4*size, height = 3*size)


# within-domestic adoption, aggregate small industries into one
df %>% filter(app_code_name_eng == 'TOTAL') %>% 
  filter(code_union != 'export') %>% 
  ungroup() %>% group_by(year_5) %>% 
  mutate(
    share = sales.m_sum/sum(sales.m_sum),
    sales.t_sum = sales.m_sum/1000000,
    ind_union_eng_fct = factor(ind_union_eng)
  ) -> df_share

df_share %>% mutate(ind_union_eng_3 = ifelse(code_union %in% c('5', '7-3'), as.character(ind_union_eng), 'Others')) -> df_share
df_share %>% ungroup %>% group_by(ind_union_eng_3, year_5) %>% summarize(sales.t_sum = sum(sales.t_sum)) -> df_share_3
df_share_3 %>% ungroup %>% mutate(
  ind_union_eng_3 = ifelse(ind_union_eng_3 == 'Electric machine', 'Electric', ind_union_eng_3),
  ind_union_eng_3 = ifelse(ind_union_eng_3 == 'Automobile', 'Transportation', ind_union_eng_3),
) -> df_share_3
df_share_3$ind_union_eng_3 <- factor(df_share_3$ind_union_eng_3, levels = c('Electric', 'Transportation', 'Others'))

# Combine this info with the application share table
df_share_3 <- df_share_3 %>% mutate(sales.m_sum = sales.t_sum * 1000)
write_csv(df_share_3, "output_final/tab_2_aggregate.csv")
