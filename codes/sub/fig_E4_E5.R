# this file deals with by-application-by-type table. for data creation, see `analysis_v3/plot_by-application-by-type.R` (from this file, otherwise, this code inherits a lot.)

# housekeeping ------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(readxl)
library(zoo)


# read --------------------------------------------------------------------


df <- read_csv('data/by-application-by-type_1982-1991_labelled.csv')

# drop manual manipulator since it is not 'automatically controlled' as in the definition of ISO, and the trade value is small.
df %>% filter(code_union_type != "A") -> df

# calculate shares
df <- 
  df %>% 
  ungroup %>% group_by(code_union_app, year) %>% 
  mutate(share_type = sales.m/sum(sales.m))

# factorize and order the types. somehow ungrouping at this timing is important. https://www.r-graph-gallery.com/267-reorder-a-variable-in-ggplot2.html
df %>% mutate(type_name_eng = fct_reorder(type_name_eng, code_union_type)) %>%
  ungroup %>% mutate(name_union_short = fct_reorder(name_union_short, code_union_app)) -> df


# Figure E4 ---------------------------------------------------------------



p <- ggplot(df, aes(x = year, y = share_type, fill = type_name_eng)) +
  geom_area() +
  facet_wrap(~ name_union_short) +
  ylab('Type expenditure share') +
  theme_classic() +
  guides(fill = guide_legend(title = 'Type')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # add if size <= 3. see https://stackoverflow.com/questions/15838533/rotating-axis-labels-in-date-format
plot(p)
size <- 3
file <- 'output_final/fig_E4.png'
ggsave(file, p, width = 2*size, height = 1.5*size)


# Figure E5 ---------------------------------------------------------------



df %>% ungroup %>% group_by(code_union_type, type_name_eng, year) %>% summarize(quantity = sum(quantity), sales.m = sum(sales.m)) %>% mutate(unitvalue = sales.m/quantity) -> df_agg

# moving average to smooth
#df_agg %>% ungroup %>% group_by(code_union_type) %>% mutate(sales_ma3 = rollapply(sales.m, 3, sum, fill = NA), quantity_ma3 = rollapply(quantity, 3, sum, fill = NA)) %>% mutate(uv_ma3 = sales_ma3/quantity_ma3) -> df_agg


p <- ggplot(df_agg, aes(x = year, y = unitvalue, color = type_name_eng, linetype = type_name_eng)) +
  geom_line() +
  ylab('Unit value (Million JPY)') +
  #scale_y_log10() +
  theme_classic() +
  guides(color = guide_legend(title = 'Type'), linetype = guide_legend(title = 'Type')) 
#plot(p)
size <- 3
file <- 'output_final/fig_E5.png'
ggsave(file, p, width = 2*size, height = 1.5*size)
