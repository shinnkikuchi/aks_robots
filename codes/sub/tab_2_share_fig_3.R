# this file plots JARA, ait-level data


# housekeeping ------------------------------------------------------------

rm(list = ls())

library(tidyverse)


# read and prep -----------------------------------------------------------

df <- read_csv('data/JARA_ait.csv')

# factor order of application
df <- df %>% mutate(name_app_short = ifelse(name_app_short == "Material handling", "Tending", name_app_short))
df$name_app_short <- factor(df$name_app_short, levels = c('Tending', 'Welding', 'Dispensing', 'Processing', 'Assembling', 'Others'))

# prep consistent color palette: 
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
palette <- df$name_app_short %>% unique %>% length %>% gg_color_hue()

# aggregate by 3 industries ----------------------------------------------

# drop export
df %>% filter(name_ind_short != '(export)') -> df_agg

# aggregate to 3 industries
df_agg %>% mutate(name_ind_short = ifelse(code_union_ess %in% c('5', '7'), as.character(name_ind_short), 'Others')) -> df_agg
df_agg %>% ungroup %>% group_by(name_ind_short, name_app_short, year) %>% summarize_at(vars('sales.m_value_5_avg', 'quantity_value_5_avg'), sum) -> df_agg

# rename
df_agg %>% ungroup %>% mutate(
  name_ind_short = ifelse(name_ind_short == 'Transport machine', 'Transportation', name_ind_short),
  name_ind_short = ifelse(name_ind_short == 'Electric machine', 'Electric', name_ind_short)
) -> df_agg

# factor order of industries
df_agg$name_ind_short <- factor(df_agg$name_ind_short, levels = c('Electric', 'Transportation', 'Others'))


# aggregate completely for unit values ------------------------------------

# aggregate
df %>% ungroup %>% group_by(name_app_short, year) %>% summarize_at(vars('sales.m_value_5_avg', 'quantity_value_5_avg'), sum) -> df_agg_uv

# unit values
df_agg_uv %>% mutate(unitval_m_value_5_avg = sales.m_value_5_avg/quantity_value_5_avg) -> df_agg_uv

# aggregate also at the efficiency-adjusted prices
df %>% ungroup %>% group_by(name_app_short, year) %>% summarize_at(vars('sales.m_stock_12', starts_with('quantity_stock_12')), sum) -> df_agg_uv_ea
df_agg_uv_ea %>% mutate_at(vars(starts_with('quantity_stock_12')), funs(unitval = sales.m_stock_12/.)) -> df_agg_uv_ea
df_agg_uv %>% left_join(df_agg_uv_ea, by = c('name_app_short', 'year')) -> df_agg_uv

# plot unit values, for slides -------------------------------------------

outvar <- 'quantity_stock_12_unitval'
#outvar <- 'quantity_stock_12_ea_unitval'
#outvar <- 'unitval_m_value_5_avg'

p <- ggplot(df_agg_uv, aes(x = year, y = get(outvar), color = name_app_short, linetype = name_app_short)) + 
  geom_line() +
  scale_x_continuous(breaks = seq(1982, 2017, by = 5)) +
  scale_y_log10() +
  xlab('') + ylab('average unit value (mil. JPY)') +
  theme_classic() +
  guides(color = guide_legend(title = 'application'), linetype = guide_legend(title = 'application'))
#scale_color_brewer(palette = 'Dark2') # perhaps better for pretty bright room.
#plot(p)
size <- 2
file <- paste0('output_final/fig_3.png')
ggsave(file, p, width = 2.25*size, height = 1.5*size)



# expenditure shares ------------------------------------------------------

# calculate exp. shares
df_agg %>% ungroup %>% group_by(name_ind_short, year) %>% mutate(exp_share_5_avg = sales.m_value_5_avg/sum(sales.m_value_5_avg)) -> df_agg

# output using table
df_agg_out <- df_agg %>% 
  mutate(exp_share_5_avg_fmt = round(exp_share_5_avg*1000)/10) %>% 
  select(name_ind_short, name_app_short, year, exp_share_5_avg_fmt) %>% 
  spread(year, exp_share_5_avg_fmt)
write_csv(df_agg_out, "output_final/tab_2_share.csv")
