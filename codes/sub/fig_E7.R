# read comtrade data


# housekeeping ------------------------------------------------------------

rm(list = ls())

library(tidyverse)

jaradir <- '/Users/daisukeadachi/Dropbox/projects/Kawaguchi_Saito/ロボット工業会/generated.data/'

# read --------------------------------------------------------------------

df <- read_csv('data/comtrade_847950.csv')

df %>% filter(`Trade Flow Code` == 1) %>% select(Year, `Trade Value (US$)`) -> df_imp


# read domestic data and compare ------------------------------------------

# jara in aggregate, in current million jpy
#df_jara <- read_csv(paste0(jaradir, 'by-industry_by-application_1978-2017.csv'))
df_jara <- read_csv('data/by-industry_by-application_1978-2017.csv')
# take domestic absorption
df_jara %>% filter(code_union != 'export') -> df_jara
# aggregate
df_jara %>% filter(variable == 'sales.m') %>% ungroup %>% group_by(year) %>% summarize(value_my = sum(value)) -> df_jara

# exchange rate
exrate <- read_csv('data/exrate/AEXJPUS.csv')
exrate$DATE <- as.integer(substr(exrate$DATE, 1, 4))

# convert to USD
df_jara %>% left_join(exrate, by = c('year' = 'DATE')) %>% mutate(value_md = value_my/AEXJPUS) -> df_jara

# match COMTRADE and JARA and calculate the domestic shares
df_jara %>% left_join(df_imp, by = c('year' = 'Year')) %>% mutate(value_md_imp = `Trade Value (US$)`/1000000, share = value_md/(value_md + value_md_imp)) -> df_matched


# long format for figures
df_matched %>% select(year, value_md, value_md_imp) %>% gather(source, value, -year) %>% mutate(source = ifelse(source == 'value_md', 'Domestic Shipment', 'Import')) -> df_matched_long



# render Figure E7 --------------------------------------------------------



scale <- 'log'

plot_scale <- function(scale){
  p <- ggplot(df_matched_long, aes(x = year, y = value, color = source, linetype = source)) + 
    geom_line() +
    xlab('Year') + ylab('Current million USD') +
    theme_bw() +
    theme(legend.position = 'bottom')
  if (scale == 'log') p <- p + scale_y_log10()
  #plot(p)
  size <- 2
  if (scale == 'level') {
    subfigure <- 'a'
  } else if (scale == 'log'){
    subfigure <- 'b'
  }
  file <- paste0('output_final/fig_E7', subfigure, '.png')
  ggsave(file, p, width = 4*size, height = 3*size)       
  
}

c('level', 'log') %>% map(plot_scale)
