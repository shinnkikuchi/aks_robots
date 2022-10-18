# this file imports 1974-2000 data and generates stock of robots


# housekeeping ------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(readxl)
library(lfe)

# read --------------------------------------------------------------------

df <- read_csv('data/by-industry_by-type_1974-2000_noGHI_noNonManuf.csv')

# preliminary plots
df %>% ungroup %>% group_by(year, variable) %>% summarize(value = sum(value)) %>% mutate(variable = as.factor(variable), value = value/1000) -> df_agg
levels(df_agg$variable) <- c('quantity (thousand units)', 'sales (current billion JPY)') # https://stackoverflow.com/questions/3472980/how-to-change-facet-labels
p <- ggplot(df_agg, aes(x = year, y = value)) + geom_line() + facet_wrap(~ variable, scale = 'free') + geom_vline(xintercept = 1978, color = 'red', linetype = 'dashed') + 
  theme_classic() + 
  labs(caption = 'Note: Red lines indicate year 1978, the baseline initial year of our data.')
#plot(p)
size <- 2
file <- 'output_final/fig_E3.png'
ggsave(file, p, width = 4*size, height = 3*size)