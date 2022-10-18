# generate price data

# housekeeping ------------------------------------------------------------

rm(list = ls())

library(data.table)
library(tidyverse)
library(readxl)

# read --------------------------------------------------------------------

dt <- fread("data/bojprice/raw/revision/nme_R031.24549.20210330072035.02_utf.csv", skip = 1)

# header
v_head <- colnames(dt)
v_head[1] <- "year"
colnames(dt) <- v_head
dt


# match consistent industry code ------------------------------------------

conc <- read_excel("data/industry_codes_within_jara_v2_to_essCompatibleJARA2002.xlsx", sheet = "boj_to_compatible")
conc <- data.table(conc)

dt_long <- melt(dt, id.vars = v_head[1], measure.vars = v_head[2:length(v_head)], variable.name = "indjpn", value.name = "price")

colnames(conc)[2] <- "indjpn"
colnames(conc)
dt_long <- conc[dt_long, on = "indjpn"]


# simple plot -------------------------------------------------------------

p <- ggplot(dt_long, aes(x=year, y=price, color=name_short, linetype = name_short)) + 
  geom_line() +
  labs(x="",y="Output Price Index (2005=100)",color="Industry",linetype="Industry") +
  theme_classic()
#plot(p)
size <- 2
file <- paste0("output_final/fig_E8.png")
ggsave(file, p, width=3*size, height=2*size)

