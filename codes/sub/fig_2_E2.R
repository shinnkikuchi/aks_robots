# this file generates the raw trends between Japan and other countries
# inherited from `Dropbox/projects/Kawaguchi_Saito/ロボット工業会/codes/raw_trends_international_comparison.R`


# housekeeping ------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(readxl)

lagyear_jara <- 12

# read --------------------------------------------------------------------

# read JARA
df_jara <- read_csv('data/jara/generated.data/by-industry_by-application_1978-2017.csv')

# read IFR
df_ifr <- read_excel('data/ifr/industry_000 - All Industries.xlsx', skip = 2)



# clean up JARA -----------------------------------------------------------

# drop exports from JARA
df_jara %>% filter(code_union != 'export') -> df_jara

# aggregate all applications by quantity
df_jara %>% filter(variable == 'quantity') %>% group_by(year) %>% summarize(value = sum(value)) -> df_jara_agg

# generate operational stock by immediate withdrawal method. https://stackoverflow.com/questions/47463429/r-group-lag-sum
df_jara_agg %>% ungroup %>% mutate(stock = reduce(map(0:8, ~ lag(value, ., 0)), `+`)) -> df_jara_agg


# clean up IFR ------------------------------------------------------------

# rename column names
colnames(df_ifr) <- c('year', 'junk', colnames(df_ifr)[3:length(colnames(df_ifr))])

# drop unnecessary row and make values numeric (*)
df_ifr %>% filter(!is.na(year)) %>% mutate_all(as.numeric) -> df_ifr

# select countries
countries <- c('WR-WORLD', 'US-United States (North America)', 'CA-Canada Until 2010 included in US (North America)', 'MX-Mexico Until 2010 included in US (North America)', 'CN-China', 'JP-Japan', 'KR-Rep. of Korea', 'EU-EUROPE', 'DE-Germany')
df_ifr %>% select(year, countries) -> df_ifr_selected

# tidy
df_ifr_selected %>% mutate(
  `WR-WORLD` = `WR-WORLD` - `US-United States (North America)` - `CA-Canada Until 2010 included in US (North America)` - `MX-Mexico Until 2010 included in US (North America)` - `CN-China` - `JP-Japan` - `KR-Rep. of Korea` - `EU-EUROPE`,
  `US-United States (North America)` = `US-United States (North America)` - `CA-Canada Until 2010 included in US (North America)` - `MX-Mexico Until 2010 included in US (North America)`,
  `EU-EUROPE` = `EU-EUROPE` - `DE-Germany`
) %>% select(-`CA-Canada Until 2010 included in US (North America)`, -`MX-Mexico Until 2010 included in US (North America)`) -> df_ifr_selected
colnames(df_ifr_selected) <- c('year', 'ROW', 'USA', 'CHN', 'JPN_IFR', 'KOR', 'EUR, net of DEU', 'DEU')



# normalize by employment size, employment data ---------------------------



# read OECD
df_oecd <- read_csv('data/oecd/ALFS_SUMTAB_23042021012546383.csv')
df_oecd %>% select(LOCATION) %>% unique %>% unlist %>% unname
df_oecd %>% select(TIME) %>% unique %>% unlist %>% unname

# follow acemoglu-restrepo countries, to compute the size of employment
set_AR <- c("JPN", "DEU", "USA", "DNK", "FIN", "FRA", "ITA", "SWE", "NOR", "ESP", "GBR", "KOR")
df_oecd_selected_AR <- df_oecd %>% filter(Subject == "Employment", LOCATION %in% set_AR) %>% select(LOCATION, TIME, Value)

# aggregate to AR groups of countries
df_oecd_selected_AR <- df_oecd_selected_AR %>% 
  mutate(country_group = ifelse(LOCATION == "JPN", "Japan",
                                ifelse(LOCATION == "DEU", "Germany",
                                       ifelse(LOCATION %in% c("DNK", "FIN", "FRA", "ITA", "SWE"), "DFFIS",
                                              ifelse(LOCATION == "USA", "United States",
                                                     ifelse(LOCATION %in% c("NOR", "ESP", "GBR"), "Norway, Spain, and UK", "South Korea")))))) %>% 
  ungroup %>% group_by(country_group, TIME) %>% 
  summarize(emp_t = sum(Value))

# add china employment statistics: National Bureau of Statistics of China, http://www.stats.gov.cn/tjsj/ndsj/
df_china <- read_excel("data/oecd/china/Employment.xls", range = "A1:B69")
df_china <- df_china %>% mutate(country_group = "China", TIME = as.numeric(str_sub(year, 1, 4)), emp_t = `Unit: 10000`*10) %>% select(country_group, TIME, emp_t)
df_oecd_selected_AR <- df_oecd_selected_AR %>% bind_rows(df_china)


# normalize by employment size, robot data --------------------------------

# need to take the same set of countries from IFR too
df_ifr_selected_AR <- df_ifr %>% 
  select(year, `JP-Japan`, `DE-Germany`, `US-United States (North America)`, `DK-Denmark`, `FI-Finland`, `FR-France`, `IT-Italy`, `SE-Sweden`, `NO-Norway`, `ES-Spain`, `UK-United Kingdom`, `KR-Rep. of Korea`, `CN-China`)
colnames(df_ifr_selected_AR) <- c("year", set_AR, "CHN")

# aggregate to AR groups of countries
df_ifr_selected_AR <- df_ifr_selected_AR %>% 
  mutate(
    DFFIS = DNK + FIN + FRA + ITA + SWE,
    `Norway, Spain, and UK` = NOR + ESP + GBR
  ) %>% 
  rename(Japan = JPN, Germany = DEU, `United States` = USA, `South Korea` = KOR, China = CHN) %>% 
  select(-DNK, -FIN, -FRA, -ITA, -SWE, -NOR, -ESP, -GBR)

# match JARA and IFR
df_jara_agg %>% left_join(df_ifr_selected_AR, by = 'year') -> df_gather_AR

# tidy
df_gather_AR %>% select(-value) %>% rename(Japan_JARA = stock) -> df_gather_AR

# long format for plot
df_gather_AR %>% gather(country_group, value, -year) -> df_gather_AR

# grouping for coloring. see https://stackoverflow.com/questions/53131232/contain-condition-in-r for grepl.
df_gather_AR %>% mutate(source = ifelse(country_group == 'Japan_JARA', 'JARA', 'IFR')) -> df_gather_AR
df_gather_AR %>% mutate(country_group = ifelse(grepl("Japan", country_group), 'Japan', country_group)) -> df_gather_AR


# normalize by employment size, match and plot ----------------------------
df_gather_AR_emp <- df_gather_AR %>% left_join(df_oecd_selected_AR, by = c("country_group", "year" = "TIME"))

df_gather_AR_emp <- df_gather_AR_emp %>% 
  mutate(robot_per_emp_t = value/emp_t)

# combine country group and source
df_gather_AR_emp <- df_gather_AR_emp %>% 
  mutate(country_group_source = ifelse(country_group == "Japan", paste0(country_group, " (", source, " data)"), country_group))

# order the factor
df_gather_AR_emp$country_group_source <- factor(df_gather_AR_emp$country_group_source, levels = c("Japan (JARA data)", "Japan (IFR data)", 'Germany', 'DFFIS', 'United States', 'Norway, Spain, and UK', 'South Korea', 'China'))


# Figure 2 ----------------------------------------------------------------


leg_title <- "Region"
p <- ggplot(df_gather_AR_emp, aes(x = year, y = robot_per_emp_t, color = country_group_source, linetype = country_group_source, shape = country_group_source)) +
  geom_line() + 
  geom_point() +
  xlab('') + ylab('Robot Stock per Thousand Workers') +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  #scale_shape_manual(values=1:nlevels(df_gather_AR_emp$country_group_source)) +
  #labs(color = 'Region', linetype = 'Region', shape = 'Region')
  scale_color_manual(leg_title, values=1+c(rep(1,2), 2:nlevels(as.factor(df_gather_AR_emp$country_group)))) +
  scale_linetype_manual(leg_title, values=c(1,rep(2,nlevels(as.factor(df_gather_AR_emp$country_group))))) +
  scale_shape_manual(leg_title, values = c(16, 1:7))
print(p)
size <- 5
file <- 'output_final/fig_2.png'
ggsave(file, p, width = 1.25*size, height = 0.75*size)


# Figure E2 ---------------------------------------------------------------


p <- ggplot(df_gather_AR_emp, aes(x = year, y = value/1000, color = country_group_source, linetype = country_group_source, shape = country_group_source)) +
  geom_line() + 
  geom_point() +
  xlab('') + ylab('Robot Stock (Thousand Units)') +
  theme_classic() +
  theme(legend.position = "bottom") +
  #scale_shape_manual(values=1:nlevels(df_gather_AR_emp$country_group_source)) +
  #labs(color = 'Region', linetype = 'Region', shape = 'Region')
  scale_color_manual(leg_title, values=1+c(rep(1,2), 2:nlevels(as.factor(df_gather_AR_emp$country_group)))) +
  scale_linetype_manual(leg_title, values=c(1,rep(2,nlevels(as.factor(df_gather_AR_emp$country_group))))) +
  scale_shape_manual(leg_title, values=c(rep(1,2), 2:nlevels(as.factor(df_gather_AR_emp$country_group)))) 
plot(p)
size <- 3
file <- 'output_final/fig_E2.png'
ggsave(file, p, width = 2.5*size, height = 1.5*size)
