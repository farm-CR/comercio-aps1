library(tidyverse)
library(haven)
library(sjlabelled)
library(readxl)
library(ggthemes)

theme_set(theme_bw() +
            theme(text = element_text(family = "serif"),
                  plot.title = element_text(size = 12),
                  axis.text = element_text(size = 10),
                  axis.title = element_text(size = 11),
                  plot.caption = element_text(size = 11, hjust=0),
                  legend.title = element_text(size = 11),
                  legend.text = element_text(size = 10)))

## A) ----
itpd <- read_dta("dados/itpd.dta")

itpd.a <- itpd %>% 
  filter(importer_iso3 != exporter_iso3) %>% 
  group_by(exporter_iso3, year, industry_id, broad_sector) %>% 
  summarise(total_pais_industria = sum(trade / 1000))

itpd.b <- itpd.a %>% 
  left_join(itpd.a %>%
              group_by(exporter_iso3, year) %>% 
              summarise(total_pais = sum(total_pais_industria)),
            by = c("exporter_iso3", "year")) %>% 
  mutate(vantagem_absoluta = total_pais_industria / total_pais)

itpd.c <- itpd.b %>% 
  left_join(itpd.b %>%
              group_by(year) %>% 
              summarise(total_ano = sum(total_pais_industria)),
            by = c("year")) %>% 
  left_join(itpd.b %>%
              group_by(industry_id, year) %>% 
              summarise(total_industria = sum(total_pais_industria)),
            by = c("industry_id", "year")) %>% 
  mutate(RCA = vantagem_absoluta / (total_industria / total_ano)) %>% 
  select(exporter_iso3, year, industry_id, broad_sector, RCA)

## B) ----
pwt1001 <- read_excel("dados/pwt1001.xlsx", sheet = "Data")

pwt1001 <- pwt1001 %>% 
  filter(between(year, 2000, 2016)) %>% 
  select(countrycode, year, human_capital_index = hc, 
         capital_stock_ppp = cn, capital_stock_constant = rnna)

## C) ----
wdi <- read_csv("dados/wdi.csv")

wdi <- wdi %>% 
  filter(indicatorcode == "AG.LND.AGRI.K2") %>% 
  select(countrycode, countryname, v2000:v2016) %>% 
  pivot_longer(cols = starts_with("v"), names_to = "year", values_to = "agricultural_land_area") %>% 
  mutate(year = year %>% str_remove("v") %>% as_numeric())

## D) ----
df <- itpd.c %>%
  left_join(pwt1001, by = c("exporter_iso3" = "countrycode", "year")) %>% 
  left_join(wdi, by = c("exporter_iso3" = "countrycode", "year"))

df <- df %>% 
  left_join(itpd.c$industry_id %>% get_labels() %>% as_tibble() %>% 
              mutate(id = seq(1, itpd.c$industry_id %>% get_labels() %>% length(), 1)) %>% 
              rename(industry_name = value),
            by = c("industry_id" = "id")) %>% 
  left_join(itpd.c$broad_sector %>% get_labels() %>% as_tibble() %>% 
              mutate(id = seq(1, itpd.c$broad_sector %>% get_labels() %>% length(), 1)) %>% 
              rename(sector_name = value),
            by = c("broad_sector" = "id")) %>% 
  select(exporter_iso3, year, industry_name, sector_name, RCA, agricultural_land_area,
         human_capital_index, capital_stock_ppp, capital_stock_constant)

df %>% 
  write.csv(., file = "relatorios/tabela_ex4.csv")

## E) ----
df %>% 
  filter(!is.na(human_capital_index), !is.na(RCA), RCA != 0, year == 2016) %>% 
  ggplot(aes(x = human_capital_index, y = RCA)) +
  scale_y_continuous(trans='log10') +
  stat_summary_bin(fun = "mean", bins = 1000, geom = "point", alpha = 0.75) +
  geom_smooth(method = "lm", color = "#C6151D") +
  facet_wrap(~ sector_name) +
  labs(x = "Estoque de capital humano",
       y = "RCA")
ggsave('plots/rca_hc.png', dpi = 600, height = 10, width = 16, unit = 'cm', bg = 'white')

df %>% 
  filter(!is.na(agricultural_land_area), !is.na(RCA), RCA != 0, year == 2016) %>% 
  ggplot(aes(x = agricultural_land_area, y = RCA)) +
    scale_y_continuous(trans='log10') +
    scale_x_continuous(trans='log10') +
    stat_summary_bin(fun = "mean", bins = 1000, geom = "point", alpha = 0.75) +
    geom_smooth(method = "lm", color = "#C6151D") +
    facet_wrap(~ sector_name) +
    labs(x = "Área de terras agrícolas",
        y = "RCA")
ggsave('plots/rca_land.png', dpi = 600, height = 10, width = 16, unit = 'cm', bg = 'white')

df %>% 
  filter(!is.na(capital_stock_constant), !is.na(RCA), RCA != 0, year == 2016) %>% 
  ggplot(aes(x = capital_stock_constant, y = RCA)) +
    scale_y_continuous(trans='log10') +
    scale_x_continuous(trans='log10') +
    stat_summary_bin(fun = "mean", bins = 1000, geom = "point", alpha = 0.75) +
    geom_smooth(method = "lm", color = "#C6151D") +
    facet_wrap(~ sector_name) +
    labs(x = "Estoque de capital a preços nacionais correntes (milhões 2017US$)",
        y = "RCA")
ggsave('plots/rca_k_constant.png', dpi = 600, height = 10, width = 16, unit = 'cm', bg = 'white')

df %>% 
  filter(!is.na(capital_stock_ppp), !is.na(RCA), RCA != 0, year == 2016) %>% 
  ggplot(aes(x = capital_stock_ppp, y = RCA)) +
    scale_y_continuous(trans='log10') +
    scale_x_continuous(trans='log10') +
    stat_summary_bin(fun = "mean", bins = 1000, geom = "point", alpha = 0.75) +
    geom_smooth(method = "lm", color = "#C6151D") +
    facet_wrap(~ sector_name) +
    labs(x = "Estoque de capital corrigidos pela PPP (mil. 2017US$)",
        y = "RCA")
ggsave('plots/rca_k_ppp.png', dpi = 600, height = 10, width = 16, unit = 'cm', bg = 'white')
