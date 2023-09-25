library(tidyverse)
library(haven)
library(sjlabelled)
library(readxl)

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

## E) ----
df %>% 
  filter(!is.na(human_capital_index), !is.na(RCA), year == 2016) %>% 
  ggplot(aes(human_capital_index, log(RCA))) +
  # geom_point() +
  stat_summary_bin(fun = "mean", bins = 500, geom = "point") +
  facet_wrap(~ sector_name)


  