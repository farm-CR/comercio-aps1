library(tidyverse)
library(haven)
library(sjlabelled)

  itpd <- read_dta("dados/itpd.dta")

## A) ----
itpd.a <- itpd %>% 
  filter(importer_iso3 != exporter_iso3) %>% 
  group_by(exporter_iso3, year, industry_id) %>% 
  summarise(total_industria = sum(trade / 1000))

## B) ----
itpd.b <- itpd.a %>% 
  left_join(itpd.a %>%
              group_by(exporter_iso3, year) %>% 
              summarise(total_trade = sum(total_industria)),
            by = c("exporter_iso3", "year")) %>% 
  mutate(vantagem_absoluta = total_industria / total_trade)

## C) ----
itpd.c <- itpd.b %>% 
  left_join(itpd.b %>%
              group_by(year) %>% 
              summarise(total_trade_ano = sum(total_industria)),
            by = c("year")) %>% 
  left_join(itpd.b %>%
              group_by(industry_id, year) %>% 
              summarise(total_trade_industria = sum(total_industria)),
            by = c("industry_id", "year")) %>% 
  mutate(RCA = vantagem_absoluta / (total_trade_industria / total_trade_ano)) %>% 
  select(exporter_iso3, year, industry_id, RCA)

## Salvar base
itpd.c %>% 
  left_join(get_labels(itpd.c$industry_id) %>% 
              as_tibble() %>% 
              mutate(id = seq(1, get_labels(itpd.c$industry_id) %>% length(), 1)),
            by = c("industry_id" = "id")) %>% 
  select(exporter_iso3, year, RCA, industry_name = value) %>% 
  write.csv(., file = "relatorios/tabela_ex3.csv")

