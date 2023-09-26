library(tidyverse)
library(haven)
library(sjlabelled)
library(readxl)

## A) ----
itpd <- read_dta("dados/itpd.dta")

itpd.a <- itpd %>% 
  filter(year == 2016, exporter_iso3 != importer_iso3) %>% 
  mutate(trade = trade / 1000) %>% 
  select(exporter_iso3, importer_iso3, industry_id, broad_sector, trade)

## B) e C) ----
WDICountry <- read_excel("dados/WDICountry.xlsx", sheet = "List of economies")

itpd.b <- itpd.a %>%
  group_by(exporter_iso3, industry_id, broad_sector) %>% 
  summarise(X_ij = sum(trade)) %>% 
  left_join(itpd.a %>% 
              group_by(importer_iso3, industry_id, broad_sector) %>% 
              summarise(M_ij = sum(trade)),
            by = c("exporter_iso3" = "importer_iso3", "industry_id", "broad_sector")) %>% 
  left_join(WDICountry %>% select(Code, income_group = `Income group`),
            by = c("exporter_iso3" = "Code")) %>% 
  mutate(GL_ij = 1 - abs(X_ij - M_ij) / (X_ij + M_ij))
  
## D) e E) ----
itpd.d <- itpd.b %>% 
  mutate(setor = ifelse(broad_sector == 4, "servicos", "nao-servicos"),
         renda = ifelse(income_group == "High income", "renda alta", "renda nao-alta")) %>% 
  select(exporter_iso3, industry_id, GL = GL_ij, setor, renda)

## F) ----
itpd.d %>% 
  filter(!is.na(renda)) %>% 
  ggplot() +
  geom_density(aes(GL)) +
  facet_wrap(~ setor + renda)


