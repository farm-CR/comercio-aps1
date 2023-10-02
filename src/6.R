library(tidyverse)
library(haven)
library(sjlabelled)

## A) ----
itpd <- read_dta("dados/itpd.dta")

eu_countries <- c("BEL", "BGR", "CZE", "DNK", "DEU", "EST", "IRL", "GRC", "ESP",
                  "FRA", "HRV", "ITA", "CYP", "LVA", "LTU", "LUX", "HUN", "MLT",
                  "NLD", "AUT", "POL", "PRT", "ROU", "SVN", "SVK", "FIN", "SWE")

itpd.a <- itpd %>%
  filter(exporter_iso3 %in% eu_countries | importer_iso3 %in% eu_countries,
         between(year, 2010, 2016))

## B) ----
itpd.b <- itpd.a %>%
  filter(exporter_iso3 %in% eu_countries) %>% 
  group_by(exporter_iso3, year, industry_id, broad_sector) %>% 
  summarise(X_ij = sum(trade)) %>% 
  left_join(itpd.a %>% 
              filter(importer_iso3 %in% eu_countries) %>% 
              group_by(importer_iso3, year, industry_id, broad_sector) %>% 
              summarise(M_ij = sum(trade)),
            by = c("exporter_iso3" = "importer_iso3", "industry_id", "broad_sector", "year")) %>% 
  mutate(GL_ij = 1 - abs(X_ij - M_ij) / (X_ij + M_ij)) %>% 
  left_join(itpd.a$industry_id %>% get_labels() %>% as_tibble() %>% 
              mutate(id = seq(1, itpd.a$industry_id %>% get_labels() %>% length(), 1)) %>% 
              rename(industry_name = value),
            by = c("industry_id" = "id")) %>% 
  left_join(itpd.a$broad_sector %>% get_labels() %>% as_tibble() %>% 
              mutate(id = seq(1, itpd.a$broad_sector %>% get_labels() %>% length(), 1)) %>% 
              rename(sector_name = value),
            by = c("broad_sector" = "id"))

itpd.b %>%
  filter(industry_id ==  144, year == 2016) %>% 
  arrange(GL_ij) %>% 
  ggplot() +
  geom_col(aes(reorder(exporter_iso3, GL_ij), GL_ij)) +
  theme_bw() +
  labs(x = "País", y = "Índice de Grubel-Lloyd",
       title = "Índice (Aircraft and spacecraft) - 2016")
ggsave('plots/gl_eu.png', dpi = 600, height = 14, width = 25, unit = 'cm', bg = 'white')

df <- itpd.b %>%
  filter(industry_id ==  144) %>% 
  group_by(industry_id, year) %>% 
  slice_max(GL_ij, n = 2) %>% 
  bind_rows(itpd.b %>%
              filter(industry_id ==  144) %>% 
              group_by(industry_id, year) %>% 
              slice_min(GL_ij, n = 2)) %>% 
  arrange(year, GL_ij) %>% 
  select(year, exporter_iso3, GL = GL_ij)

## C) ----
mercosul <- c("BRA", "ARG", "URY", "PRY", "BOL", "CHL", "COL", "ECU", "PER")

itpd.c <- itpd.a %>%
  filter(exporter_iso3 %in% mercosul) %>% 
  group_by(exporter_iso3, year, industry_id, broad_sector) %>% 
  summarise(X_ij = sum(trade)) %>% 
  left_join(itpd.a %>% 
              filter(importer_iso3 %in% mercosul) %>% 
              group_by(importer_iso3, year, industry_id, broad_sector) %>% 
              summarise(M_ij = sum(trade)),
            by = c("exporter_iso3" = "importer_iso3", "industry_id", "broad_sector", "year")) %>% 
  mutate(GL_ij = 1 - abs(X_ij - M_ij) / (X_ij + M_ij)) %>% 
  left_join(itpd.a$industry_id %>% get_labels() %>% as_tibble() %>% 
              mutate(id = seq(1, itpd.a$industry_id %>% get_labels() %>% length(), 1)) %>% 
              rename(industry_name = value),
            by = c("industry_id" = "id")) %>% 
  left_join(itpd.a$broad_sector %>% get_labels() %>% as_tibble() %>% 
              mutate(id = seq(1, itpd.a$broad_sector %>% get_labels() %>% length(), 1)) %>% 
              rename(sector_name = value),
            by = c("broad_sector" = "id"))

itpd.b %>%
  filter(industry_id ==  144) %>% 
  group_by(year) %>% 
  summarise(EU = mean(GL_ij)) %>% 
  left_join(itpd.c %>% 
              filter(industry_id ==  144) %>% 
              group_by(year) %>% 
              summarise(LATAM = mean(GL_ij)),
            by = c("year"))



