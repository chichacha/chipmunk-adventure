library(tidyverse)
library(rvest)

test <- read_html("https://download.geonames.org/export/zip/")
country_list <-test %>% html_elements("a~ img+ a") %>%
  html_attr("href")

geonames <- tibble(
  name=country_list
) %>%
  mutate(country=str_sub(name,1L,2L)) %>%
  mutate(full_url = str_glue("https://download.geonames.org/export/zip/{name}")) %>%
  filter(!str_detect(name,"(export|read|all)"))


library(countrycode)
?countrycode::countrycode

geonames$country_name <-countrycode::countrycode(geonames$country,origin="iso2c",destination="country.name")
geonames$continent <-countrycode::countrycode(geonames$country,origin="iso2c",destination="continent")
geonames$currency <-countrycode::countrycode(geonames$country,origin="iso2c",destination="currency")

ecom <- c("US", "BE", "FR", "NO", "CH", "FI", "SE", "AT", "ES", "NL", "DE", "DK", "CA", "CZ", "IE", "PL", "IT", "GB")

geonames %>% filter(country %in% ecom) %>%
  mutate(full_version=str_detect(name,"full")) %>%
  mutate(currency = if_else(country=="CZ","Euro",currency)) %>%
  select(-name) %>%
  pivot_wider(names_from=full_version,values_from=full_url) %>%
  arrange(continent,currency,country_name) %>%
  rename(ver_min=`FALSE`,ver_full=`TRUE`) %>%
  mutate(ver_full=coalesce(ver_full,ver_min))
