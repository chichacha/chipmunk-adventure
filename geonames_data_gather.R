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

geonames_min <-geonames %>% filter(country %in% ecom) %>%
  mutate(full_version=str_detect(name,"full")) %>%
  mutate(currency = if_else(country=="CZ","Euro",currency)) %>%
  select(-name) %>%
  pivot_wider(names_from=full_version,values_from=full_url) %>%
  arrange(continent,currency,country_name) %>%
  rename(ver_min=`FALSE`,ver_full=`TRUE`) %>%
  mutate(ver_full=coalesce(ver_full,ver_min))

geonames_min <- geonames_min %>% mutate(destfile=str_c("t/",country,".zip"))

### Download all the Zip Files
for (i in seq_along(geonames_min$ver_full)){
  download.file(geonames_min$ver_full[i], destfile=geonames_min$destfile[i])
}

### unzip with bash from terminal
## unzip "*.zip"

t_df <- fs::dir_info("t/")
tmp_df <-t_df %>% filter(str_detect(path,".txt")) %>%
  filter(!str_detect(path,"readme")) %>%
  select(path) %>%
  mutate(df=map(path,read_tsv,col_names=F, col_types="cccccccccddi"))

tmp_df <- tmp_df %>% unnest(df)

tmp_df


names(tmp_df) <- c("path","country_code","postal_code","place_name","admin_name1",
                   "admin_code1","admin_name2","admin_code2","admin_name3",
                   "admin_code3","latitude","longitude","accuracy")


tmp_df %>% mutate(postal_code_len=str_count(postal_code)) %>%
  group_by(postal_code_len,country_code) %>%
  summarise(cnt=n(),
            unique_cnt=n_distinct(postal_code),
            example=first(postal_code)) %>%
  ungroup() %>%
  arrange(country_code) %>%
  add_count(country_code,wt=cnt) %>%
  mutate(share=cnt/n) %>%
  gt::gt() %>%
  gt::fmt_percent(share)

tmp_df %>%
  add_count(postal_code,country_code) %>%
  filter(n>1) %>%
  arrange(desc(n)) %>%
  relocate(latitude,longitude) %>%
  group_by(postal_code,country_code) %>%
  summarise(latitude=mean(latitude,na.rm=T),
            longitude=mean(longitude,na.rm=T),
            place_name=paste(unique(sort(str_c(place_name," (",admin_code1,")"))),collapse=", "),
            admin1=paste(unique(admin_code1),collapse=", "),
            cnt=n()) %>%
  ungroup() %>%
  arrange(-cnt) 
  arrange(longitude) %>%
  ggplot(aes(x=longitude,y=latitude)) +
  geom_point(aes(color=factor(cnt))) +
  coord_map()

  tmp_df %>% filter(postal_code=="V0N 1P0") %>% relocate(accuracy)
  
  t_df %>%
    mutate(ext_type=str_sub(path,-3L,-1L)) %>%
    group_by(ext_type) %>%
    arrange(desc(size)) %>%
    select(path, size,birth_time,ext_type) %>%
    mutate(path=str_remove(path,"t/")) %>%
    gt::gt()
  