geo_country <- read_html("https://www.geonames.org/countries/")


get_admin_url <- function(url,...) {
  tmp <- read_html(url) %>%
    html_elements("td td a:nth-child(1)") %>%
    html_attr("href")
  return(tmp)
}


geo_country2 <-tibble(
  code=geo_country %>% html_elements("#countries a") %>%
    html_attr("name") ,
  url=geo_country %>% html_elements("#countries a") %>%
    html_attr("href")
) %>% fill(code) %>%
  filter(!is.na(url)) %>%
  filter(code %in% ecom) %>%
  mutate(url=str_c("https://www.geonames.org",url)) %>%
  mutate(admin_url=map_chr(url,get_admin_url))

geo_country2 <- geo_country2 %>%
  mutate(admin_url=str_c("https://www.geonames.org",admin_url))

geo_country2 <- geo_country2 %>%
  left_join(geo_country %>% html_table() %>%
              pluck(2) %>%
              filter(`ISO-3166alpha2` %in% ecom) %>%
              rename(code=`ISO-3166alpha2`))

geo_country2 <- geo_country2 %>%
  janitor::clean_names()

geo_country2 %>% mutate(area_in_km2=as.numeric(gsub(",","",area_in_km2)),
                        population=as.numeric(gsub(",","",population))) %>%
  arrange(desc(population)) %>%
  left_join(geonames_min %>% select(code=country, currency, ver_full)) %>%
  group_by(continent, currency) %>%
  gt::gt() %>%
  gt::fmt_number(is.numeric,decimals=0)

library(leaflet)

tmp_df %>% filter(country_code %in% c("GB")) %>%
  leaflet() %>%
  leaflet::addProviderTiles(provider=providers$CartoDB.DarkMatter) %>%
  addCircleMarkers( clusterOptions = markerClusterOptions())
  


whse <- tibble(
  whse_country=c("US","CA","DE"),
  whse=c("OGDEN","VAN","UBER"),
  geo = list(c(41.2305233,-112.0577054),c(49.1954223,-122.9430083),c(49.2696569,6.7311441)
))

whse<-whse %>% unnest(geo) %>%
  mutate(geo_type=if_else(row_number()%%2==1,"lat","lon")) %>%
  pivot_wider(names_from=geo_type,values_from=geo) 


tmp_df <- tmp_df %>% mutate(whse_country=if_else(country_code %in% c("CA","US"), country_code, "DE"))
tmp_df <- tmp_df %>% left_join(whse)

library(geosphere)

tmp_df2 <-tmp_df %>%
  mutate(dist=distHaversine(cbind(longitude,latitude),cbind(lon,lat))) 

tmp_df2 <- tmp_df2 %>% mutate(dist_km=dist/1000)



tmp_df2 %>% ggplot(aes(x=dist_km)) +
  stat_density()

tmp_df2 %>% filter(dist_km>5000) %>% 
  filter(country_code=="US")  %>% count(place_name)
  leaflet::leaflet() %>%
  addProviderTiles(provider=providers$Stamen.Toner) %>%
  addCircleMarkers(lng=~longitude,lat=~latitude)
  
paste_unique <- function(x,...){
  paste(sort(unique(x)),collapse="|")
}
  
tmp_df2 %>%
  mutate_at(vars(matches("(lat|lon)")), round) %>%
  filter(!str_detect(place_name,"(APO|FPO)")) %>%
  filter(dist_km<=6000) %>%
  group_by(latitude,longitude,lat,lon) %>%
  summarise(dist=mean(dist),
            n=n(),
            country=paste_unique(country_code),
            admin1=paste_unique(admin_code1),
            place_name=paste_unique(place_name),
            postal_code=paste_unique(postal_code))  %>%
  arrange(desc(dist)) %>% arrange(desc(latitude))
  ggplot(aes(x=longitude,y=latitude)) +
  geom_segment(aes(xend=lon, yend=lat, color=(str_sub(postal_code,1L,1L)), linewidth=1/dist)) +
  see::scale_color_material() +
  coord_map() +
  scale_linewidth_continuous(range=c(0.1,2)) +
  theme_void()

library(tmap)
library(leaflet)
library(sf)
  
tmp_df2 %>%
    mutate_at(vars(matches("(lat|lon)")), round) %>%
    filter(!str_detect(place_name,"(APO|FPO)")) %>%
    filter(dist_km<=6000) %>%
  #filter(country_code=="US") %>%
    group_by(latitude,longitude,lat,lon) %>%
    summarise(dist=mean(dist),
              n=n(),
              country=paste_unique(country_code),
              admin1=paste_unique(admin_code1),
              place_name=paste_unique(place_name),
              postal_code=paste_unique(postal_code),
              origin=first(whse)) %>% 
  ungroup() %>%
  arrange(desc(dist)) %>%
  mutate(wkt=str_glue("LINESTRING ({longitude} {latitude},{lon} {lat})")) %>%
  st_as_sf(wkt="wkt", crs="epsg:4326") %>%
  ggplot() +
  geom_sf(aes(color=str_sub(postal_code,1L,2L),linewidth=1/dist)) +
  scale_linewidth_continuous(range=c(0.1,8), guide="none") +
  see::scale_color_material(guide="none") +
  theme_void() 


see::palette_material()(18) %>% farver::decode_colour() -> my_pal
library(colourvalues)
farver::decode_colour(c("#d0d0d0","#023323"))

pts = matrix(1:16, , 2)
pts2 = matrix(16:1,,2)
dummy <-bind_cols(pts,pts2) %>%
  as_tibble() 
names(dummy) <- c("lon1","lat1","lon2","lat2")






