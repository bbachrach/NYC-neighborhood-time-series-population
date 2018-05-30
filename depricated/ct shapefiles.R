################################################
## Shapefiles 
################################################

## need to pull in both the tract files and the county files to use a join and limit to just the tracts of interest

## 1980 
ct_shape_1980_tract <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Shapefiles/Census Tract Shapefiles 1970_2010/nhgis0003_shapefile_tl2000_us_tract_1980/US_tract_1980.shp"
                               , layer = "US_tract_1980"
                               # ,promote_to_multi = F
)

ct_shape_1980_tract <- st_transform(ct_shape_1980_tract, "+proj=longlat +datum=WGS84")

ct_shape_1980_county <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Shapefiles/Census Tract Shapefiles 1970_2010/nhgis0003_shapefile_tl2000_us_tract_1980/US_tractcounty_1980.shp"
                                , layer = "US_tractcounty_1980"
                                # ,promote_to_multi = F
)
ct_shape_1980_county <- st_transform(ct_shape_1980_county, "+proj=longlat +datum=WGS84")

ct_shape_1980 <- semi_join(ct_shape_1980_tract,
                           as.data.frame(ct_shape_1980_county %>% 
                                           filter(STATENAM %in% "New York" & 
                                                    ICPSRNAM %in% c(
                                                      "BRONX"
                                                      ,"KINGS"
                                                      ,"NEW YORK"
                                                      ,"QUEENS"
                                                      ,"RICHMOND"
                                                    )
                                           ))
                           ,by=c("NHGISST","NHGISCTY")
)

ct_shape_1980 <- left_join(ct_shape_1980,
                           as.data.frame(ct_shape_1980_county %>% 
                                           filter(STATENAM %in% "New York" & 
                                                    ICPSRNAM %in% c(
                                                      "BRONX"
                                                      ,"KINGS"
                                                      ,"NEW YORK"
                                                      ,"QUEENS"
                                                      ,"RICHMOND"
                                                    )
                                           )) %>% 
                             select(ICPSRNAM,NHGISST,NHGISCTY)
                           ,by=c("NHGISST","NHGISCTY")
)




## 1990 
ct_shape_1990_tract <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Shapefiles/Census Tract Shapefiles 1970_2010/nhgis0003_shapefile_tl2000_us_tract_1990/US_tract_1990.shp"
                               , layer = "US_tract_1990"
                               # ,promote_to_multi = F
)

ct_shape_1990_tract <- st_transform(ct_shape_1990_tract, "+proj=longlat +datum=WGS84")

ct_shape_1990_county <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Shapefiles/Census Tract Shapefiles 1970_2010/nhgis0003_shapefile_tl2000_us_tract_1990/US_tractcounty_1990.shp"
                                , layer = "US_tractcounty_1990"
                                # ,promote_to_multi = F
)
ct_shape_1990_county <- st_transform(ct_shape_1990_county, "+proj=longlat +datum=WGS84")

ct_shape_1990 <- semi_join(ct_shape_1990_tract,
                           as.data.frame(ct_shape_1990_county %>% 
                                           filter(STATENAM %in% "New York" & 
                                                    ICPSRNAM %in% c(
                                                      "BRONX"
                                                      ,"KINGS"
                                                      ,"NEW YORK"
                                                      ,"QUEENS"
                                                      ,"RICHMOND"
                                                    )
                                           ))
                           ,by=c("NHGISST","NHGISCTY")
)

ct_shape_1990 <- left_join(ct_shape_1990,
                           as.data.frame(ct_shape_1990_county %>% 
                                           filter(STATENAM %in% "New York" & 
                                                    ICPSRNAM %in% c(
                                                      "BRONX"
                                                      ,"KINGS"
                                                      ,"NEW YORK"
                                                      ,"QUEENS"
                                                      ,"RICHMOND"
                                                    )
                                           )) %>% 
                             select(ICPSRNAM,NHGISST,NHGISCTY)
                           ,by=c("NHGISST","NHGISCTY")
)



## 2000 
ct_shape_2000_tract <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Shapefiles/Census Tract Shapefiles 1970_2010/nhgis0003_shapefile_tl2000_us_tract_2000/US_tract_2000.shp"
                               , layer = "US_tract_2000"
                               # ,promote_to_multi = F
)

ct_shape_2000_tract <- st_transform(ct_shape_2000_tract, "+proj=longlat +datum=WGS84")

ct_shape_2000_county <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Shapefiles/Census Tract Shapefiles 1970_2010/nhgis0003_shapefile_tl2000_us_tract_2000/US_tractcounty_2000.shp"
                                , layer = "US_tractcounty_2000"
                                # ,promote_to_multi = F
)
ct_shape_2000_county <- st_transform(ct_shape_2000_county, "+proj=longlat +datum=WGS84")

ct_shape_2000 <- semi_join(ct_shape_2000_tract,
                           as.data.frame(ct_shape_2000_county %>% 
                                           filter(STATENAM %in% "New York" & 
                                                    ICPSRNAM %in% c(
                                                      "BRONX"
                                                      ,"KINGS"
                                                      ,"NEW YORK"
                                                      ,"QUEENS"
                                                      ,"RICHMOND"
                                                    )
                                           ))
                           ,by=c("NHGISST","NHGISCTY")
)

ct_shape_2000 <- left_join(ct_shape_2000,
                           as.data.frame(ct_shape_2000_county %>% 
                                           filter(STATENAM %in% "New York" & 
                                                    ICPSRNAM %in% c(
                                                      "BRONX"
                                                      ,"KINGS"
                                                      ,"NEW YORK"
                                                      ,"QUEENS"
                                                      ,"RICHMOND"
                                                    )
                                           )) %>% 
                             select(ICPSRNAM,NHGISST,NHGISCTY)
                           ,by=c("NHGISST","NHGISCTY")
)




## 2010 
ct_shape_2010_tract <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Shapefiles/Census Tract Shapefiles 1970_2010/nhgis0003_shapefile_tl2010_us_tract_2010/US_tract_2010.shp"
                               , layer = "US_tract_2010"
                               # ,promote_to_multi = F
)

ct_shape_2010_tract <- semi_join(ct_shape_2010_tract
                                 ,ct_pop_nyc %>%
                                   filter(!is.na(GJOIN2010)) %>%
                                   rename(GISJOIN = GJOIN2010)
                                 ,by="GISJOIN"
)

## putting in that stupid fucking ICPSRNAM variable so this will work with the downstream function
derp <- ct_shape_1990 %>% 
  mutate(GISJOIN = as.character(GISJOIN)
         ,ICPSRNAM = as.character(ICPSRNAM)) %>%
  select(GISJOIN,ICPSRNAM) %>%
  left_join(as.data.frame(ct_shape_2010_tract) %>% 
              mutate(GISJOIN = as.character(GISJOIN)
                     ,COUNTYFP10 = as.character(COUNTYFP10)) %>% 
              select(GISJOIN,COUNTYFP10)
            ,by="GISJOIN"
  ) %>% 
  filter(!is.na(COUNTYFP10)) %>% 
  filter(!duplicated(ICPSRNAM)) %>% 
  as.data.frame() %>%
  select(COUNTYFP10,ICPSRNAM)

ct_shape_2010_tract <- left_join(ct_shape_2010_tract
                                 ,derp %>% 
                                   select(COUNTYFP10,ICPSRNAM)
)

ct_shape_2010 <- st_transform(ct_shape_2010_tract, "+proj=longlat +datum=WGS84")


## 2011 
ct_shape_2011_tract <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Shapefiles/Census Tract Shapefiles 2011_2015/nhgis0004_shapefile_tl2011_us_tract_2011/US_tract_2011.shp"
                               , layer = "US_tract_2011"
                               # ,promote_to_multi = F
)

########
######
####
## fix this one
ct_shape_2011_tract <- semi_join(ct_shape_2011_tract
                                 ,ct_pop_nyc_2011
                                 ,by="GISJOIN"
)
########
########


ct_shape_2011 <- st_transform(ct_shape_2011_tract, "+proj=longlat +datum=WGS84")


## 2012 
ct_shape_2012_tract <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Shapefiles/Census Tract Shapefiles 2011_2015/nhgis0004_shapefile_tl2012_us_tract_2012/US_tract_2012.shp"
                               , layer = "US_tract_2012"
                               # ,promote_to_multi = F
)

ct_shape_2012_tract <- semi_join(ct_shape_2012_tract
                                 ,ct_pop_nyc_2012
                                 ,by="GISJOIN"
)

ct_shape_2012 <- st_transform(ct_shape_2012_tract, "+proj=longlat +datum=WGS84")


## 2013 
ct_shape_2013_tract <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Shapefiles/Census Tract Shapefiles 2011_2015/nhgis0004_shapefile_tl2013_us_tract_2013/US_tract_2013.shp"
                               , layer = "US_tract_2013"
                               # ,promote_to_multi = F
)

ct_shape_2013_tract <- semi_join(ct_shape_2013_tract
                                 ,ct_pop_nyc_2013
                                 ,by="GISJOIN"
)

ct_shape_2013 <- st_transform(ct_shape_2013_tract, "+proj=longlat +datum=WGS84")


## 2014 
ct_shape_2014_tract <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Shapefiles/Census Tract Shapefiles 2011_2015/nhgis0004_shapefile_tl2014_us_tract_2014/US_tract_2014.shp"
                               , layer = "US_tract_2014"
                               # ,promote_to_multi = F
)

ct_shape_2014_tract <- semi_join(ct_shape_2014_tract
                                 ,ct_pop_nyc_2014
                                 ,by="GISJOIN"
)

ct_shape_2014 <- st_transform(ct_shape_2014_tract, "+proj=longlat +datum=WGS84")



## 2015 
ct_shape_2015_tract <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Shapefiles/Census Tract Shapefiles 2011_2015/nhgis0004_shapefile_tl2015_us_tract_2015/us_tract_2015.shp"
                               , layer = "US_tract_2015"
                               # ,promote_to_multi = F
)

ct_shape_2015_tract <- semi_join(ct_shape_2015_tract
                                 ,ct_pop_nyc_2015
                                 ,by="GISJOIN"
)

ct_shape_2015 <- st_transform(ct_shape_2015_tract, "+proj=longlat +datum=WGS84")


## 2016 
ct_shape_2016_tract <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Shapefiles/Census Tract Shapefiles 2011_2015/nhgis0011_shapefile_tl2016_us_tract_2016"
                               , layer = "US_tract_2016"
)

ct_shape_2016_tract <- semi_join(ct_shape_2016_tract
                                 ,ct_pop_nyc_2016
                                 ,by="GISJOIN"
)

ct_shape_2016 <- st_transform(ct_shape_2016_tract, "+proj=longlat +datum=WGS84")



ct_shape_2011_2016.list <- list("ct_2011" = ct_shape_2011
                                ,"ct_2012" = ct_shape_2012
                                ,"ct_2013" = ct_shape_2013
                                ,"ct_2014" = ct_shape_2014
                                ,"ct_2015" = ct_shape_2015
                                ,"ct_2016" = ct_shape_2016
)


ct_shape_1980_2010.list <- list("ct_1970" = ct_shape_1980
                                ,"ct_1980" = ct_shape_1980
                                ,"ct_1990" = ct_shape_1990
                                ,"ct_2000" = ct_shape_2000
                                ,"ct_2010" = ct_shape_2010
)


setwd("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Population Estimates/Historical")
saveRDS(ct_shape_1980_2010.list,"ct_shape_1980_2010.list.rds")
saveRDS(ct_shape_2011_2016.list,"ct_shape_2011_2016.list.rds")
saveRDS(ct_pops_census.list,"ct_pops_census.list.rds")
saveRDS(acs.list,"acs.list.rds")



## read in pedia cities 
largeshape.url <- "http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1largecitiesnycneighborhoods.geojson"
large.map <- geojson_read(as.location(largeshape.url),
                          method="local",
                          what="sp")
large.map <- large.map %>% spTransform(CRS("+proj=longlat +datum=WGS84"))
large.map <- st_as_sf(large.map)