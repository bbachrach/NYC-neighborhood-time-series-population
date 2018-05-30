
rm(list=ls())
options(scipen=999)

#   -----------------------------------------------------------------------

# POPULATION DATA PRE PROCESSING
# Mapping census population data to various shapefiles. Also, 
# interpolating the missing years from the resulting time series,
# and adding more recent years from ACS estimates

#   -----------------------------------------------------------------------

# library(data.table)
library(tidyverse)
library(stringr)
library(sf)

library(dplyr)
library(rgdal)
library(ggplot2)
library(maptools)
library(sp)
library(rgeos)
library(geojsonio)
library(PBSmapping)
library(broom)
library(parallel)



# SECTION 1) BORO ---------------------------------------------------------

county_pop <- read_csv("data/population/Census Population 1970-2010/nhgis0001_ts_nominal_cty_sub.csv")

# field descriptions from data dict ---------------------------------------
# AV0AA1970:   1970: Persons: Total
# AV0AA1980:   1980: Persons: Total
# AV0AA1990:   1990: Persons: Total
# AV0AA2000:   2000: Persons: Total
# AV0AA2010:   2010: Persons: Total
# AV0AA125:    2008-2012: Persons: Total
# AV0AA125M:   Margin of error: 2008-2012: Persons: Total

nyc_boro_pop <- 
  county_pop %>% 
  filter(STATE=="New York") %>% 
  rename(
    'Pop_1970' = AV0AA1970
    , 'Pop_1980' = AV0AA1980
    , 'Pop_1990' = AV0AA1990
    , 'Pop_2000' = AV0AA2000
    , 'Pop_2010' = AV0AA2010
    , 'Pop_2008_2012' = AV0AA125
    , 'MarError2008' = AV0AA125M
  ) %>% 
  select(COUNTY,Pop_1970:Pop_2008_2012) %>% 
  filter(COUNTY%in%c("New York County","Richmond County"
                     ,"Kings County","Queens County","Bronx County")
  ) %>% 
  gather(Year,Population,-COUNTY) %>% 
  mutate(Year = str_replace(Year,"Pop_","")) %>% 
  mutate(COUNTY = str_replace(COUNTY," County","")) %>% 
  mutate(BoroName = COUNTY
         ,BoroName = ifelse(BoroName=="Kings","Brooklyn",BoroName)
         ,BoroName = ifelse(BoroName=="New York","Manhattan",BoroName)
         ,BoroName = ifelse(BoroName=="Richmond","Staten Island",BoroName)
         ,BoroName = ifelse(BoroName=="Bronx","Bronx",BoroName)
         ,BoroName = ifelse(BoroName=="Queens","Queens",BoroName)
  )


boro_shape <- st_read("data/Shapefiles/boro_shapefile.json")
boro_shape_with_pop <- left_join(boro_shape,nyc_boro_pop, by = c('BoroName'))

st_write(boro_shape_with_pop,"data/population/data_with_shapefiles/boro_shape_with_pop.json", driver = "GeoJSON")





# SECTION 2) CENSUS TRACT -------------------------------------------------
ct_pop <- read_csv("/Users/billbachrach/Dropbox (hodgeswardelliott)/hodgeswardelliott Team Folder/Teams/Data/Bill Bachrach/Data Sources/Census/Population Estimates/nhgis0001_csv 2/nhgis0001_ts_nominal_tract.csv")


# scrub: 
ct_pop_nyc <- 
  ct_pop %>% 
  filter(STATE=="New York"
         , COUNTY%in%c("New York County","Richmond County"
                       ,"Kings County","Queens County","Bronx County")
  ) %>% 
  select(GJOIN1970:GJOIN2012,NAME1970:NAME2012,AV0AA1970:AV0AA125,COUNTY) %>% 
  rename(
    'Pop_1970' = AV0AA1970
    , 'Pop_1980' = AV0AA1980
    , 'Pop_1990' = AV0AA1990
    , 'Pop_2000' = AV0AA2000
    , 'Pop_2010' = AV0AA2010
    , 'Pop_2008_2012' = AV0AA125
  ) %>% 
  gather(Year, Population,-COUNTY , -GJOIN1970, -GJOIN1980, -GJOIN1990, -GJOIN2000
         , -GJOIN2010, -GJOIN2012, -NAME1970, -NAME1980, -NAME1990
         , -NAME2000, -NAME2010, -NAME2012) %>% 
  mutate(Year = str_replace(Year,"Pop_",""))




ct_pop_nyc_2011 <- read_csv("/Users/billbachrach/Dropbox (hodgeswardelliott)/hodgeswardelliott Team Folder/Teams/Data/Bill Bachrach/Data Sources/Census/Population Estimates/nhgis0004_csv/nhgis0004_ds184_20115_2011_tract.csv") %>% 
  filter(STATE=="New York"
         , COUNTY%in%c("New York County","Richmond County"
                       ,"Kings County","Queens County","Bronx County")
  ) %>% 
  mutate(Year = year(parse_date_time(
    gsub(".*-","",YEAR)
    ,"Y!"))
  ) %>% 
  rename(Population = MNTE001
         ,STATEFP = STATEA
         ,MOE = MNTM001) %>%
  select(STATEFP,GISJOIN,Year,Population,COUNTY)

ct_pop_nyc_2012 <- read_csv("/Users/billbachrach/Dropbox (hodgeswardelliott)/hodgeswardelliott Team Folder/Teams/Data/Bill Bachrach/Data Sources/Census/Population Estimates/nhgis0004_csv/nhgis0004_ds191_20125_2012_tract.csv") %>% 
  filter(STATE=="New York"
         , COUNTY%in%c("New York County","Richmond County"
                       ,"Kings County","Queens County","Bronx County")
  ) %>% 
  mutate(Year = year(parse_date_time(
    gsub(".*-","",YEAR)
    ,"Y!"))
  ) %>% 
  rename(Population = QSPE001
         ,STATEFP = STATEA
         ,MOE = QSPM001) %>%
  select(STATEFP,GISJOIN,Year,Population,COUNTY)

ct_pop_nyc_2013 <- read_csv("/Users/billbachrach/Dropbox (hodgeswardelliott)/hodgeswardelliott Team Folder/Teams/Data/Bill Bachrach/Data Sources/Census/Population Estimates/nhgis0004_csv/nhgis0004_ds201_20135_2013_tract.csv") %>% 
  filter(STATE=="New York"
         , COUNTY%in%c("New York County","Richmond County"
                       ,"Kings County","Queens County","Bronx County")
  ) %>% 
  mutate(Year = year(parse_date_time(
    gsub(".*-","",YEAR)
    ,"Y!"))
  ) %>% 
  rename(Population = UEPE001
         ,STATEFP = STATEA
         ,MOE = UEPM001) %>%
  select(STATEFP,GISJOIN,Year,Population,COUNTY)

ct_pop_nyc_2014 <- read_csv("/Users/billbachrach/Dropbox (hodgeswardelliott)/hodgeswardelliott Team Folder/Teams/Data/Bill Bachrach/Data Sources/Census/Population Estimates/nhgis0004_csv/nhgis0004_ds206_20145_2014_tract.csv") %>% 
  filter(STATE=="New York"
         , COUNTY%in%c("New York County","Richmond County"
                       ,"Kings County","Queens County","Bronx County")
  ) %>% 
  mutate(Year = year(parse_date_time(
    gsub(".*-","",YEAR)
    ,"Y!"))
  ) %>% 
  rename(Population = ABA1E001
         ,STATEFP = STATEA
         ,MOE = ABA1M001) %>%
  select(STATEFP,GISJOIN,Year,Population,COUNTY)

ct_pop_nyc_2015 <- read_csv("/Users/billbachrach/Dropbox (hodgeswardelliott)/hodgeswardelliott Team Folder/Teams/Data/Bill Bachrach/Data Sources/Census/Population Estimates/nhgis0004_csv/nhgis0004_ds215_20155_2015_tract.csv") %>% 
  filter(STATE=="New York"
         , COUNTY%in%c("New York County","Richmond County"
                       ,"Kings County","Queens County","Bronx County")
  ) %>% 
  mutate(Year = year(parse_date_time(
    gsub(".*-","",YEAR)
    ,"Y!"))
  ) %>% 
  rename(Population = ADKWE001
         ,STATEFP = STATEA
         ,MOE = ADKWM001) %>%
  select(STATEFP,GISJOIN,Year,Population,COUNTY)


ct_pops.list <- list("ct_pop11" = ct_pop_nyc_2011
                     ,"ct_pop12" = ct_pop_nyc_2012
                     ,"ct_pop13" = ct_pop_nyc_2013
                     ,"ct_pop14" = ct_pop_nyc_2014
                     ,"ct_pop15" = ct_pop_nyc_2015)



# split into individual years for convinience -----------------------------

ct_pop_nyc_1970 <- ct_pop_nyc %>% filter(Year=="1970") %>% select("GISJOIN" = GJOIN1970, "NAME" = NAME1970, COUNTY, Year, Population) %>% mutate(GISJOIN = as.character(GISJOIN))
ct_pop_nyc_1980 <- ct_pop_nyc %>% filter(Year=="1980") %>% select("GISJOIN" = GJOIN1980, "NAME" = NAME1980, COUNTY, Year, Population) %>% mutate(GISJOIN = as.character(GISJOIN))
ct_pop_nyc_1990 <- ct_pop_nyc %>% filter(Year=="1990") %>% select("GISJOIN" = GJOIN1990, "NAME" = NAME1990, COUNTY, Year, Population) %>% mutate(GISJOIN = as.character(GISJOIN))
ct_pop_nyc_2000 <- ct_pop_nyc %>% filter(Year=="2000") %>% select("GISJOIN" = GJOIN2000, "NAME" = NAME2000, COUNTY, Year, Population) %>% mutate(GISJOIN = as.character(GISJOIN))
ct_pop_nyc_2010 <- ct_pop_nyc %>% filter(Year=="2010") %>% select("GISJOIN" = GJOIN2010, "NAME" = NAME2010, COUNTY, Year, Population) %>% mutate(GISJOIN = as.character(GISJOIN))
ct_pop_nyc_2008_2012 <- ct_pop_nyc %>% filter(Year=="2008_2012") %>% select("GISJOIN" = GJOIN2012, "NAME" = NAME2012, COUNTY, Year, Population) %>% mutate(GISJOIN = as.character(GISJOIN))

ct_pops_census.list <- list("ct_pop70" = ct_pop_nyc_1970
                     ,"ct_pop80" = ct_pop_nyc_1980
                     ,"ct_pop90" = ct_pop_nyc_1990
                     ,"ct_pop00" = ct_pop_nyc_2000
                     ,"ct_pop10" = ct_pop_nyc_2010)

# read in shapefiles for individual years
## need to pull in both the tract files and the county files to use a join and limit to just the tracts of interest

## 1980 
ct_shape_1980_tract <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/hodgeswardelliott Team Folder/Teams/Data/Bill Bachrach/Data Sources/Census/Shapefiles/Census Tract Shapefiles 1970_2010/nhgis0003_shapefile_tl2000_us_tract_1980/US_tract_1980.shp"
                         , layer = "US_tract_1980"
                         # ,promote_to_multi = F
)

ct_shape_1980_tract <- st_transform(ct_shape_1980_tract, "+proj=longlat +datum=WGS84")

ct_shape_1980_county <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/hodgeswardelliott Team Folder/Teams/Data/Bill Bachrach/Data Sources/Census/Shapefiles/Census Tract Shapefiles 1970_2010/nhgis0003_shapefile_tl2000_us_tract_1980/US_tractcounty_1980.shp"
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
ct_shape_1990_tract <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/hodgeswardelliott Team Folder/Teams/Data/Bill Bachrach/Data Sources/Census/Shapefiles/Census Tract Shapefiles 1970_2010/nhgis0003_shapefile_tl2000_us_tract_1990/US_tract_1990.shp"
                               , layer = "US_tract_1990"
                               # ,promote_to_multi = F
)

ct_shape_1990_tract <- st_transform(ct_shape_1990_tract, "+proj=longlat +datum=WGS84")

ct_shape_1990_county <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/hodgeswardelliott Team Folder/Teams/Data/Bill Bachrach/Data Sources/Census/Shapefiles/Census Tract Shapefiles 1970_2010/nhgis0003_shapefile_tl2000_us_tract_1990/US_tractcounty_1990.shp"
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
ct_shape_2000_tract <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/hodgeswardelliott Team Folder/Teams/Data/Bill Bachrach/Data Sources/Census/Shapefiles/Census Tract Shapefiles 1970_2010/nhgis0003_shapefile_tl2000_us_tract_2000/US_tract_2000.shp"
                               , layer = "US_tract_2000"
                               # ,promote_to_multi = F
)

ct_shape_2000_tract <- st_transform(ct_shape_2000_tract, "+proj=longlat +datum=WGS84")

ct_shape_2000_county <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/hodgeswardelliott Team Folder/Teams/Data/Bill Bachrach/Data Sources/Census/Shapefiles/Census Tract Shapefiles 1970_2010/nhgis0003_shapefile_tl2000_us_tract_2000/US_tractcounty_2000.shp"
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
ct_shape_2010_tract <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/hodgeswardelliott Team Folder/Teams/Data/Bill Bachrach/Data Sources/Census/Shapefiles/Census Tract Shapefiles 1970_2010/nhgis0003_shapefile_tl2010_us_tract_2010/US_tract_2010.shp"
                               , layer = "US_tract_2010"
                               # ,promote_to_multi = F
)

ct_shape_2010_tract <- semi_join(ct_shape_2010_tract
                                 ,ct_pop_nyc_2010
                                 ,by="GISJOIN"
                                 )
## putting in that stupid fucking ICPSRNAM variable so this will work with the downstream function
derp <- as.data.frame(left_join(ct_shape_1990 %>% 
                    mutate(GISJOIN = as.character(GISJOIN)
                           ,ICPSRNAM = as.character(ICPSRNAM)) %>%
                    select(GISJOIN,ICPSRNAM)
                  ,as.data.frame(ct_shape_2010) %>% 
                    mutate(GISJOIN = as.character(GISJOIN)
                           ,COUNTYFP10 = as.character(COUNTYFP10)) %>% 
                    select(GISJOIN,COUNTYFP10)
                  ,by="GISJOIN"
) %>% 
  filter(!is.na(COUNTYFP10)) %>% 
  filter(!duplicated(ICPSRNAM))) %>% 
  select(COUNTYFP10,ICPSRNAM)

ct_shape_2010_tract <- left_join(ct_shape_2010_tract
                                 ,derp %>% 
                                   select(COUNTYFP10,ICPSRNAM)
                                 )

ct_shape_2010 <- st_transform(ct_shape_2010_tract, "+proj=longlat +datum=WGS84")





## 2011 
ct_shape_2011_tract <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/hodgeswardelliott Team Folder/Teams/Data/Bill Bachrach/Data Sources/Census/Shapefiles/Census Tract Shapefiles 2011_2015/nhgis0004_shapefile_tl2011_us_tract_2011/US_tract_2011.shp"
                               , layer = "US_tract_2011"
                               # ,promote_to_multi = F
)

ct_shape_2011_tract <- semi_join(ct_shape_2011_tract
                                 ,ct_pop_nyc_2011
                                 ,by="GISJOIN"
)

ct_shape_2011 <- st_transform(ct_shape_2011_tract, "+proj=longlat +datum=WGS84")



## 2012 
ct_shape_2012_tract <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/hodgeswardelliott Team Folder/Teams/Data/Bill Bachrach/Data Sources/Census/Shapefiles/Census Tract Shapefiles 2011_2015/nhgis0004_shapefile_tl2012_us_tract_2012/US_tract_2012.shp"
                               , layer = "US_tract_2012"
                               # ,promote_to_multi = F
)

ct_shape_2012_tract <- semi_join(ct_shape_2012_tract
                                 ,ct_pop_nyc_2012
                                 ,by="GISJOIN"
)

ct_shape_2012 <- st_transform(ct_shape_2012_tract, "+proj=longlat +datum=WGS84")


## 2013 
ct_shape_2013_tract <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/hodgeswardelliott Team Folder/Teams/Data/Bill Bachrach/Data Sources/Census/Shapefiles/Census Tract Shapefiles 2011_2015/nhgis0004_shapefile_tl2013_us_tract_2013/US_tract_2013.shp"
                               , layer = "US_tract_2013"
                               # ,promote_to_multi = F
)

ct_shape_2013_tract <- semi_join(ct_shape_2013_tract
                                 ,ct_pop_nyc_2013
                                 ,by="GISJOIN"
)

ct_shape_2013 <- st_transform(ct_shape_2013_tract, "+proj=longlat +datum=WGS84")



## 2014 
ct_shape_2014_tract <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/hodgeswardelliott Team Folder/Teams/Data/Bill Bachrach/Data Sources/Census/Shapefiles/Census Tract Shapefiles 2011_2015/nhgis0004_shapefile_tl2014_us_tract_2014/US_tract_2014.shp"
                               , layer = "US_tract_2014"
                               # ,promote_to_multi = F
)

ct_shape_2014_tract <- semi_join(ct_shape_2014_tract
                                 ,ct_pop_nyc_2014
                                 ,by="GISJOIN"
)

ct_shape_2014 <- st_transform(ct_shape_2014_tract, "+proj=longlat +datum=WGS84")





## 2015 
ct_shape_2015_tract <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/hodgeswardelliott Team Folder/Teams/Data/Bill Bachrach/Data Sources/Census/Shapefiles/Census Tract Shapefiles 2011_2015/nhgis0004_shapefile_tl2015_us_tract_2015/us_tract_2015.shp"
                               , layer = "US_tract_2015"
                               # ,promote_to_multi = F
)

ct_shape_2015_tract <- semi_join(ct_shape_2015_tract
                                 ,ct_pop_nyc_2015
                                 ,by="GISJOIN"
)

ct_shape_2015 <- st_transform(ct_shape_2015_tract, "+proj=longlat +datum=WGS84")




ct_shape_2011_2015.list <- list("ct_2011" = ct_shape_2011
                                ,"ct_2012" = ct_shape_2012
                                ,"ct_2013" = ct_shape_2013
                                ,"ct_2014" = ct_shape_2014
                                ,"ct_2015" = ct_shape_2015)


ct_shape_1980_2010.list <- list("ct_1970" = ct_shape_1980
                                ,"ct_1980" = ct_shape_1980
                                ,"ct_1990" = ct_shape_1990
                                ,"ct_2000" = ct_shape_2000
                                ,"ct_2010" = ct_shape_2010
)



nyc_county_fp <- unique(as.data.frame(ct_pop %>% 
  filter(STATE=="New York"
         , COUNTY%in%c("New York County","Richmond County"
                       ,"Kings County","Queens County","Bronx County")
  )
  ,stringsAsFactors=F)[,"COUNTYFP"])


## read in pedia cities 
largeshape.url <- "http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1largecitiesnycneighborhoods.geojson"
large.map <- geojson_read(as.location(largeshape.url),
                          method="local",
                          what="sp")
large.map <- large.map %>% spTransform(CRS("+proj=longlat +datum=WGS84"))
large.map <- st_as_sf(large.map)


# st_is_longlat(ct_shape_2015)
st_is_longlat(ct_shape_2010_tract)
st_is_longlat(large.map)





##########################################
### calculations 
##########################################




## 1980 
## first determine what neighborhoods each CT intersects with 
# intersect.list <- st_intersects(ct_shape_2015,large.map,sparse=T)
intersect.list <- st_intersects(ct_shape_1980,large.map,sparse=T)


## Determine what proportion of each census tract lies in respective neighborhoods
# outer.len <- 100
outer.len <- length(intersect.list)
ptm <- proc.time()
cl <- makeCluster(detectCores()-1,type="FORK")

# x <- 1
# out.tmp <- lapply(1:outer.len, function(x){
out.tmp <- parLapply(cl,1:outer.len, function(x){
  # cat("begin ",x,"out of ",outer.len,"\n")
  
  ## iteratively create dataframes of individual census tract and overlapping neighborhoods
  ct.curr <- ct_shape_1980[x,]
  nbrhd_curr.df <- large.map[intersect.list[[x]],]
  
  ## area of census tract 
  ct.area <- st_area(ct.curr)
  
  ## some CTs don't intersect w/ any neighborhoods, setting those values to NA 
  if(length(intersect.list[[x]])>0){
    
    overlap.len <- nrow(nbrhd_curr.df)
    
    area.vec <- unlist(lapply(1:overlap.len,function(z){
      # cat(z,"\n")
      suppressWarnings(
        try(
          st_area(st_intersection(nbrhd_curr.df[z,],ct.curr))
          ,silent=T
        )
      )
    }
    )
    )
    
    ## not sure why, but throwing an occasional warning -
    ### Error in (function (classes, fdef, mtable)  : 
    ###             unable to find an inherited method for function ‘areaPolygon’ for signature ‘"SpatialLines"’
    ## catching as try error and then setting to NA
    area.vec[grepl("[[:alpha:]]",area.vec)] <- NA
    area.vec <- as.numeric(area.vec)
    
    area.props_raw <- area.vec/ct.area
    area.props <- area.vec/sum(area.vec,na.rm=T)
    
    prop_sum_raw <- sum(as.numeric(area.props_raw),na.rm=T)
    prop_sum_adj <- sum(as.numeric(area.props),na.rm=T)
    
    
    out <- as.data.frame(
      cbind(
        rep(
          as.character(as.data.frame(ct.curr)[,"GISJOIN"])
          ,overlap.len
        )
        ,rep(
          as.character(as.data.frame(ct.curr)[,"ICPSRNAM"])
          ,overlap.len
        )
        ,as.character(as.data.frame(nbrhd_curr.df)[,"neighborhood"])
        ,area.props_raw
        ,area.props
        ,rep(prop_sum_raw
             ,overlap.len)
        ,rep(prop_sum_adj
             ,overlap.len)
        ,rep(x
             ,overlap.len)
      )
      ,stringsAsFactors=F
    )
    
  } else{
    out <- as.data.frame(
      cbind(
        as.character(as.data.frame(ct.curr)[,"GISJOIN"])
        ,as.character(as.data.frame(ct.curr)[,"ICPSRNAM"])
        ,NA
        ,NA
        ,NA
        ,NA
        ,NA
        ,x
      )
      ,stringsAsFactors=F
    )
  }
  
  colnames(out) <- c("GISJOIN","ICPSRNAM","neighborhood","prop_raw","prop","sum_props_raw","sum_props_adj","order")
  return(out)
}
)

stopCluster(cl)
calc.time <- proc.time() - ptm

out.df <- bind_rows(out.tmp) %>% 
  mutate(YEAR = year(parse_date_time(
    as.character(as.data.frame(ct_shape_1980_county)[1,"DECADE"])
    ,"Y!")))

# out.df.hold2 <- out.df
out.df <- left_join(
  out.df
  ,curr_pop %>% 
    select(GISJOIN,Year,Population)
)


pop_1980_ct <- out.df
pop_1980_nbrhd <- ungroup(
  out.df %>% 
    mutate(Population=as.numeric(Population)
           ,prop=as.numeric(prop)
           ,pop_adj = Population * prop) %>% 
    group_by(neighborhood) %>% 
    summarize(
      # Population = sum(Population*prop,na.rm=T)
              Population = sum(pop_adj,na.rm=T)
              ,Year = Year[1]
    )
)

pop_1980_nbrhd <- left_join(pop_1980_nbrhd
          ,ungroup(
            ungroup(
              out.df %>% 
                mutate(Borough = as.character(
                  factor(ICPSRNAM
                         ,levels=c("NEW YORK","BRONX","KINGS","QUEENS","RICHMOND")
                         ,labels=c("Mahattan","Bronx","Brooklyn","Queens","Staten Island"))
                )) %>%
                group_by(neighborhood,Borough) %>% 
                summarize(count = n())
            ) %>% 
              group_by(neighborhood) %>% 
              top_n(1,count) 
          ) %>% 
            select(-count)
          ,by="neighborhood"
) %>% 
  select(neighborhood,Borough,Population,Year)


# getwd()
setwd("/Users/billbachrach/Dropbox (hodgeswardelliott)/hodgeswardelliott Team Folder/Teams/Data/Bill Bachrach/Data Sources/Census/Population Estimates/Historical")
write.csv(pop_1980_nbrhd,"neighborhood_populations_1980.csv",row.names=F)
write.csv(pop_1980_ct,"census_tract_populations_1980.csv",row.names=F)


sum(pop_1980_nbrhd[,"Population"])







## first determine what neighborhoods each CT intersects with 
# intersect.list <- st_intersects(ct_shape_2015,large.map,sparse=T)
intersect.list <- st_intersects(ct_shape_1990,large.map,sparse=T)


## Determine what proportion of each census tract lies in respective neighborhoods
# outer.len <- 100
outer.len <- length(intersect.list)
ptm <- proc.time()
cl <- makeCluster(detectCores()-1,type="FORK")

# x <- 1
# out.tmp <- lapply(1:outer.len, function(x){
out.tmp <- parLapply(cl,1:outer.len, function(x){
  # cat("begin ",x,"out of ",outer.len,"\n")
  
  ## iteratively create dataframes of individual census tract and overlapping neighborhoods
  ct.curr <- ct_shape_1990[x,]
  nbrhd_curr.df <- large.map[intersect.list[[x]],]
  
  ## area of census tract 
  ct.area <- st_area(ct.curr)
  
  ## some CTs don't intersect w/ any neighborhoods, setting those values to NA 
  if(length(intersect.list[[x]])>0){
    
    overlap.len <- nrow(nbrhd_curr.df)
    
    area.vec <- unlist(lapply(1:overlap.len,function(z){
      # cat(z,"\n")
      suppressWarnings(
        try(
          st_area(st_intersection(nbrhd_curr.df[z,],ct.curr))
          ,silent=T
        )
      )
    }
    )
    )
    
    ## not sure why, but throwing an occasional warning -
    ### Error in (function (classes, fdef, mtable)  : 
    ###             unable to find an inherited method for function ‘areaPolygon’ for signature ‘"SpatialLines"’
    ## catching as try error and then setting to NA
    area.vec[grepl("[[:alpha:]]",area.vec)] <- NA
    area.vec <- as.numeric(area.vec)
    
    area.props_raw <- area.vec/ct.area
    area.props <- area.vec/sum(area.vec,na.rm=T)
    
    prop_sum_raw <- sum(as.numeric(area.props_raw),na.rm=T)
    prop_sum_adj <- sum(as.numeric(area.props),na.rm=T)
    
    
    out <- as.data.frame(
      cbind(
        rep(
          as.character(as.data.frame(ct.curr)[,"GISJOIN"])
          ,overlap.len
        )
        ,rep(
          as.character(as.data.frame(ct.curr)[,"ICPSRNAM"])
          ,overlap.len
        )
        ,as.character(as.data.frame(nbrhd_curr.df)[,"neighborhood"])
        ,area.props_raw
        ,area.props
        ,rep(prop_sum_raw
             ,overlap.len)
        ,rep(prop_sum_adj
             ,overlap.len)
        ,rep(x
             ,overlap.len)
      )
      ,stringsAsFactors=F
    )
    
  } else{
    out <- as.data.frame(
      cbind(
        as.character(as.data.frame(ct.curr)[,"GISJOIN"])
        ,as.character(as.data.frame(ct.curr)[,"ICPSRNAM"])
        ,NA
        ,NA
        ,NA
        ,NA
        ,NA
        ,x
      )
      ,stringsAsFactors=F
    )
  }
  
  colnames(out) <- c("GISJOIN","ICPSRNAM","neighborhood","prop_raw","prop","sum_props_raw","sum_props_adj","order")
  return(out)
}
)

stopCluster(cl)
calc.time <- proc.time() - ptm

out.df <- bind_rows(out.tmp) %>% 
  mutate(YEAR = year(parse_date_time(
    as.character(as.data.frame(ct_shape_1990_county)[1,"DECADE"])
    ,"Y!")))

# out.df.hold2 <- out.df
out.df <- left_join(
  out.df
  ,ct_pop_nyc_1990 %>% 
    select(GISJOIN,Year,Population)
)


pop_1990_ct <- out.df
pop_1990_nbrhd <- ungroup(
  out.df %>% 
    mutate(Population=as.numeric(Population)
           ,prop=as.numeric(prop)
           ,pop_adj = Population * prop) %>% 
    group_by(neighborhood) %>% 
    summarize(
      # Population = sum(Population*prop,na.rm=T)
      Population = sum(pop_adj,na.rm=T)
      ,Year = Year[1]
    )
)

pop_1990_nbrhd <- left_join(pop_1990_nbrhd
                            ,ungroup(
                              ungroup(
                                out.df %>% 
                                  mutate(Borough = as.character(
                                    factor(ICPSRNAM
                                           ,levels=c("NEW YORK","BRONX","KINGS","QUEENS","RICHMOND")
                                           ,labels=c("Mahattan","Bronx","Brooklyn","Queens","Staten Island"))
                                  )) %>%
                                  group_by(neighborhood,Borough) %>% 
                                  summarize(count = n())
                              ) %>% 
                                group_by(neighborhood) %>% 
                                top_n(1,count) 
                            ) %>% 
                              select(-count)
                            ,by="neighborhood"
) %>% 
  select(neighborhood,Borough,Population,Year)


# getwd()
setwd("/Users/billbachrach/Dropbox (hodgeswardelliott)/hodgeswardelliott Team Folder/Teams/Data/Bill Bachrach/Data Sources/Census/Population Estimates/Historical")
write.csv(pop_1990_nbrhd,"neighborhood_populations_1990.csv",row.names=F)
write.csv(pop_1990_ct,"census_tract_populations_1990.csv",row.names=F)


sum(pop_1990_nbrhd[,"Population"])







## 2000 
## first determine what neighborhoods each CT intersects with 
# intersect.list <- st_intersects(ct_shape_2015,large.map,sparse=T)
intersect.list <- st_intersects(ct_shape_2000,large.map,sparse=T)


## Determine what proportion of each census tract lies in respective neighborhoods
# outer.len <- 100
outer.len <- length(intersect.list)
ptm <- proc.time()
cl <- makeCluster(detectCores()-1,type="FORK")

# x <- 1
# out.tmp <- lapply(1:outer.len, function(x){
out.tmp <- parLapply(cl,1:outer.len, function(x){
  # cat("begin ",x,"out of ",outer.len,"\n")
  
  ## iteratively create dataframes of individual census tract and overlapping neighborhoods
  ct.curr <- ct_shape_2000[x,]
  nbrhd_curr.df <- large.map[intersect.list[[x]],]
  
  ## area of census tract 
  ct.area <- st_area(ct.curr)
  
  ## some CTs don't intersect w/ any neighborhoods, setting those values to NA 
  if(length(intersect.list[[x]])>0){
    
    overlap.len <- nrow(nbrhd_curr.df)
    
    area.vec <- unlist(lapply(1:overlap.len,function(z){
      # cat(z,"\n")
      suppressWarnings(
        try(
          st_area(st_intersection(nbrhd_curr.df[z,],ct.curr))
          ,silent=T
        )
      )
    }
    )
    )
    
    ## not sure why, but throwing an occasional warning -
    ### Error in (function (classes, fdef, mtable)  : 
    ###             unable to find an inherited method for function ‘areaPolygon’ for signature ‘"SpatialLines"’
    ## catching as try error and then setting to NA
    area.vec[grepl("[[:alpha:]]",area.vec)] <- NA
    area.vec <- as.numeric(area.vec)
    
    area.props_raw <- area.vec/ct.area
    area.props <- area.vec/sum(area.vec,na.rm=T)
    
    prop_sum_raw <- sum(as.numeric(area.props_raw),na.rm=T)
    prop_sum_adj <- sum(as.numeric(area.props),na.rm=T)
    
    
    out <- as.data.frame(
      cbind(
        rep(
          as.character(as.data.frame(ct.curr)[,"GISJOIN"])
          ,overlap.len
        )
        ,rep(
          as.character(as.data.frame(ct.curr)[,"ICPSRNAM"])
          ,overlap.len
        )
        ,as.character(as.data.frame(nbrhd_curr.df)[,"neighborhood"])
        ,area.props_raw
        ,area.props
        ,rep(prop_sum_raw
             ,overlap.len)
        ,rep(prop_sum_adj
             ,overlap.len)
        ,rep(x
             ,overlap.len)
      )
      ,stringsAsFactors=F
    )
    
  } else{
    out <- as.data.frame(
      cbind(
        as.character(as.data.frame(ct.curr)[,"GISJOIN"])
        ,as.character(as.data.frame(ct.curr)[,"ICPSRNAM"])
        ,NA
        ,NA
        ,NA
        ,NA
        ,NA
        ,x
      )
      ,stringsAsFactors=F
    )
  }
  
  colnames(out) <- c("GISJOIN","ICPSRNAM","neighborhood","prop_raw","prop","sum_props_raw","sum_props_adj","order")
  return(out)
}
)

stopCluster(cl)
calc.time <- proc.time() - ptm

out.df <- bind_rows(out.tmp) %>% 
  mutate(YEAR = year(parse_date_time(
    as.character(as.data.frame(ct_shape_2000_county)[1,"DECADE"])
    ,"Y!")))

# out.df.hold2 <- out.df
out.df <- left_join(
  out.df
  ,ct_pop_nyc_2000 %>% 
    select(GISJOIN,Year,Population)
)


pop_2000_ct <- out.df
pop_2000_nbrhd <- ungroup(
  out.df %>% 
    mutate(Population=as.numeric(Population)
           ,prop=as.numeric(prop)
           ,pop_adj = Population * prop) %>% 
    group_by(neighborhood) %>% 
    summarize(
      # Population = sum(Population*prop,na.rm=T)
      Population = sum(pop_adj,na.rm=T)
      ,Year = Year[1]
    )
)

pop_2000_nbrhd <- left_join(pop_2000_nbrhd
                            ,ungroup(
                              ungroup(
                                out.df %>% 
                                  mutate(Borough = as.character(
                                    factor(ICPSRNAM
                                           ,levels=c("NEW YORK","BRONX","KINGS","QUEENS","RICHMOND")
                                           ,labels=c("Mahattan","Bronx","Brooklyn","Queens","Staten Island"))
                                  )) %>%
                                  group_by(neighborhood,Borough) %>% 
                                  summarize(count = n())
                              ) %>% 
                                group_by(neighborhood) %>% 
                                top_n(1,count) 
                            ) %>% 
                              select(-count)
                            ,by="neighborhood"
) %>% 
  select(neighborhood,Borough,Population,Year)


# getwd()
setwd("/Users/billbachrach/Dropbox (hodgeswardelliott)/hodgeswardelliott Team Folder/Teams/Data/Bill Bachrach/Data Sources/Census/Population Estimates/Historical")
write.csv(pop_2000_nbrhd,"neighborhood_populations_2000.csv",row.names=F)
write.csv(pop_2000_ct,"census_tract_populations_2000.csv",row.names=F)


sum(pop_2000_nbrhd[,"Population"])


View(pop_2000_nbrhd)





## 2010 
## first determine what neighborhoods each CT intersects with 
# intersect.list <- st_intersects(ct_shape_2015,large.map,sparse=T)
intersect.list <- st_intersects(ct_shape_2010,large.map,sparse=T)

## Determine what proportion of each census tract lies in respective neighborhoods
# outer.len <- 100
outer.len <- length(intersect.list)
ptm <- proc.time()
# cl <- makeCluster(detectCores()-1,type="FORK")

# x <- 1
out.tmp <- lapply(1:outer.len, function(x){
# out.tmp <- parLapply(cl,1:outer.len, function(x){
  cat("begin ",x,"out of ",outer.len,"\n")
  
  ## iteratively create dataframes of individual census tract and overlapping neighborhoods
  ct.curr <- ct_shape_2010[x,]
  nbrhd_curr.df <- large.map[intersect.list[[x]],]
  
  ## area of census tract 
  ct.area <- st_area(ct.curr)
  
  ## some CTs don't intersect w/ any neighborhoods, setting those values to NA 
  if(length(intersect.list[[x]])>0){
    
    overlap.len <- nrow(nbrhd_curr.df)
    
    area.vec <- unlist(lapply(1:overlap.len,function(z){
      # cat(z,"\n")
      suppressWarnings(
        try(
          st_area(st_intersection(nbrhd_curr.df[z,],ct.curr))
          ,silent=T
        )
      )
    }
    )
    )
    
    ## not sure why, but throwing an occasional warning -
    ### Error in (function (classes, fdef, mtable)  : 
    ###             unable to find an inherited method for function ‘areaPolygon’ for signature ‘"SpatialLines"’
    ## catching as try error and then setting to NA
    area.vec[grepl("[[:alpha:]]",area.vec)] <- NA
    area.vec <- as.numeric(area.vec)
    
    area.props_raw <- area.vec/ct.area
    area.props <- area.vec/sum(area.vec,na.rm=T)
    
    prop_sum_raw <- sum(as.numeric(area.props_raw),na.rm=T)
    prop_sum_adj <- sum(as.numeric(area.props),na.rm=T)
    
    
    out <- as.data.frame(
      cbind(
        rep(
          as.character(as.data.frame(ct.curr)[,"GISJOIN"])
          ,overlap.len
        )
        ,rep(
          as.character(as.data.frame(ct.curr)[,"COUNTYFP10"])
          ,overlap.len
        )
        ,as.character(as.data.frame(nbrhd_curr.df)[,"neighborhood"])
        ,area.props_raw
        ,area.props
        ,rep(prop_sum_raw
             ,overlap.len)
        ,rep(prop_sum_adj
             ,overlap.len)
        ,rep(x
             ,overlap.len)
      )
      ,stringsAsFactors=F
    )
    
  } else{
    out <- as.data.frame(
      cbind(
        as.character(as.data.frame(ct.curr)[,"GISJOIN"])
        ,as.character(as.data.frame(ct.curr)[,"COUNTYFP10"])
        ,NA
        ,NA
        ,NA
        ,NA
        ,NA
        ,x
      )
      ,stringsAsFactors=F
    )
  }
  
  colnames(out) <- c("GISJOIN","COUNTYFP10","neighborhood","prop_raw","prop","sum_props_raw","sum_props_adj","order")
  return(out)
}
)

stopCluster(cl)
calc.time <- proc.time() - ptm

out.df <- bind_rows(out.tmp) %>% 
  mutate(YEAR = year(parse_date_time(
    as.character(2010)
    ,"Y!")))

# out.df.hold2 <- out.df
out.df <- left_join(
  out.df
  ,ct_pop_nyc_2010 %>% 
    select(GISJOIN,Year,Population,COUNTY)
)

# View(ct_pop_nyc_2010)
# View(pop_2010_ct)
pop_2010_ct <- out.df

# tmp <- left_join(pop_2010_ct %>% 
#                    select(GISJOIN,COUNTYFP10)
#                  ,ct_pop_nyc_2010 %>% 
#                    select(GISJOIN,COUNTY)
#                  ,by="GISJOIN") %>%
#   filter(!duplicated(COUNTY))

pop_2010_nbrhd <- ungroup(
  out.df %>% 
    mutate(Population=as.numeric(Population)
           ,prop=as.numeric(prop)
           ,pop_adj = Population * prop) %>% 
    group_by(neighborhood) %>% 
    summarize(
      # Population = sum(Population*prop,na.rm=T)
      Population = sum(pop_adj,na.rm=T)
      ,Year = Year[1]
    )
)

pop_2010_nbrhd <- left_join(pop_2010_nbrhd
                            ,ungroup(
                              ungroup(
                                out.df %>% 
                                  mutate(Borough = as.character(
                                    factor(COUNTY
                                           ,levels=c("New York County","Bronx County","Kings County","Queens County","Richmond County")
                                           ,labels=c("Mahattan","Bronx","Brooklyn","Queens","Staten Island"))
                                  )) %>%
                                  group_by(neighborhood,Borough) %>% 
                                  summarize(count = n())
                              ) %>% 
                                group_by(neighborhood) %>% 
                                top_n(1,count) 
                            ) %>% 
                              select(-count)
                            ,by="neighborhood"
) %>% 
  select(neighborhood,Borough,Population,Year)


# getwd()
setwd("/Users/billbachrach/Dropbox (hodgeswardelliott)/hodgeswardelliott Team Folder/Teams/Data/Bill Bachrach/Data Sources/Census/Population Estimates/Historical")
write.csv(pop_2010_nbrhd,"neighborhood_populations_2010.csv",row.names=F)
write.csv(pop_2010_ct,"census_tract_populations_2010.csv",row.names=F)

# colnames(pop_1980_nbrhd)
# colnames(pop_1990_nbrhd)
# colnames(pop_2000_nbrhd)
# colnames(pop_2010_nbrhd)
pop_1980_2010_nbrhd <- bind_rows(pop_1980_nbrhd
                                 ,pop_1990_nbrhd
                                 ,pop_2000_nbrhd
                                 ,pop_2010_nbrhd)


saveRDS(pop_1980_2010_nbrhd,"nbrhd_pops_1980_2010.rds")





View(pop_1980_2010_nbrhd)











## remove rows without an identifier and change identifier name to "id"
tmp.df <- out.n %>% 
  filter(!is.na(Neighborhood) & Neighborhood != "Manhattan") %>% 
  rename(id = Neighborhood)
## combine toy data with shapefile dataframe
# gg.df <- merge(nyc.nbrhds,tmp.df,by.x='id') %>% 
#   mutate(Ratio= ifelse(Ratio>.8,NA,Ratio*100))

# gg.df <- merge(nyc.pedia,tmp.df,by.x='id')

gg.df <- nyc.pedia

for(i in 2:length(colnames(tmp.df))){
  gg.df[,colnames(tmp.df)[i]] <- NA
}

ids <- unique(nyc.pedia[,"id"])
# gg.df[,"value"] <- NA
for(i in 1:length(ids)){
  restr.gg <- gg.df[,"id"] == ids[i]
  restr.tmp <- tmp.df[,"id"] == ids[i]
  if(sum(restr.tmp)==1){
    for(j in 2:length(colnames(tmp.df))){
      gg.df[restr.gg,colnames(tmp.df)[j]] <- tmp.df[restr.tmp,colnames(tmp.df)[j]]
    }
  }
  cat(i,"\n")
}

gg.df <- filter(gg.df,BoroCode != 5)

# nyc.box <- make_bbox(gg.df[,"long"],gg.df[,"lat"]
#                      # ,f=-1
# )

mapImage.nyc.s.terline <- get_map(location = "Brooklyn",
                                  maptype="terrain-lines",
                                  source = "stamen",
                                  color = "bw")




## condo / coop proportions 
condo.nbrhd.p <- ggmap(mapImage.nyc.s.terline
                       ,extent='normal') + 
  geom_sf(tmp)
  
  geom_polygon(aes(fill=Condo_Coop_Ratio,x=long,y=lat,group=group),
               data=gg.df,
               alpha=0.8,
               color="black",
               size= 0.2) + 
  labs(x="",
       y="",
       fill="% Condo",
       title="Condos as a Percentage of All \nOwner Occupied Units by Neighborhood",
       caption="Source:\nDOF PLUTO Database; DOF Property Assessment Data; \nDCP Property Address Directory; Zillow") +
  scale_x_continuous(labels = NULL, limits=c(nyc.box["left"],nyc.box["right"])) +
  scale_y_continuous(labels = NULL, limits=c(nyc.box["bottom"],nyc.box["top"])) +
  # annotate('rect', xmin=x.min, ymin=y.min, xmax=x.max, ymax=y.max, color= "black", fill="grey80") +
  # annotate("text", x=xcenter, y=ycenter, label = "Morningside Heights", colour = I("black"), size = 3) +
  # annotate("segment", x=xcenter, xend=ms.center[1], y=y.min, yend=ms.center[2],
  #          colour=I("black"), arrow = arrow(length=unit(0.1,"cm")), size = .75) +
  scale_fill_continuous(labels = scales::percent) + 
  theme_hwe() + 
  theme(panel.grid.major = element_blank(),
        plot.caption = element_text(hjust=0,vjust=15),
        plot.title = element_text(vjust=-10)
  )

condo.nbrhd.p

save.image("/Users/billbachrach/Dropbox (hodgeswardelliott)/hodgeswardelliott Team Folder/Teams/Data/Bill Bachrach/Data Sources/Census/Shapefileshistorical_shapefile_merging.RData")




