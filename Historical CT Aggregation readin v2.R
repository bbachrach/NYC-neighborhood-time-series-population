
# library(data.table)
library(tidyverse)
library(stringr)
library(sf)
library(dplyr)
# library(rgdal)
library(ggplot2)
# library(maptools)
library(sp)
# library(rgeos)
library(geojsonio)
# library(PBSmapping)
# library(broom)
library(parallel)
library(lubridate)

options(scipen=999)

source("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/useful functions/HWE_FUNCTIONS.r")
source("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/useful functions/-.R")
source("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/useful functions/useful minor functions.R")
source("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/useful functions/hwe colors.R")





# SECTION 1) BORO ---------------------------------------------------------

county_pop <- read_csv("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Population Estimates/nhgis0001_csv 2/nhgis0001_ts_nominal_cty_sub.csv")

# field descriptions from data dict ---------------------------------------
# AV0AA1970:   1970: Persons: Total
# AV0AA1980:   1980: Persons: Total
# AV0AA1990:   1990: Persons: Total
# AV0AA2000:   2000: Persons: Total
# AV0AA2010:   2010: Persons: Total
# AV0AA125:    2008-2012: Persons: Total
# AV0AA125M:   Margin of error: 2008-2012: Persons: Total

nyc_boro_pop <- county_pop %>% 
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


# boro_shape <- st_read("data/Shapefiles/boro_shapefile.json")
# boro_shape_with_pop <- left_join(boro_shape,nyc_boro_pop, by = c('BoroName'))
# st_write(boro_shape_with_pop,"data/population/data_with_shapefiles/boro_shape_with_pop.json", driver = "GeoJSON")


# SECTION 2) CENSUS TRACT -------------------------------------------------

tmp.file <- tempfile()
download.file("https://www.dropbox.com/s/1ary0s467jc3hsn/nhgis0001_ts_nominal_tract.csv?raw=1",tmp.file)

ct_pop <- read_csv(tmp.file)

unlink(tmp.file)

## county fp codes for nyc 
nyc_county_fp <- unique(as.data.frame(ct_pop %>% 
                                        filter(STATE=="New York"
                                               , COUNTY%in%c("New York County","Richmond County"
                                                             ,"Kings County","Queens County","Bronx County")
                                        )
                                      ,stringsAsFactors=F)[,"COUNTYFP"])

# scrub: 
ct_pop_nyc <- ct_pop %>% 
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

## 2011 to 2016

## download from dropbox
tmp.file <- tempfile()
download.file("https://www.dropbox.com/s/e4pr8jr7miavxa9/nhgis%20B01003%20Total%20Population%202011-2016.zip?raw=1",tmp.file)

## unzip
tmp.dir <- tempdir()
unzip(tmp.file
      ,exdir = tmp.dir
      )

unlink(tmp.file)


## read in csv files
working.dir <- list.dirs(path=tmp.dir)[grepl("_csv",list.dirs(path=tmp.dir))]
setwd(working.dir)

csv.filenames <- list.files()[grepl("\\.csv",list.files())]

csv.list <- lapply(csv.filenames, function(x){
  out <- read.csv(x
                  ,stringsAsFactors=F)
  return(out)
}
)

unlink(tmp.dir)

## name objects in list with year indicated in filename
tract_pattern_locations.vec <- str_locate(csv.filenames,"_tract")[,1]
names(csv.list) <- unlist(lapply(1:length(csv.filenames), function(x)
  str_sub(csv.filenames[x],start=tract_pattern_locations.vec[x]-4,-5)
))



# Mutate files in list ----------------------------------------------------

ct_pop_nyc_2011 <- csv.list[[names(csv.list)[grepl("2011",names(csv.list))]]] %>% 
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

ct_pop_nyc_2012 <- csv.list[[names(csv.list)[grepl("2012",names(csv.list))]]] %>% 
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

ct_pop_nyc_2013 <- csv.list[[names(csv.list)[grepl("2013",names(csv.list))]]] %>% 
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

ct_pop_nyc_2014 <- csv.list[[names(csv.list)[grepl("2014",names(csv.list))]]] %>% 
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

ct_pop_nyc_2015 <- csv.list[[names(csv.list)[grepl("2015",names(csv.list))]]] %>% 
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


# colnames(ct_pop_nyc_2016)
# View(ct_pop_nyc_2016)

ct_pop_nyc_2016 <- csv.list[[names(csv.list)[grepl("2016",names(csv.list))]]] %>% 
  filter(STATE=="New York"
         , COUNTY%in%c("New York County","Richmond County"
                       ,"Kings County","Queens County","Bronx County")
  ) %>% 
  mutate(Year = year(parse_date_time(
    gsub(".*-","",YEAR)
    ,"Y!"))
  ) %>% 
  rename(Population = AF2LE001
         ,STATEFP = STATEA
         ,MOE = AF2LM001) %>%
  select(STATEFP,GISJOIN,Year,Population,COUNTY)


ct_pops.list <- list("ct_pop11" = ct_pop_nyc_2011
                     ,"ct_pop12" = ct_pop_nyc_2012
                     ,"ct_pop13" = ct_pop_nyc_2013
                     ,"ct_pop14" = ct_pop_nyc_2014
                     ,"ct_pop15" = ct_pop_nyc_2015
                     ,"ct_pop16" = ct_pop_nyc_2016
                     )


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



# Shapefiles --------------------------------------------------------------
## census tract populations are required to limit the shapefiles to census tracts of interest
## hence shapefiles being put in the middle of the script



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

ct_shape_2011_tract <- semi_join(ct_shape_2011_tract
                                 ,ct_pop_nyc_2011
                                 ,by="GISJOIN"
)

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


# ACS ---------------------------------------------------------------------

## ACS 2011 
acs_pops_11 <- read.csv("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Population Estimates/ACS_11_5YR_DP05/ACS_11_5YR_DP05_with_ann.csv"
                        ,stringsAsFactors=F)
acs_pops_11 <- left_join(
  as.data.frame(ct_shape_2010) %>% 
    mutate(GEOID10 = as.character(GEOID10)
           ,GISJOIN = as.character(GISJOIN)) %>% 
    select(GEOID10,STATEFP10,GISJOIN)
  ,acs_pops_11[2:nrow(acs_pops_11),1:4] %>% 
    rename(GEOID10 = GEO.id2) %>% 
    mutate(GEOID10 = as.character(GEOID10)
           ,Year=year(parse_date_time(2011,"Y!"))
           ,Population = as.numeric(HC01_VC03))
  ,by="GEOID10"
) %>% 
  select(-HC01_VC03)

acs_pops_11 <- left_join(
  acs_pops_11
  ,ct_pop_nyc %>%
    filter(!is.na(GJOIN2010)) %>%
    rename(GISJOIN = GJOIN2010) %>%
    select(GISJOIN,COUNTY)
  ,by="GISJOIN"
)



## acs 2012
acs_pops_12 <- read.csv("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Population Estimates/ACS_12_5YR_DP05/ACS_12_5YR_DP05_with_ann.csv"
                        ,stringsAsFactors=F)
acs_pops_12 <- left_join(
  as.data.frame(ct_shape_2010) %>% 
    mutate(GEOID10 = as.character(GEOID10)
           ,GISJOIN = as.character(GISJOIN)) %>% 
    select(GEOID10,STATEFP10,GISJOIN)
  ,acs_pops_12[2:nrow(acs_pops_12),1:4] %>% 
    rename(GEOID10 = GEO.id2) %>% 
    mutate(GEOID10 = as.character(GEOID10)
           ,Year=year(parse_date_time(2012,"Y!"))
           ,Population = as.numeric(HC01_VC03))
  ,by="GEOID10"
) %>% 
  select(-HC01_VC03)

acs_pops_12 <- left_join(
  acs_pops_12
  ,ct_pop_nyc %>%
    filter(!is.na(GJOIN2010)) %>%
    rename(GISJOIN = GJOIN2010) %>%
    select(GISJOIN,COUNTY)
  ,by="GISJOIN"
)




## ACS 2013 
acs_pops_13 <- read.csv("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Population Estimates/ACS_13_5YR_DP05/ACS_13_5YR_DP05_with_ann.csv"
                        ,stringsAsFactors=F)
acs_pops_13 <- left_join(
  as.data.frame(ct_shape_2010) %>% 
    mutate(GEOID10 = as.character(GEOID10)
           ,GISJOIN = as.character(GISJOIN)) %>% 
    select(GEOID10,STATEFP10,GISJOIN)
  ,acs_pops_13[2:nrow(acs_pops_13),1:4] %>% 
    rename(GEOID10 = GEO.id2) %>% 
    mutate(GEOID10 = as.character(GEOID10)
           ,Year=year(parse_date_time(2013,"Y!"))
           ,Population = as.numeric(HC01_VC03))
  ,by="GEOID10"
) %>% 
  select(-HC01_VC03)

acs_pops_13 <- left_join(
  acs_pops_13
  ,ct_pop_nyc %>%
    filter(!is.na(GJOIN2010)) %>%
    rename(GISJOIN = GJOIN2010) %>%
    select(GISJOIN,COUNTY)
  ,by="GISJOIN"
)


## ACS 2014 
acs_pops_14 <- read.csv("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Population Estimates/ACS_14_5YR_DP05/ACS_14_5YR_DP05_with_ann.csv"
                        ,stringsAsFactors=F)
acs_pops_14 <- left_join(
  as.data.frame(ct_shape_2010) %>% 
    mutate(GEOID10 = as.character(GEOID10)
           ,GISJOIN = as.character(GISJOIN)) %>% 
    select(GEOID10,STATEFP10,GISJOIN)
  ,acs_pops_14[2:nrow(acs_pops_14),1:4] %>% 
    rename(GEOID10 = GEO.id2) %>% 
    mutate(GEOID10 = as.character(GEOID10)
           ,Year=year(parse_date_time(2014,"Y!"))
           ,Population = as.numeric(HC01_VC03))
  ,by="GEOID10"
) %>% 
  select(-HC01_VC03)

acs_pops_14 <- left_join(
  acs_pops_14
  ,ct_pop_nyc %>%
    filter(!is.na(GJOIN2010)) %>%
    rename(GISJOIN = GJOIN2010) %>%
    select(GISJOIN,COUNTY)
  ,by="GISJOIN"
)


## ACS 2015 
acs_pops_15 <- read.csv("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Population Estimates/ACS_15_5YR_DP05/ACS_15_5YR_DP05_with_ann.csv"
                        ,stringsAsFactors=F)
acs_pops_15 <- left_join(
  as.data.frame(ct_shape_2010) %>% 
    mutate(GEOID10 = as.character(GEOID10)
           ,GISJOIN = as.character(GISJOIN)) %>% 
    select(GEOID10,STATEFP10,GISJOIN)
  ,acs_pops_15[2:nrow(acs_pops_15),1:4] %>% 
    rename(GEOID10 = GEO.id2) %>% 
    mutate(GEOID10 = as.character(GEOID10)
           ,Year=year(parse_date_time(2015,"Y!"))
           ,Population = as.numeric(HC01_VC03))
  ,by="GEOID10"
) %>% 
  select(-HC01_VC03)

acs_pops_15 <- left_join(
  acs_pops_15
  ,ct_pop_nyc %>%
    filter(!is.na(GJOIN2010)) %>%
    rename(GISJOIN = GJOIN2010) %>%
    select(GISJOIN,COUNTY)
  ,by="GISJOIN"
)


## ACS 2016 
acs_pops_16 <- read.csv("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Population Estimates/ACS_16_5YR_DP05/ACS_16_5YR_DP05_with_ann.csv"
                        ,stringsAsFactors=F)
acs_pops_16 <- left_join(
  as.data.frame(ct_shape_2010) %>% 
    mutate(GEOID10 = as.character(GEOID10)
           ,GISJOIN = as.character(GISJOIN)) %>% 
    select(GEOID10,STATEFP10,GISJOIN)
  ,acs_pops_16[2:nrow(acs_pops_16),1:4] %>% 
    rename(GEOID10 = GEO.id2) %>% 
    mutate(GEOID10 = as.character(GEOID10)
           ,Year=year(parse_date_time(2016,"Y!"))
           ,Population = as.numeric(HC01_VC03))
  ,by="GEOID10"
) %>% 
  select(-HC01_VC03)

acs_pops_16 <- left_join(
  acs_pops_16
  ,ct_pop_nyc %>%
    filter(!is.na(GJOIN2010)) %>%
    rename(GISJOIN = GJOIN2010) %>%
    select(GISJOIN,COUNTY)
  ,by="GISJOIN"
)

acs.list <- list("acs11"=acs_pops_11
                 ,"acs12"=acs_pops_12
                 ,"acs13"=acs_pops_13
                 ,"acs14"=acs_pops_14
                 ,"acs15"=acs_pops_15
                 ,"acs16"=acs_pops_16
)

setwd("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Population Estimates/Historical")
saveRDS(ct_shape_1980_2010.list,"ct_shape_1980_2010.list.rds")
saveRDS(ct_shape_2011_2016.list,"ct_shape_2011_2016.list.rds")
saveRDS(ct_pops_census.list,"ct_pops_census.list.rds")
saveRDS(acs.list,"acs.list.rds")


## read in pedia cities 
# largeshape.url <- "http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1largecitiesnycneighborhoods.geojson"
# large.map <- geojson_read(as.location(largeshape.url),
#                           method="local",
#                           what="sp")
# large.map <- large.map %>% spTransform(CRS("+proj=longlat +datum=WGS84"))
# large.map <- st_as_sf(large.map)