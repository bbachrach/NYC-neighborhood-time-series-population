
rm(list=ls())
options(scipen=999)

## script takes previously compiled lists of population data and corresponding shapefiles and aggregates to neighborhood
## script further incorporates intercensal boro level population data 

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
library(lubridate)

options(scipen=999)

source("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/useful functions/HWE_FUNCTIONS.r")
source("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/useful functions/-.R")
source("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/useful functions/useful minor functions.R")
source("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/useful functions/hwe colors.R")



# Read in data ------------------------------------------------------------



setwd("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Population Estimates/Historical")
ct_shape_1980_2010.list <- readRDS("ct_shape_1980_2010.list.rds")
ct_shape_2011_2016.list <- readRDS("ct_shape_2011_2016.list.rds")
ct_pops_census.list <- readRDS("ct_pops_census.list.rds")
acs.list <- readRDS("acs.list.rds")



# load("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Population Estimates/Historical/acs_neighborhood_workspace_20170617.RData")
# population_nbrhd_boro_1970_2017 <- readRDS("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Population Estimates/Historical/neighborhood_boro_population_1970_2016.rds")


## read in pedia cities shapefiles 
largeshape.url <- "http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1largecitiesnycneighborhoods.geojson"
large.map <- geojson_read(as.location(largeshape.url),
                          method="local",
                          what="sp")
large.map <- large.map %>% spTransform(CRS("+proj=longlat +datum=WGS84"))
large.map <- st_as_sf(large.map)

## pull in the lists created in previous script 
# ct_shape_1980_2010.list
# ct_shape_2011_2016.list
# ct_pops_census.list
# acs.list


# Functions ---------------------------------------------------------------


###########################################################################
# Function for 1970-2010
###########################################################################

ct_prop.fun <- function(curr.shape,large.map=large.map,curr_pop){
  
  ## first determine what neighborhoods each CT intersects with 
  intersect.list <- st_intersects(curr.shape,large.map,sparse=T)
  
  
  ## Determine what proportion of each census tract lies in respective neighborhoods
  outer.len <- length(intersect.list)

    out.tmp <- lapply(1:outer.len, function(x){
  # out.tmp <- parLapply(cl,1:outer.len, function(x){
    cat("begin ",x,"out of ",outer.len,"\n")
    
    ## iteratively create dataframes of individual census tract and overlapping neighborhoods
    ct.curr <- curr.shape[x,]
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
  
  # stopCluster(cl)
  # calc.time <- proc.time() - ptm
  out.df <- bind_rows(out.tmp)
  
  out.df <- left_join(
    out.df
    ,curr_pop %>% 
      select(GISJOIN,Year,Population)
  )
  
  ## apply proportions to each census tract figure and tabulate 
  pop_curr_ct <- out.df
  pop_curr_nbrhd <- ungroup(
    out.df %>% 
      mutate(Population=as.numeric(Population)
             ,prop=as.numeric(prop)
             ,pop_adj = Population * prop) %>% 
      group_by(neighborhood) %>% 
      summarize(
        Population = sum(pop_adj,na.rm=T)
        ,Year = Year[1]
      )
  )
  
  ## dropping in boro
  pop_curr_nbrhd <- left_join(pop_curr_nbrhd
                              ,ungroup(
                                ungroup(
                                  out.df %>% 
                                    mutate(Borough = as.character(
                                      factor(ICPSRNAM
                                             ,levels=c("NEW YORK","BRONX","KINGS","QUEENS","RICHMOND")
                                             ,labels=c("Manhattan","Bronx","Brooklyn","Queens","Staten Island"))
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
  
  
  out.list <- list("pop_ct"= pop_curr_ct
                   ,"pop_nbrhd"=pop_curr_nbrhd)
  return(out.list)
  
}


###########################################################################
# Function for 2011-2015
###########################################################################


ct_prop_2011_2015.fun <- function(curr.shape,large.map=large.map,curr_pop){
  # curr_pop <- as.data.frame(curr_pop,stringsAsFactors=F)
  ## first determine what neighborhoods each CT intersects with 
  cat("intersect list\n")
  intersect.list <- st_intersects(curr.shape,large.map,sparse=T)
  
  
  ## Determine what proportion of each census tract lies in respective neighborhoods
  # outer.len <- 100
  outer.len <- length(intersect.list)
  # ptm <- proc.time()
  # cat("makecluster\n")
  # cl <- makeCluster(detectCores()-1,type="FORK")
  
  # x <- 1322
  out.tmp <- lapply(1:outer.len, function(x){
  # out.tmp <- parLapply(cl,1:outer.len, function(x){
    # cat("begin ",x,"out of ",outer.len,"\n")
    
    ## iteratively create dataframes of individual census tract and overlapping neighborhoods
    ct.curr <- curr.shape[x,]
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
            as.character(as.data.frame(ct.curr)[,"COUNTYFP"])
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
          ,as.character(as.data.frame(ct.curr)[,"COUNTYFP"])
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
  # stopCluster(cl)
  # calc.time <- proc.time() - ptm
  
  cat("bind rows\n")
  out.df <- bind_rows(out.tmp)
  out.df.hold <- out.df
  
  cat("join with out.df\n")
  
  out.df <- left_join(
    out.df
    ,curr_pop %>% 
      select(GISJOIN,Year,Population,COUNTY)
  )
  
  pop_curr_ct <- out.df

  cat("calculating for neighborhood\n")  
  pop_curr_nbrhd <- ungroup(
    ungroup(out.df) %>% 
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
  
  cat("joining with borough\n")  
  pop_curr_nbrhd <- left_join(pop_curr_nbrhd
                              ,ungroup(
                                ungroup(
                                  out.df %>% 
                                    mutate(Borough = as.character(
                                      factor(COUNTY
                                             ,levels=c("New York County","Bronx County","Kings County","Queens County","Richmond County")
                                             ,labels=c("Manhattan","Bronx","Brooklyn","Queens","Staten Island"))
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
  
  cat("putting in list\n")
  out.list <- list("pop_ct"= pop_curr_ct
                   ,"pop_nbrhd"=pop_curr_nbrhd
                   # ,"pop_nbrhd.full"=pop_nbrhd.full
                   )
  return(out.list)
}



# Run Functions -----------------------------------------------------------


cl <- makeCluster(detectCores()-1,type="FORK")
# census_out.pops <- lapply(1:length(ct_shape_1980_2010.list), function(y){
census_out.pops <- parLapply(cl,1:length(ct_shape_1980_2010.list), function(y){
  # acs_out.pops <- lapply(2:3, function(y){
  cat(y,"\n")
  out.final <- ct_prop.fun(curr.shape=ct_shape_1980_2010.list[[y]]
                           ,large.map=large.map
                           ,curr_pop=ct_pops_census.list[[y]])
  return(out.final)
}
)
stopCluster(cl)

  
census_nbrhd_pops.list <- lapply(census_out.pops, function(x){
  x[[2]]}
)

census_nbrhd_pops.df <- bind_rows(census_nbrhd_pops.list) %>%
  mutate(Year = year(parse_date_time(Year,"Y!"))
         ,Source="Census")


## Run function for ACS
cl <- makeCluster(detectCores()-1,type="FORK")
# acs_out.pops <- lapply(1:length(ct_shape_2011_2016.list), function(y){
acs_out.pops <- parLapply(cl,1:length(ct_shape_2011_2016.list), function(y){
  # acs_out.pops <- lapply(2:3, function(y){
  cat(y,"\n")
  out.final <- ct_prop_2011_2015.fun(curr.shape=ct_shape_2011_2016.list[[y]]
                                     ,large.map=large.map
                                     ,curr_pop=ct_pops.list[[y]])
  return(out.final)
}
)
stopCluster(cl)

acs_nbrhd_pops.list <- lapply(acs_out.pops, function(x){
  x[[2]]}
  )

acs_nbrhd_pops.df <- bind_rows(acs_nbrhd_pops.list) %>% 
  mutate(Year = year(parse_date_time(Year,"Y!"))
         ,Source="ACS")

## combine
pop_1980_2016 <- bind_rows(census_nbrhd_pops.df
  ,acs_nbrhd_pops.df
  ) %>% 
  arrange(Year,neighborhood) %>% 
  filter(Year>1970)



View(pop_1980_2016 %>%
       # group_by(neighborhood) %>%
       arrange(neighborhood,Year)
     )


# write.csv(pop_1980_2015,file="neighborhood_population_1980_2015_v2.csv",row.names=F)
# saveRDS(pop_1980_2015,file="neighborhood_population_1980_2015.rds")


############################################################
## adding in postcensal data for boroughs 
############################################################

## postcensal data downloaded from NYC opendata annual population estimates 
## https://data.ny.gov/Government-Finance/Annual-Population-Estimates-for-New-York-State-and/krt9-ym2k/data

## postcensal cleanse 
postcensal.df <- read.csv("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Population Estimates/PostCensal/Annual_Population_Estimates_for_New_York_State_and_Counties_1970-2017.csv"
               ,stringsAsFactors=F) %>% 
  filter(Geography %in% c(
    "Bronx County"
    ,"Kings County"
    ,"New York County"
    ,"Queens County"
    ,"Richmond County"
  ) 
  & !Program.Type %in% "Census Base Population") %>% 
  mutate(neighborhood="Borough"
         ,Borough = factor(Geography
                           ,levels=c(
                             "New York County"
                             ,"Bronx County"
                             ,"Kings County"
                             ,"Queens County"
                             ,"Richmond County"
                           )
                           ,labels= c("Manhattan"
                                      ,"Bronx"
                                      ,"Brooklyn"
                                      ,"Queens"
                                      ,"Staten Island")
         )
         ,Population=as.numeric(Population)
         ,Year = year(parse_date_time(Year,"Y!"))
         ,Source="NY_opendata_PostCensal"
  ) %>% 
  select(neighborhood,Borough,Population,Year,Source)

population_nbrhd_boro_1970_2017 <- bind_rows(
  postcensal.df %>% 
    arrange(desc(Year))
  ,pop_1980_2016 %>%
    arrange(desc(Year),neighborhood)
)


##########################################
## adjusting for low ACS estimates 
##########################################

tmp.df <- population_nbrhd_boro_1970_2017

tmp.df <- bind_rows(tmp.df
                    ,tmp.df %>% 
                      filter(Source %in% "Census") %>% 
                      group_by(Year,Borough) %>% 
                      summarize(Population = sum(Population)
                                ,Source = Source[1]
                                ,neighborhood = "Borough")
                    ,tmp.df %>% 
                      filter(Source %in% "ACS") %>% 
                      group_by(Year,Borough) %>% 
                      summarize(Population = sum(Population)
                                ,Source = Source[1]
                                ,neighborhood = "Borough")
)


## calculate difference between census and ACS for 2010 
acs_pops_10 <- read.csv("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Population Estimates/ACS_10_5YR_DP05/ACS_10_5YR_DP05_with_ann.csv"
                        ,stringsAsFactors=F)
acs_pops_10 <- acs_pops_10[2:nrow(acs_pops_10),1:4]

totalpop_2010.acs <- sum(as.numeric(acs_pops_10[,4]))
totalpop_2010.census <- sum(ct_pop_nyc_2010[,"Population"],na.rm=T)
# totalpop_2010.ic <- as.numeric(
#   tmp.df %>% 
#     filter(!Source %in% c("Census","ACS") & Year==2010) %>% 
#     summarize(totalpop = sum(Population))
# )


# totalpop_2010.ic - totalpop_2010.census
# totalpop_2010.ic - totalpop_2010.acs

acs.adjustment <- totalpop_2010.census/totalpop_2010.acs

population_nbrhd_boro_1970_2017 <- tmp.df %>% 
  mutate(Population.adj = ifelse(Source %in% "ACS"
                                 ,Population * acs.adjustment
                                 ,Population)
         )



####################################################################################
## Smoothing out the neighborhood level populations 
####################################################################################

tmp.df <- population_nbrhd_boro_1970_2017 %>% 
  filter(Source %in% c("ACS","Census") & !neighborhood %in% "Borough" & Year>=2010)

smooth.fun <- function(x,len){
  Population.smooth <- vector()
  Population.smooth[1] <- x[1]
  for(i in 2:len){
    Population.smooth[i] <- mean(x[(i-1):i])
  }
  return(Population.smooth)
}

tmp.df <- tmp.df %>% 
  group_by(neighborhood) %>%
  arrange(Year) %>% 
  mutate(Population.smooth = smooth.fun(Population.adj,n()))


population_nbrhd_boro_1970_2017 <- bind_rows(anti_join(population_nbrhd_boro_1970_2017
                              ,tmp.df
                              ,by=c("neighborhood","Year","Source")
                              ) %>% 
                      mutate(Population.smooth=Population.adj)
                    ,tmp.df
                    ) %>% 
  mutate(Population = round(Population)
         ,Population.adj = round(Population.adj)
         ,Population.smooth = round(Population.smooth))



setwd("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Population Estimates/Historical")
write.csv(population_nbrhd_boro_1970_2017,file="neighborhood_boro_population_1970_2017.csv",row.names=F)
saveRDS(population_nbrhd_boro_1970_2017,file="neighborhood_boro_population_1970_2017.rds")

save.image(
  paste(
    "acs_neighborhood_workspace_"
    ,format(
      Sys.time()
      ,"%Y%m%d_%H%M")
    ,".RData"
    ,sep=""
  )
)




## diagnostics and basic plotting 

# diff.summary <- tmp.df %>% 
#   filter(neighborhood %in% "Borough" & 
#            Year %in% c(1980,1990,2000,2010)) %>% 
#   group_by(Borough,Year) %>% 
#   summarize(diff = Population[Source %in% c("Census","ACS")] - Population[Source %in% "NY_opendata_PostCensal"]
#             ,prop_diff = diff/Population[Source %in% "Census"])
# 
# 
# gg.df1 <- tmp.df %>% 
#   filter(neighborhood %in% c("Upper West Side","Upper East Side","Flushing","Harlem","Bedford-Stuyvesant")
#          & Year>=1980
#          # & Borough %in% "Brooklyn"
#          )
# 
# 
# gg.df2 <- pop_1980_2015 %>% 
#   filter(neighborhood %in% c("Upper West Side","Upper East Side","Flushing","Harlem","Bedford-Stuyvesant")
#          & Year>=1980
#          # & Borough %in% "Brooklyn"
#   )
# population_nbrhd_boro_1970_2017
# 
# gg.df3 <- population_nbrhd_boro_1970_2017 %>% 
#   filter(neighborhood %in% c("Upper West Side","Upper East Side","Flushing","Harlem","Bedford-Stuyvesant")
#          & Year>=1980
#          # & Borough %in% "Brooklyn"
#   )
# 
# 
# View(tmp.df %>% 
#        filter(Year == 2011 & Source %in% "ACS" & neighborhood!="Borough") %>% 
#        arrange(desc(Population.adj)))
# 
# 
# ggplot(gg.df3, aes(x=Year,y=Population.smooth
#                   ,color=neighborhood
#                   )) + 
#   geom_line() + 
#   geom_line(data = gg.df3
#             ,aes(x=Year,y=Population.adj
#                  ,color=neighborhood
#             )
#             ,linetype=2
#             ) + 
#   theme_bw()