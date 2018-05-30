library(dplyr)

# supply.df <- readRDS("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/ad_hoc/Multifamily pipeline/Data/multifam_supply20170613_171943.rds") %>% 
supply.df <- readRDS("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Major projects/Multifamily Supply/Refresh/data/multifam_supply20180501_171329.rds") %>% 
  filter(!is.na(AREA)) %>% 
  mutate(
    # Year = as.character(Year)
    AREA = trimws(as.character(AREA))
  )

pop.df <- readRDS("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/Census/Population Estimates/Historical/neighborhood_boro_population_1970_2017.rds") %>% 
  mutate(Borough = ifelse(Borough %in% "Mahattan"
                          ,"Manhattan"
                          ,Borough)
  )

# View(supply.df)

pop.df <- pop.df %>% 
  mutate(AREA = trimws(ifelse(neighborhood=="Borough"
                              ,toupper(as.character(Borough))
                              ,as.character(neighborhood)
  ))
  ,AreaType = as.character(ifelse(neighborhood=="Borough"
                                  ,"Borough"
                                  ,"Neighborhood"
  )
  )
  )

## removing inter-censal and post-censal estimates where wehave actual census data 
pop.df.hold <- pop.df

pop.df <- anti_join(pop.df
                    ,left_join(pop.df %>% 
                                 filter(AreaType %in% "Borough") %>%
                                 group_by(Year) %>% 
                                 summarize(AreaCount = n()) %>% 
                                 filter(AreaCount > 5) %>% 
                                 select(Year)
                               ,pop.df %>% 
                                 filter(AreaType %in% "Borough"
                                        & Source %in% "NY_opendata_PostCensal")
                    )
                    ,by=c("Year","Source","AreaType")
)


pop_nyc.df <- pop.df %>% 
  filter(AreaType %in% "Borough") %>%
  group_by(Year) %>% 
  summarize(
    AREA = as.character("NYC")
    ,AreaType = as.character("City")
    ,Population = sum(Population)
    ,Population.adj = sum(Population.adj)
    ,Population.smooth = sum(Population.smooth)
  )


pop.df <- bind_rows(pop_nyc.df,
                    pop.df)


# unique(pop.df[,"AREA"])
# unique(supply.df[,"AREA"])
# 
# supply.df %>% 
#   summarize(sum(duplicated(
#     paste(AREA,AREA)
#   )))

popsup.df <- left_join(
  supply.df
  ,pop.df %>% 
    select(Year,AREA,Borough
           ,Population
           ,Population.adj
           ,Population.smooth)
  ,by=c("Year","AREA")
)

write.csv(popsup.df
          ,"/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Major projects/Multifamily Supply/Refresh/data/multifamily_supply_population_1980-2017.csv"
          ,row.names=F
          ,na=""
          )

?write.csv()

change.summary <- popsup.df %>% 
  group_by(AREA) %>% 
  summarize(
    Borough= Borough[1]
    ,pop_80 = Population.smooth[Year==1980]
    ,sup_80 = Rentals.adj[Year==1980]
    ,pop_15 = Population.smooth[Year==2015]
    ,sup_15 = Rentals.adj[Year==2015]
    ,abschange_pop = pop_15 - pop_80
    ,abschange_sup = sup_15 - sup_80
    ,perchange_pop = (pop_15 - pop_80) / pop_80
    ,perchange_sup = (sup_15 - sup_80) / sup_80
    ,change.metric = (pop_15/sup_15) - (pop_80/sup_80)
  )


View(change.summary %>% arrange(change.metric))

