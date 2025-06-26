#------------------------------------#
### Bison Space-Use Analysis       ###
#------------------------------------#
## R. Ritson, 2/17/2025            ##
#------------------------------------#
### Utah Additions: Classify Seasons ###
#------------------------------------#
# Load packages
require(sf)
require(terra)
library(data.table)
require(lubridate)
require(dplyr)

# Load function
devtools::source_url("https://github.com/r2j2ritson/RSpatial_Wildlife/blob/main/R/coords_as_sf.R?raw=TRUE")

# Load data
fp <- "C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/"

## GPS
bison_gps <- data.table::fread(paste0(fp,"Data/","Master_Bison_GPS_02172025.csv")) 
bison_gps$doy<-as.numeric(format(bison_gps$Timestamp.std2,"%j"))
anyNA(bison_gps$doy) #FALSE! (no nas)
bison_gps <- bison_gps[order(bison_gps$ID,bison_gps$Timestamp.std2),]
bison_gps <- bison_gps[!duplicated(bison_gps[,ID,Timestamp.std2])]

## VHF
bison_vhf <- data.table::fread(paste0(fp,"Data/","Bison_VHF_CO_2.csv"))
bison_vhf$Timestamp.std2<-as.POSIXct(bison_vhf$Date_Time,format="%m/%d/%Y %H:%M", tz = "UTC")
bison_vhf$doy<-as.numeric(format(bison_vhf$Timestamp.std2,"%j"))
anyNA(bison_vhf$doy) #FALSE! (no nas)

## Combine Location datasets
bison <- bison_gps %>%
  dplyr::mutate(Type = "GPS") %>%
  dplyr::mutate(Study_Area = `Study area`) %>%
  dplyr::select(ID,tag,sex,Study_Area,Date,Time,Timezone,Timestamp.std2,year,doy,Type,long,lat,DOP)

vhf_sf <- coords_as_sf(bison_vhf,x="UTM east",y="UTM north",crs="epsg:32613") 
vhf_longlat <- sf::st_transform(vhf_sf,"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
bison_vhf <- bison_vhf %>%
  dplyr::mutate(Type = "VHF") %>%
  dplyr::mutate(Study_Area = "Medano-Zapata Ranch") %>%
  dplyr::mutate(Timezone = "MST") %>%
  dplyr::mutate(DOP = NA) %>%
  dplyr::mutate(year = lubridate::year(Timestamp.std2)) %>%
  dplyr::mutate(sex = ifelse(Sex == "Cow","F",
                             ifelse(Sex == "Bull","M",NA))) %>%
  dplyr::mutate(long = sf::st_coordinates(vhf_longlat)[,1],
                lat = sf::st_coordinates(vhf_longlat)[,2]) %>%
  dplyr::select(tag,sex,Study_Area,Date,Time,Timezone,Timestamp.std2,year,doy,Type,long,lat,DOP)

out <- NULL
for(i in 1:length(unique(bison_vhf$tag))){
  tmp <- bison_vhf[which(bison_vhf$tag == unique(bison_vhf$tag)[i]),]
  tmp$ID <- i + max(bison_gps$ID)
  out <- rbind(out,tmp)
}
bison_vhf <- out
bison <- rbind(bison,bison_vhf)
bison <- bison[order(bison$ID,bison$Timestamp.std2),]


rm(bison_gps,bison_vhf,vhf_sf,vhf_longlat,i,tmp,out)


foo <- which(bison$Timestamp.std2[1:(nrow(bison)-1)] != bison$Timestamp.std2[2:(nrow(bison))] & 
               bison$ID[1:(nrow(bison)-1)] == bison$ID[2:(nrow(bison))])

bison <- bison[foo,]

data.table::fwrite(bison,paste0(fp,"Data/","Master_All_Bison_02172025.csv"))

foo <- bison %>%
  dplyr::group_by(Study_Area) %>%
  dplyr::summarise(N_IDs = length(unique(ID)))
foo

bison_vect <- coords_as_sf(bison,x="long",y="lat",crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% terra::vect(.)

# Load rasters (reporjected)
eost_avg <-terra::rast("D:/Bison_thesis/ArcGIS/Bison_GISlayers/NDVI/Reprojected/strt_gu_avg") #file names messed up!!
terra::set.names(eost_avg,"eost_avg") 
eost_avg
sost_avg <-terra::rast("D:/Bison_thesis/ArcGIS/Bison_GISlayers/NDVI/Reprojected/end_gu_avg") #file names messed up!!
terra::set.names(sost_avg,"sost_avg") 
sost_avg
max_avg <-terra::rast("D:/Bison_thesis/ArcGIS/Bison_GISlayers/NDVI/Reprojected/max_gu_avg")
terra::set.names(max_avg,"max_avg") 
max_avg

# Extract values
bison_vect <- terra::project(bison_vect,sost_avg) #reproject coordinate system to match rasters!!!
identical(terra::crs(bison_vect),terra::crs(eost_avg)) #TRUE
identical(terra::crs(bison_vect),terra::crs(sost_avg)) #TRUE
identical(terra::crs(bison_vect),terra::crs(max_avg)) #TRUE

#terra::plot(sost_avg)
#terra::plot(bison_vect,add=T)
# 
#terra::plot(eost_avg)
#terra::plot(bison_vect,add=T)
# 
#terra::plot(max_avg)
#terra::plot(bison_vect,add=T)

bison_vect<- terra::extract(sost_avg,bison_vect,method='simple',bind=T)
head(bison_vect)
summary(bison_vect$sost_avg)

bison_vect<- terra::extract(eost_avg,bison_vect,method='simple',bind=T)
head(bison_vect)
summary(bison_vect$eost_avg)

bison_vect<- terra::extract(max_avg,bison_vect,method='simple',bind=T)
head(bison_vect)
summary(bison_vect$max_avg)

bison_vect$sost_avg <- ifelse(bison_vect$sost_avg > 366 | bison_vect$sost_avg < 1,NA,bison_vect$sost_avg)
summary(bison_vect$sost_avg)

bison_vect$eost_avg <- ifelse(bison_vect$eost_avg > 366 | bison_vect$eost_avg < 1,NA,bison_vect$eost_avg)
summary(bison_vect$eost_avg)

bison_vect$max_avg <- ifelse(bison_vect$max_avg > 366 | bison_vect$max_avg < 1,NA,bison_vect$max_avg)
summary(bison_vect$max_avg)

bison_season_avg <- bison_vect %>%
  as.data.frame(.) %>%
  dplyr::group_by(Study_Area) %>%
  dplyr::summarise(SOST_avg = round(mean(unique(sost_avg),na.rm=T)), #average start of season for vicinity (don't weigh by frequency)
                   EOST_avg = round(mean(unique(eost_avg),na.rm=T)), #average end of season for vicinity (don't weigh by frequency)
                   Max_avg = round(mean(unique(max_avg),na.rm=T)), #average max of season for vicinity (don't weigh by frequency)
                   Max_Start = Max_avg - 14, #two weeks leading up to average max ndvi
                   Max_End =  Max_avg + 14) #two weeks following average max ndvi
head(bison_season_avg)
data.table::fwrite(bison_season_avg,paste0(fp,"Data/","SeasonDates_NDVI.csv"))

rm(eost_avg,max_avg,sost_avg)

#Classify Growing and Focal Seasons
bison <- as.data.frame(bison_vect) %>%
  dplyr::left_join(.,bison_season_avg,by="Study_Area") %>%
  dplyr::mutate(season = ifelse(doy >= SOST_avg & doy <= EOST_avg,"Growing","Nongrowing"),
                season_max = ifelse(doy >= Max_Start & doy <= Max_End,"True","False"))
head(bison)
unique(bison$season)
unique(bison$season_max)

rm(bison_vect)

#Summarize by year and season
bison$ID2<-ifelse(bison$Type == "GPS", paste0(bison$ID,"_",bison$year), bison$ID)
bison_grow<-subset(bison,bison$season=="Growing")
bison_focal<-subset(bison,bison$season_max=="True")

bison_grow_summary<-bison_grow%>%group_by(ID2,tag,Study_Area,year)%>%summarise(n.locs=n(), n.days=(max(doy)-min(doy)))
bison_focal_summary<-bison_focal%>%group_by(ID2,tag,Study_Area,year)%>%summarise(n.locs=n(), n.days=(max(doy)-min(doy)))

bison_season_avg$Grow_Length<-(bison_season_avg$EOST_avg - bison_season_avg$SOST_avg)
bison_season_avg$min.days<-(0.6*bison_season_avg$Grow_Length) # @ least 60% of the growing season should be sampled [gains 23 additional samples from 80%]
bison_season_avg$min.days.ng<-(0.6*(365-bison_season_avg$Grow_Length))
bison_season_avg$min.days.focal<-(0.6*28)

bison_season_avg
data.table::fwrite(bison_season_avg,"C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/SeasonDates_NDVI.csv")

##Subset Individuals with sufficient observations
## Annual
bison_annual <- bison
bison_annual_summary <- bison %>%
  mutate(Month = lubridate::month(Timestamp.std2)) %>%
  group_by(ID2,tag,Study_Area,Type) %>% 
  summarise(n.locs=n(), n.days=(max(doy)-min(doy)), n_month = length(unique(Month)))
bison_annual_summary

b_annual_gps <- bison_annual_summary %>%
  dplyr::filter(Type == "GPS") %>%
  dplyr::filter(n_month >=7) #7 month coverage minimum to be annual (VHF min)

b_annual_vhf <- bison_annual_summary %>%
  dplyr::filter(Type == "VHF")

b_annual <- rbind(b_annual_gps,b_annual_vhf)
bison_annual <- bison_annual[which(bison_annual$ID2 %in% b_annual$ID2),]
bison_annual$season <- "Annual"
rm(b_annual,b_annual_gps,b_annual_vhf,bison_annual_summary)

##Growing Season
b_grow <- bison_grow_summary %>%
  dplyr::left_join(.,bison_season_avg,by="Study_Area") %>%
  dplyr::filter(n.days >= min.days)

bison_grow <- bison_grow[which(bison_grow$ID2 %in% b_grow$ID2),]
head(bison_grow)

rm(b_grow,bison_grow_summary)

##Focal Season
b_focal <- bison_focal_summary %>%
  dplyr::left_join(.,bison_season_avg,by="Study_Area") %>%
  dplyr::filter(n.days >= min.days.focal)
bison_focal <- bison_focal[which(bison_focal$ID2 %in% b_focal$ID2),]
head(bison_focal)
bison_focal$season <- "Focal"
rm(b_focal,bison_focal_summary)

##Non-Growing Season
bison <- bison %>%
  dplyr::arrange(ID,Timestamp.std2)

out <- NULL
for(i in 1:length(unique(bison$ID))){
  if(i == 1){
    tmp <- bison[which(bison$ID == unique(bison$ID)[i]),]
    foo <- which((tmp$season[1:(nrow(tmp)-1)] != tmp$season[2:(nrow(tmp))]))
    tmp$firstRec <- rep(0,nrow(tmp)) #populate new column with "0"s
    tmp$firstRec[foo+1] <- 1 #First record of each indiv gets a "1"
    tmp$firstRec[1] <- 1
    tmp$grp <- cumsum(tmp$firstRec)
    g <- max(tmp$grp)
  }else{
    tmp <- bison[which(bison$ID == unique(bison$ID)[i]),]
    foo <- which((tmp$season[1:(nrow(tmp)-1)] != tmp$season[2:(nrow(tmp))]))
    tmp$firstRec <- rep(0,nrow(tmp)) #populate new column with "0"s
    tmp$firstRec[foo+1] <- 1 #First record of each indiv gets a "1"
    tmp$firstRec[1] <- g+1
    tmp$grp <- cumsum(tmp$firstRec)
    g <- max(tmp$grp)
  }
  out <- rbind(out,tmp)
}
rm(foo,g,i,tmp)

out_ng <- out %>%
  dplyr::filter(Type == "GPS") %>%
  dplyr::filter(season == "Nongrowing") %>%
  dplyr::mutate(ID3 = paste0(ID,"_",grp)) %>%
  dplyr::select(ID3,grp) %>%
  unique()

bison_nongrow <- out %>%
  dplyr::filter(Type == "GPS") %>%
  dplyr::filter(season == "Nongrowing") %>%
  dplyr::inner_join(.,out_ng,by="grp")

bison_nongrow_summary <- bison_nongrow %>%
  group_by(ID3,tag,Study_Area) %>% 
  summarise(n.locs=n(), 
            min_day = min(Timestamp.std2),
            max_day = max(Timestamp.std2)) %>%
  dplyr::mutate(Dur = difftime(max_day,min_day,units="days")) %>%
  dplyr::mutate(n.days = ceiling(as.numeric(Dur)))

b_nongrow <- bison_nongrow_summary %>%
  dplyr::left_join(.,bison_season_avg,by="Study_Area") %>%
  dplyr::rowwise(.) %>%
  dplyr::filter(n.days >= 60)

bison_nongrow <- bison_nongrow[which(bison_nongrow$ID3 %in% b_nongrow$ID3),]
head(bison_nongrow)

rm(out_ng,b_nongrow,bison_nongrow_summary,bison_season_avg)

bison_nongrow$ID2 <- bison_nongrow$ID3
bison_nongrow$ID3 <- NULL
bison_nongrow$grp <- NULL
bison_nongrow$firstRec <- NULL
bison_all <- rbind(bison_annual,bison_grow,bison_nongrow,bison_focal)

#Export
data.table::fwrite(bison_all,file = paste0(fp,"Data/","BisonLocsAll_Seasonal_Master.csv"))
###

length(unique(bison_focal$ID2))
length(unique(bison_focal$Study_Area))
length(unique(bison_focal$season))
length(unique(bison_focal$Type))
#n=92
length(unique(bison_grow$ID2))
length(unique(bison_grow$Study_Area))
length(unique(bison_grow$season))
length(unique(bison_grow$Type))
#n=111
length(unique(bison_nongrow$ID2))
length(unique(bison_nongrow$Study_Area))
length(unique(bison_nongrow$season))
length(unique(bison_nongrow$Type))
#n=116
length(unique(bison_annual$ID2))
length(unique(bison_annual$Study_Area))
length(unique(bison_annual$season))
length(unique(bison_annual$Type))
#n=122
##
rm(list=ls())
