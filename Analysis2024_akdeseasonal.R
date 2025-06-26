#------------------------------------#
### Bison Space-Use Analysis       ###
#------------------------------------#
## R. Ritson, 2/17/2025             ##
#------------------------------------#
### Utah Additions: AKDE homerange ###
#------------------------------------#
# Load packages
require(sf)
require(terra)
library(data.table)
require(lubridate)
require(ctmm)
require(move)
devtools::source_url("https://github.com/r2j2ritson/RSpatial_Wildlife/blob/main/R/coords_as_sf.R?raw=TRUE")

# Load data
bison <- data.table::fread("C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/Data/BisonLocsAll_Seasonal_Master.csv")

#Set file structure
dir.create('C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/Home Range')
dir.create('C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/Home Range/Annual_AKDE')
dir.create('C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/Home Range/Grow_AKDE')
dir.create('C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/Home Range/Nongrow_AKDE')
dir.create('C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/Home Range/Focal_AKDE')

## Calculate AKDE Home Range - Growing season ####
#Order observations
bison_grow <- bison %>%
  dplyr::filter(season == "Growing") %>%
  dplyr::arrange(ID,ID2,Timestamp.std2)
foo <- which(bison_grow$Timestamp.std2[1:(nrow(bison_grow)-1)] != bison_grow$Timestamp.std2[2:(nrow(bison_grow))] & 
               bison_grow$ID[1:(nrow(bison_grow)-1)] == bison_grow$ID[2:(nrow(bison_grow))])
bison_grow <- bison_grow[foo]
rm(foo)

#Reproject Coordinates
xy <- coords_as_sf(bison_grow,x="long",y="lat",crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
xy.aea <- sf::st_transform(xy,crs = "epsg:9822") #Albers equal area
bison_grow$long<-sf::st_coordinates(xy.aea)[,1]
bison_grow$lat<-sf::st_coordinates(xy.aea)[,2]
rm(xy,xy.aea)

#AKDE for loop
grow.akde.summary<-data.frame(matrix(nrow= length(unique(bison_grow$ID2)), ncol = 7))
colnames(grow.akde.summary)<- c("tag","ID2","Bandwidth","CI.Low","CI.Est","CI.High","Est.Units")
pb = txtProgressBar(min = 0, max = length(unique(bison_grow$ID2)), initial = 0) 
for (i in 1:length(unique(bison_grow$ID2))){
  b1<-subset(bison_grow,bison_grow$ID2==unique(bison_grow$ID2)[i])
  bison.move2<-move(x=b1$long,y=b1$lat,time=b1$Timestamp.std2,data=b1,proj=CRS("epsg:9822"),animal=b1$ID)
  suppressMessages(bison.telemetry<-as.telemetry(bison.move2,timeformat="",timezone="",projection="epsg:9822",UERE=NULL,drop=TRUE))
  bison.guess<-ctmm.guess(bison.telemetry,interactive = FALSE)
  bison.fit<-ctmm.fit(bison.telemetry,bison.guess)
  bison.select<-ctmm.select(bison.telemetry,bison.fit)
  bison.akde<-akde(bison.telemetry,bison.select,debias=TRUE,smooth=TRUE,error=0.001,res = 10,grid = NULL)
  bison.hr<-homerange(bison.telemetry,bison.fit,method = "AKDE")
  writeVector(bison.hr,paste0('C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/Home Range/Grow_AKDE/',b1$tag[1],"_",unique(b1$ID2)[1],".shp"),
              level.UD=0.95,level=0.95,overwrite = T)
  hr.summary<-summary(bison.hr) #convert home range summary to data frame
  grow.akde.summary[i, 1]<- b1$tag[1] #tag
  grow.akde.summary[i, 2]<- b1$ID2[1] #ID2
  grow.akde.summary[i, 3]<- hr.summary$DOF[[2]] #bandwidth
  grow.akde.summary[i, 4]<- hr.summary$CI[[1]] #low
  grow.akde.summary[i, 5]<- hr.summary$CI[[2]] #est
  grow.akde.summary[i, 6]<- hr.summary$CI[[3]] #high
  grow.akde.summary[i, 7]<- tibble::rownames_to_column(as.data.frame(hr.summary$CI))$rowname #units
  setTxtProgressBar(pb,i)
}
close(pb)
rm(b1,bison.move2,bison.telemetry,bison.hr,bison.fit,bison.guess,hr.summary,i, bison.select, bison.akde)
grow.akde.summary<-grow.akde.summary[order(grow.akde.summary$tag),]

#Export
data.table::fwrite(grow.akde.summary,"C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/Home Range/Grow_AKDE/Grow_HomeRange_Summary.csv")

## Calculate AKDE Home Range - Nongrowing season ####
#Order observations
bison_nongrow <- bison %>%
  dplyr::filter(season == "Nongrowing") %>%
  dplyr::mutate(ID2 = ifelse(ID2 == "",paste0(tag,"_",year),ID2)) %>%
  dplyr::arrange(ID2,Timestamp.std2)
foo <- which(bison_nongrow$Timestamp.std2[1:(nrow(bison_nongrow)-1)] != bison_nongrow$Timestamp.std2[2:(nrow(bison_nongrow))] & 
               bison_nongrow$ID2[1:(nrow(bison_nongrow)-1)] == bison_nongrow$ID2[2:(nrow(bison_nongrow))])
bison_nongrow <- bison_nongrow[foo]
rm(foo)

#Reproject Coordinates
xy <- coords_as_sf(bison_nongrow,x="long",y="lat",crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
xy.aea <- sf::st_transform(xy,crs = "epsg:9822") #Albers equal area
bison_nongrow$long<-sf::st_coordinates(xy.aea)[,1]
bison_nongrow$lat<-sf::st_coordinates(xy.aea)[,2]
rm(xy,xy.aea)

#AKDE for loop
nongrow.akde.summary<-data.frame(matrix(nrow= length(unique(bison_nongrow$ID2)), ncol = 7))
colnames(nongrow.akde.summary)<- c("tag","ID2","Bandwidth","CI.Low","CI.Est","CI.High","Est.Units")
pb = txtProgressBar(min = 0, max = length(unique(bison_nongrow$ID2)), initial = 0) 
for (i in 1:length(unique(bison_nongrow$ID2))){
  b1<-subset(bison_nongrow,bison_nongrow$ID2==unique(bison_nongrow$ID2)[i])
  bison.move2<-move(x=b1$long,y=b1$lat,time=b1$Timestamp.std2,data=b1,proj=CRS("epsg:9822"),animal=b1$ID2)
  suppressMessages(bison.telemetry<-as.telemetry(bison.move2,timeformat="",timezone="",projection="epsg:9822",UERE=NULL,drop=TRUE))
  bison.guess<-ctmm.guess(bison.telemetry,interactive = FALSE)
  bison.fit<-ctmm.fit(bison.telemetry,bison.guess)
  bison.select<-ctmm.select(bison.telemetry,bison.fit)
  bison.akde<-akde(bison.telemetry,bison.select,debias=TRUE,smooth=TRUE,error=0.001,res = 10,grid = NULL)
  bison.hr<-homerange(bison.telemetry,bison.fit,method = "AKDE")
  #if(class(bison.hr)[1] == "UD"){
    writeVector(bison.hr,paste0('C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/Home Range/Nongrow_AKDE/',b1$tag[1],"_",unique(b1$ID2)[1],".shp"),
                level.UD=0.95,level=0.95,overwrite = T)
    hr.summary<-summary(bison.hr) #convert home range summary to data frame
    nongrow.akde.summary[i, 1]<- b1$tag[1] #tag
    nongrow.akde.summary[i, 2]<- b1$ID2[1] #ID2
    nongrow.akde.summary[i, 3]<- hr.summary$DOF[[2]] #bandwidth
    nongrow.akde.summary[i, 4]<- hr.summary$CI[[1]] #low
    nongrow.akde.summary[i, 5]<- hr.summary$CI[[2]] #est
    nongrow.akde.summary[i, 6]<- hr.summary$CI[[3]] #high
    nongrow.akde.summary[i, 7]<- tibble::rownames_to_column(as.data.frame(hr.summary$CI))$rowname #units
  #}
  setTxtProgressBar(pb,i)
}
close(pb)
rm(b1,bison.move2,bison.telemetry,bison.hr,bison.fit,bison.guess,hr.summary,i,bison.select,bison.akde)
nongrow.akde.summary<-nongrow.akde.summary[order(nongrow.akde.summary$tag),]

#Export
data.table::fwrite(nongrow.akde.summary,"C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/Home Range/Nongrow_AKDE/Nongrow_HomeRange_Summary.csv")

## Calculate AKDE Home Range - Focal season ####
#Order observations
bison_focal <- bison %>%
  dplyr::filter(season == "Focal") %>%
  dplyr::arrange(tag,ID2,Timestamp.std2)
foo <- which(bison_focal$Timestamp.std2[1:(nrow(bison_focal)-1)] != bison_focal$Timestamp.std2[2:(nrow(bison_focal))] & 
               bison_focal$tag[1:(nrow(bison_focal)-1)] == bison_focal$tag[2:(nrow(bison_focal))])
bison_focal <- bison_focal[foo]
rm(foo)

#Reproject Coordinates
xy <- coords_as_sf(bison_focal,x="long",y="lat",crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
xy.aea <- sf::st_transform(xy,crs = "epsg:9822") #Albers equal area
bison_focal$long<-sf::st_coordinates(xy.aea)[,1]
bison_focal$lat<-sf::st_coordinates(xy.aea)[,2]
rm(xy,xy.aea)

#AKDE for loop
focal.akde.summary<-data.frame(matrix(nrow= length(unique(bison_focal$ID2)), ncol = 7))
colnames(focal.akde.summary)<- c("tag","ID2","Bandwidth","CI.Low","CI.Est","CI.High","Est.Units")
pb = txtProgressBar(min = 0, max = length(unique(bison_focal$ID2)), initial = 0) 
for (i in 1:length(unique(bison_focal$ID2))){
  b1<-subset(bison_focal,bison_focal$ID2==unique(bison_focal$ID2)[i])
  bison.move2<-move(x=b1$long,y=b1$lat,time=b1$Timestamp.std2,data=b1,proj=CRS("epsg:9822"),animal=b1$ID)
  suppressMessages(bison.telemetry<-as.telemetry(bison.move2,timeformat="",timezone="",projection="epsg:9822",UERE=NULL,drop=TRUE))
  bison.guess<-ctmm.guess(bison.telemetry,interactive = FALSE)
  bison.fit<-ctmm.fit(bison.telemetry,bison.guess)
  bison.select<-ctmm.select(bison.telemetry,bison.fit)
  bison.akde<-akde(bison.telemetry,bison.select,debias=TRUE,smooth=TRUE,error=0.001,res = 10,grid = NULL)
  bison.hr<-homerange(bison.telemetry,bison.fit,method = "AKDE")
  if(class(bison.hr)[1] == "UD"){
    writeVector(bison.hr,paste0('C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/Home Range/Focal_AKDE/',b1$tag[1],"_",unique(b1$ID2)[1],".shp"),
              level.UD=0.95,level=0.95,overwrite = T)
    hr.summary<-summary(bison.hr) #convert home range summary to data frame
    focal.akde.summary[i, 1]<- b1$tag[1] #tag
    focal.akde.summary[i, 2]<- b1$ID2[1] #ID2
    focal.akde.summary[i, 3]<- hr.summary$DOF[[2]] #bandwidth
    focal.akde.summary[i, 4]<- hr.summary$CI[[1]] #low
    focal.akde.summary[i, 5]<- hr.summary$CI[[2]] #est
    focal.akde.summary[i, 6]<- hr.summary$CI[[3]] #high
    focal.akde.summary[i, 7]<- tibble::rownames_to_column(as.data.frame(hr.summary$CI))$rowname #units
  }
  setTxtProgressBar(pb,i)
}
close(pb)
rm(b1,bison.move2,bison.telemetry,bison.hr,bison.fit,bison.guess,hr.summary,i, bison.select, bison.akde)
focal.akde.summary<-focal.akde.summary[order(focal.akde.summary$tag),]

#Export
data.table::fwrite(focal.akde.summary,"C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/Home Range/Focal_AKDE/Focal_HomeRange_Summary.csv")

## Calculate AKDE Home Range - Annual season ####
#Order observations
bison_annual <- bison %>%
  dplyr::filter(season == "Annual") %>%
  dplyr::arrange(tag,ID2,Timestamp.std2)
foo <- which(bison_annual$Timestamp.std2[1:(nrow(bison_annual)-1)] != bison_annual$Timestamp.std2[2:(nrow(bison_annual))] & 
               bison_annual$tag[1:(nrow(bison_annual)-1)] == bison_annual$tag[2:(nrow(bison_annual))])
bison_annual <- bison_annual[foo]
rm(foo)

#Reproject Coordinates
xy <- coords_as_sf(bison_annual,x="long",y="lat",crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
xy.aea <- sf::st_transform(xy,crs = "epsg:9822") #Albers equal area
bison_annual$long<-sf::st_coordinates(xy.aea)[,1]
bison_annual$lat<-sf::st_coordinates(xy.aea)[,2]
rm(xy,xy.aea)

#AKDE for loop
annual.akde.summary<-data.frame(matrix(nrow= length(unique(bison_annual$ID2)), ncol = 7))
colnames(annual.akde.summary)<- c("tag","ID2","Bandwidth","CI.Low","CI.Est","CI.High","Est.Units")
pb = txtProgressBar(min = 0, max = length(unique(bison_annual$ID2)), initial = 0) 
for (i in 1:length(unique(bison_annual$ID2))){
  b1<-subset(bison_annual,bison_annual$ID2==unique(bison_annual$ID2)[i])
  bison.move2<-move(x=b1$long,y=b1$lat,time=b1$Timestamp.std2,data=b1,proj=CRS("epsg:9822"),animal=b1$ID2)
  suppressMessages(bison.telemetry<-as.telemetry(bison.move2,timeformat="",timezone="",projection="epsg:9822",UERE=NULL,drop=TRUE))
  bison.guess<-ctmm.guess(bison.telemetry,interactive = FALSE)
  bison.fit<-ctmm.fit(bison.telemetry,bison.guess)
  bison.select<-ctmm.select(bison.telemetry,bison.fit)
  bison.akde<-akde(bison.telemetry,bison.select,debias=TRUE,smooth=TRUE,error=0.001,res = 10,grid = NULL)
  bison.hr<-homerange(bison.telemetry,bison.fit,method = "AKDE")
  if(class(bison.hr)[1] == "UD"){
    writeVector(bison.hr,paste0('C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/Home Range/Annual_AKDE/',b1$tag[1],"_",unique(b1$ID2)[1],".shp"),
              level.UD=0.95,level=0.95,overwrite = T)
    hr.summary<-summary(bison.hr) #convert home range summary to data frame
    annual.akde.summary[i, 1]<- b1$tag[1] #tag
    annual.akde.summary[i, 2]<- b1$ID2[1] #ID2
    annual.akde.summary[i, 3]<- hr.summary$DOF[[2]] #bandwidth
    annual.akde.summary[i, 4]<- hr.summary$CI[[1]] #low
    annual.akde.summary[i, 5]<- hr.summary$CI[[2]] #est
    annual.akde.summary[i, 6]<- hr.summary$CI[[3]] #high
    annual.akde.summary[i, 7]<- tibble::rownames_to_column(as.data.frame(hr.summary$CI))$rowname #units
  }
  setTxtProgressBar(pb,i)
}
close(pb)
rm(b1,bison.move2,bison.telemetry,bison.hr,bison.fit,bison.guess,hr.summary,i, bison.select, bison.akde)
annual.akde.summary<-annual.akde.summary[order(annual.akde.summary$tag),]

#Export
data.table::fwrite(annual.akde.summary,"C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/Home Range/Annual_AKDE/Annual_HomeRange_Summary.csv")
#####
## Combine home range files
seasons <- c("Grow","Nongrow","Annual","Focal")
out <- NULL
for(s in seasons){ #homerange
  hr_summary <- data.table::fread(paste0("C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/Home Range/",s,"_AKDE/",s,"_HomeRange_Summary.csv")) %>%
    dplyr::mutate(homerange = CI.Est,
                  Season = ifelse(s %in% c("Grow","Nongrow"),paste0(s,"ing"),s)) %>%
    dplyr::select(ID2,tag,Season,homerange)
  out <- rbind(out,hr_summary)
}
rm(s,hr_summary,seasons)
data.table::fwrite(out,"C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/Home Range/All_HomeRanges_Summary.csv")
#####