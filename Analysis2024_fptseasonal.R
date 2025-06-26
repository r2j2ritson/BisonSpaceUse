#------------------------------------#
### Bison Space-Use Analysis       ###
#------------------------------------#
## R. Ritson, 2/17/2025             ##
#------------------------------------#
### Utah Additions: First-passage Time ###
#------------------------------------#
# Load packages
require(sf)
require(terra)
library(data.table)
require(lubridate)
library(adehabitatLT)
require(sp)
require(dplyr)
devtools::source_url("https://github.com/r2j2ritson/RSpatial_Wildlife/blob/main/R/coords_as_sf.R?raw=TRUE")

# Load data
bison <- data.table::fread("C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/Data/BisonLocsAll_Seasonal_Master.csv")
bison$Date_Time <- bison$Timestamp.std2
str(bison)

#Set file structure
dir.create('C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/First Passage Time_output')
dir.create('C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/First Passage Time_output/Growing')
dir.create('C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/First Passage Time_output/Nongrowing')
dir.create('C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/First Passage Time_output/Focal')
dir.create('C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/First Passage Time_output/Annual')

## Growing Season FPT ####
bison_grow <- bison %>%
  dplyr::filter(season == "Growing") %>%
  dplyr::arrange(tag,ID2,Timestamp.std2)
foo <- which(bison_grow$Timestamp.std2[1:(nrow(bison_grow)-1)] != bison_grow$Timestamp.std2[2:(nrow(bison_grow))] & 
               bison_grow$tag[1:(nrow(bison_grow)-1)] == bison_grow$tag[2:(nrow(bison_grow))])
bison_grow <- bison_grow[foo]
rm(foo)

#Reproject Coordinates
xy <- coords_as_sf(bison_grow,x="long",y="lat",crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
xy.aea <- sf::st_transform(xy,crs = "epsg:9822") #Albers equal area

#Create ltraj object
xy<-sf::st_coordinates(xy.aea)
da<-bison_grow$Date_Time
id<-bison_grow$ID2
bison_grow.ltraj<-as.ltraj(xy, da, id, burst=id, typeII = TRUE, slsp = c("remove", "missing"))

#Calculate First-Passage Time (takes a while with large datasets)
k <-fpt (bison_grow.ltraj, seq(50,150000, by=25), units = c("seconds"))

#Tell R where to save output
setwd('C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/First Passage Time_output/Growing') 

#Create custom function for variance (Kohl)
varfpt <- function (f, graph = TRUE)
{
  if (!inherits(f, "fipati"))
    stop("f should be of class 'fipati'")
  if (graph)
    opar <- par(mfrow = n2mfrow(length(f)))
  s <- attr(f, "radii")
  soso <- lapply(f, function(y) {
    so <- apply(y, 2, function(z) var(z, na.rm = TRUE))
    if (graph)
      plot(s, so, ty = "l", xlab = "Radius (m)", ylab = "Variance of FPT",
           main = attr(y, "burst"))
    return(so)
  })
  soso <- as.data.frame(do.call("rbind", soso))
  row.names(soso) <- unlist(lapply(f, function(z) attr(z, "burst")))
  ##
  ## make sure this information matches the scales in the fpt line above
  ##
  names(soso) <- paste(seq(50,15000, by=25), sep = "")
  attr(soso, "radii") <- attr(f, "radii")
  
  write.csv(t(soso), file="fpt_out.csv")
  if (graph)
    par(opar)
  if (graph) {
    invisible(soso)
  }
  else {
    return(soso)
  }
}
## runs the above function
## produces table with first column as scale values
varfpt(k, graph = FALSE)


testmax <- read.csv("fpt_out.csv")
testmax$"X"
testmax$area <- pi * (testmax$"X"^2)

for(j in 1:(ncol(testmax)-2)) {
  current <-  testmax[,j+1]
  fpt.area.current <- current / testmax$area
  deer <- names(testmax)[j+1]
  out_file <- paste(deer,"_fpt.jpg",sep="")
  ##call graphics device, give file name, and choose image size
  jpeg(file=out_file, width=1200, height=1200)
  
  par(mar=c(8,8,4,2),mgp=c(4, 1, 0), cex.axis=2.7, family="serif")
  plot(testmax$"X", fpt.area.current, xlab="Scale (m)", ylab=expression(paste("VarFPT" %.% "area (",s^2 %.% m^2,")",sep="")), 
       bty="l", main="", xlim=c(0,10000), type="l", lwd=3.5, cex.axis=3, cex.lab=3.5)  
  
  ##turn graphics device off
  dev.off() #turns off write-to-file device
}

testmax <- read.csv("fpt_out.csv")

count <- ncol(testmax)

testmax$area <- pi * (testmax$"X"^2)

output <- as.data.frame(cbind(testmax[,1],(testmax[,2:count]/testmax$area)))

names(output)[1] <- "RADIUS"

write.csv(output,file="var_fpt_area.csv")
##

bisonfpt<-read.csv("var_fpt_area.csv")

#Get Radius of Max Variance
require(dplyr)
fpt.summary<-data.frame(bison_grow$ID2,bison_grow$tag,bison_grow$Study_Area)
colnames(fpt.summary)<- c("ID2","tag","Study_Area")
fpt.summary<-unique(fpt.summary)
fpt.summary<-fpt.summary[order(fpt.summary$ID2),]

radii1<-c()
radii2<-c()
for (i in 3:ncol(bisonfpt)){
  foo<-data.frame(radius=bisonfpt[,2],Var=bisonfpt[,i])
  boo<-foo%>%top_n(2)  #Select top 2 peaks
  scale1<-boo$radius[1]
  scale2<-boo$radius[2]
  radii1<-c(radii1,scale1)
  radii2<-c(radii2,scale2)
}

fpt.summary$FPT_Radius_1<-radii1
fpt.summary$FPT_Radius_2<-radii2
write.csv(fpt.summary,file="fpt_summary.csv")

rm(output,spxy,testmax,xy,xy.aea,count,current,da,deer,foo,i,id,j,out_file,radii,scale,fpt.area.current)
#####
## Nongrowing Season FPT ####
bison_nongrow <- bison %>%
  dplyr::filter(season == "Nongrowing") %>%
  dplyr::arrange(tag,ID2,Timestamp.std2)
foo <- which(bison_nongrow$Timestamp.std2[1:(nrow(bison_nongrow)-1)] != bison_nongrow$Timestamp.std2[2:(nrow(bison_nongrow))] & 
               bison_nongrow$tag[1:(nrow(bison_nongrow)-1)] == bison_nongrow$tag[2:(nrow(bison_nongrow))])
bison_nongrow <- bison_nongrow[foo]
rm(foo)
#Reproject Coordinates
xy <- coords_as_sf(bison_nongrow,x="long",y="lat",crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
xy.aea <- sf::st_transform(xy,crs = "epsg:9822") #Albers equal area

#Create ltraj object
xy<-sf::st_coordinates(xy.aea)
da<-bison_nongrow$Date_Time
id<-bison_nongrow$ID2
bison_nongrow.ltraj<-as.ltraj(xy, da, id, burst=id, typeII = TRUE, slsp = c("remove", "missing"))

#Calculate First-Passage Time (takes a while with large datasets)
k <-fpt (bison_nongrow.ltraj, seq(50,150000, by=25), units = c("seconds"))

#Tell R where to save output
setwd('C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/First Passage Time_output/Nongrowing') 

#Create custom function for variance (Kohl)
varfpt <- function (f, graph = TRUE)
{
  if (!inherits(f, "fipati"))
    stop("f should be of class 'fipati'")
  if (graph)
    opar <- par(mfrow = n2mfrow(length(f)))
  s <- attr(f, "radii")
  soso <- lapply(f, function(y) {
    so <- apply(y, 2, function(z) var(z, na.rm = TRUE))
    if (graph)
      plot(s, so, ty = "l", xlab = "Radius (m)", ylab = "Variance of FPT",
           main = attr(y, "burst"))
    return(so)
  })
  soso <- as.data.frame(do.call("rbind", soso))
  row.names(soso) <- unlist(lapply(f, function(z) attr(z, "burst")))
  ##
  ## make sure this information matches the scales in the fpt line above
  ##
  names(soso) <- paste(seq(50,15000, by=25), sep = "")
  attr(soso, "radii") <- attr(f, "radii")
  
  write.csv(t(soso), file="fpt_out.csv")
  if (graph)
    par(opar)
  if (graph) {
    invisible(soso)
  }
  else {
    return(soso)
  }
}

## runs the above function
## produces table with first column as scale values
varfpt(k, graph = FALSE)


testmax <- read.csv("fpt_out.csv")
testmax$"X" 
testmax$area <- pi * (testmax$"X"^2)

for(j in 38:(ncol(testmax)-2)) {
  current <-  testmax[,j+1]
  #if(anyNA(current)){next}
  fpt.area.current <- current / testmax$area
  deer <- names(testmax)[j+1]
  out_file <- paste(deer,"_fpt.jpg",sep="")
  ##call graphics device, give file name, and choose image size
  jpeg(file=out_file, width=1200, height=1200)
  
  par(mar=c(8,8,4,2),mgp=c(4, 1, 0), cex.axis=2.7, family="serif")
  plot(testmax$"X", fpt.area.current, xlab="Scale (m)", ylab=expression(paste("VarFPT" %.% "area (",s^2 %.% m^2,")",sep="")), 
       bty="l", main="", xlim=c(0,10000), type="l", lwd=3.5, cex.axis=3, cex.lab=3.5)  
  
  ##turn graphics device off
  dev.off() #turns off write-to-file device
}

testmax <- read.csv("fpt_out.csv")

count <- ncol(testmax)

testmax$area <- pi * (testmax$"X"^2)

output <- as.data.frame(cbind(testmax[,1],(testmax[,2:count]/testmax$area)))

names(output)[1] <- "RADIUS"

write.csv(output,file="var_fpt_area.csv")
##

bisonfpt<-read.csv("var_fpt_area.csv")

#Get Radius of Max Variance
require(dplyr)
fpt.summary<-data.frame(bison_nongrow$ID2,bison_nongrow$tag,bison_nongrow$Study_Area)
colnames(fpt.summary)<- c("ID2","tag","Study_Area")
fpt.summary<-unique(fpt.summary)
fpt.summary<-fpt.summary[order(fpt.summary$ID2),]

radii1<-c()
radii2<-c()
for (i in 3:ncol(bisonfpt)){
  foo<-data.frame(radius=bisonfpt[,2],Var=bisonfpt[,i])
  boo<-foo%>%top_n(2)  #Select top 2 peaks
  scale1<-boo$radius[1]
  scale2<-boo$radius[2]
  radii1<-c(radii1,scale1)
  radii2<-c(radii2,scale2)
}

fpt.summary$FPT_Radius_1<-radii1
fpt.summary$FPT_Radius_2<-radii2
write.csv(fpt.summary,file="fpt_summary.csv")

rm(output,spxy,testmax,xy,xy.aea,count,current,da,deer,foo,i,id,j,out_file,radii,scale,fpt.area.current)
#####
## Focal Season FPT ####
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

#Create ltraj object
xy<-sf::st_coordinates(xy.aea)
da<-bison_focal$Date_Time
id<-bison_focal$ID2
bison_focal.ltraj<-as.ltraj(xy, da, id, burst=id, typeII = TRUE, slsp = c("remove", "missing"))

#Calculate First-Passage Time (takes a while with large datasets)
k <-fpt (bison_focal.ltraj, seq(50,150000, by=25), units = c("seconds"))

#Tell R where to save output
setwd('C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/First Passage Time_output/Focal') 

#Create custom function for variance (Kohl)
varfpt <- function (f, graph = TRUE)
{
  if (!inherits(f, "fipati"))
    stop("f should be of class 'fipati'")
  if (graph)
    opar <- par(mfrow = n2mfrow(length(f)))
  s <- attr(f, "radii")
  soso <- lapply(f, function(y) {
    so <- apply(y, 2, function(z) var(z, na.rm = TRUE))
    if (graph)
      plot(s, so, ty = "l", xlab = "Radius (m)", ylab = "Variance of FPT",
           main = attr(y, "burst"))
    return(so)
  })
  soso <- as.data.frame(do.call("rbind", soso))
  row.names(soso) <- unlist(lapply(f, function(z) attr(z, "burst")))
  ##
  ## make sure this information matches the scales in the fpt line above
  ##
  names(soso) <- paste(seq(50,15000, by=25), sep = "")
  attr(soso, "radii") <- attr(f, "radii")
  
  write.csv(t(soso), file="fpt_out.csv")
  if (graph)
    par(opar)
  if (graph) {
    invisible(soso)
  }
  else {
    return(soso)
  }
}

## runs the above function
## produces table with first column as scale values
varfpt(k, graph = FALSE)


testmax <- read.csv("fpt_out.csv")
testmax$"X"
testmax$area <- pi * (testmax$"X"^2)

for(j in 1:(ncol(testmax)-2)) {
  current <-  testmax[,j+1]
  fpt.area.current <- current / testmax$area
  deer <- names(testmax)[j+1]
  out_file <- paste(deer,"_fpt.jpg",sep="")
  ##call graphics device, give file name, and choose image size
  jpeg(file=out_file, width=1200, height=1200)
  
  par(mar=c(8,8,4,2),mgp=c(4, 1, 0), cex.axis=2.7, family="serif")
  plot(testmax$"X", fpt.area.current, xlab="Scale (m)", ylab=expression(paste("VarFPT" %.% "area (",s^2 %.% m^2,")",sep="")), 
       bty="l", main="", xlim=c(0,10000), type="l", lwd=3.5, cex.axis=3, cex.lab=3.5)  
  
  ##turn graphics device off
  dev.off() #turns off write-to-file device
}

testmax <- read.csv("fpt_out.csv")

count <- ncol(testmax)

testmax$area <- pi * (testmax$"X"^2)

output <- as.data.frame(cbind(testmax[,1],(testmax[,2:count]/testmax$area)))

names(output)[1] <- "RADIUS"

write.csv(output,file="var_fpt_area.csv")
##

bisonfpt<-read.csv("var_fpt_area.csv")

#Get Radius of Max Variance
require(dplyr)
fpt.summary<-data.frame(bison_focal$ID2,bison_focal$tag,bison_focal$Study_Area)
colnames(fpt.summary)<- c("ID2","tag","Study_Area")
fpt.summary<-unique(fpt.summary)
fpt.summary<-fpt.summary[order(fpt.summary$ID2),]

radii1<-c()
radii2<-c()
for (i in 3:ncol(bisonfpt)){
  foo<-data.frame(radius=bisonfpt[,2],Var=bisonfpt[,i])
  boo<-foo%>%top_n(2)  #Select top 2 peaks
  scale1<-boo$radius[1]
  scale2<-boo$radius[2]
  radii1<-c(radii1,scale1)
  radii2<-c(radii2,scale2)
}

fpt.summary$FPT_Radius_1<-radii1
fpt.summary$FPT_Radius_2<-radii2
write.csv(fpt.summary,file="fpt_summary.csv")

rm(output,spxy,testmax,xy,xy.aea,count,current,da,deer,foo,i,id,j,out_file,radii,scale,fpt.area.current)
#####
## Annual FPT ####
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

#Create ltraj object
xy<-sf::st_coordinates(xy.aea)
da<-bison_annual$Date_Time
id<-bison_annual$ID2
bison_annual.ltraj<-as.ltraj(xy, da, id, burst=id, typeII = TRUE, slsp = c("remove", "missing"))

#Calculate First-Passage Time (takes a while with large datasets)
k <-fpt (bison_annual.ltraj, seq(50,150000, by=25), units = c("seconds"))

#Tell R where to save output
setwd('C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/First Passage Time_output/Annual') 

#Create custom function for variance (Kohl)
varfpt <- function (f, graph = TRUE)
{
  if (!inherits(f, "fipati"))
    stop("f should be of class 'fipati'")
  if (graph)
    opar <- par(mfrow = n2mfrow(length(f)))
  s <- attr(f, "radii")
  soso <- lapply(f, function(y) {
    so <- apply(y, 2, function(z) var(z, na.rm = TRUE))
    if (graph)
      plot(s, so, ty = "l", xlab = "Radius (m)", ylab = "Variance of FPT",
           main = attr(y, "burst"))
    return(so)
  })
  soso <- as.data.frame(do.call("rbind", soso))
  row.names(soso) <- unlist(lapply(f, function(z) attr(z, "burst")))
  ##
  ## make sure this information matches the scales in the fpt line above
  ##
  names(soso) <- paste(seq(50,15000, by=25), sep = "")
  attr(soso, "radii") <- attr(f, "radii")
  
  write.csv(t(soso), file="fpt_out.csv")
  if (graph)
    par(opar)
  if (graph) {
    invisible(soso)
  }
  else {
    return(soso)
  }
}

## runs the above function
## produces table with first column as scale values
varfpt(k, graph = FALSE)


testmax <- read.csv("fpt_out.csv")
testmax$"X"
testmax$area <- pi * (testmax$"X"^2)

for(j in 1:(ncol(testmax)-2)) {
  current <-  testmax[,j+1]
  fpt.area.current <- current / testmax$area
  deer <- names(testmax)[j+1]
  out_file <- paste(deer,"_fpt.jpg",sep="")
  ##call graphics device, give file name, and choose image size
  jpeg(file=out_file, width=1200, height=1200)
  
  par(mar=c(8,8,4,2),mgp=c(4, 1, 0), cex.axis=2.7, family="serif")
  plot(testmax$"X", fpt.area.current, xlab="Scale (m)", ylab=expression(paste("VarFPT" %.% "area (",s^2 %.% m^2,")",sep="")), 
       bty="l", main="", xlim=c(0,10000), type="l", lwd=3.5, cex.axis=3, cex.lab=3.5)  
  
  ##turn graphics device off
  dev.off() #turns off write-to-file device
}

testmax <- read.csv("fpt_out.csv")

count <- ncol(testmax)

testmax$area <- pi * (testmax$"X"^2)

output <- as.data.frame(cbind(testmax[,1],(testmax[,2:count]/testmax$area)))

names(output)[1] <- "RADIUS"

write.csv(output,file="var_fpt_area.csv")
##

bisonfpt<-read.csv("var_fpt_area.csv")

#Get Radius of Max Variance
require(dplyr)
fpt.summary<-data.frame(bison_annual$ID2,bison_annual$tag,bison_annual$Study_Area)
colnames(fpt.summary)<- c("ID2","tag","Study_Area")
fpt.summary<-unique(fpt.summary)
fpt.summary<-fpt.summary[order(fpt.summary$ID2),]

radii1<-c()
radii2<-c()
for (i in 3:ncol(bisonfpt)){
  foo<-data.frame(radius=bisonfpt[,2],Var=bisonfpt[,i])
  boo<-foo%>%top_n(2)  #Select top 2 peaks
  scale1<-boo$radius[1]
  scale2<-boo$radius[2]
  radii1<-c(radii1,scale1)
  radii2<-c(radii2,scale2)
}

fpt.summary$FPT_Radius_1<-radii1
fpt.summary$FPT_Radius_2<-radii2
write.csv(fpt.summary,file="fpt_summary.csv")

rm(output,testmax,xy,xy.aea,count,current,da,deer,foo,i,id,j,out_file,radii,scale,fpt.area.current)
###########

