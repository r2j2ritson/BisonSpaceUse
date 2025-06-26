#------------------------------------#
### Bison Space-Use Analysis       ###
#------------------------------------#
## R. Ritson, 2/17/2025            ##
#------------------------------------#
### Utah Additions and Re-analysis ###
#------------------------------------#

# Load Libraires
require(dplyr)
devtools::source_url("https://github.com/r2j2ritson/RSpatial_Wildlife/blob/main/R/coords_as_sf.R?raw=TRUE")

## Load Current Data
bison_master <- data.table::fread("D:/Bison_Thesis/Analysis_thesis/Master_Bison_GPS.csv")
head(bison_master)
unique(bison_master$tag)

## Load New Utah Data
ut_bison_new_1 <- data.table::fread("C:/Users/r2j2r/Documents/Research Projects/Book Cliffs bison/UT_bison_data_part_1.csv")
ut_bison_new_2 <- data.table::fread("C:/Users/r2j2r/Documents/Research Projects/Book Cliffs bison/UT_biosn_data_part_2.csv")
ut_bison_new <- rbind(ut_bison_new_1,ut_bison_new_2)
rm(ut_bison_new_1,ut_bison_new_2)

head(ut_bison_new)
tail(ut_bison_new)
unique(ut_bison_new$uniqueID)

bison_sf <- coords_as_sf(ut_bison_new,x="longitude",y="latitude",crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
bison_sf
plot(bison_sf$geometry)

## Filter previous UT bison from current data set
bison_master_02162025 <- bison_master %>%
  dplyr::filter(!(`Study area` %in% c("Book Cliffs","Henry Mountains"))) #take out previous UT bison
head(bison_master_02162025 )

colnames(bison_master_02162025 )
# [1] "V1"             "ID"             "tag"            "Date"           "Time"           "Date_Time"      "Timezone"       "long"          
# [9] "lat"            "DOP"            "sex"            "Study area"     "Timestamp.std"  "Timestamp.std2" "firstRec"       "dt"            
# [17] "sl"             "bb1"            "year"  
colnames(bison_sf)
# [1] "collarObjectId"    "species"           "dateYearAndJulian" "collarID"          "uniqueID"          "mortality"         "sex"              
# [8] "currentAge"        "latitude"          "longitude"         "currentCohort"     "freq"              "projectName"       "captureUnit"      
# [15] "captureSubUnit"    "birthYear"         "realCaptureArea"   "lowBattVoltage"    "numSats"           "dop"               "fixTime"          
# [22] "dimension"         "temperature"       "activity"          "speed"             "earTag"            "geometry" 
ut_new <- bison_sf %>%
  dplyr::filter(dateYearAndJulian <= as.POSIXct("2018-12-31 23:59:59",format="%Y-%m-%d %H:%M:%S",tz="UTC")) %>% #filter up to 2018
  dplyr::filter(mortality == F) %>% # remove morts
  dplyr::rowwise(.) %>%
  dplyr::mutate(tag = gsub("BI",ifelse(captureUnit=="Henry Mountains","HM","BC"),uniqueID)) %>%
  dplyr::mutate(DOP = dop,
                Timezone = "MST",
                `Study area` = captureUnit,
                lat = as.numeric(latitude),
                long = as.numeric(longitude),
                Timestamp.std = dateYearAndJulian,
                Date_Time = format(Timestamp.std,tz="MST"),
                Date = format(as.POSIXct(Date_Time,format="%Y-%m-%d %H:%M:%S",tz="MST"),"%m/%d/%Y"),
                Time = format(as.POSIXct(Date_Time,format="%Y-%m-%d %H:%M:%S",tz="MST"),"%H:%M:%S"),
                year = format(as.POSIXct(Date_Time,format="%Y-%m-%d %H:%M:%S",tz="MST"),"%Y"),
                Timestamp.std2 = as.POSIXct(strptime(Date_Time,"%Y-%m-%d %H:%M:%S"),format="%Y-%m-%d %H:%M:%S", tz = "MST")) %>%
  dplyr::ungroup(.) %>%
  as.data.frame(.) %>%
  dplyr::select(tag,Date,Time,Date_Time,Timezone,long,lat,DOP,sex,`Study area`,Timestamp.std, Timestamp.std2,year) 
head(ut_new)
## 
## Set New Data Structure ####
bison <- ut_new
bison$tag<-as.factor(bison$tag)
head(bison)
summary(bison)

nas<- bison[which(is.na(bison$Timestamp.std2)),]
nas[["Timestamp.std2"]] <- as.POSIXct(strptime(nas$Timestamp.std,"%Y-%m-%d %H:%M:%S"),format="%Y-%m-%d %H:%M:%S", tz = "MST")
summary(nas)

bison <- bison[which(is.na(bison$Timestamp.std2)==F),]
bison <- rbind(bison,nas)
summary(bison)

#Order IDs
bison<-bison[order(bison$tag,bison$Timestamp.std2),]

#Remove Duplicates
duplicatedLocs <- which(bison$Timestamp.std2[1:(nrow(bison)-1)] == bison$Timestamp.std2[2:(nrow(bison))])
bison<- bison[-duplicatedLocs,]
rm(duplicatedLocs)

####Identify First Record of Each Individual####
foo <- which(bison$tag[1:(nrow(bison)-1)] != bison$tag[2:(nrow(bison))])
bison$firstRec <- rep(0,nrow(bison)) #populate new column with "0"s
bison$firstRec[foo+1] <- 1 #First record of each indiv gets a "1"
bison$firstRec[1] <- 1

#Check
length(unique(bison$tag)) # count N individuals
sum(bison$firstRec)	
bison[sort(c(foo-1,foo,foo+1)),c('tag','Date_Time','firstRec')] # first records seem correctly identified
rm(foo)

####Calculate Time Intervals####
foo <-c(as.numeric(difftime(bison$Timestamp.std2[2:length(bison$Timestamp.std2)],
                            bison$Timestamp.std2[1:length(bison$Timestamp.std2)-1],
                            units="hours")),NA)
foo <- c(NA, foo)
summary(as.numeric(foo))
foo <- ifelse(bison$firstRec == 1, NA, foo) #first record becomes NA
summary(as.numeric(foo))
bison$dt <- foo
rm(foo)  

####Ghantt Chart####
#b1<-subset(bison,bison$ID==1)
#plot(tag ~ Timestamp.std2, data = bison)

#####Fix-Rate: Utah bison (Hm & BC)####
bison.hm<-subset(bison,bison$`Study area`=="Henry Mountains")
bison.bc<-subset(bison,bison$`Study area`=="Book Cliffs")
bison.utah<-rbind(bison.hm,bison.bc)

#Resample Fix-rate to 6 hour interval (+/- 30 min) (remember dt is currently in "hours")
hr.all <-(6) # 
flx.all<-(0.5) # 
#
#Find the fixes that have fix rates within the defined range 
to.use.all <- as.numeric(which(bison.utah$dt <= (hr.all + flx.all) & bison.utah$dt >= (hr.all - flx.all))) 
to.use.all <- c(to.use.all, to.use.all + 1)
to.use.all <- unique(to.use.all) 

#Store information
fixes <- bison.utah[to.use.all,] 
ID <- unique(fixes$tag)

# Find the time laspes that are less than the minimum and have at least two consectuive points
slots <- which(bison.utah$dt < (hr.all - flx.all))    
slotplus <- slots[2:(length(slots))]            
slots <- slots[1:(length(slots) - 1)]          
slots <- slots[slots + 1 == slotplus]  

# Get the time lapse and collar ID info 
lapse <- bison.utah$dt
collar <- bison.utah$tag

# Create a vector for the start and end fixes that retain 1 minute fix interval
start.fix <- numeric()
end.fix <- numeric()

# Now make a loop that will go through every identified slot
#    NOTE: This will likely take a long while to complete. 

for(i in 1:length(slots)){
  
  repcount <- 1             # Track the number of slots forward we're looking
  
  repeat
  {
    timesum <- sum(lapse[slots[i] : (slots[i] + repcount)], na.rm = TRUE)
    {
      if(timesum <= (hr.all+flx.all) & timesum >= (hr.all-flx.all) & collar[slots[i]] == collar[slots[i] + repcount])
      {
        start.fix <- c(start.fix, (slots[i]))
        end.fix <- c(end.fix, (slots[i] + repcount))
        break
      }
      else if (timesum > (hr.all + flx.all) | collar[slots[i]] != collar[slots[i] + repcount] | is.na(collar[slots[i] + repcount]))
      {
        break
      }
    }
    repcount <- repcount + 1
  }
}

start.end <- data.frame(start.fix, end.fix)

# Create storage vector
pos <- 1
loopcount <- 1 
start.bias <- start.end[,1]        # Get the col w/ rows of start fixes
start.indep <- start.end[pos, 1]   # Gets the first number from start.end
end.indep <- start.end[pos, 2]     # Gets the first number of 2nd col start.end

repeat {
  
  # This will find the next combination of fixes that has a starting point after the most recent endpoint
  pos <- min(which(start.bias > end.indep[loopcount]))         # THIS CAN RETURN A WARNING MESSAGE OF INF VALUE
  
  # If nothing is found or it returns an infinity, break the loop
  if(length(pos) == 0 | is.infinite(pos) == TRUE)
  {
    fix.indep <- data.frame(start.indep, end.indep)
    break
  }
  
  # Otherwise, add the new fixes to the growing vector and iterate
  else
  { 
    start.indep <- c(start.indep, start.end[pos,1])
    end.indep <- c(end.indep, start.end[pos,2])
    loopcount <- loopcount + 1
  }
}

# Bind data together and sort by collar and date/time
fixes <- rbind(fixes, bison.utah[fix.indep[,1],])    # Combine the fixes and the start col of independ fixes found
fixes <- fixes[with(fixes, order(fixes$tag, fixes$Timestamp.std2)),]

#Recalculate TimeDiff
fixes$dt<-c(as.numeric(difftime(fixes$Timestamp.std2[2:length(fixes$Timestamp.std2)],
                                fixes$Timestamp.std2[1:(length(fixes$Timestamp.std2)-1)],
                                units="hours")),NA)
#drop the stragglers
fixes<-fixes[which(fixes$dt <=(hr.all+flx.all)),]
fixes<-fixes[which(fixes$dt >=(hr.all-flx.all)),]
min(fixes$dt, na.rm = T) # 
max(fixes$dt, na.rm = T) #
mean(fixes$dt) #

hist(fixes$dt[fixes$dt<=(hr.all+flx.all) & fixes$dt>=(hr.all-flx.all)])
boxplot(fixes$dt[fixes$dt<=(hr.all+flx.all) & fixes$dt>=(hr.all-flx.all)])

#Remove Unneccessary Objects
rm(collar,end.fix,end.indep,flx.all,hr.all,i,ID,lapse,loopcount,pos,repcount,
   slotplus,slots,start.bias,start.fix,start.indep,timesum,to.use.all,
   fix.indep,start.end)

##Calculate Step lengths (Use haversine formula since all locs in lat/long)###
bison.utah.std<-fixes

#Identify First Record of Each Individual
foo <- which(bison.utah.std$tag[1:(nrow(bison.utah.std)-1)] != bison.utah.std$tag[2:(nrow(bison.utah.std))])
bison.utah.std$firstRec <- rep(0,nrow(bison.utah.std)) #populate new column with "0"s
bison.utah.std$firstRec[foo+1] <- 1 #First record of each indiv gets a "1"
bison.utah.std$firstRec[1] <- 1

#Check
length(unique(bison.utah.std$tag)) # count N individuals
sum(bison.utah.std$firstRec)	
bison.utah.std[sort(c(foo-1,foo,foo+1)),c('tag','Timestamp.std2','firstRec')] # first records seem correctly identified
rm(foo)

#Calculate Time Intervals
foo <-c(as.numeric(difftime(bison.utah.std$Timestamp.std2[2:length(bison.utah.std$Timestamp.std2)],
                            bison.utah.std$Timestamp.std2[1:length(bison.utah.std$Timestamp.std2)-1],
                            units="hours")),NA)
foo <- c(NA, foo)
summary(as.numeric(foo))
foo <- ifelse(bison.utah.std$firstRec == 1, NA, foo) #first record becomes NA
summary(as.numeric(foo))
bison.utah.std$dt <- foo
rm(foo)  

library(fossil) #calculations on non-projected data
# syntax: deg.dist(long1, lat1, long2, lat2)

foo <- deg.dist(bison.utah.std$long[1:(nrow(bison.utah.std)-1)], bison.utah.std$lat[1:(nrow(bison.utah.std)-1)], 
                bison.utah.std$long[2:nrow(bison.utah.std)], bison.utah.std$lat[2:nrow(bison.utah.std)])
foo <- foo*1000 #Convert km to meters

# add to the dataframe, set first value to NA for each individual, then compare the two step length calculations
bison.utah.std$sl <- c(NA, foo)
#takes into account curvature of the earth

#Code beginning of each burst
bison.utah.std$bb1<-ifelse(bison.utah.std$dt <=6.5,0,1)
bison.utah.std$bb1<-ifelse(bison.utah.std$firstRec ==1,1,bison.utah.std$bb1)

#Delete first step length in each burst
bison.utah.std$sl <- ifelse(bison.utah.std$bb1 == 1, NA, bison.utah.std$sl)

#Remove empty bursts
foo2 <-((bison.utah.std$bb1[1:length(bison.utah.std$bb1)-1])+(bison.utah.std$bb1[2:length(bison.utah.std$bb1)]))
bison.utah.std$bb2<-c(foo2,0)

bison.utah.std<-subset(bison.utah.std,bison.utah.std$bb2 != 2)
bison.utah.std$bb2<-NULL

#Clean up workspace
rm(foo,foo2)

#Check Fix Rates#
unique(bison.utah$tag)
unique(bison.utah.std$tag)

bison.utah.std$year<-lubridate::year(bison.utah.std$Timestamp.std2)

rm(bison.bc,bison.hm, fixes,bison.utah)

####Outliers#####
bison.std2<-bison.utah.std
hm <- subset(bison.std2,bison.std2$`Study area`=="Henry Mountains")
bc<-subset(bison.std2,bison.std2$`Study area`=="Book Cliffs")
bc$event.id<-seq(1:nrow(bc))
plot(lat~long, data=bc)
foo<-subset(bc,bc$lat > 39.7)
bc<-subset(bc,!bc$event.id %in% foo$event.id)
bc$event.id<-NULL
plot(lat~long, data=bc)
rm(foo)

hm$event.id<-seq(1:nrow(hm))
plot(lat~long, data=hm)
hm$event.id<-NULL

#Comine All into one dataset
bison.std3<-rbind(hm,bc)
plot(lat~long, data=bison.std3)
bison.std<-bison.std3

apr <- bison_master_02162025[which(bison_master_02162025$`Study area`=="American Prairie Reserve"),]
plot(lat~long, data=apr)

mzr <- bison_master_02162025[which(bison_master_02162025$`Study area`=="Medano-Zapata Ranch"),]
plot(lat~long, data=mzr)

cc <- bison_master_02162025[which(bison_master_02162025$`Study area`=="Caprock Canyon"),]
plot(lat~long, data=cc)
rm(cc,apr,mzr)

rm(nas,hm,bc,ut_new,ut_bison_new,bison.std2,bison.std3,bison.utah.std,bison_sf)
rm(bison,bison_master)

colnames(bison_master_02162025)
colnames(bison.std)
bison_master_02162025$V1 <- NULL
bison_master_02162025$ID <- NULL
bison_master <- rbind(bison.std,bison_master_02162025)

out <- NULL
for(i in 1:length(unique(bison_master$tag))){
  tmp <- bison_master[which(bison_master$tag == unique(bison_master$tag)[i]),]
  tmp$ID <- i 
  out <- rbind(out,tmp)
}
head(out)
bison_master <- out
rm(i,tmp,out)

bison_master <- bison_master %>%
  dplyr::mutate(Timestamp.std = as.character(Timestamp.std)) %>%
  dplyr::select(ID,tag,Date,Time,Date_Time,Timezone,long,lat,DOP,sex,`Study area`,Timestamp.std,Timestamp.std2,firstRec,dt,sl,bb1,year)

head(bison_master)
tail(bison_master)
summary(bison_master)
data.table::fwrite(bison_master,file = "C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/Data/Master_Bison_GPS_02172025.csv") #Cleaned up GPS points 
rm(list=ls())
gc()

require(dplyr)
devtools::source_url("https://github.com/r2j2ritson/RSpatial_Wildlife/blob/main/R/coords_as_sf.R?raw=TRUE")
bison <- data.table::fread("C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/Data/Master_Bison_GPS_02172025.csv")
bison_sf <- coords_as_sf(bison,x="long",y="lat",crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
plot(bison_sf$geometry)
rm(list=ls())
gc()
