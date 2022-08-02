library(rgeos)
library(rgdal)
library(RPostgreSQL)
library(maptools)
library(sp)
library(maps)
library(raster)
library(lubridate)
library(dismo)
library(lme4)
library(MuMIn)
library(spdep)
library(pscl)
library(magrittr)
library(plyr)
library(dplyr)
library(tidyr)

# Downloading range maps from BIEN - only do once ----
#all plant names run together

all_plants <- read.csv("data/all_plants.csv")
plant_vector <- all_plants$acceptedname

plant_matches <- BIEN_ranges_species(plant_vector, 
                                     directory = "plant_ranges") 
#writing file of which species range maps were downloaded and not
write.csv(plant_matches, file = "plantmatches.csv") 

# End of downloading range maps from BIEN ----

files <- list.files("plant_ranges/",".shp") #pulling all file names from plant_ranges folder

plant_ranges<-list() #empty list for plant range vector files
name<-substr(files,start=1,stop=nchar(files)-4) #removing the .shp from all the file names

for(a in name){
  
  shp<-readOGR("plant_ranges",a) #importing all the vector shapefiles
  proj4string(shp)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" #projecting the shapefiles
  plant_ranges[a]<-shp #saving the vectors into a list
  
}

#creating map of north america
countries<-c("USA","Mexico","Canada")
north_america<-map(database="world", countries, xlim=c(-125,-103),ylim=c(25,49))

#creating blank raster for rasterize
e<-c(-125,-103,25,49)
r<-raster(,ncol=22,nrow=25)
extent(r)<-e
res(r)<-0.33918298 #30km
#res(r)<-0.67695244 #60km
#res(r)<-1.0128573 #90km

#converting plant range vectors to rasters
range_ras<-lapply(plant_ranges,function(x) {
  ras<-rasterize(x, r)
})

#plotting a raster onto north america
map(north_america)
plot(range_ras[[1]],add=T)

#phenology table
phenology<-read.csv("phenology.csv")

#replace space in accepted name with underscore to match raster names
phenology$species<-gsub(" ", "_", phenology$acceptedname)
phenology<-subset(phenology,species %in% name)

#find number associated with month name
mo2Num <- function(x){
  match(tolower(x), tolower(month.name))
}
phenology$startmonth<-mo2Num(phenology$begin.flowering)
phenology$endmonth<-mo2Num(phenology$end.flowering)

#create time interval for phenology
phenology$date1<-as.POSIXct(paste("2010-",phenology$startmonth,"-1",sep=""))
phenology$date2<-as.POSIXct(paste("2010-",phenology$endmonth,"-1",sep=""))
phenology$interval<-interval(phenology$date1,phenology$date2)

#create vector of species blooming for each month

jan<-phenology$species[as.POSIXct("2010-01-01") %within% phenology$interval]
feb<-phenology$species[as.POSIXct("2010-02-01") %within% phenology$interval]
mar<-phenology$species[as.POSIXct("2010-03-01") %within% phenology$interval]
apr<-phenology$species[as.POSIXct("2010-04-01") %within% phenology$interval]
may<-phenology$species[as.POSIXct("2010-05-01") %within% phenology$interval]
jun<-phenology$species[as.POSIXct("2010-06-01") %within% phenology$interval]
jul<-phenology$species[as.POSIXct("2010-07-01") %within% phenology$interval]
aug<-phenology$species[as.POSIXct("2010-08-01") %within% phenology$interval]
sep<-phenology$species[as.POSIXct("2010-09-01") %within% phenology$interval]
oct<-phenology$species[as.POSIXct("2010-10-01") %within% phenology$interval]
nov<-phenology$species[as.POSIXct("2010-11-01") %within% phenology$interval]
dec<-phenology$species[as.POSIXct("2010-12-01") %within% phenology$interval]

#stack plant raster by months blooming
jan_stack<-stack(range_ras[jan])
feb_stack<-stack(range_ras[feb])
mar_stack<-stack(range_ras[mar])
apr_stack<-stack(range_ras[apr])
may_stack<-stack(range_ras[may])
jun_stack<-stack(range_ras[jun])
jul_stack<-stack(range_ras[jul])
aug_stack<-stack(range_ras[aug])
sep_stack<-stack(range_ras[sep])
oct_stack<-stack(range_ras[oct])
nov_stack<-stack(range_ras[nov])
dec_stack<-stack(range_ras[dec])

#sum rasters in month stacks
jan_calc<-calc(jan_stack, function(x){
  sum(x,na.rm=T)})

feb_calc<-calc(feb_stack, function(x){
  sum(x,na.rm=T)})

mar_calc<-calc(mar_stack, function(x){
  sum(x,na.rm=T)})

apr_calc<-calc(apr_stack, function(x){
  sum(x,na.rm=T)})

may_calc<-calc(may_stack, function(x){
  sum(x,na.rm=T)})

jun_calc<-calc(jun_stack, function(x){
  sum(x,na.rm=T)})

jul_calc<-calc(jul_stack, function(x){
  sum(x,na.rm=T)})

aug_calc<-calc(aug_stack, function(x){
  sum(x,na.rm=T)})

sep_calc<-calc(sep_stack, function(x){
  sum(x,na.rm=T)})

oct_calc<-calc(oct_stack, function(x){
  sum(x,na.rm=T)})

nov_calc<-calc(nov_stack, function(x){
  sum(x,na.rm=T)})

dec_calc<-calc(dec_stack, function(x){
  sum(x,na.rm=T)})

plant_months<-list(jan_calc,feb_calc,mar_calc,apr_calc,may_calc,
                   jun_calc,jul_calc,aug_calc,sep_calc,oct_calc,
                   nov_calc,dec_calc)

plant_df<-lapply(plant_months, function(x)as.data.frame(x,xy=T))
for(a in 1:12){
  plant_df[[a]]$month<-a
}
plant_df<-ldply(plant_df)
plant_df<-SpatialPointsDataFrame(coords=cbind(plant_df$x,plant_df$y),data=plant_df,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
plant_df$cellnum<-cellFromXY(r,plant_df)

names(plant_df)[3]<-"plant_rich"
plant_df<-as.data.frame(plant_df[,3:5])
plant_df<-plant_df[,1:3]
plant_df$plant_rich<-sqrt(plant_df$plant_rich)

#loading centroid bird files
#setwd("~/masters/Hummingbirds/output-data")
#bird_files<-list()
#bird_files <- list.files("output-data",".txt")

#remove ruby throat
#bird_files<-bird_files[!bird_files %in% "west_centroidsArchilochuscolubris.txt"]
#
#setwd("~/masters/Hummingbirds/output-data/output-data")
#bird_pts<-list()
#bird_pts<-lapply(bird_files, function(x)read.table(x, header=T)) 

#migration points
bird_pts <- read.csv("bird_pts.csv")
birds <- bird_pts

#create bird presence dataframe for every cell.
#birds<-ldply(bird_pts)
#birds<-subset(birds,select=c("spname","lon","lat","month"))
birds<-SpatialPointsDataFrame(coords=cbind(birds$lon,birds$lat),data=birds,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
birds$cellnum<-cellFromXY(r,birds)
birds<-as.data.frame(birds)
birds<-na.omit(birds)
birds<-subset(birds, select=c("spname","month","cellnum"))
birds$presence<-1
birds<-unique(birds)


#blank raster set up
uni_birds<-unique(birds$spname)
uni_birds<-unique(birds$spname)
blank<-expand.grid(cellnum=c(1:4615),month=c(1:12),spname=uni_birds) #30km
#blank<-expand.grid(cellnum=c(1:1120),month=c(1:12),spname=uni_birds) #60km
#blank<-expand.grid(cellnum=c(1:528),month=c(1:12),spname=uni_birds) #90km

#create dataframe with all cells with bird presence information
birds_sp<-merge(blank,birds,by=c('cellnum','month','spname'),all.x=TRUE)
birds_sp[is.na(birds_sp)]<-0

#match plant richness to all cells with species presence
pb_sp<-merge(birds_sp,plant_df, by=c("month","cellnum"),all.x=TRUE)
pb_sp[is.na(pb_sp)]<-0

#load elevation raster
elevationr<-raster("alt.bil")

#crop the elevation raster to the proper extent
elevation<-crop(elevationr,extent(-125,-103,25,49))
#make elevation raster more course.
elevation_resam<-aggregate(elevation,fact=2,fun=mean) #30km
#elevation_resam<-aggregate(elevation,fact=4,fun=mean) #60km
#elevation_resam<-aggregate(elevation,fact=6,fun=mean) #90km
elevation_resam[is.na(elevation_resam[])]<-0 #NA data is set at sea level
#resample to get raster to match up with the other rasters.
elevation_resam<-resample(elevation_resam,r,method="ngb")
elevation_data<-as.data.frame(elevation_resam)
elevation_data$cellnum<-c(1:4615) #30km
#elevation_data$cellnum<-c(1:1120) #60km
#elevation_data$cellnum<-c(1:528) #60km
#add elevation to dataframe
birds_spe<-merge(elevation_data,pb_sp,by="cellnum",all.y=TRUE)

#scale elevation
birds_spe$scelev<-as.numeric(scale(birds_spe$alt))

#merge evi data
load("EVI_2004_2014/final_evi.RData")
birds_spe<-merge(final_evi,birds_spe,by=c("month","cellnum"))

#calculate bird richness
#group by factors I wanted to keep
#calculated richness by summing up the presences
birds_rich<-birds_spe %>%
  group_by(month,cellnum,scelev,alt)%>% 
  summarise(rich=sum(presence))

#match plant richness to all cells
pb_rich<-merge(birds_rich,plant_df,by=c("month","cellnum"))

pb_rich<-merge(pb_rich,final_evi,by=c("month","cellnum"))

#removing cells with 0 plants and 0 hummingbirds
L<-(pb_rich$rich != 0) | (pb_rich$plant_rich != 0)
pb_rich<-pb_rich[L,]

S<-(birds_spe$presence != 0) | (birds_spe$plant_rich != 0)
pb_sp<-birds_spe[S,]

#remove evi NAs
pb_rich_evi<-pb_rich[complete.cases(pb_rich),]

pb_sp_evi<-pb_sp[complete.cases(pb_sp),]

#fitting models
source("mod_fit.R")
mig_months <- read.csv("mig_months.csv")
#load("pb_rich_evi.rda")
#load("data/pb_sp_evi.rda")

res_table <- list()
for(ssn in c('spring', 'fall')) {
  for(sp in as.character(unique(pb_sp_evi$spname))) {
    out <- mod_fit(pb_sp_evi, sp, ssn, mig_months)
    res <- lapply(out$mod_out, table_magic)
    res <- ldply(res)
    rsq <- ldply(out$rsq)
    res <- cbind(res, rsq)
    res_table[[paste0(sp, "_", ssn)]] <- res
  }
  #out <- mod_fit(pb_rich_evi, 'all', ssn, mig_months)
  #res <- lapply(out$mod_out, table_magic)
  #res <- ldply(res)
  #rsq <- ldply(out$rsq)
  #res <- cbind(res, rsq)
  #res_table[[paste0("rich_", ssn)]] <- res
}

