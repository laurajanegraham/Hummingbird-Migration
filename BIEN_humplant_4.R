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

#run BIEN_API_FILE_Compilation.R before this code
#workspace is set as BIEN folder in R for masters

#all plant names run together
setwd("~/masters/Hummingbirds/R for masters/BIEN3")
all_plants<-read.csv("all_plants.csv")
plant_vector<-all_plants$acceptedname
#pulling all BIEN range maps for species vector
setwd("~/masters/Hummingbirds/R for masters/2BIEN3")
plant_matches<-BIEN.ranges.species(plant_vector, matched=T) 
#writing file of which species range maps were downloaded and not
write.csv(plant_matches, file = "Laura_plantmatches.csv") 

setwd("~/masters/Hummingbirds/R for masters/2BIEN3")
files <- list.files("plant_ranges",".shp") #pulling all file names from plant_ranges folder

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
setwd("~/masters/Hummingbirds/R for masters/BIEN3")
phenology<-read.csv("phenology.csv")
setwd("~/masters/Hummingbirds/R for masters/2BIEN3")
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

#loading bird files
setwd("~/masters/Hummingbirds/output-data")
bird_files<-list()
bird_files <- list.files("output-data",".txt")

#remove ruby throat
bird_files<-bird_files[!bird_files %in% "west_centroidsArchilochuscolubris.txt"]

setwd("~/masters/Hummingbirds/output-data/output-data")
bird_pts<-list()
bird_pts<-lapply(bird_files, function(x)read.table(x, header=T)) 

setwd("~/masters/Hummingbirds/R for masters/2BIEN3")

#create bird presence dataframe for every cell.
birds<-ldply(bird_pts)
birds<-subset(birds,select=c("spname","lon","lat","month"))
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
birds_sp<-merge(blank,birds,by=c('cellnum','month','spname'),all.x=T)
birds_sp[is.na(birds_sp)]<-0

#match plant richness to all cells with species presence
pb_sp<-merge(birds_sp,plant_df, by=c("month","cellnum"),all.x=T)
pb_sp[is.na(pb_sp)]<-0

#load elevation raster
setwd("~/masters/Hummingbirds/R for masters/2BIEN3/alt_10m_bil")
elevationr<-raster("alt.bil")
setwd("~/masters/Hummingbirds/R for masters/2BIEN3")

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
birds_spe<-merge(elevation_data,pb_sp,by="cellnum",all.y=T)

#scale elevation
birds_spe$scelev<-as.numeric(scale(birds_spe$alt))

#merge evi data
load("~/masters/Hummingbirds/R for masters/EVI_2004_2014/final_evi.RData")
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
bird_rich_models<-list()
bird_rich_models[[1]]<-glmer(cbind(pb_rich_evi$rich,4-pb_rich_evi$rich)~ (1|pb_rich_evi$month),
                       family=binomial)
bird_rich_models[[2]]<-glmer(cbind(pb_rich_evi$rich,4-pb_rich_evi$rich)~ pb_rich_evi$plant_rich + (1|pb_rich_evi$month),
                             family=binomial)
#bird_rich_models[[3]]<-glmer(cbind(pb_rich$rich,4-pb_rich$rich)~ pb_rich$scelev + (1|pb_rich$month),
 #                            family=binomial)
bird_rich_models[[3]]<-glmer(cbind(pb_rich_evi$rich,4-pb_rich_evi$rich)~ pb_rich_evi$plant_rich + pb_rich_evi$scelev + (1|pb_rich_evi$month),
                             family=binomial)
#bird_rich_models[[4]]<-glmer(cbind(pb_rich$rich,4-pb_rich$rich)~ pb_rich$plant_rich + pb_rich$scelev + pb_rich$evi + (1|pb_rich$month),
                             #family=binomial)
spring_rich<-pb_rich_evi %>%
  filter(month >= 2 & month <= 5)
bird_rich_models[[4]]<-glmer(cbind(spring_rich$rich,4-spring_rich$rich)~ (1|spring_rich$month),
                             family=binomial)
bird_rich_models[[5]]<-glmer(cbind(spring_rich$rich,4-spring_rich$rich)~ spring_rich$plant_rich + (1|spring_rich$month),
                             family=binomial)
#bird_rich_models[[7]]<-glmer(cbind(spring_rich$rich,4-spring_rich$rich)~ spring_rich$scelev + (1|spring_rich$month),
 #                            family=binomial)
bird_rich_models[[6]]<-glmer(cbind(spring_rich$rich,4-spring_rich$rich)~ spring_rich$plant_rich + spring_rich$scelev + (1|spring_rich$month),
                             family=binomial)
#bird_rich_models[[8]]<-glmer(cbind(spring_rich$rich,4-spring_rich$rich)~ spring_rich$plant_rich + spring_rich$scelev + spring_rich$evi + (1|spring_rich$month),
       #                      family=binomial)

fall_rich<-pb_rich_evi %>%
  filter(month >= 5 & month <= 11)
bird_rich_models[[7]]<-glmer(cbind(fall_rich$rich,4-fall_rich$rich)~ (1|fall_rich$month),
                             family=binomial)
bird_rich_models[[8]]<-glmer(cbind(fall_rich$rich,4-fall_rich$rich)~ fall_rich$plant_rich + (1|fall_rich$month),
                              family=binomial)
#bird_rich_models[[11]]<-glmer(cbind(fall_rich$rich,4-fall_rich$rich)~ fall_rich$scelev + (1|fall_rich$month),
 #                            family=binomial)
bird_rich_models[[9]]<-glmer(cbind(fall_rich$rich,4-fall_rich$rich)~ fall_rich$plant_rich + fall_rich$scelev + (1|fall_rich$month),
                             family=binomial)
#bird_rich_models[[12]]<-glmer(cbind(fall_rich$rich,4-fall_rich$rich)~ fall_rich$plant_rich + fall_rich$scelev + fall_rich$evi + (1|fall_rich$month),
 #                             family=binomial)
bird_rich_models[[10]]<-glmer(cbind(pb_rich_evi$rich,4-pb_rich_evi$rich)~ pb_rich_evi$plant_rich + pb_rich_evi$scelev + 
                                pb_rich_evi$plant_rich*pb_rich_evi$scelev + (1|pb_rich_evi$month), family=binomial)
bird_rich_models[[11]]<-glmer(cbind(spring_rich$rich,4-spring_rich$rich)~ spring_rich$plant_rich + spring_rich$scelev +
                                                      spring_rich$plant_rich*spring_rich$scelev + (1|spring_rich$month),family=binomial)
bird_rich_models[[12]]<-glmer(cbind(fall_rich$rich,4-fall_rich$rich)~ fall_rich$plant_rich + fall_rich$scelev + 
                                                      fall_rich$plant_rich*fall_rich$scelev + (1|fall_rich$month), family=binomial)

bird_rich_rsq<-list()
bird_rich_rsq<-lapply(bird_rich_models, function(x) r.squaredGLMM(x))

#individual species models
alex_models<-list()

alex_data<-pb_sp_evi %>%
  filter(spname == 'Archilochusalexandri')
alex_models[[1]]<-glmer(alex_data$presence~ (1|alex_data$month),
                        family=binomial)
alex_models[[2]]<-glmer(alex_data$presence~ alex_data$plant_rich + (1|alex_data$month),
      family=binomial) 
#alex_models[[3]]<-glmer(alex_data$presence~ alex_data$scelev + (1|alex_data$month),
   #   family=binomial)
alex_models[[3]]<-glmer(alex_data$presence~ alex_data$plant_rich + alex_data$scelev + (1|alex_data$month),
                        family=binomial)
#alex_models[[4]]<-glmer(alex_data$presence~ alex_data$plant_rich + alex_data$scelev + alex_data$evi + (1|alex_data$month),
 #                       family=binomial)

alex_spring<-alex_data %>% filter(month >= 2 & month <= 6)
alex_models[[4]]<-glmer(alex_spring$presence~ (1|alex_spring$month),
      family=binomial)
alex_models[[5]]<-glmer(alex_spring$presence~ alex_spring$plant_rich + (1|alex_spring$month),
                        family=binomial)
#alex_models[[7]]<-glmer(alex_spring$presence~ alex_spring$scelev + (1|alex_spring$month),
           #             family=binomial)
alex_models[[6]]<-glmer(alex_spring$presence~ alex_spring$plant_rich + alex_spring$scelev + (1|alex_spring$month),
                        family=binomial)
#alex_models[[8]]<-glmer(alex_spring$presence~ alex_spring$plant_rich + alex_spring$scelev + alex_spring$evi + (1|alex_spring$month),
 #                       family=binomial)

alex_fall<-alex_data %>% filter(month >= 6 & month <= 12)
alex_models[[7]]<-glmer(alex_fall$presence~ (1|alex_fall$month),
                        family=binomial)
alex_models[[8]]<-glmer(alex_fall$presence~ alex_fall$plant_rich + (1|alex_fall$month),
                         family=binomial)
#alex_models[[11]]<-glmer(alex_fall$presence~ alex_fall$scelev + (1|alex_fall$month),
  #                      family=binomial)
alex_models[[9]]<-glmer(alex_fall$presence~ alex_fall$plant_rich + alex_fall$scelev + (1|alex_fall$month),
                        family=binomial)
#alex_models[[12]]<-glmer(alex_fall$presence~ alex_fall$plant_rich + alex_fall$scelev + alex_fall$evi + (1|alex_fall$month),
 #                        family=binomial)
alex_models[[10]]<-glmer(alex_data$presence~ alex_data$plant_rich + alex_data$scelev + 
                           alex_data$plant_rich*alex_data$scelev + (1|alex_data$month), family=binomial)
alex_models[[11]]<-glmer(alex_spring$presence~ alex_spring$plant_rich + alex_spring$scelev + 
                           alex_spring$plant_rich*alex_spring$scelev + (1|alex_spring$month), family=binomial)
alex_models[[12]]<-glmer(alex_fall$presence~ alex_fall$plant_rich + alex_fall$scelev + 
                           alex_fall$plant_rich*alex_fall$scelev + (1|alex_fall$month), family=binomial)

alex_rsq<-lapply(alex_models, function(x) r.squaredGLMM(x))

cal_models<-list()
cal_data<-pb_sp_evi %>% filter(spname == 'Selasphoruscalliope')

cal_models[[1]]<-glmer(cal_data$presence~ (1|cal_data$month),
      family=binomial)
cal_models[[2]]<-glmer(cal_data$presence~ cal_data$plant_rich + (1|cal_data$month),
      family=binomial)
#cal_models[[3]]<-glmer(cal_data$presence~ cal_data$scelev + (1|cal_data$month),
      #                 family=binomial)
cal_models[[3]]<-glmer(cal_data$presence~ cal_data$plant_rich + cal_data$scelev + (1|cal_data$month),
                       family=binomial)
#cal_models[[4]]<-glmer(cal_data$presence~ cal_data$plant_rich + cal_data$scelev + cal_data$evi + (1|cal_data$month),
 #                      family=binomial)

cal_spring<-cal_data %>% filter(month >= 3 & month <= 5)
cal_models[[4]]<-glmer(cal_spring$presence~ (1|cal_spring$month),
                       family=binomial)
cal_models[[5]]<-glmer(cal_spring$presence~ cal_spring$plant_rich + (1|cal_spring$month),
                       family=binomial)
#cal_models[[6]]<-glmer(cal_spring$presence~ cal_spring$scelev + (1|cal_spring$month),
          #             family=binomial)
cal_models[[6]]<-glmer(cal_spring$presence~ cal_spring$plant_rich + cal_spring$scelev + (1|cal_spring$month),
                       family=binomial)
#cal_models[[8]]<-glmer(cal_spring$presence~ cal_spring$plant_rich + cal_spring$scelev + cal_spring$evi + (1|cal_spring$month),
 #                      family=binomial)

cal_fall<-cal_data %>% filter(month >= 5 & month <= 11)
cal_models[[7]]<-glmer(cal_fall$presence~ (1|cal_fall$month),
                       family=binomial)
cal_models[[8]]<-glmer(cal_fall$presence~ cal_fall$plant_rich + (1|cal_fall$month),
                        family=binomial)
#cal_models[[11]]<-glmer(cal_fall$presence~ cal_fall$scelev + (1|cal_fall$month),
 #                      family=binomial)
cal_models[[9]]<-glmer(cal_fall$presence~ cal_fall$plant_rich + cal_fall$scelev + (1|cal_fall$month),
                       family=binomial)
#cal_models[[12]]<-glmer(cal_fall$presence~ cal_fall$plant_rich + cal_fall$scelev + cal_fall$evi + (1|cal_fall$month),
 #                       family=binomial)
cal_models[[10]]<-glmer(cal_data$presence~ cal_data$plant_rich + cal_data$scelev +
  cal_data$plant_rich*cal_data$scelev + (1|cal_data$month), family=binomial)
cal_models[[11]]<-glmer(cal_spring$presence~ cal_spring$plant_rich + cal_spring$scelev +
        cal_spring$plant_rich*cal_spring$scelev + (1|cal_spring$month), family=binomial)
cal_models[[12]] <- glmer(cal_fall$presence~ cal_fall$plant_rich + cal_fall$scelev +
                            cal_fall$plant_rich*cal_fall$scelev + (1|cal_fall$month), family=binomial)

cal_rsq<-lapply(cal_models, function(x) r.squaredGLMM(x))

plat_models<-list()
plat_data<-pb_sp_evi %>% filter(spname == 'Selasphorusplatycercus')

plat_models[[1]]<-glmer(plat_data$presence~ (1|plat_data$month),
                        family=binomial)
plat_models[[2]]<-glmer(plat_data$presence~ plat_data$plant_rich + (1|plat_data$month),
                        family=binomial)
#plat_models[[3]]<-glmer(plat_data$presence~ plat_data$scelev + (1|plat_data$month),
 #                       family=binomial)
plat_models[[3]]<-glmer(plat_data$presence~ plat_data$plant_rich + plat_data$scelev + (1|plat_data$month),
                        family=binomial)
#plat_models[[4]]<-glmer(plat_data$presence~ plat_data$plant_rich + plat_data$scelev + plat_data$evi + (1|plat_data$month),
 #                       family=binomial)

plat_spring<-plat_data %>% filter(month >= 2 & month <= 7)

plat_models[[4]]<-glmer(plat_spring$presence~ (1|plat_spring$month),
                        family=binomial)
plat_models[[5]]<-glmer(plat_spring$presence~ plat_spring$plant_rich + (1|plat_spring$month),
                        family=binomial)
#plat_models[[6]]<-glmer(plat_spring$presence~ plat_spring$scelev + (1|plat_spring$month),
     #                   family=binomial)
plat_models[[6]]<-glmer(plat_spring$presence~ plat_spring$plant_rich + plat_spring$scelev + (1|plat_spring$month),
                        family=binomial)
#plat_models[[8]]<-glmer(plat_spring$presence~ plat_spring$plant_rich + plat_spring$scelev + plat_spring$evi + (1|plat_spring$month),
 #                       family=binomial)

plat_fall<-plat_data %>% filter(month >= 7 & month <= 12)
plat_models[[7]]<-glmer(plat_fall$presence~ (1|plat_fall$month),
                        family=binomial)
plat_models[[8]]<-glmer(plat_fall$presence~ plat_fall$plant_rich + (1|plat_fall$month),
                         family=binomial)
#plat_models[[11]]<-glmer(plat_fall$presence~ plat_fall$scelev + (1|plat_fall$month),
   #                      family=binomial)
plat_models[[9]]<-glmer(plat_fall$presence~ plat_fall$plant_rich + plat_fall$scelev + (1|plat_fall$month),
                         family=binomial)
#plat_models[[12]]<-glmer(plat_fall$presence~ plat_fall$plant_rich + plat_fall$scelev + plat_fall$evi + (1|plat_fall$month),
 #                        family=binomial)
plat_models[[10]]<-glmer(plat_data$presence~ plat_data$plant_rich + plat_data$scelev + 
                           plat_data$plant_rich*plat_data$scelev + (1|plat_data$month), family=binomial)
plat_models[[11]]<-glmer(plat_spring$presence~ plat_spring$plant_rich + plat_spring$scelev + 
                           plat_spring$plant_rich*plat_spring$scelev + (1|plat_spring$month), family=binomial)
plat_models[[12]]<-glmer(plat_fall$presence~ plat_fall$plant_rich + plat_fall$scelev + 
                           plat_fall$plant_rich*plat_fall$scelev + (1|plat_fall$month), family=binomial)

plat_rsq<-lapply(plat_models, function(x) r.squaredGLMM(x))

ruf_models<-list()
ruf_data<-pb_sp_evi %>% filter(spname == 'Selasphorusrufus')

ruf_models[[1]]<-glmer(ruf_data$presence~ (1|ruf_data$month),
                       family=binomial)
ruf_models[[2]]<-glmer(ruf_data$presence~ ruf_data$plant_rich + (1|ruf_data$month),
                       family=binomial)
#ruf_models[[3]]<-glmer(ruf_data$presence~ ruf_data$scelev + (1|ruf_data$month),
 #                      family=binomial)
ruf_models[[3]]<-glmer(ruf_data$presence~ ruf_data$plant_rich + ruf_data$scelev + (1|ruf_data$month),
                       family=binomial)
#ruf_models[[4]]<-glmer(ruf_data$presence~ ruf_data$plant_rich + ruf_data$scelev + ruf_data$evi + (1|ruf_data$month),
 #                      family=binomial)

ruf_spring<-ruf_data %>% filter(month >= 1 & month <= 5)
ruf_models[[4]]<-glmer(ruf_spring$presence~ (1|ruf_spring$month),
                       family=binomial)
ruf_models[[5]]<-glmer(ruf_spring$presence~ ruf_spring$plant_rich + (1|ruf_spring$month),
                       family=binomial)
#ruf_models[[6]]<-glmer(ruf_spring$presence~ ruf_spring$scelev + (1|ruf_spring$month),
 #                      family=binomial)
ruf_models[[6]]<-glmer(ruf_spring$presence~ ruf_spring$plant_rich + ruf_spring$scelev + (1|ruf_spring$month),
                       family=binomial)
#ruf_models[[8]]<-glmer(ruf_spring$presence~ ruf_spring$plant_rich + ruf_spring$scelev + ruf_spring$evi + (1|ruf_spring$month),
 #                      family=binomial)

ruf_fall<-ruf_data %>% filter(month >= 5 & month <= 11)
ruf_models[[7]]<-glmer(ruf_fall$presence~ (1|ruf_fall$month),
                       family=binomial)
ruf_models[[8]]<-glmer(ruf_fall$presence~ ruf_fall$plant_rich + (1|ruf_fall$month),
                        family=binomial)
#ruf_models[[11]]<-glmer(ruf_fall$presence~ ruf_fall$scelev + (1|ruf_fall$month),
 #                       family=binomial)
ruf_models[[9]]<-glmer(ruf_fall$presence~ ruf_fall$plant_rich + ruf_fall$scelev + (1|ruf_fall$month),
                        family=binomial)
#ruf_models[[12]]<-glmer(ruf_fall$presence~ ruf_fall$plant_rich + ruf_fall$scelev + ruf_fall$evi + (1|ruf_fall$month),
 #                       family=binomial)
ruf_models[[10]]<-glmer(ruf_data$presence~ ruf_data$plant_rich + ruf_data$scelev + 
                          ruf_data$plant_rich*ruf_data$scelev + (1|ruf_data$month), family=binomial)
ruf_models[[11]]<-glmer(ruf_spring$presence~ ruf_spring$plant_rich + ruf_spring$scelev + 
                          ruf_spring$plant_rich*ruf_spring$scelev + (1|ruf_spring$month), family=binomial)
ruf_models[[12]]<-glmer(ruf_fall$presence~ ruf_fall$plant_rich + ruf_fall$scelev + 
                          ruf_fall$plant_rich*ruf_fall$scelev + (1|ruf_fall$month),family=binomial)

ruf_rsq<-lapply(ruf_models, function(x) r.squaredGLMM(x))


#AIC tests
bird_yr<-AIC(bird_rich_models[[1]],bird_rich_models[[2]],bird_rich_models[[3]])
bird_sp<-AIC(bird_rich_models[[4]],bird_rich_models[[5]],bird_rich_models[[6]])
bird_fa<-AIC(bird_rich_models[[7]],bird_rich_models[[8]],bird_rich_models[[9]])

alex_yr<-AIC(alex_models[[1]],alex_models[[2]],alex_models[[3]])
alex_sp<-AIC(alex_models[[4]],alex_models[[5]],alex_models[[6]])
alex_fa<-AIC(alex_models[[7]],alex_models[[8]],alex_models[[9]])

cal_yr<-AIC(cal_models[[1]],cal_models[[2]],cal_models[[3]])
cal_sp<-AIC(cal_models[[4]],cal_models[[5]],cal_models[[6]])
cal_fa<-AIC(cal_models[[7]],cal_models[[8]],cal_models[[9]])

plat_yr<-AIC(plat_models[[1]],plat_models[[2]],plat_models[[3]])
plat_sp<-AIC(plat_models[[4]],plat_models[[5]],plat_models[[6]])
plat_fa<-AIC(plat_models[[7]],plat_models[[8]],plat_models[[9]])

ruf_yr<-AIC(ruf_models[[1]],ruf_models[[2]],ruf_models[[3]])
ruf_sp<-AIC(ruf_models[[4]],ruf_models[[5]],ruf_models[[6]])
ruf_fa<-AIC(ruf_models[[7]],ruf_models[[8]],ruf_models[[9]])

#save
save(bird_rich_rsq, file="bird_rich_rsq.Rdata")
save(bird_sp,file="bird_sp.Rdata")
save(bird_fa,file="bird_fa.Rdata")
save(alex_rsq,file="alex_rsq.Rdata")
save(cal_rsq,file="cal_rsq.Rdata")
save(plat_rsq,file="plat_rsq.Rdata")
save(ruf_rsq,file="ruf_rsq.Rdata")
save(alex_yr,file="alex_yr.Rdata")
save(alex_sp,file="alex_sp.Rdata")
save(alex_fa,file="alex_fa.Rdata")
save(cal_yr,file="cal_yr.Rdata")
save(cal_sp,file="cal_sp.Rdata")
save(cal_fa,file="cal_fa.Rdata")
save(plat_yr,file="plat_yr.Rdata")
save(plat_sp,file="plat_sp.Rdata")
save(plat_fa,file="plat_fa.Rdata")
save(ruf_yr,file="ruf_yr.Rdata")
save(ruf_sp,file="ruf_sp.Rdata")
save(ruf_fa,file="ruf_fa.Rdata")