library(matrixStats)

# load in all files except the 2004 data (we aren't using 2004)
evi <- list.files("EVI_2004_2014",".csv", full.names = TRUE)
f_remove <- list.files("EVI_2004_2014", "2004", full.names = TRUE)
evi <- evi[!evi %in% f_remove]
evi_yr<-lapply(evi, function(x) read.csv(x, header=TRUE))

# ensure all years are in the same order
evi_yr<-lapply(evi_yr,function(x) x[order(x$system.index),])

# get just the mean (EVI value) and put into column per year
evi_r<-lapply(evi_yr,function(x) data.frame(x$mean))
evi_means<-do.call("cbind",evi_r)

# get an average EVI across years 
evi_avg <- as.data.frame(rowMeans(evi_means, na.rm = TRUE))
ident <- data.frame(evi_yr[[1]]$system.index, evi_yr[[1]]$id)
evi_avg<-cbind(ident,evi_avg)
colnames(evi_avg)<-c("system.index","id","mean")


evi_avg$month <- data.frame(do.call('rbind',strsplit(as.character(evi_avg$system.index),'_',fixed=TRUE)))
evi_avg <- data.frame(evi_avg$month,evi_avg$id,evi_avg$mean)
colnames(evi_avg) <- c("month","index","id","mean")
evi_avg$cmonth <- as.numeric(as.character(evi_avg$month)) + 1

final_evi <- data.frame(evi_avg$id,evi_avg$cmonth,evi_avg$mean)
colnames(final_evi) <- c('cellnum','month','evi') #30km
save(final_evi, file="EVI_2004_2014/final_evi.RData")
#changing resolution
#centroids<-read.csv("raster_centroids.csv")
#evi_coords<-merge(final_evi,centroids,by=c("cellnum"),all.x=T)
#evi_coords<-evi_coords[which(complete.cases(evi_coords)),]

#evi_months<-list()
#for(a in 1:12){
  #evi_months[[a]]<-evi_coords[which(evi_coords$month==a),]
#}



#evi_spat<-lapply(evi_months, function(x){
  #SpatialPointsDataFrame(coords=cbind(x$x,x$y),data=x,
                         #proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
#})



#e<-c(-125,-103,25,49)
#r<-raster(,ncol=22,nrow=25)
#extent(r)<-e
#res(r)<-0.33918298
#evi_ras<-list()

#evi_ras<-lapply(evi_spat,function(x) {
  #rasterize(x,r,field=x$evi)
#})

#evi_60<-lapply(evi_ras, function(x){
  #aggregate(x,fact=2,fun=mean)
#})

#evi_90<-lapply(evi_ras,function(x){
  #aggregate(x,fact=3,fun=mean)
#})

#res(r)<-0.67695244 #60km
#evi_60<-lapply(evi_60,function(x)as.data.frame(x,xy=T))
#for(a in 1:12){
  #evi_60[[a]]$month<-a
#}
#evi_60<-ldply(evi_60)
#evi_60pts<-SpatialPointsDataFrame(coords=cbind(evi_60$x,evi_60$y),data=evi_60,
                       #proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
#evi_60$cellnum<-cellFromXY(r,evi_60pts)
#evi_60<-evi_60[3:5]
#colnames(evi_60)<-c("evi","month","cellnum")

#res(r)<-1.0128573 #90km
#evi_90<-lapply(evi_90,function(x)as.data.frame(x,xy=T))
#for(a in 1:12){
  #evi_90[[a]]$month<-a
#}
#evi_90<-ldply(evi_90)
#evi_90pts<-SpatialPointsDataFrame(coords=cbind(evi_90$x,evi_90$y),data=evi_90,
                                  #proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
#evi_90$cellnum<-cellFromXY(r,evi_90pts)
#evi_90<-evi_90[3:5]
#colnames(evi_90)<-c("evi","month","cellnum")
