library(matrixStats)

setwd("~/masters/Hummingbirds/R for masters/EVI_2004_2014") 
evi<-list()
evi<-list.files(,".csv")
evi_yr<-lapply(evi, function(x)read.csv(x, header=T))
evi_yr<-lapply(evi_yr,function(x)x[order(x$system.index),])

evi_r<-lapply(evi_yr,function(x)data.frame(x$mean))
evi_means<-do.call("cbind",evi_r)

years<-2004:2014
colnames(evi_means)<-years

evi_avg<-as.data.frame(rowMeans(evi_means))
ident<-data.frame(evi_yr[[1]]$system.index,evi_yr[[1]]$id)
evi_avg<-cbind(ident,evi_avg)
colnames(evi_avg)<-c("system.index","id","mean")

matrix_evi<-data.matrix(evi_means)
evi_sd<-apply(matrix_evi,1,sd)
evi_sd<-as.data.frame(evi_sd)
hist(evi_sd)

evi_avg$month<-data.frame(do.call('rbind',strsplit(as.character(evi_avg$system.index),'_',fixed=T)))
evi_avg<-data.frame(evi_avg$month,evi_avg$id,evi_avg$mean)
colnames(evi_avg)<-c("month","index","id","mean")
evi_avg$cmonth<-0

evi_avg[which(evi_avg$month==0),5]<-1
evi_avg[which(evi_avg$month==1),5]<-2
evi_avg[which(evi_avg$month==2),5]<-3
evi_avg[which(evi_avg$month==3),5]<-4
evi_avg[which(evi_avg$month==4),5]<-5
evi_avg[which(evi_avg$month==5),5]<-6
evi_avg[which(evi_avg$month==6),5]<-7
evi_avg[which(evi_avg$month==7),5]<-8
evi_avg[which(evi_avg$month==8),5]<-9
evi_avg[which(evi_avg$month==9),5]<-10
evi_avg[which(evi_avg$month==10),5]<-11
evi_avg[which(evi_avg$month==11),5]<-12

final_evi<-data.frame(evi_avg$id,evi_avg$cmonth,evi_avg$mean)
colnames(final_evi)<-c('cellnum','month','evi') #30km

#changing resolution
centroids<-read.csv("raster_centroids.csv")
evi_coords<-merge(final_evi,centroids,by=c("cellnum"),all.x=T)
evi_coords<-evi_coords[which(complete.cases(evi_coords)),]

evi_months<-list()
for(a in 1:12){
  evi_months[[a]]<-evi_coords[which(evi_coords$month==a),]
}



evi_spat<-lapply(evi_months, function(x){
  SpatialPointsDataFrame(coords=cbind(x$x,x$y),data=x,
                         proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
})



e<-c(-125,-103,25,49)
r<-raster(,ncol=22,nrow=25)
extent(r)<-e
res(r)<-0.33918298
evi_ras<-list()

evi_ras<-lapply(evi_spat,function(x) {
  rasterize(x,r,field=x$evi)
})

evi_60<-lapply(evi_ras, function(x){
  aggregate(x,fact=2,fun=mean)
})

evi_90<-lapply(evi_ras,function(x){
  aggregate(x,fact=3,fun=mean)
})

res(r)<-0.67695244 #60km
evi_60<-lapply(evi_60,function(x)as.data.frame(x,xy=T))
for(a in 1:12){
  evi_60[[a]]$month<-a
}
evi_60<-ldply(evi_60)
evi_60pts<-SpatialPointsDataFrame(coords=cbind(evi_60$x,evi_60$y),data=evi_60,
                       proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
evi_60$cellnum<-cellFromXY(r,evi_60pts)
evi_60<-evi_60[3:5]
colnames(evi_60)<-c("evi","month","cellnum")

res(r)<-1.0128573 #90km
evi_90<-lapply(evi_90,function(x)as.data.frame(x,xy=T))
for(a in 1:12){
  evi_90[[a]]$month<-a
}
evi_90<-ldply(evi_90)
evi_90pts<-SpatialPointsDataFrame(coords=cbind(evi_90$x,evi_90$y),data=evi_90,
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
evi_90$cellnum<-cellFromXY(r,evi_90pts)
evi_90<-evi_90[3:5]
colnames(evi_90)<-c("evi","month","cellnum")
