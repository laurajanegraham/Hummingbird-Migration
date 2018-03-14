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