library(sp)
library(gstat)

head(gg)
gg2<-gg
coordinates(gg2)<-c("lon","lat")
grd<-as.data.frame(spsample(gg2, "regular",n=50000))
names(grd)<-c("lon","lat")
coordinates(grd)<-c("lon","lat")
gridded(grd)=TRUE
fullgrid(grd)<-TRUE

mse.idw <-gstat::idw(mean.mtx~1, gg2, newdata=grd, idp=2)
install.packages("raster")
library(raster)
r<- raster(mse.idw)
plot(r)
library(maps)
library(mapdata)
map(database='worldHires',region='South Korea', add=T)

library(ggmap)
p<-get_map("Korea",maptype="toner",source="stamen")
plot(p)
