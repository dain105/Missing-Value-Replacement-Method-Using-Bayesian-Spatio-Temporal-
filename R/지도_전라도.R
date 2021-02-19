station_asos<-read.csv("S:/Lab/기상환경/NIMR/data/기상청/stnInfo_ASOS.csv", header=T)
station_aws<-read.csv("S:/Lab/기상환경/NIMR/data/기상청/stnInfo_AWS.csv", header=T)
nong<-read.csv("S:/Lab/기상환경/NIMR/data/농진청/지리정보_강원+전남.csv", header=T)

colnames(station_asos)<-c("id","loc","lon","lat","a")
colnames(station_aws)<-c("id","loc","lon","lat","a")
# install.packages("ggmap")
library(ggmap)



# indicate to AWS, ASOS, whether station for agiculture, agricultural land)
p<-get_map(location = c(126.5, 34.5), zoom = 8, maptype="toner-lite", source = "stamen")


p<-get_map(location = c(126.5, 34.5), zoom = 8, maptype="terrain")
p<-get_map(location = c(126.5, 34.5), zoom = 8, maptype="roadmap")
plot(p)
# draw of station and agricultural land (blue=KMA, red=RDA, Green=Agricultural land)
ggmap(p)+ geom_point(data=station_asos, aes(x=lat, y=lon), size=2, color="blue", alpha=0.7) +  geom_point(data=station_aws, aes(x=lat, y=lon),size=2, color="blue", alpha=0.7) +
  geom_point(data=nong, aes(x=x, y=y),size=2, color="red", alpha=0.7)
