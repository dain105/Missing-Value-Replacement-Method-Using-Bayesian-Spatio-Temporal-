library(stringr)
library(XML)

#################################################################
# Making a dataset

setwd("S:/Lab/기상환경/NIMR/data/농진청")
list.files()

agri.stat<-read.csv("지리정보_강원+전남.csv",header=T)

setwd("S:/Lab/기상환경/NIMR/data/농진청/강원도")
l.gangwon<-list.files()

data.1<-list()

for (i in 1:length(l.gangwon)){
temp<-readHTMLTable(htmlParse(l.gangwon[i]))
temp2<-data.frame(temp)
temp3<-temp2[-1:-4,]
head(temp3)
names(temp3)<-c("date","temp","rh","wd","ws","prep")
head(temp3)
temp4<-temp3[,-4]
nchar(l.gangwon[1])
year <-substr(l.gangwon[i], nchar(l.gangwon[i])-7,nchar(l.gangwon[i])-4)
id <-substr(l.gangwon[i], 1,nchar(l.gangwon[i])-9)

head(temp4)
temp4$id <- rep(id, dim(temp4)[1])
temp4$year <- rep(year, dim(temp4)[1])

data.1<-rbind(data.1,temp4)
}

# write.csv(data.1,"data.csv")

setwd("S:/Lab/기상환경/NIMR/data/농진청/전남")
l.gangwon<-list.files()

data.2<-list()

for (i in 1:length(l.gangwon)){
  
  temp<-readHTMLTable(htmlParse(l.gangwon[i]))
  temp2<-data.frame(temp)
  temp3<-temp2[-1:-4,]
  head(temp3)
  names(temp3)<-c("date","temp","rh","wd","ws","prep")
  head(temp3)
  temp4<-temp3[,-4]
  nchar(l.gangwon[1])
  year <-substr(l.gangwon[i], nchar(l.gangwon[i])-7,nchar(l.gangwon[i])-4)
  id <-substr(l.gangwon[i], 1,nchar(l.gangwon[i])-9)
  
  head(temp4)
  temp4$id <- rep(id, dim(temp4)[1])
  temp4$year <- rep(year, dim(temp4)[1])
  
  data.2<-rbind(data.2,temp4)
}

# write.csv(data.2,"data.csv")

a.data <- rbind(data.1, data.2)
a.data2 <- a.data[a.data$year != "자료없음",]
cbind(unique(a.data2$id),paste(agri.stat[,1]))
names(agri.stat) <-c("id","lon","lat")
agri.stat$id <- unique(a.data2$id)

a.data3 <-merge(a.data2, agri.stat, by="id", all=T)
head(a.data3)   #농촌진흥청에서 운영하는 농업기상 정보

setwd("S:/Lab/기상환경/NIMR/data/기상청")
c.data1<-read.csv("ASOS.csv", header=T)
names(c.data1) <-c("tmsid", "date","temp","prec","ws","rh")

c.data2<-read.csv("AWS.csv", header=T)
names(c.data2) <-c("tmsid", "date","temp","prec","ws")

c.data2$rh<-NA
c.data<-rbind(c.data1, c.data2)

c.stat1<-read.csv("stnInfo_ASOS.csv",header=T)
c.stat2<-read.csv("stnInfo_AWS.csv",header=T)
c.stat<-rbind(c.stat1, c.stat2)

head(c.stat)
names(c.stat)<-c("tmsid","name","lat","lon","alt")

c.data3 <-merge(c.data, c.stat, by="tmsid", all=T)

#######################################################################################

library(spTimer)

c.data3$date <- as.Date(c.data3$date)
a.data3$date <- as.Date(a.data3$date)

DataFit<-c.data3[c.data3$lat>37,]
DataVal<-a.data3[a.data3$lat>37,]

DataFit <- DataFit[format(DataFit$date, "%Y") != "2017",]
DataVal <- DataVal[format(DataVal$date, "%Y") != "2017",]

DataFit <- DataFit[format(DataFit$date, "%m-%d") != "02-29",]
DataVal <- DataVal[format(DataVal$date, "%m-%d") != "02-29",]

DataFit.s<-names(table(DataFit$tmsid)==1460)[table(DataFit$tmsid)==1460]

DataFit <- DataFit[DataFit$tmsid %in% DataFit.s,]

z<-(split(DataVal,DataVal$id))
str(z)
#강원도 삼척시 근덕동 1825개
#강원도 태백시 매봉산 730개
#강원도 삼척시 미로면 730개
#강원도_정선군_임계면 	1095 obs. of  10 variables:
#강원도_정선군_신동읍 	730 obs. of  10 variables:
z1<-subset(DataVal,DataVal$id!="강원도_태백시_매봉산")
z1<-subset(z1,z1$id!="강원도_삼척시_미로면")
z1<-subset(z1,z1$id!="강원도_정선군_임계면")
z1<-subset(z1,z1$id!="강원도_정선군_신동읍")
z1<-subset(z1,z1$id!="강원도_삼척시_근덕동")
dim(z1)
z2<-subset(DataVal,DataVal$id=="강원도_삼척시_근덕동")
z2<-z2[1:1460,]
DataVal<-rbind(z1,z2)
dim(DataVal)
33580/23
coords<-as.matrix(unique(cbind(DataFit[,c("lon","lat","alt")])))

pred.coords<-as.data.frame(unique(cbind(DataVal[,c("id","lon","lat")])))

##################################################################
# alt<-as.data.frame(cbind(pred.alt@coords, pred.alt@data$var1.pred))
# names(alt)<-c("lon","lat","alt")
# dim(alt)


#######################################################################
######################################################################

##################################################################
# making altitude at agricultural weather station 
# using digital elevation dataset on 1km resolution

d.alt<-read.table("S:/Lab/기상환경/NIMR/data/Digital_grd.dat")
names(d.alt)<-c("x","y","lat","lon","alt","a","b","c")

d.alt <- d.alt[d.alt$lat<36 & d.alt$lon<127.8,]
t1<-unique(DataVal[,c("lon","lat")])
t1<-as.data.frame(t1)
d.alt<-as.data.frame(d.alt)
library(sp)
library(gstat)

coordinates(d.alt) = ~lon+lat
coordinates(t1) = ~lon+lat

pred.alt <- idw(alt~1,d.alt, newdata=t1, idp=2)
alt<-cbind(pred.alt@coords, pred.alt@data$var1.pred)
u.alt<-unique(alt)
colnames(u.alt)<-c("lon","lat","alt")


DataVal <- merge(DataVal, u.alt, by=c("lon","lat"))
head(DataVal,3)


####################################################3
####################################################3
#spatial.decay<-spT.decay(distribution=Gamm(2,1), tuning=0.002)
# spatial.decay<-spatial.decay<-spT.decay(distribution=Unif(0.01,0.02),npoints=5)
# spatial.decay<-spatial.decay<-spT.decay(distribution="FIXED", value=0.006)


gp.mtx <- matrix(rep(NA, 7*12),ncol=7)
para.spatial<-seq(0.05,0.070,0.005)
priors<-spT.priors(model="GP",inv.var.prior=Gamm(2,1),
                   beta.prior=Norm(0,10^4))

# length(para.spatial)
# for (i in 1:12){
#   print("#######################################")
#   print(i)
  
  set.seed(12343)
  m_gp <- spT.Gibbs(formula=temp ~ lon + lat+ alt,
                    time.data=spT.time(t.series=365, segment=4),
                    data=DataFit,
                    model="GP",
                    coords=coords[,1:2],
                    priors=priors,
                    nItr=13000,
                    nBurn=3000,
                    cov.fnc="exponential",
                    report=1000,
                    # distance.method="geodetic:km",
                    # spatial.decay=spatial.decay,
                    spatial.decay=spT.decay(distribution="FIXED", value=0.068),
                    tol.dist=0.1)
  # 
  # library(coda)
  # plot(m_gp)
  # summary(m_gp)
  # autocorr.diag(as.mcmc(m_gp))

  pred.gp <- predict(m_gp, newdata=DataVal, newcoords=pred.coords[,2:3], type="spatial", tol.dist=0.05)

 
  gp.mtx<-spT.validation(as.numeric(paste(DataVal$temp)),c(pred.gp$Median))

# m_ar <- spT.Gibbs(formula=temp ~ lon + lat + alt,
#                   time.data=spT.time(t.series=365, segment=4),
#                   data=DataFit,
#                   model="AR",
#                   coords=coords,
#                   nItr=2000,
#                   nBurn=1000,
#                   cov.fnc="exponential",
#                   distance.method="geodetic:km",
#                   spatial.decay=spT.decay(distribution="FIXED", value=para.spatial[i]),
#                   tol.dist=0.1)
# 
# # plot(m_ar)
# # summary(m_ar)
# # autocorr.diag(as.mcmc(m_ar))
# 
# pred.ar <- predict(m_ar, newdata=DataVal, newcoords=pred.coords, type="spatial", tol.dist=0.05)

# ar.mtx[i,]<-spT.validation(as.numeric(paste(DataVal$temp)),c(pred.ar$Median))
}

gp.mtx
 # ar.mtx

plot(gp.mtx[,3], axes=F, type="o", xlab="spatial decay parameter", ylab="RMSE")
axis(1, at=1:10, para.spatial)
axis(2)
box()
