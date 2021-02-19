library(stringr)
library(XML)

#################################################################
# Making a dataset

setwd("S:/Lab/ê¸°ìƒ?™˜ê²?/NIMR/data/?†ì§„ì²­")
list.files()

agri.stat<-read.csv("ì§€ë¦¬ì •ë³?_ê°•ì›+? „?‚¨.csv",header=T)

setwd("S:/Lab/ê¸°ìƒ?™˜ê²?/NIMR/data/?†ì§„ì²­/ê°•ì›?„")
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

setwd("S:/Lab/ê¸°ìƒ?™˜ê²?/NIMR/data/?†ì§„ì²­/? „?‚¨")
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
a.data2 <- a.data[a.data$year != "?ë£Œì—†?Œ",]
cbind(unique(a.data2$id),paste(agri.stat[,1]))
names(agri.stat) <-c("id","lon","lat")
agri.stat$id <- unique(a.data2$id)

a.data3 <-merge(a.data2, agri.stat, by="id", all=T)
head(a.data3)   #?†ì´Œì§„?¥ì²??—?„œ ?š´?˜?•˜?Š” ?†?—…ê¸°ìƒ ? •ë³?

setwd("S:/Lab/ê¸°ìƒ?™˜ê²?/NIMR/data/ê¸°ìƒì²?")
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
##################################################################
# making altitude at agricultural weather station 
# using digital elevation dataset on 1km resolution

d.alt<-read.table("S:/Lab/ê¸°ìƒ?™˜ê²?/NIMR/data/Digital_grd.dat")
names(d.alt)<-c("x","y","lat","lon","alt","a","b","c")

d.alt <- d.alt[d.alt$lat<36 & d.alt$lon<127.8,]
t1<-unique(DataVal[,c("lon","lat")])

library(sp)
library(gstat)

coordinates(d.alt) = ~lon+lat
coordinates(t1) = ~lon+lat

pred.alt <- idw(alt~1,d.alt, newdata=t1, idp=2)
alt<-cbind(pred.alt@coords, pred.alt@data$var1.pred)
u.alt<-unique(alt)
colnames(u.alt)<-c("lon","lat","alt")

#######################################################################################

library(spTimer)

c.data3$date <- as.Date(c.data3$date)
a.data3$date <- as.Date(a.data3$date)

DataFit<-c.data3[c.data3$lat<36,]
DataVal<-a.data3[a.data3$lat<36,]

DataFit <- DataFit[format(DataFit$date, "%Y") != "2017",]
DataVal <- DataVal[format(DataVal$date, "%Y") != "2017",]

DataFit <- DataFit[format(DataFit$date, "%m-%d") != "02-29",]
DataVal <- DataVal[format(DataVal$date, "%m-%d") != "02-29",]

DataFit.s<-names(table(DataFit$tmsid)==1460)[table(DataFit$tmsid)==1460]

DataFit <- DataFit[DataFit$tmsid %in% DataFit.s,]
DataVal <- merge(DataVal, u.alt, by=c("lon","lat"))


coords<-as.matrix(unique(cbind(DataFit[,c("lon","lat")])))
pred.coords<-as.matrix(unique(cbind(DataVal[,c("lon","lat")])))

spatial.decay<-spT.decay(distribution=Gamm(2,1), tuning=0.002)
# spatial.decay<-spatial.decay<-spT.decay(distribution=Unif(0.01,0.02),npoints=5)
# spatial.decay<-spatial.decay<-spT.decay(distribution="FIXED", value=0.006)

  gp.mtx <- matrix(rep(NA, 7*12),ncol=7)
  para.spatial<-seq(0.005,0.060,0.005)
  
  priors<-spT.priors(model="GP",inv.var.prior=Gamm(2,1),
                     beta.prior=Norm(0,10^4))

    
for (i in 1:12){
  print("#######################################")
  print(i)
  
  set.seed(12343)
  m_gp <- spT.Gibbs(formula=temp ~ lon + lat+alt,
                    time.data=spT.time(t.series=365, segment=4),
                    data=DataFit,
                    model="GP",
                    coords=coords,
                    priors=priors,
                    nItr=3000,
                    nBurn=1000,
                    cov.fnc="exponential",
                    report=1000,
                    # distance.method="geodetic:km",
                    # spatial.decay=spatial.decay,
                    spatial.decay=spT.decay(distribution="FIXED", value=para.spatial[i]),
                    tol.dist=0.1)
  
   # plot(m_gp)
  # summary(m_gp)
  # autocorr.diag(as.mcmc(m_gp))
  
  pred.gp <- predict(m_gp, newdata=DataVal, newcoords=pred.coords, type="spatial", tol.dist=0.05)
  # pred.gp$Median
  
  gp.mtx[i,]<-spT.validation(as.numeric(paste(DataVal$temp)),c(pred.gp$Median))

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
axis(1, at=1:12, para.spatial)
axis(2)
box()
