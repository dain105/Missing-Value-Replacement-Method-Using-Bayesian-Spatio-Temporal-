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
##################################################################
# making altitude at agricultural weather station 
# using digital elevation dataset on 1km resolution

d.alt<-read.table("S:/Lab/기상환경/NIMR/data/Digital_grd.dat")
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
head(u.alt,3)
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
head(DataVal)
DataFit.s<-names(table(DataFit$tmsid)==1460)[table(DataFit$tmsid)==1460]

DataFit <- DataFit[DataFit$tmsid %in% DataFit.s,]
DataVal <- merge(DataVal, u.alt, by=c("lon","lat"))


coords<-as.matrix(unique(cbind(DataFit[,c("lon","lat")])))
pred.coords<-as.matrix(unique(cbind(DataVal[,c("lon","lat")])))

spatial.decay<-spT.decay(distribution=Gamm(2,1), tuning=0.002)
# spatial.decay<-spatial.decay<-spT.decay(distribution=Unif(0.01,0.02),npoints=5)
# spatial.decay<-spatial.decay<-spT.decay(distribution="FIXED", value=0.006)

library(lubridate)
DataFit$month<-month(DataFit$date)
spring.Fit<-subset(DataFit,DataFit$month>2&DataFit$month<6)
summer.Fit<-subset(DataFit,DataFit$month>5&DataFit$month<9)
fall.Fit<-subset(DataFit,DataFit$month>8&DataFit$month<12)
winter1.Fit<-subset(DataFit,DataFit$month>11)
winter2.Fit<-subset(DataFit,DataFit$month<3)
winter.Fit<-rbind(winter1.Fit,winter2.Fit)

DataVal$month<-month(DataVal$date)
spring.Val<-subset(DataVal,DataVal$month>2&DataVal$month<6)
summer.Val<-subset(DataVal,DataVal$month>5&DataVal$month<9)
fall.Val<-subset(DataVal,DataVal$month>8&DataVal$month<12)
winter1.Val<-subset(DataVal,DataVal$month>11)
winter2.Val<-subset(DataVal,DataVal$month<3)
winter.Val<-rbind(winter1.Val,winter2.Val)


  # para.spatial<-seq(0.03,0.05,0.001)
  # length(para.spatial)
  # gp.mtx <- matrix(rep(NA, 7*21),ncol=7)
  
  priors<-spT.priors(model="GP",inv.var.prior=Gamm(2,1),
                     beta.prior=Norm(0,10^4))

    
# for (i in 1:21){
#   print("#######################################")
#   print(i)
#   
  set.seed(12343)
  m_spring <- spT.Gibbs(formula=temp ~ lon + lat,
                    time.data=spT.time(t.series=92, segment=4),
                    data=spring.Fit,
                    model="GP",
                    coords=coords,
                    priors=priors,
                    nItr=13000,
                    nBurn=3000,
                    cov.fnc="exponential",
                    report=3000,
                    # distance.method="geodetic:km",
                    # spatial.decay=spatial.decay,
                    spatial.decay=spT.decay(distribution="FIXED", value=0.041),
                    tol.dist=0.1)
  
  #plot(m_spring)
  summary(m_spring)
  autocorr.diag(as.mcmc(m_spring))
  pred.spring <- predict(m_spring, newdata=spring.Val, newcoords=pred.coords, type="spatial", tol.dist=0.05)
  spring.mtx<-spT.validation(as.numeric(paste(spring.Val$temp)),c(pred.spring$Median))

  
  set.seed(12343)
  m_summer <- spT.Gibbs(formula=temp ~ lon + lat,
                        time.data=spT.time(t.series=92, segment=4),
                        data=summer.Fit,
                        model="GP",
                        coords=coords,
                        priors=priors,
                        nItr=13000,
                        nBurn=3000,
                        cov.fnc="exponential",
                        report=3000,
                        # distance.method="geodetic:km",
                        # spatial.decay=spatial.decay,
                        spatial.decay=spT.decay(distribution="FIXED", value=0.041),
                        tol.dist=0.1)
  
  plot(m_summer)
  summary(m_summer)
  autocorr.diag(as.mcmc(m_summer))
  pred.summer <- predict(m_summer, newdata=summer.Val, newcoords=pred.coords, type="spatial", tol.dist=0.05)
  summer.mtx<-spT.validation(as.numeric(paste(summer.Val$temp)),c(pred.summer$Median))
  
  
  
  set.seed(12343)
  m_fall <- spT.Gibbs(formula=temp ~ lon + lat,
                      time.data=spT.time(t.series=92, segment=4),
                      data=fall.Fit,
                      model="GP",
                      coords=coords,
                      priors=priors,
                      nItr=13000,
                      nBurn=3000,
                      cov.fnc="exponential",
                      report=3000,
                      # distance.method="geodetic:km",
                      # spatial.decay=spatial.decay,
                      spatial.decay=spT.decay(distribution="FIXED", value=0.041),
                      tol.dist=0.1)
  
  plot(m_fall)
  summary(m_fall)
  autocorr.diag(as.mcmc(m_fall))
  pred.fall <- predict(m_fall, newdata=fall.Val, newcoords=pred.coords, type="spatial", tol.dist=0.05)
  fall.mtx<-spT.validation(as.numeric(paste(fall.Val$temp)),c(pred.fall$Median))
  
  
  
  
  set.seed(12343)
  m_winter <- spT.Gibbs(formula=temp ~ lon + lat,
                        time.data=spT.time(t.series=92, segment=4),
                        data=winter.Fit,
                        model="GP",
                        coords=coords,
                        priors=priors,
                        nItr=13000,
                        nBurn=3000,
                        cov.fnc="exponential",
                        report=3000,
                        # distance.method="geodetic:km",
                        # spatial.decay=spatial.decay,
                        spatial.decay=spT.decay(distribution="FIXED", value=0.041),
                        tol.dist=0.1)
  
  plot(m_winter)
  summary(m_winter)
  autocorr.diag(as.mcmc(m_winter))
  pred.winter <- predict(m_winter, newdata=winter.Val, newcoords=pred.coords, type="spatial", tol.dist=0.05)
  winter.mtx<-spT.validation(as.numeric(paste(winter.Val$temp)),c(pred.winter$Median))
  
  

