library(lubridate)
DataFit$month<-month(DataFit$date)
DataVal$month<-month(DataVal$date)

setwd("S:/Users/Dain/연구/베이지안시공간 모형을 이용한 농업기상정보 결측치 보정/결과/ByStation")
#결측치 확인

temp<-as.numeric(paste0(DataVal$temp))
DataVal$temp<-temp

sum.na<-matrix(NA,25,ncol=12)
for(i in 1:12){
  for( j in 1:25){
    data<-get(paste0("month.val",i,",",j))
    sum.na[j,i]<-sum(is.na(data$temp))
  }}
sum(is.na(DataVal$temp))
str(`month.val1,1`)
#write.csv(sum.na,"sum.na.csv")



for(i in 1:12){
  data<-subset(DataFit,DataFit$month==i)
  as.data.frame(assign(paste0("month.fit",i),data))
}
for(i in 1:12){
  for( j in 1:25){
    data<-subset(DataVal,DataVal$month==i)
    val.site<-unique(data[,3])
    data1<-subset(data,data[,3]==val.site[j])
    as.data.frame(assign(paste0("month.val",i,",",j),data1))
  }}

for( i in 1:12){
  set.seed(12343)
  m_gp <- spT.Gibbs(formula=temp ~ lon + lat+alt,
                    time.data=spT.time(t.series=dim(get(paste0("month.fit",i)))[1]/292, segment=4),
                    data=get(paste0("month.fit",i)),
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
                    tol.dist=0.001)
  assign(paste("gp",i, sep=""),get("m_gp"))
}


# plot(m_gp)
# summary(m_gp)
# autocorr.diag(as.mcmc(m_gp))
gp.mtx<-matrix(NA,25,ncol=12)
for(i in 1:12){
  for( j in 1:25){
    pred.coords<-as.matrix(unique(get(paste0("month.val",i,",",j))[,1:2]))
    pred.gp <- predict(get(paste0("gp",i)), newdata=get(paste0("month.val",i,",",j)), newcoords=pred.coords, type="spatial", tol.dist=0.05)
    gp.mtx[j,i]<-spT.validation(as.numeric(get(paste0("month.val",i,",",j))$temp),c(pred.gp$Median))[2]
  }}

# pred.gp$Median