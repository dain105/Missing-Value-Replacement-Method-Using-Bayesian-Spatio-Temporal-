gp.mtx<-read.csv("S:/Users/Dain/연구/베이지안시공간 모형을 이용한 농업기상정보 결측치 보정/결과/ByStation/gp.mtx.csv")

get(paste0("pred",i,",",j))

boxplot(gp.mtx[1:25,2:13],boxwex =0.25,pch = "-",xlab="hour",ylab="ozone")
#월별 예츠
for(i in 1:12){
  assign(paste0("med8.",i),get(paste0("pred",i,",",8))$Median)
}
station8<- do.call(rbind, lapply( paste0("med8.",1:12), get))

for(i in 1:12){
  assign(paste0("med9.",i),get(paste0("pred",i,",",9))$Median)
}
station9<- do.call(rbind, lapply( paste0("med9.",1:12), get))

for(i in 1:12){
  assign(paste0("med12.",i),get(paste0("pred",i,",",12))$Median)
}
station12<- do.call(rbind, lapply( paste0("med12.",1:12), get))

for(i in 1:12){
  assign(paste0("med13.",i),get(paste0("pred",i,",",13))$Median)
}
station13<- do.call(rbind, lapply( paste0("med13.",1:12), get))


for(i in 1:12){
  assign(paste0("med14.",i),get(paste0("pred",i,",",14))$Median)
}
station14<- do.call(rbind, lapply( paste0("med14.",1:12), get))

for(i in 1:12){
  assign(paste0("med16.",i),get(paste0("pred",i,",",16))$Median)
}
station16<- do.call(rbind, lapply( paste0("med16.",1:12), get))

for(i in 1:12){
  assign(paste0("med22.",i),get(paste0("pred",i,",",22))$Median)
}
station22<- do.call(rbind, lapply( paste0("med22.",1:12), get))

#monthly
boxplot(pred.m_fit$Median)
par(mfrow=c(1,2))
boxplot(station8,main="station8")
boxplot(pred.m_fit$Median[,8],main="station8 of all predictions")
par(mfrow=c(1,2))
boxplot(station9,main="station9")
boxplot(pred.m_fit$Median[,9],main="station9 of all predictions")
par(mfrow=c(1,2))
boxplot(station12,main="station12")
boxplot(pred.m_fit$Median[,12],main="station12 of all predictions")
par(mfrow=c(1,2))
boxplot(station13,main="station13")
boxplot(pred.m_fit$Median[,13],main="station13 of all predictions")
par(mfrow=c(1,2))
boxplot(station16,main="station16")
boxplot(pred.m_fit$Median[,16],main="station16 of all predictions")
par(mfrow=c(1,2))
boxplot(station22,main="station22")
boxplot(pred.m_fit$Median[,22],main="station22 of all predictions")

#weather
boxplot(pred.spring$Median,ylim=c(-5,25))
boxplot(pred.summer$Median)
boxplot(pred.fall$Median)
boxplot(pred.winter$Median)
pred.median<-pred.m_fit$Median
boxplot(pred.median[,])
boxplot(pred.median[60:151,],ylim=c(-5,25))
length(DataVal$temp)
pred.spring$Median[60:151,1]

spT.validation(as.numeric(paste(spring.Val$temp[1:368])),pred.spring$Median[,1])
spT.validation(as.numeric(paste(DataVal$temp[60:151])),pred.m_fit$Median[60:151,1])

spT.validation(as.numeric(paste(summer.Val$temp[1:368])),pred.summer$Median[,1])
spT.validation(as.numeric(paste(DataVal$temp[152:243])),pred.m_fit$Median[152:243])

spT.validation(as.numeric(paste(fall.Val$temp[1:364])),pred.fall$Median[,1])
spT.validation(as.numeric(paste(DataVal$temp[244:334])),pred.m_fit$Median[244:334])

spT.validation(as.numeric(paste(winter.Val$temp)),c(pred.winter$Median))
spT.validation(as.numeric(paste(DataVal$temp[335:365])),pred.m_fit$Median[335:365])
