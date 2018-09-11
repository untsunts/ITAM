library(forecast)
sp<-read_csv('S&P500_r.csv',col_types = list('Date'=col_date(format='%m/%d/%Y')))
sp<-sp%>%mutate(lnSP=log(PX_LAST))

sp$D5<-NA
sp$D10<-NA
sp$DD5<-NA
sp$DD10<-NA
for(i in 1:(nrow(sp)-5))
{
  sp$D5[i]<-sp$PX_LAST[i+5]-sp$PX_LAST[i]
}
for(i in 1:nrow(sp)-10)
{
  sp$D10[i]<-sp$PX_LAST[i+10]-sp$PX_LAST[i]
}
for(i in 1:nrow(sp))
{
  sp$DD5[i]<-sp$D5[i+1]-sp$D5[i]
}
for(i in 1:nrow(sp))
{
  sp$DD10[i]<-sp$D10[i+1]-sp$D10[i]
}
for(i in 1:nrow(sp-1))
{
  sp$DDD5[i]<-sp$DD5[i+1]-sp$DD5[i]
}
for(i in 1:nrow(sp-1))
{
  sp$DDD10[i]<-sp$DD10[i+1]-sp$DD10[i]
}

sp0.fit<-auto.arima(sp$PX_LAST,d=0,trace=TRUE)
sp1.fit<-auto.arima(sp$PX_LAST,d=1,trace=TRUE)
sp2.fit<-auto.arima(sp$PX_LAST,d=2,trace=TRUE)
sp0ln.fit<-auto.arima(sp$lnSP,d=0,trace=TRUE)
sp1ln.fit<-auto.arima(sp$lnSP,d=1,trace=TRUE)
sp2ln.fit<-auto.arima(sp$lnSP,d=2,trace=TRUE)


sp0.for10<-forecast(sp0.fit,h=10,level=c(66,80,95))
sp1.for10<-forecast(sp1.fit,h=10,level=c(66,80,95))
sp2.for10<-forecast(sp2.fit,h=10,level=c(66,80,95))
sp0ln.for10<-forecast(sp0ln.fit,h=10,level=c(66,80,95))
sp1ln.for10<-forecast(sp1ln.fit,h=10,level=c(66,80,95))
sp2ln.for10<-forecast(sp2ln.fit,h=10,level=c(66,80,95))
sp0.for15<-forecast(sp0.fit,h=15,level=c(66,80,95))
sp1.for15<-forecast(sp1.fit,h=15,level=c(66,80,95))
sp2.for15<-forecast(sp2.fit,h=15,level=c(66,80,95))
sp0.for20<-forecast(sp0.fit,h=20,level=c(66,80,95))
sp1.for20<-forecast(sp1.fit,h=20,level=c(66,80,95))
sp2.for20<-forecast(sp2.fit,h=20,level=c(66,80,95))

par(mfrow=c(1,3))
plot(sp0.for10)
plot(sp1.for10)
plot(sp2.for10)
plot(sp0ln.for10)
plot(sp1ln.for10)
plot(sp2ln.for10)
