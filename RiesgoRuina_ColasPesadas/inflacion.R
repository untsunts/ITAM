library(forecast)

inf<-read_csv('inflacionEEUU.csv')
View(inf)
prom.inf<-rev(inf$Average)
set.seed(100)
par(mfrow=c(1,1))

plot(prom.inf, type='l')
sp0.fit<-auto.arima(prom.inf,d=0,trace=TRUE)
sp1.fit<-auto.arima(prom.inf,d=1,trace=TRUE)
sp2.fit<-auto.arima(prom.inf,d=2,trace=TRUE)


sp0.for10<-forecast(sp0.fit,h=2,level=c(66,80,95))
plot(sp0.for10)

sp0.for10$mean
sp0.for10$x
