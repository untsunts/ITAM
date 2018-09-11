library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(dplyr)
library(magrittr)
library(gridExtra)

#leer archivo , separarlo por plazos, transformación logaritmo natural, quitar últimos 12 datos
SP500<-read_csv('S&P500_r.csv')
SP500
SP500<-read_csv('S&P500_r.csv',col_types = list('Date'=col_date(format='%m/%d/%Y')))
year(SP500$Date)<-year(SP500$Date)+2000
id=1:nrow(SP500)
SP500 <- SP500%>%mutate(dia=day(Date), semana=week(Date), mes= month(Date), anio=year(Date), lnSP=log(PX_LAST))
SP500 <- cbind(id,SP500)
head(SP500)
SP500<-SP500%>%filter(id<=nrow(SP500)-12)
#resumen y gráficas de los datos
summary(SP500)
c('fivenum',fivenum(SP500$PX_LAST),'varianza',var(SP500$PX_LAST))
c('fivenum',fivenum(SP500$lnSP),'varianza',var(SP500$lnSP))
par(mfrow=c(1,2))
plot(SP500$PX_LAST, cex=0.75,type='o', col='gray8',pch=22,bg='gray',main='Precio S&P500', xlab='fecha: 1/10/2014 - 1/10/2015', ylab='precio')
grid(NA,7,lwd=1.5)
plot(SP500$lnSP, cex=0.75,type='o', col='gray8',pch=22,bg='gray',main='Precio S&P500', xlab='fecha: 1/10/2014 - 1/10/2015', ylab='precio')
grid(NA,7,lwd=1.5)

#gráficas por plazos
gd<-ggplot(SP500, aes(x= dia, y = PX_LAST))+geom_point(colour='dark blue')
gs<-ggplot(SP500, aes(x= semana, y = PX_LAST))+geom_point(colour='dark red')
gm<-ggplot(SP500, aes(x= mes, y = PX_LAST))+geom_point(colour='dark green')
grid.arrange(gd, gs,gm, nrow=3)

#gráficas por plazos de ln
gdln<-ggplot(SP500, aes(x= dia, y = lnSP))+geom_point(colour='dark blue')
gsln<-ggplot(SP500, aes(x= semana, y = lnSP))+geom_point(colour='dark red')
gmln<-ggplot(SP500, aes(x= mes, y = lnSP))+geom_point(colour='dark green')
grid.arrange(gdln, gsln,gmln, nrow=3)

#agrupar por semana y por mes para graficar
SP500s <- SP500%>%group_by(semana)
ggplot(SP500s, aes(x=Date, y= PX_LAST,colour=as.character(semana), group=semana)) + geom_point() + geom_line()
SP500m <- SP500%>%group_by(mes)
ggplot(SP500m, aes(x=Date, y= PX_LAST,colour=as.character(mes), group=mes)) + geom_point() + geom_line()

#gráfica agrupados de logaritmo natural
ggplot(SP500s, aes(x=Date, y= lnSP,colour=as.character(semana), group=semana)) + geom_point() + geom_line()
ggplot(SP500m, aes(x=Date, y= lnSP,colour=as.character(mes), group=mes)) + geom_point() + geom_line()

#viendo cómo sería algún tipo de ajuste ingenuo para buscar por alguna tendencia
qloess<-ggplot(SP500, aes(x=Date, y= PX_LAST)) + geom_point(colour=as.character(SP500$mes)) +
  stat_smooth(method = "lm", formula = y~x, size = 1)+ylab('Px')

#fijándonos sólo en los meses intermedios
tendencia<-SP500%>%filter(mes!=10&mes!=9&mes!=8)
qlm<-ggplot(tendencia, aes(x=Date, y= PX_LAST)) + geom_point(colour=as.character(tendencia$mes)) +
  stat_smooth(method = "lm", formula = y~x, size = 1)+ylab('Px')
grid.arrange(qloess,qlm,ncol=1,nrow=2)

#ver datos en términos de media y desviación
estimadores_s <- SP500s%>% summarise(media = mean(PX_LAST), varianza = var(PX_LAST), medialn= mean(lnSP), varianzaln= var(lnSP))
estimadores_m <- SP500m%>% summarise(media = mean(PX_LAST), varianza = var(PX_LAST), medialn= mean(lnSP), varianzaln= var(lnSP))

estimadores_s<- estimadores_s%>% mutate(x_inf=media-2*sqrt(varianza),x_sup=media+2*sqrt(varianza),lnx_inf=medialn-2*sqrt(varianzaln),lnx_sup=medialn+2*sqrt(varianzaln)) 
estimadores_m<- estimadores_m%>% mutate(x_inf=media-2*sqrt(varianza),x_sup=media+2*sqrt(varianza),lnx_inf=medialn-2*sqrt(varianzaln),lnx_sup=medialn+2*sqrt(varianzaln)) 

est_s<-as.data.frame(cbind(estimadores_s$media,estimadores_s$x_inf,estimadores_s$x_sup))
est_m<-as.data.frame(cbind(estimadores_m$media,estimadores_m$x_inf,estimadores_m$x_sup))

estln_s<-as.data.frame(cbind(estimadores_s$medialn,estimadores_s$lnx_inf,estimadores_s$lnx_sup))
estln_m<-as.data.frame(cbind(estimadores_m$medialn,estimadores_m$lnx_inf,estimadores_m$lnx_sup))

#resumen de las medias y varianzas
summary(estimadores_s)
summary(estimadores_m)

#gráficas por media y varianza
par(mfrow=c(1,2))
matplot(est_s,type='l',xlab='semana', ylab='', main='Media +- 2 desviaciones')
grid(53,7,lwd=1.4)
matplot(est_m,type='l',xlab='mes',ylab='', main='Media +- 2 desviaciones')
grid(13,7,lwd=1.4)

matplot(estln_s,type='l',xlab='semana', ylab='', main='Media +- 2 desviaciones')
grid(53,7,lwd=1.4)
matplot(estln_m,type='l',xlab='mes',ylab='', main='Media +- 2 desviaciones')
grid(13,7,lwd=1.4)

par(mfrow=c(1,2))
plot(estimadores_s$varianza,cex=1.3,pch=20,col='orange', xlab='semana', ylab='varianza', main='Varianza de Px dentro de cada semana')
grid(NA,12,lwd=1.5)
plot(estimadores_m$varianza,cex=2.0,pch=20,col='orange', xlab='mes', ylab='varianza', main='Varianza de Px dentro de cada mes')
grid(NA,12,lwd=1.5)

plot(estimadores_s$varianzaln,cex=1.3,pch=20,col='orange', xlab='semana', ylab='varianza(log)', main='Varianza de log(Px) dentro de cada semana')
grid(NA,12,lwd=1.5)
plot(estimadores_m$varianzaln,cex=2.0,pch=20,col='orange', xlab='mes', ylab='varianza(log)', main='Varianza de log(Px) dentro de cada mes')
grid(NA,12,lwd=1.5)

