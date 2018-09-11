#TRABAJO FINAL CALCULO ACTUARIAL 3
#Guillermo Santiago Novoa Pérez 000125089
#Lorena Piedras Saenz  000125825

#--------PAQUETES Y LIMPIAR ESPACIO------------
library(lubridate)
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(magrittr)
library(gridExtra)
library(e1071)
library(glmnet)
library(reshape2)
library(fExtremes)
library(fitdistrplus)

rm(list=ls())
set.seed(1)

#------------------ANALISIS EXPLORATORIO--------------
setwd(dir = '~/Documents/ITAM/CalculoActuarialIII/NoDiego/Final/')
datos_act<- read_csv('Allstate_Claim_Data.csv')
str(datos_act)
print(datos_act, n = 13, width = 380)

###
###
###
#inflación y predicción de inflación
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
### 
###
###

#datos_act<-datos_act%>%group_by(Calendar_Year)
datos_act<-datos_act%>%mutate(Age=Calendar_Year-Model_Year)
datos_posit<-datos_act%>%dplyr::filter(Claim_Amount>0.0)
datos5<-datos_posit%>%dplyr::filter(Calendar_Year==2005)
datos6<-datos_posit%>%dplyr::filter(Calendar_Year==2006)
datos7<-datos_posit%>%dplyr::filter(Calendar_Year==2007)

# inflacion<-c(3.33, 4.05, 3.76)
# inflacion<-t(inflacion)
# inflacion<-as.data.frame(inflacion)
# names(inflacion)<-cbind('cinco','seis','siete')

# datos_cinco<-datos_cinco%>%mutate(valor_inf=Claim_Amount*(1+inflacion$cinco)*(1+inflacion$seis))
# datos_seis<-datos_cinco%>%mutate(valor_inf=Claim_Amount*(1+inflacion$seis))
# datos_siete<-datos_siete%>%mutate(valor_inf=Claim_Amount)


#datos_samp<-sample_n(datos_act,100000)

summary(datos_posit$Claim_Amount)
ggplot(datos_posit, aes(x=Claim_Amount)) + geom_density(alpha=.7)
ggplot(datos_posit, aes(x=Claim_Amount)) + geom_histogram(alpha=.7)
#qplot(datos_posit, aes(Claim_Amount), colour = Calendar_Year) + geom_histogram(fill = datos_posit$Calendar_Year)
qplot(Claim_Amount, data= datos_posit,geom = "histogram", colour=Calendar_Year, fill=Calendar_Year) + facet_grid(~Calendar_Year) + geom_point(aes(x=Claim_Amount, y= Age*100), colour="darkgray", alpha=I(1/12))
g<-ggplot(datos_act, aes(x=Claim_Amount, colour=Calendar_Year))
g+geom_histogram() + facet_grid(~Calendar_Year)

# g_p<- ggplot(datos_posit, aes(Claim_Amount))
# g_p<- g_p+geom_histogram() + facet_grid(~Calendar_Year)
# g_p
g_c<- ggplot(datos_posit, aes(Claim_Amount, colour=Calendar_Year)) + geom_histogram() + facet_grid(~Calendar_Year)
g_c


# ggplot(datos_act, aes(x = Calendar_Year, y = Claim_Amount)) +  geom_bar() 
# ggplot(datos_samp, aes(x = Model_Year, y = Claim_Amount, colour = Calendar_Year)) + geom_point(size=2.2)
# ggplot(datos_posit, aes(x = Claim_Amount, colour = Calendar_Year)) + geom_histogram(size=1.5)+facet_wrap(~Calendar_Year)
# 
# resp10 <- cv.glmnet(datos_act, datos_act$Calendar_Year, nfolds=10, alpha=1.0, family = 'binomial',keep = TRUE)
ggplot(datos_posit, aes(Claim_Amount)) + geom_histogram() + facet_grid(~Model_Year)


#----------AJUSTES CON fEXTREMES----------
#----------Ajustes sin separación por años-----
fit.sev.gev <- gevFit(datos_posit$Claim_Amount)
fit.sev.gpd <- gpdFit(datos_posit$Claim_Amount)
 
# ?gevFit
# ?gpdFit

summary(fit.sev.gev)
summary(fit.sev.gpd)

# <-gpdSim(model=list(xi = ))

#-----------Ajustes por cada año-----------------
###

par(mfrow=c(1,2))
#---------2005---------
fit.sev.gev.2005.i <- gevFit(datos5$Claim_Amount)
fit.sev.gpd.2005.i <- gpdFit(datos5$Claim_Amount)

summary(fit.sev.gev.2005.i)
summary(fit.sev.gpd.2005.i) #por log-likelihood, tomamos gpd

set.seed(1)
#simulación
frec5<-gpdSim(model = list(xi =fit.sev.gpd.2005.i@fit$par.ests[1], mu = 0, beta = fit.sev.gpd.2005.i@fit$par.ests[2]), n=nrow(datos5))

#pruebas gráficas de ajuste con la simulación
prueba2005a<-data.frame(datos=datos5$Claim_Amount,sim = frec5)
prueba_2005_p <- prueba2005a %>%
  gather(tipo, costo, datos:GPD)
#qqplot
ggplot(prueba_2005_p) +
  stat_qq(aes(sample = costo, colour = factor(tipo)))
# Interleaved histograms
ggplot(prueba_2005_p, aes(x=costo, fill=tipo)) +
  geom_histogram(binwidth=.5, position="dodge") +
  xlim(c(0,max(prueba_2005_p$costo)+100)) +
  ylim(c(0,30))
# Density plots with semi-transparent fill
ggplot(prueba_2005_p, aes(x=costo, fill=tipo)) + geom_density(alpha=.3) +
  xlim(c(0,max(prueba_2005_p$costo)+100))

#---------2006---------
fit.sev.gev.2006.i <- gevFit(datos6$Claim_Amount)
fit.sev.gpd.2006.i <- gpdFit(datos6$Claim_Amount)

summary(fit.sev.gev.2006.i)
summary(fit.sev.gpd.2006.i)#por log-likelihood, tomamos gpd

set.seed(2)
#simulación
frec6<-gpdSim(model = list(xi =fit.sev.gpd.2006.i@fit$par.ests[1], mu = 0, beta = fit.sev.gpd.2006.i@fit$par.ests[2]), n=nrow(datos6))

#pruebas gráficas de ajuste
prueba2006a<-data.frame(datos=datos6$Claim_Amount,sim = frec6)
prueba_2006_p <- prueba2006a %>%
  gather(tipo, costo, datos:GPD)
#qqplot
ggplot(prueba_2006_p) +
  stat_qq(aes(sample = costo, colour = factor(tipo)))
# Interleaved histograms
ggplot(prueba_2006_p, aes(x=costo, fill=tipo)) +
  geom_histogram(binwidth=.5, position="dodge") +
  xlim(c(0,max(prueba_2006_p$costo)+100)) +
  ylim(c(0,30))
# Density plots with semi-transparent fill
ggplot(prueba_2006_p, aes(x=costo, fill=tipo)) + geom_density(alpha=.3) +
  xlim(c(0,max(prueba_2006_p$costo)+100))

#---------2007---------
fit.sev.gev.2007.i <- gevFit(datos7$Claim_Amount)
fit.sev.gpd.2007.i <- gpdFit(datos7$Claim_Amount)

summary(fit.sev.gev.2007.i)
summary(fit.sev.gpd.2007.i)#por log-likelihood, tomamos gpd

set.seed(3)
#simulación
frec7<-gpdSim(model = list(xi =fit.sev.gpd.2007.i@fit$par.ests[1], mu = 0, beta = fit.sev.gpd.2007.i@fit$par.ests[2]), n=nrow(datos7))

#pruebas gráficas de ajuste con la simulación
prueba2007a<-data.frame(datos=datos7$Claim_Amount,sim = frec7)
prueba_2007_p <- prueba2007a %>%
  gather(tipo, costo, datos:GPD)
#qqplot
ggplot(prueba_2007_p) +
  stat_qq(aes(sample = costo, colour = factor(tipo)))
# Interleaved histograms
ggplot(prueba_2007_p, aes(x=costo, fill=tipo)) +
  geom_histogram(binwidth=.5, position="dodge") +
  xlim(c(0,max(prueba_2007_p$costo)+100)) +
  ylim(c(0,30))
# Density plots with semi-transparent fill
ggplot(prueba_2007_p, aes(x=costo, fill=tipo)) + geom_density(alpha=.3) +
  xlim(c(0,max(prueba_2007_p$costo)+100))

lk5e_f<- -fit.sev.gev.2005.i@fit$llh
lk6e_f<- -fit.sev.gev.2006.i@fit$llh
lk7e_f<- -fit.sev.gev.2007.i@fit$llh

lk5p_f<- -fit.sev.gpd.2005.i@fit$llh
lk6p_f<- -fit.sev.gpd.2006.i@fit$llh
lk7p_f<- -fit.sev.gpd.2007.i@fit$llh

#--------Checar supuesto de tendencia-----
ajuste2006_2005<-as.vector(gpdSim(model = list(xi =fit.sev.gpd.2005.i@fit$par.ests[1], mu = 0, beta = fit.sev.gpd.2005.i@fit$par.ests[2]), n = nrow(datos6)))
prueba4<-data.frame(datos=datos6$Claim_Amount,sim=ajuste2006_2005)
prueba_separada4 <- prueba4 %>%
  gather(tipo, costo, datos:sim)
# Interleaved histograms
ggplot(prueba_separada4, aes(x=costo, fill=tipo)) +
  geom_histogram(binwidth=.5, position="dodge")+ 
  xlim(c(0,max(prueba_separada4$costo+100)))+
  ylim(c(0,30))

# Density plots with semi-transparent fill
ggplot(prueba_separada4, aes(x=costo, fill=tipo)) + geom_density(alpha=.3)


#------------AJUSTES CON EVDBAYES --------------------------
library(evd)
library(evdbayes)
library(coda)
library(lubridate)
library(readr)
library(tidyr)
library(dplyr)
library(magrittr)
#rm(list=ls())
#setwd(dir = '~/Documents/ITAM/CalculoActuarialIII/NoDiego/Final/')
#datos_act<- read_csv('Allstate_Claim_Data.csv')

#separar por año
datos_2005<-datos_act%>%dplyr::filter(Calendar_Year==2005)
datos_2006<-datos_act%>%dplyr::filter(Calendar_Year==2006)
datos_2007<-datos_act%>%dplyr::filter(Calendar_Year==2007)

#separar por positivos
datos_2005p<-datos_2005%>%dplyr::filter(Claim_Amount>0)
datos_2006p<-datos_2006%>%dplyr::filter(Claim_Amount>0)
datos_2007p<-datos_2007%>%dplyr::filter(Claim_Amount>0)

#cantidades por año
theta<-as.data.frame(list(cinco=nrow(datos_2005p)/nrow(datos_2005),seis=nrow(datos_2006p)/nrow(datos_2006),siete=nrow(datos_2007p)/nrow(datos_2007)))
theta[2,]<-c(nrow(datos_2005), nrow(datos_2006), nrow(datos_2007))
theta[3,]<-c(nrow(datos_2005p),nrow(datos_2006p),nrow(datos_2007p))
theta[4,]<-c(prom.inf[92],prom.inf[93],prom.inf[94])
rownames(theta)<-c("proporcion","asegurados","siniestros","inflacion")
lambda<-as.data.frame(t(theta))
theta
lambda


###
#  #------------GEV--------------
###
#------------2005--------------
#ajuste de modelos por evdbayes
set.seed(11)
mat <- diag(c(10000, 10000, 100))
pn <- prior.norm(mean = c(0,0,0), cov = mat)
n <- 10000 ; t0 <- c(5,1,0.1) ; s <- c(.1,.02,.05)
ptpmc <- posterior(n, t0, prior = pn, lh = "gev", data = datos_2005p$Claim_Amount, psd = s)
attributes(ptpmc)$ar
ptp.mcmc <- mcmc(ptpmc, start = 0, end = 10000)
par(mar = rep(2, 4))
plot(ptp.mcmc, den = TRUE, sm = FALSE)

#con valores nuevos
set.seed(12)
t05 <- c(17,26,1.25) 
psd5 <- ar.choice(init = t05, prior = pn, lh = "gev", data = datos_2005p$Claim_Amount, psd =
                   s, tol = rep(0.02, 3))$psd
s5 <- round(psd5, 2)
ptpmc5 <- posterior(n, t05, prior = pn, lh = "gev", data = datos_2005p$Claim_Amount, psd = s5)
attributes(ptpmc)$ar
ptp.mcmc5 <- mcmc(ptpmc5, start = 0, end = 10000)
par(mar = rep(2, 4))
plot(ptp.mcmc5, den = TRUE, sm = FALSE)

#comprobación modelos
ptp.mcmc5 <- window(ptp.mcmc5, start = 1000)
geweke.diag(ptp.mcmc5)
geweke.diag(ptp.mcmc5, 0.2, 0.4)
geweke.plot(ptp.mcmc5)

raftery.diag(ptp.mcmc5, r = 0.01, s = 0.75)

#todo junto?
bwf <- function(x) sd(x)/2
plot(ptp.mcmc5, trace = TRUE, bwf = bwf)
summary(ptp.mcmc5)

autocorr(ptp.mcmc5)
autocorr.plot(ptp.mcmc5)

library(sm)
n5 <- nrow(ptp.mcmc5)
rep5 <- matrix(0,n5,1)
lk5e_b <- 0
#d5 <- rep(0,n5)
set.seed(13)
for(i in 1:n5){ 
  rep5[i]<- rgev(1,ptp.mcmc5[i,1],ptp.mcmc5[i,2],ptp.mcmc5[i,3])
  if(rep5[i]<=0) {rep5[i]=0.000000001}
  lk5e_b<-lk5e_b+gevlik(c(ptp.mcmc5[i,1],ptp.mcmc5[i,2],ptp.mcmc5[i,3]), rep5[i]) #calculo log verosimilitud para comparar modelos
}
d5<-density(rep5)
d5l<-density(log(rep5))
plot(d5) # plots the results
plot(d5l)
hist(log(rep5))
plot(rep5)
rep5max<-apply(rep5,1,max)
d5max<-density(log(rep5max))
plot(d5max) # plots the results
hist(log(rep5max))
par(mfrow=c(1,2))
plot(density(log(datos_2005p$Claim_Amount)))
plot(d5l)

#------------2006--------------
#ajuste de modelos por evdbayes
set.seed(21)
ptpmc <- posterior(n, t0, prior = pn, lh = "gev", data = datos_2006p$Claim_Amount, psd = s)
attributes(ptpmc)$ar
ptp.mcmc <- mcmc(ptpmc, start = 0, end = 10000)
par(mar = rep(2, 4))
plot(ptp.mcmc, den = TRUE, sm = FALSE)

#con valores nuevos
set.seed(22)
t06 <- c(17,27,1.5)
psd6 <- ar.choice(init = t06, prior = pn, lh = "gev", data = datos_2006p$Claim_Amount, psd =
                   s, tol = rep(0.02, 3))$psd
s6<-round(psd6, 2)
ptpmc6 <- posterior(n, t06, prior = pn, lh = "gev", data = datos_2006p$Claim_Amount, psd = s6)
attributes(ptpmc6)$ar
ptp.mcmc6 <- mcmc(ptpmc6, start = 0, end = 10000)
par(mar = rep(2, 4))
plot(ptp.mcmc6, den = TRUE, sm = FALSE)

#comprobación modelos
ptp.mcmc6 <- window(ptp.mcmc6, start = 1200)
geweke.diag(ptp.mcmc6)
geweke.diag(ptp.mcmc6, 0.2, 0.4)
geweke.plot(ptp.mcmc6)

raftery.diag(ptp.mcmc6, r = 0.01, s = 0.75)

#todo junto?
plot(ptp.mcmc6, trace = TRUE, bwf = bwf)
summary(ptp.mcmc6)
autocorr(ptp.mcmc6)
autocorr.plot(ptp.mcmc6)

n6 <- nrow(ptp.mcmc6)
rep6 <- matrix(0,n6,1)
lk6e_b <- 0
set.seed(23)
for(i in 1:n6){ 
  rep6[i]<- rgev(1,ptp.mcmc6[i,1],ptp.mcmc6[i,2],ptp.mcmc6[i,3])
    if(rep6[i]<=0) rep6[i]=0.000000001
  lk6e_b<-lk6e_b+gevlik(c(ptp.mcmc6[i,1],ptp.mcmc6[i,2],ptp.mcmc6[i,3]), rep6[i]) #calculo log verosimilitud para comparar modelos
}
d6<-density(rep6)
d6l<-density(log(rep6))
plot(d6) # plots the results
plot(d6l)
hist(log(rep6))
plot(rep6)
rep6max<-apply(rep6,1,max)
d6max<-density(log(rep6max))
plot(d6max) # plots the results
hist(log(rep6max))
par(mfrow=c(1,2))
plot(density(log(datos_2006p$Claim_Amount)))
plot(d6l)

#------------2007--------------
#ajuste de modelos por evdbayes
set.seed(31)
ptpmc <- posterior(n, t0, prior = pn, lh = "gev", data = datos_2007p$Claim_Amount, psd = s)
attributes(ptpmc)$ar
ptp.mcmc <- mcmc(ptpmc, start = 0, end = 10000)
par(mar = rep(2, 4))
plot(ptp.mcmc, den = TRUE, sm = FALSE)

#con valores nuevos
set.seed(32)
t07 <- c(17.5,26,1.25) 
psd7 <- ar.choice(init = t07, prior = pn, lh = "gev", data = datos_2007p$Claim_Amount, psd =
                   s, tol = rep(0.02, 3))$psd
s7 <- round(psd7, 3)
ptpmc7 <- posterior(n, t07, prior = pn, lh = "gev", data = datos_2007p$Claim_Amount, psd = s7)
attributes(ptpmc7)$ar
ptp.mcmc7 <- mcmc(ptpmc7, start = 0, end = 10000)
par(mar = rep(2, 4))
plot(ptp.mcmc7, den = TRUE, sm = FALSE)

#comprobación modelos
ptp.mcmc7 <- window(ptp.mcmc7, start = 1000)
geweke.diag(ptp.mcmc7)
geweke.diag(ptp.mcmc7, 0.2, 0.4)
geweke.plot(ptp.mcmc7)

raftery.diag(ptp.mcmc7, r = 0.01, s = 0.75)

#todo junto?
plot(ptp.mcmc7, trace = TRUE, bwf = bwf)
summary(ptp.mcmc7)
autocorr(ptp.mcmc7)
autocorr.plot(ptp.mcmc7)

n7 <- nrow(ptp.mcmc7)
rep7 <- matrix(0,n7,1)
lk7e_b <- 0
set.seed(33)
for(i in 1:n7){ 
  rep7[i]<- rgev(1,ptp.mcmc7[i,1],ptp.mcmc7[i,2],ptp.mcmc7[i,3])
    if(rep7[i]<0) rep7[i]=0.000000001
  lk7e_b<-lk7e_b+gevlik(c(ptp.mcmc7[i,1],ptp.mcmc7[i,2],ptp.mcmc7[i,3]), rep7[i]) #calculo log verosimilitud para comparar modelos
}
d7<-density(rep7)
d7l<-density(log(rep7))
plot(d7) # plots the results
plot(d7l)
hist(log(rep7))
plot(rep7)
rep7max<-apply(rep7,1,max)
d7max<-density(log(rep7max))
plot(d7max) # plots the results
hist(log(rep7max))
par(mfrow=c(1,2))
plot(density(log(datos_2007p$Claim_Amount)))
plot(d7l)

# #-------------Comparación 3 años---------------
# 
# library(gridExtra)
# 
plot(ptp.mcmc5,trace=TRUE,bwf=bwf)
plot(ptp.mcmc6,trace=TRUE,bwf=bwf)
plot(ptp.mcmc7,trace=TRUE,bwf=bwf)
# 
# summary(ptp.mcmc5)
# summary(ptp.mcmc6)
# summary(ptp.mcmc7)
# ###

###


#  #-------------PARETO------------
###
#------------2005--------------
#ajuste de modelos por evdbayes
set.seed(11)
s_p<-c(0.01,0.1,0.06)
t0_p<-c(0,1,0)
ptpmc_p <- posterior(n, t0_p, prior = pn, lh = "gpd", data = datos_2005p$Claim_Amount, psd = s_p)
attributes(ptpmc_p)$ar
ptp.mcmc_p <- mcmc(ptpmc_p, start = 0, end = 10000)
par(mar = rep(2, 4))
plot(ptp.mcmc_p, den = TRUE, sm = FALSE)

#con valores nuevos
set.seed(12)
t05_p <- c(0.04,33,1.1) 
psd5_p <- ar.choice(init = t05_p, prior = pn, lh = "gpd", data = datos_2005p$Claim_Amount, psd =
                    s_p, tol = rep(0.02, 3))$psd
s5_p <- round(psd5_p, 2)
ptpmc5_p <- posterior(n, t05_p, prior = pn, lh = "gpd", data = datos_2005p$Claim_Amount, psd = s5_p)
attributes(ptpmc_p)$ar
ptp.mcmc5_p <- mcmc(ptpmc5_p, start = 0, end = 10000)
par(mar = rep(2, 4))
plot(ptp.mcmc5_p, den = TRUE, sm = FALSE)

#comprobación modelos
ptp.mcmc5_p <- window(ptp.mcmc5_p, start = 100)
geweke.diag(ptp.mcmc5_p)
geweke.diag(ptp.mcmc5_p, 0.2, 0.4)
geweke.plot(ptp.mcmc5_p)

raftery.diag(ptp.mcmc5_p, r = 0.01, s = 0.75)

#todo junto?
plot(ptp.mcmc5_p, trace = TRUE, bwf = bwf)
summary(ptp.mcmc5_p)
autocorr(ptp.mcmc5_p)
autocorr.plot(ptp.mcmc5_p)

#simulación
n5p <- nrow(ptp.mcmc5_p)
rep5p <- matrix(0,n5p,1)
lk5p_b<-0
set.seed(13)
for(i in 1:n5p){ 
  rep5p[i]<- evd::rgpd(1,ptp.mcmc5_p[i,1],ptp.mcmc5_p[i,2],ptp.mcmc5_p[i,3])
  if(rep5p[i]<=0) rep5p[i]=0.000000001
  lk5p_b<-lk5p_b+gpdlik(c(ptp.mcmc5_p[i,1],ptp.mcmc5_p[i,2],ptp.mcmc5_p[i,3]), rep5p[i])
}
d5p<-density(rep5p)
d5lp<-density(log(rep5p))
plot(d5p) # plots the results
plot(d5lp)
hist(log(rep5p))
plot(rep5p)
rep5maxp<-apply(rep5p,1,max)
d5maxp<-density(log(rep5maxp))
plot(d5maxp) # plots the results
hist(log(rep5maxp))
par(mfrow=c(1,2))
plot(density(log(datos_2005p$Claim_Amount)))
plot(d5lp)

#------------2006--------------
#ajuste de modelos por evdbayes
set.seed(21)
ptpmc_p <- posterior(n, t0_p, prior = pn, lh = "gpd", data = datos_2006p$Claim_Amount, psd = s_p)
attributes(ptpmc_p)$ar
ptp.mcmc_p <- mcmc(ptpmc_p, start = 0, end = 10000)
par(mar = rep(2, 4))
plot(ptp.mcmc_p, den = TRUE, sm = FALSE)

#con valores nuevos
set.seed(22)
t06_p <- c(-0.03,32,1.2)
psd6_p <- ar.choice(init = t06_p, prior = pn, lh = "gpd", data = datos_2006p$Claim_Amount, psd =
                    s_p, tol = rep(0.02, 3))$psd
s6_p<-round(psd6_p, 2)
ptpmc6_p <- posterior(n, t06_p, prior = pn, lh = "gpd", data = datos_2006p$Claim_Amount, psd = s6_p)
attributes(ptpmc6_p)$ar
ptp.mcmc6_p <- mcmc(ptpmc6_p, start = 0, end = 10000)
par(mar = rep(2, 4))
plot(ptp.mcmc6_p, den = TRUE, sm = FALSE)

#comprobación modelos
ptp.mcmc6_p <- window(ptp.mcmc6_p, start = 100)
geweke.diag(ptp.mcmc6_p)
geweke.diag(ptp.mcmc6_p, 0.2, 0.4)
geweke.plot(ptp.mcmc6_p)

raftery.diag(ptp.mcmc6_p, r = 0.01, s = 0.75)

#todo junto?
plot(ptp.mcmc6_p, trace = TRUE, bwf = bwf)
summary(ptp.mcmc6_p)
autocorr(ptp.mcmc6_p)
autocorr.plot(ptp.mcmc6_p)

#simulación
n6p <- nrow(ptp.mcmc6_p)
rep6p <- matrix(0,n6p,1)
lk6p_b<-0
set.seed(23)
for(i in 1:n6p){ 
  rep6p[i]<- evd::rgpd(1,ptp.mcmc6_p[i,1],ptp.mcmc6_p[i,2],ptp.mcmc6_p[i,3])
    if(rep6p[i]<=0) rep6p[i]=0.000000001
  lk6p_b<-lk6p_b+gpdlik(c(ptp.mcmc6_p[i,1],ptp.mcmc6_p[i,2],ptp.mcmc6_p[i,3]), rep6p[i])
}
d6p<-density(rep6p)
d6lp<-density(log(rep6p))
plot(d6p) # plots the results
plot(d6lp)
hist(log(rep6p))
plot(rep6p)
rep6maxp<-apply(rep6p,1,max)
d6maxp<-density(log(rep6maxp))
plot(d6maxp) # plots the results
hist(log(rep6maxp))
par(mfrow=c(1,2))
plot(density(log(datos_2006p$Claim_Amount)))
plot(d6lp)

#------------2007--------------
#ajuste de modelos por evdbayes
set.seed(31)
ptpmc_p <- posterior(n, t0_p, prior = pn, lh = "gpd", data = datos_2007p$Claim_Amount, psd = s_p)
attributes(ptpmc_p)$ar
ptp.mcmc_p <- mcmc(ptpmc_p, start = 0, end = 10000)
par(mar = rep(2, 4))
plot(ptp.mcmc_p, den = TRUE, sm = FALSE)

#con valores nuevos
set.seed(32)
t07_p <- c(0.015,36,1.05) 
psd7_p <- ar.choice(init = t07_p, prior = pn, lh = "gpd", data = datos_2007p$Claim_Amount, psd =
                    s_p, tol = rep(0.02, 3))$psd
s7_p <- round(psd7_p, 3)
ptpmc7_p <- posterior(n, t07_p, prior = pn, lh = "gpd", data = datos_2007p$Claim_Amount, psd = s7_p)
attributes(ptpmc7_p)$ar
ptp.mcmc7_p <- mcmc(ptpmc7_p, start = 0, end = 10000)
par(mar = rep(2, 4))
plot(ptp.mcmc7_p, den = TRUE, sm = FALSE)

#comprobación modelos
ptp.mcmc7_p <- window(ptp.mcmc7_p, start = 100)
geweke.diag(ptp.mcmc7_p)
geweke.diag(ptp.mcmc7_p, 0.2, 0.4)
geweke.plot(ptp.mcmc7_p)

raftery.diag(ptp.mcmc7_p, r = 0.01, s = 0.75)

#todo junto?
plot(ptp.mcmc7_p, trace = TRUE, bwf = bwf)
summary(ptp.mcmc7_p)
autocorr(ptp.mcmc7_p)
autocorr.plot(ptp.mcmc7_p)

# 
n7p <- nrow(ptp.mcmc7_p)
#simulación
n7p <- nrow(ptp.mcmc7_p)
rep7p <- matrix(0,n7p,1)
lk7p_b<-0
set.seed(33)
for(i in 1:n7p){ 
  rep7p[i]<- evd::rgpd(1,ptp.mcmc7_p[i,1],ptp.mcmc7_p[i,2],ptp.mcmc7_p[i,3])
  if(rep7p[i]<=0) rep7p[i]=0.000000001
  lk7p_b<-lk7p_b+gpdlik(c(ptp.mcmc7_p[i,1],ptp.mcmc7_p[i,2],ptp.mcmc7_p[i,3]), rep7p[i])
}
d7p<-density(rep7p)
d7lp<-density(log(rep7p))
plot(d7p) # plots the results
plot(d7lp)
hist(log(rep7p))
plot(rep7p)
rep7maxp<-apply(rep7p,1,max)
d7maxp<-density(log(rep7maxp))
plot(d7maxp) # plots the results
hist(log(rep7maxp))
par(mfrow=c(1,2))
plot(density(log(datos_2007p$Claim_Amount)))
plot(d7lp)

# d1 <- density(rnorm(100))
# d2 <- density(rnorm(100))

dd5p<-density(log(datos_2005p$Claim_Amount))
dd6p<-density(log(datos_2006p$Claim_Amount))
dd7p<-density(log(datos_2007p$Claim_Amount))

# plot(range(dd7p$x, d7l$x), range(dd7p$y, d7lp$y), type = "n", xlab = "x",
#      ylab = "Density")
# lines(dd7p, col = "red")
# text(mean(dd7p$x)*5,max(dd7p$y)*1.1,"log datos", col='red', cex=0.8)
# lines(d7lp, col = "blue")
# text(mean(d7lp$x)*0.4,max(d7lp$y)*0.9,"log sim", col='blue', cex=0.8)
# 
# plot(range(dd7p$x, d7lp$x), range(dd7p$y, d7lp$y), type = "n", xlab = "x",
#      ylab = "Density")
# lines(dd7p, col = "red")
# text(mean(dd7p$x)*5,max(dd7p$y)*1.1,"log datos", col='red', cex=0.8)
# lines(d7lp, col = "blue")
# text(mean(d7lp$x)*0.3,max(d7lp$y)*0.9,"log sim p", col='blue', cex=0.8)

# #-------------Comparación 3 años---------------
# 
# library(gridExtra)
# 
# plot(ptp.mcmc5_p,trace=TRUE,bwf=bwf)
# plot(ptp.mcmc6_p,trace=TRUE,bwf=bwf)
# plot(ptp.mcmc7_p,trace=TRUE,bwf=bwf)
# 
# summary(ptp.mcmc5_p)
# summary(ptp.mcmc6_p)
# summary(ptp.mcmc7_p)
# 
# ###


#-----------Comparación modelos evdbayes año por año----------
par(mfrow=c(3,1))
#-----------2005---------------
prueba2005b<-data.frame(datos=datos_2005p$Claim_Amount,sim_gev=sample(rep5,nrow(datos_2005p)),sim_gpd = sample(rep5p,nrow(datos_2005p)))
prueba2005_s <- prueba2005b %>%
  gather(tipo, costo, datos:sim_gev:sim_gpd)
#qqplot
ggplot(prueba2005_s) +
  stat_qq(aes(sample = costo, colour = factor(tipo)))
# Interleaved histograms
# ggplot(prueba2005_s, aes(x=costo, fill=tipo)) +
#   geom_histogram(binwidth=.5, position="dodge") +
#   xlim(c(0,max(prueba2005_s$costo)+100)) +
#   ylim(c(0,30))
# Density plots with semi-transparent fill
ggplot(prueba2005_s, aes(x=costo, fill=tipo)) + geom_density(alpha=.3) +
  xlim(c(0,max(prueba2005_s$costo)+100))

#-----------2006---------------
prueba2006b<-data.frame(datos=datos_2006p$Claim_Amount,sim_gev=sample(rep6,nrow(datos_2006p)),sim_gpd = sample(rep6p,nrow(datos_2006p)))
prueba2006_s <- prueba2006b %>%
  gather(tipo, costo, datos:sim_gev:sim_gpd)
#qqplot
ggplot(prueba2006_s) +
  stat_qq(aes(sample = costo, colour = factor(tipo)))
# Interleaved histograms
# ggplot(prueba2006_s, aes(x=costo, fill=tipo)) +
#   geom_histogram(binwidth=.5, position="dodge") +
#   xlim(c(0,max(prueba2005_s$costo)+100)) +
#   ylim(c(0,30))
# Density plots with semi-transparent fill
ggplot(prueba2006_s, aes(x=costo, fill=tipo)) + geom_density(alpha=.3) +
  xlim(c(0,max(prueba2006_s$costo)+100))

#-----------2007---------------
prueba2007b<-data.frame(datos=datos_2007p$Claim_Amount,sim_gev=sample(rep7, nrow(datos_2007p)),sim_gpd = sample(rep7p, nrow(datos_2007p)))
prueba2007_s <- prueba2007b %>%
  gather(tipo, costo, datos:sim_gev:sim_gpd)
#qqplot
ggplot(prueba2007_s) +
  stat_qq(aes(sample = costo, colour = factor(tipo)))
# Interleaved histograms
# ggplot(prueba2007_s, aes(x=costo, fill=tipo)) +
#   geom_histogram(binwidth=.5, position="dodge") +
#   xlim(c(0,max(prueba2007_s$costo)+100)) +
#   ylim(c(0,30))
# Density plots with semi-transparent fill
ggplot(prueba2007_s, aes(x=costo, fill=tipo)) + geom_density(alpha=.3) +
  xlim(c(0,max(prueba2007_s$costo)+100))


#------------AIC,BIC-----------------

AICc<-function(lk,n){
  AICc <- -2*lk+2*3+2*(3+1)/(n-3-1)
}
BIC<-function(lk,n){
  BIC <- -2*lk + 3*log(n)
}

lk.list<-list(c(lk5e_f,lk6e_f,lk7e_f,
                      lk5p_f,lk6p_f,lk7p_f,
                      lk5e_b,lk6e_b,lk7e_b,
                      lk5p_b,lk6p_b,lk7p_b))
n.list<-list(rep(c(n5-1,n6-1,n7-1,n5-1),3))

crit.aic<-mapply(AICc,lk.list,n.list)
crit.bic<-mapply(BIC,lk.list,n.list)



#ahora comparemos el modelo frecuentista contra el bayesiano:
set.seed(101)
sim5f<-gpdSim(model=list(xi= fit.sev.gpd.2005.i@fit$par.ests[1],mu=0,beta=fit.sev.gpd.2005.i@fit$par.ests[2]),n=nrow(datos_2005p))
sim5b<-sample(rep5p,nrow(datos_2005p))
sim5b[which(sim5b>2*max(datos_2005p$Claim_Amount))]<-2*max(datos_2005p$Claim_Amount)
prueba<-data.frame(datos=datos_2005p$Claim_Amount,simf=sim5f,simb=sim5b)
prueba_separada5 <- prueba %>%
  gather(tipo, costo, datos:GPD:simb)
gc1<-ggplot(prueba_separada5) +
  stat_qq(aes(sample = costo, colour = factor(tipo)))
# Density plots with semi-transparent fill
gc2<-ggplot(prueba_separada5, aes(x=costo, fill=tipo)) + geom_density(alpha=.3)

sim6f<-as.data.frame(gpdSim(model=list(xi= fit.sev.gpd.2006.i@fit$par.ests[1],mu=0,beta=fit.sev.gpd.2006.i@fit$par.ests[2]),n=nrow(datos_2006p)))
sim6b<-sample(rep6p,nrow(datos_2006p))
sim6b[which(sim6b>2*max(datos_2006p$Claim_Amount))]<-2*max(datos_2006p$Claim_Amount)
prueba<-data.frame(datos=datos_2006p$Claim_Amount, simf=sim6f, simb=sim6b)
prueba_separada6 <- prueba %>%
  gather(tipo, costo, datos:GPD:simb)
gc3<-ggplot(prueba_separada6) +
  stat_qq(aes(sample = costo, colour = factor(tipo)))
gc4<-ggplot(prueba_separada6, aes(x=costo, fill=tipo)) + geom_density(alpha=.3)

sim7f<-gpdSim(model=list(xi= fit.sev.gpd.2007.i@fit$par.ests[1],mu=0,beta=fit.sev.gpd.2007.i@fit$par.ests[2]),n=nrow(datos_2007p))
sim7b<-sample(rep5p,nrow(datos_2007p))
sim7b[which(sim7b>2*max(datos_2007p$Claim_Amount))]<-2*max(datos_2007p$Claim_Amount)
prueba<-data.frame(datos=datos_2007p$Claim_Amount,simf=sim7f,simb=sim7b)
prueba_separada7 <- prueba %>%
  gather(tipo, costo, datos:GPD:simb)
gc5<-ggplot(prueba_separada7) +
  stat_qq(aes(sample = costo, colour = factor(tipo)))
gc6<-ggplot(prueba_separada7, aes(x=costo, fill=tipo)) + geom_density(alpha=.3)
gc5
gc6

grid.arrange(gc1,gc2,gc3,gc4,gc5,gc6, ncol=2,nrow=3)
grid.arrange(gc1,gc3,gc5, ncol=1,nrow=3)
grid.arrange(gc2,gc4,gc6, ncol=1,nrow=3)

grid.arrange(gc6,ncol=1,nrow=1)
#el problema de las colas pesadas es que para encontrar nuestra distr. empírica no podemos asegurar que los eventos
#muy poco probables pero que causan pérdidas catastróficas hayan aparecido, el enfoque bayesiano es más precavido en este aspecto

#elegimos modelos pareto por los valores de sus log-verosimilitudes, 
#elegimos el método bayesiano por la comparación en qqplots y densidades, además de porque tiene más cuidado en las colas.




#--------------N----------------------
library(gridExtra)

lambda<-lambda%>% mutate(year=rownames(lambda))
q1<-ggplot(lambda, aes(x=year,y=proporcion))+
  geom_point(col='red')+
  xlab('proporcion')
q2<-ggplot(lambda,aes(x=year,y=asegurados))+
  geom_point(col='blue')+
  xlab('asegurados')
q3<-ggplot(lambda,aes(x=year,y=siniestros))+
  geom_point(col='orange')+
  xlab('siniestros')
q4<-ggplot(lambda,aes(x=year,y=inflacion)) +
  geom_point(col='black')+
  xlab('inflacion')

grid.arrange(q1,q2,q3,q4, ncol=1, nrow =4)

#por el análisis descriptivo de los datos y las gráficas anteriores podemos suponer que existe una tendencia para los datos anuales
#tomaremos la tendencia exclusivamente para el número de siniestros (mayores a 0) dado que estamos utilizando un modelo colectivo

par(mfrow=c(1,1))
sin<-rep(0,2)
sin[1]<-2*lambda$siniestros[3]-lambda$siniestros[2]
sin[2]<-2*sin[1]-lambda$siniestros[3]

#--------simulación------------
N<-matrix(0,2,1000)
rownames(N)<-c('2008','2009')
set.seed(1000)
for(i in 1:2) N[i,]<-rpois(1000,sin[i])

plot(ptp.mcmc5_p, trace = TRUE, bwf = bwf)
plot(ptp.mcmc6_p, trace = TRUE, bwf = bwf)
plot(ptp.mcmc7_p, trace = TRUE, bwf = bwf)

#------comparar distribuciones para ver cómo las aproximamos----------
library(plyr)
mu <- list(primer=ptp.mcmc5_p[,1],segundo=ptp.mcmc6_p[,1],tercero=ptp.mcmc7_p[,1])
non.null.list <- lapply(mu, Filter, f = Negate(is.null))
aux_mu<-rbind.fill(lapply(non.null.list, as.data.frame))
colnames(aux_mu)<-c('primer','segundo','tercero')
mu_g <- aux_mu %>%
  gather(tipo, costo, primer:segundo:tercero) %>%
  dplyr::filter(costo !="") 


sigma <- list(primer=ptp.mcmc5_p[,2],segundo=ptp.mcmc6_p[,2],tercero=ptp.mcmc7_p[,2])
non.null.list <- lapply(sigma, Filter, f = Negate(is.null))
aux_sigma<-rbind.fill(lapply(non.null.list, as.data.frame))
colnames(aux_sigma)<-c('primer','segundo','tercero')
sigma_g <- aux_sigma %>%
  gather(tipo, costo, primer:segundo:tercero) %>%
  dplyr::filter(costo !="") 

xi <- list(primer=ptp.mcmc5_p[,3],segundo=ptp.mcmc6_p[,3],tercero=ptp.mcmc7_p[,3])
non.null.list <- lapply(xi, Filter, f = Negate(is.null))
aux_xi<-rbind.fill(lapply(non.null.list, as.data.frame))
colnames(aux_xi)<-c('primer','segundo','tercero')
xi_g <- aux_xi %>%
  gather(tipo, costo, primer:segundo:tercero) %>%
  dplyr::filter(costo !="") 

#año contra año de cada distribución

# Density plots with semi-transparent fill
t1<-ggplot(mu_g, aes(x=costo, fill=tipo)) + geom_density(alpha=.3)+xlab('mu')
# Density plots with semi-transparent fill
t2<-ggplot(sigma_g, aes(x=costo, fill=tipo)) + geom_density(alpha=.3)+xlab('sigma')
# Density plots with semi-transparent fill
t3<-ggplot(xi_g, aes(x=costo, fill=tipo)) + geom_density(alpha=.3)+xlab('xi')

grid.arrange(t1,t2,t3,nrow=3,ncol=1)
#mezcla tomando al tercer año (2007) con mayor peso que el segundo (2006) y el primero(2005)

#--------------Simulación S y teoría de ruina----------------
mx<-2*max(datos_posit$Claim_Amount)
S<-matrix(0,2,1000)
set.seed(1001)
X<-list()
Y<-list()
for(j in 1:2){
  for(i in 1:1000){
    indicador<-sample(1:10000,N[j,i])
    x<-rep(0,N[j,i])
    for(k in 1:N[j,i]){
      u<-runif(1)
      if(u<.5){
        x[k]<-evd::rgpd(1,ptpmc7_p[indicador[k],1],ptpmc7_p[indicador[k],2],ptpmc7_p[indicador[k],3])
      }
      else{
        if(u<.8){
          x[k]<-evd::rgpd(1,ptpmc6_p[indicador[k],1],ptpmc6_p[indicador[k],2],ptpmc6_p[indicador[k],3])
        }else{
          x[k]<-evd::rgpd(1,ptpmc5_p[indicador[k],1],ptpmc5_p[indicador[k],2],ptpmc5_p[indicador[k],3])
        }
      }
      if(x[k]>mx) x[k]=mx
      S[j,i]=S[j,i]+x[k]
    }
    if(j==1){X[i]<-list(x)}else{Y[i]<-list(x)}
  }
}

probabilidadRuina<-function (u,teta){
  tiempos <-rexp(n,lambda)
  tiempos <-tiempos/sum(tiempos)
  prima <- (1+teta)*mean(datos)*lambda
  s<-datos[1]
  Z<-0
  tiempo.recorr<-tiempos[1]
  i<-0
  
  while(i < n & Z==0 ){
    if(prima*tiempo.recorr+ u >= s){
      s=s+datos[i+1]
      tiempo.recorr<-tiempo.recorr+tiempos[i+1]
      i<-i+1
    }
    else{
      Z<-1
    }
  }
  Z
}

u<-rep(1:10,each=10)*4000
th<-rep(seq(0.10,1,by=0.1),10)
Z<-matrix(0,1000,100)

set.seed(1002)
for(i in 1:1000){
  aux<-as.data.frame(X[[i]])
  aux2<-as.matrix(aux)
  n<-nrow(aux2)
  datos<-aux2
  lambda<-sin[1]
  for(j in 1:100){
    Z[i,j]<-probabilidadRuina(u=u[j],teta=th[j])
  }
}

Zprob<-apply(Z,2,mean)
H<-matrix(0,1000,100)
system(command='say Phase one done')

set.seed(1003)
for(i in 1:1000){
  aux<-as.data.frame(Y[[i]])
  aux2<-as.matrix(aux)
  n<-nrow(aux2)
  datos<-aux2
  lambda<-sin[2]
  for(j in 1:100){
    H[i,j]<-probabilidadRuina(u[j],th[j])
  }
}
Hprob<-apply(H,2,mean)
system(command='say FINISHED RUNNING! YEAAAH!')


Prob<- data.frame(ocho=Zprob,nueve=Hprob,U=u,theta=th)
prob <-Prob %>%
  gather(year, probability, ocho:nueve) 

ggplot(prob, aes(x=U, y=probability, colour=year)) + 
  geom_point(alpha=0.75, size=1.5)+
  geom_hline(yintercept = 0.05, col='darkred', alpha=0.5) + 
  facet_wrap(~theta)

prob.prima.2008<-max(Zprob[which(Zprob<0.05)])
prob.prima.2009<-max(Hprob[which(Hprob<0.05)])

ind.prima.2008<-which(Zprob==prob.prima.2008)
ind.prima.2009<-which(Hprob==prob.prima.2009)

par.prima.2008<-c(Prob$U[ind.prima.2008],Prob$theta[ind.prima.2008])
par.prima.2009<-c(Prob$U[ind.prima.2009],Prob$theta[ind.prima.2009])

set.seed(1)
prima.2008<-(1+par.prima.2008[2])*mean(c(sample(rep5p,2000),sample(rep6p,3000), sample(rep7p,5000)))*sin[1]/(1+.03341139)
prima.2009<-(1+par.prima.2009[2])*mean(c(sample(rep5p,2000),sample(rep6p,3000), sample(rep7p,5000)))*sin[2]/(1+.03873204)

prima.2008
prima.2009
theta$siete[2]
prima.2008/theta$siete[2]
prima.2009/theta$siete[2]
