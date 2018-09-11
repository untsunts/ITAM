
rep5[which(rep5>max(datos_2005p$Claim_Amount)*2)]<-max(datos_2005p$Claim_Amount)*2
rep6[which(rep6>max(datos_2006p$Claim_Amount)*2)]<-max(datos_2006p$Claim_Amount)*2
rep7[which(rep7>max(datos_2007p$Claim_Amount)*2)]<-max(datos_2007p$Claim_Amount)*2

rep5p[which(rep5p>max(datos_2005p$Claim_Amount)*2)]<-max(datos_2005p$Claim_Amount)*2
rep6p[which(rep6p>max(datos_2006p$Claim_Amount)*2)]<-max(datos_2006p$Claim_Amount)*2
rep7p[which(rep7p>max(datos_2007p$Claim_Amount)*2)]<-max(datos_2007p$Claim_Amount)*2

aux<-0
for(i in 1:n5p){
  aux<-aux+gpdlik(c(ptp.mcmc5_p[i,1],ptp.mcmc5_p[i,2],ptp.mcmc5_p[i,3]), rep5p[i])
}
aux

prueba2005b_<-data.frame(datos=datos_2005p$Claim_Amount,sim_gpd = rep5p[2:nrow(rep5p)])
prueba2005_s_ <- prueba2005b_ %>%
  gather(tipo, costo, datos:sim_gpd)
#qqplot

# Interleaved histograms
 ggplot(prueba2005_s_, aes(x=costo, fill=tipo)) +
   geom_histogram(binwidth=.5, position="dodge") +
   xlim(c(0,max(prueba2005_s_$costo)+100)) +
   ylim(c(0,30))
# Density plots with semi-transparent fill

g1<-ggplot(prueba2005_s_, aes(x=costo, fill=tipo)) + geom_density(alpha=.3) +
  xlim(c(0,max(prueba2005_s_$costo)+100))
g2<-ggplot(prueba_2005_p, aes(x=costo, fill=tipo)) + geom_density(alpha=.3) +
  xlim(c(0,max(prueba_2005_p$costo)+100))

g3<-ggplot(prueba2005_s_) +
  stat_qq(aes(sample = costo, colour = factor(tipo)))
  
g4<-ggplot(prueba_2005_p) +
  stat_qq(aes(sample = costo, colour = factor(tipo)))  
library(gridExtra)
grid.arrange(g1,g2,g3,g4, ncol=2, nrow =2)


BIC5p<- -2*lk5 + 3*log(nrow(datos_2005p))
BIC5pf<- -2*lk5_f + 3*log(nrow(datos_2005p))
