prueba<-data.frame(datos=datos_2005p$Claim_Amount,sim=rep5maxp[1:3001])
prueba_separada <- prueba %>%
  gather(tipo, costo, datos:sim)
ggplot(prueba_separada) +
  stat_qq(aes(sample = costo, colour = factor(tipo)))

prueba2<-data.frame(datos=datos_2005p$Claim_Amount,sim=rep5p[1:3001,1])
prueba_separada2 <- prueba2 %>%
  gather(tipo, costo, datos:sim)
ggplot(prueba_separada2) +
  stat_qq(aes(sample = costo, colour = factor(tipo)))

rep5p<-as.vector(rep5p)
rep5p<-rep5p[-(which(rep5p>10000))]
prueba3<-data.frame(datos=datos_2005p$Claim_Amount,sim=rep5p[1:3001])
prueba_separada3 <- prueba3 %>%
  gather(tipo, costo, datos:sim)
ggplot(prueba_separada3) +
  stat_qq(aes(sample = costo, colour = factor(tipo)))


datos6<-datos_posit%>%dplyr::filter(Calendar_Year==2006)
ajuste2006<-as.vector(gpdSim(model = list(xi =fit.sev.gpd.2005.i@fit$par.ests[1], mu = 0, beta = fit.sev.gpd.2005.i@fit$par.ests[2]), n = nrow(datos6)))
prueba4<-data.frame(datos=datos6$Claim_Amount,sim=ajuste2006)
prueba_separada4 <- prueba4 %>%
  gather(tipo, costo, datos:sim)

# Interleaved histograms
ggplot(prueba_separada4, aes(x=costo, fill=tipo)) +
  geom_histogram(binwidth=.5, position="dodge")

# Density plots with semi-transparent fill
ggplot(prueba_separada4, aes(x=costo, fill=tipo)) + geom_density(alpha=.3)
