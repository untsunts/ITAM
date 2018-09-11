probabilidadRuina<-function (datos, u, teta,lambda){
  tiempos<-rexp(nrow(datos), 1/lambda)
  tiempo<-tiempos/sum(tiempos)
  prima<- (1+teta)*mean(datos)*lambda
  s<-datos[1]
  Z<-0
  tiempo.recorr<-tiempos[1]
  i<-0
  
  while(i < nrow(datos) & Z==0 ){
    if(prima*tiempo.recorr+ u >= s){
      s=s+datos[i+1]
      tiempo.recorr<-tiempo.recorr+tiempos[i+1]
      i<-i+1
    }
    else{
      Z<-1
      i<-i+1
    }
  }
  return(Z)
}