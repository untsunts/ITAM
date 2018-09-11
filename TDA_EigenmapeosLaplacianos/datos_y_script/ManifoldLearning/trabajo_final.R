library(dplyr)
library(optimbase)
library(tidyr)
library(ggplot2)
library(ElemStatLearn)
library(FNN)
library(rARPACK)
library(Matrix)
library(igraph)
library(spe)
library(scatterplot3d)
library(FactoMineR)
library(plotly)
library(fpc)
library(igraph)
library(kernlab)

rm(list=ls())
set.seed(125089)
# Topological Data Analysis -- COMPLETO

# 00. Funciones 

graficar_digitos <- function(datos){
  mat_digitos <- lapply(1:nrow(datos), 
                        function(x){ 
                          t(matrix(as.numeric(datos[x, 2:257]), 
                                   16, 16, byrow = T))[,16:1]
                        })
  image (z = Reduce("rbind", mat_digitos), col = terrain.colors(30))
  text(seq(0,1,1/10) + 0.05, 0.05, label = datos[, 1], cex = 1.5)
}

# 0. Obtencion de datos

# Consideramos imagenes escaneadas de digitos escritos a mano, procesadas a 16x16 pixeles.

zip_train <- data.frame(zip.train)
muestra <- zip_train %>% sample_n(10)
graficar_digitos(muestra)

zip_train_2 <- zip_train[,2:257]
k <- nrow(zip_train_2) # numero de individuos

# Los 16x16=256 estan escritos acomodando las filas de la imagen en vector de 256 valores (cada rengl??n de `zip.train`):

# I. Filtro usando Laplacian Eigenmaps Algoritm

# Paso I.1: Vecinos mas cercanos 

n <- 5 # Numero de vecinos

d <- knn.dist(zip_train_2,k=n, algorithm= "kd_tree")
index <- knn.index(zip_train_2,k=n, algorithm= "kd_tree")

# Paso I.2: Funcion de pesos

heat_kernel <- function(x){
  t<- 10
  h <- exp(-((x^2)/t))
}

HK <- sapply(d,heat_kernel)
HK_matrix <- matrix(HK,nrow = k,ncol = n)
HK <- as.vector(transpose(HK_matrix))

# Paso I.3: Construccion de matrices D, W, y L

# 1.3.1 Matriz de Adyacencia

rowsSp1 <- rep(1:k, each = n)
colsSp1 <- as.vector(transpose(index))

rowsSp2 <- c(rowsSp1,colsSp1)
colsSp2 <- c(colsSp1,rowsSp1)

W_sparse <- sparseMatrix(rowsSp2, colsSp2, x = rep(HK,2),use.last.ij = TRUE)
head(summary(W_sparse))

g <- graph_from_adjacency_matrix(W_sparse, weighted=TRUE,mode="undirected") # Construccion de la grafica
is.connected(g)

# 1.3.2 Matriz de grados de los vertices

degree_vec <- rowSums(W_sparse)
D_sparse_sqinv <- sparseMatrix(1:k,1:k,x = (1/sqrt(degree_vec)))


# 1.3.2.2 Laplaciana Normalizada

LN <- graph.laplacian(g, normalized=TRUE, weight=NULL,sparse=getIgraphOpt("sparsematrices"))

# 1.4 Eigenvector solver 

m<- 2 # Solo nos interesa la primera componente o las primeras 2

EV<- eigs_sym(LN, m+1, which = "SM", sigma = NULL, opts = list(),lower = TRUE)

filtro1 <- D_sparse_sqinv%*%(EV$vectors[,m])
filtro2 <- D_sparse_sqinv%*%(EV$vectors[,m-1])


# II. Particion y Clustering

df <- zip_train_2
df$f1 <- as.numeric(filtro1)
df$f2 <- as.numeric(filtro2)

#----------------------------- NECESSARY PARAMETERS -----------------------------
var_o1 <- df$f1
var_o2 <-df$f2
n_int1 <-3 #number of intervals we want
n_int2 <-2
p <- 0.1        #proportion of each interval that should overlap with the next




#----------------OTROS INTERVALOS----------------------------------
intl1 <-(max(var_o1)-min(var_o1))/n_int1
intl2 <-(max(var_o2)-min(var_o2))/n_int2
res <- list()
for(i in 1:n_int1) for(j in 1:n_int2){
  low1 <- min(var_o1) + (i-1)*intl1 - p*intl1
  low2 <- min(var_o2) + (j-1)*intl2 - p*intl2
  
  upper1 <- min(var_o1) + i*intl1 + p*intl1
  upper2 <- min(var_o2) + j*intl2 + p*intl2
  
  res <- c(res, list(df[low1 <=var_o1 & var_o1<=upper1 & low2<=var_o2 & var_o2<=upper2, ]))
}
#----------------LISTA-------------------------------------------
suma<-0   #
#conteo_mat<-matrix(0,10,n_int1*n_int2)
myBigList<-list()
mySmallList<-list()
color<-(zip_train$X1)/10
n_2 <- 4 # Numero de vecinos +1
contador<-0  #
#cont_aux<-0  #
for(i in 1:n_int1)
{
  for(j in 1:n_int2){
    k<- 1   #
    vacio=FALSE
    if(nrow(res[[n_int2*(i-1)+j]])!=0){
      if(nrow(res[[n_int2*(i-1)+j]])!=1){
        df1<-as.data.frame(res[[(n_int2)*(i-1)+j]])
        df1<-df1[,1:(ncol(df1)-2)]
        
        # Clustering con BFS
        mySmallList<-list()
        while(!vacio){
          n_min<-min(n_2,nrow(df1))
          d_2 <- knn.dist(df1,k=n_min-1, algorithm= "kd_tree") 
          index_2 <- knn.index(df1,k=n_min-1, algorithm= "kd_tree") 
          
          # Paso I.2: Funcion de pesos
          
                  heat_kernel <- function(x){  
                    t<- 1000
                    h <- exp(-((x^2)/t))
                  }    
                  HK_2 <- sapply(d_2,heat_kernel)
                  HK_matrix_2 <- matrix(HK_2,nrow = nrow(df1),ncol = n)
                  HK_2 <- as.vector(transpose(HK_matrix_2))
                  
          # Paso I.3: Construccion de matrices D, W, y L
          
          # 1.3.1 Matriz de Adyacencia
          
          rowsSp1 <- rep(1:nrow(df1), each = n_min-1)
          colsSp1 <- as.vector(transpose(index_2))
          
          rowsSp2 <- c(rowsSp1,colsSp1)
          colsSp2 <- c(colsSp1,rowsSp1)
          
          W_sparse <- sparseMatrix(rowsSp2, colsSp2, x = round(rep(HK_2,2)),use.last.ij = TRUE)
          #head(summary(W_sparse))
          
          g <- graph_from_adjacency_matrix(W_sparse,mode="undirected", weighted = TRUE) # Construccion de la grafica
          print(is.connected(g))
          result1 <- graph.bfs(g,1,"all", unreachable = FALSE, order=TRUE, rank=FALSE, father=FALSE, pred=FALSE,succ=FALSE, dist=FALSE)
          cl<-as.integer(result1$order)
          cl<-cl[!is.na(cl)]
          #l<-as.integer(j)
          #con<-is.connected(g)
          namek <- paste('cluster', k , sep='_')
          temp<-list(a=rownames(df1[cl,]))
          mySmallList[[namek]]<-temp
          df1_aux <- df1[cl,]
          df1<-subset(df1, !(rownames(df1) %in% rownames(df1_aux)))
          k<-k+1
          suma<-suma+1
          l<-nrow(df1)
          if(l==0)
          {
            vacio=TRUE
          }
        }
      } else
      {
        namek <- paste('cluster', k , sep='_')
        temp<-list(a=rownames(res[[n_int2*(i-1)+j]]))
        mySmallList[[namek]]<-temp
        suma<-suma+1
        k<-k+1
      }
      namei<-paste('inter',contador+1,sep='_')
      #temp<-list(A=mySmallList)
      myBigList[[namei]]<-mySmallList
      contador<-contador+1
    } else
    {
      namek <- paste('cluster', k , sep='_')
      temp<-list(a=0)
      mySmallList[[namek]]<-temp
      suma<-suma+1
      k<-k+1
    }
  }
}

#-------------------- MATRIZ ADYACENCIA -------------------------------
adj<-matrix(0,suma,suma)
#adj2<-matrix(0,suma,suma)
color_v<-matrix(0,suma,2)
aux<-0
temp1<-myBigList[[1]]
for(i in 1:((n_int1*n_int2)-1))
{
  temp2<-myBigList[[i+1]]
  l1<-length(temp1)
  for(j in 1:l1)
  {
    color_v[j+aux,1]<-mean(color[as.numeric(temp1[[j]]$a)])
    color_v[j+aux,2]<-length(temp1[[j]]$a)
    
    l2<-length(temp2)
    for(k in 1:l2)
    {
      adj[aux+l1+k,j+aux]<-1-((sum(temp1[[j]]$a %in% temp2[[k]]$a))/(min(length(temp1[[j]]$a),length(temp2[[k]]$a))))
      adj[aux+j,aux+l1+k]<-1-((sum(temp1[[j]]$a %in% temp2[[k]]$a))/(min(length(temp1[[j]]$a),length(temp2[[k]]$a))))
    }
  }
  temp1<-temp2
  aux<-aux+l1
}
conteo_mat<-matrix(0,suma,10)
conteo_aux<-rep(0,10)
temp1<-myBigList[[1]]
for(i in 1:((n_int1*n_int2)-1))
{
  temp2<-myBigList[[i+1]]
  l1<-length(temp1)
  for(j in 1:l1)
  {
    for(k in 1:10)
    {
      l2<-length(temp1[[j]]$a)
      for(l in 1:l2)
      {
        conteo_aux[k]<-conteo_aux[k]+(zip_train$X1[as.numeric(temp1[[j]]$a[l])]==k)
      }
    }
    conteo_mat[i,]<-conteo_aux
    conteo_aux<-rep(0,10)
  }
  temp1<-temp2
  aux<-aux+l1
}
conteo_mat[suma,10]<-length(temp2[[1]]$a)
conteo_mat_porc<-matrix(0,suma,10)
conteo_max<-rep(0,suma)
conteo_max2<-rep(0,suma)
conteo_aux<-conteo_mat
conteo_p<-rep(0,suma)
conteo_p2<-rep(0,suma)
for(i in 1:suma)
{
  #   conteo_max_aux[i]<-max(conteo_mat[i,])
  conteo_max[i]<-which(conteo_mat[i,]==max(conteo_mat[i,]))
  conteo_aux[i,conteo_max[i]]=-1
  conteo_max2[i]<-which(conteo_mat[i,]==max(conteo_aux[i,]))
  if(conteo_max[i]==10) conteo_max[i]=0
  if(conteo_max2[i]==10) conteo_max2[i]=0
  for(j in 1:10)
  {
    conteo_mat_porc[i,j]<-conteo_mat[i,j]/sum(conteo_mat[i,])
  }
  conteo_p[i]<-conteo_mat_porc[i,conteo_max[i]]
  conteo_p2[i]<-conteo_mat_porc[i,conteo_max2[i]]
}
conteo_p[suma]<-1
conteo_p[is.na(conteo_p)]<-0
conteo_p2[is.na(conteo_p2)]<-0
adj[is.na(adj)]<-0
