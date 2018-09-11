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

m<- 2 # Solo nos interesa la primera componente

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
n_int1 <-6 #number of intervals we want
n_int2 <-3
p <- 0.2        #proportion of each interval that should overlap with the next




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



#----------------------------- CREATING THE INTERVALS -----------------------------

#this section will create a data frame in which we will construct overlapping intervals
intervals_centers1 <- seq(min(var_o1),max(var_o1),length=n_int)  #basic partition = centers
intervals_centers2 <- seq(min(var_o2),max(var_o2),length=n_int)  #basic partition = centers

interval_length1 <- intervals_centers1[2]-intervals_centers1[1]  #to create the overlaps of p% of this length
interval_length2 <- intervals_centers2[2]-intervals_centers2[1]  #to create the overlaps of p% of this length

intervals1 <- data.frame(centers=intervals_centers1)            #create a data frame
intervals2 <- data.frame(centers=intervals_centers2)            #create a data frame


#create the overlapping intervals  
intervals1$min <- intervals_centers1 - (0.5+p)*interval_length1                     
intervals2$min <- intervals_centers2 - (0.5+p)*interval_length2                     

intervals1$max <- intervals_centers1 + (0.5+p)*interval_length1
intervals2$max <- intervals_centers2 + (0.5+p)*interval_length2

#decent name for the intervals e.g    [5.34;6.53)     [6.19;7.39)
intervals1$interval <- seq(1,n_int)
intervals2$interval <- seq(1,n_int)

intervals1$name <- with(intervals1, sprintf("[%.2f;%.2f)",min,max))
intervals2$name <- with(intervals2, sprintf("[%.2f;%.2f)",min,max))


#function that will split the variable according to the invervals
res <- lapply(split(intervals1,intervals2,intervals1$interval,intervals2$interval), function(x){   
  return(df[var_o1 > x$min & var_o1 <= x$max & var_o2 > x$min & var_o2 <= x$max,])     #res will be a list with each element res[i]
})                                                #being the points on the i'th subset

#res

res[[1]]


######
#ITERATE EVERY ELEMENT OF THE LIST (res[i]) AND CLUSTERIZE INSIDE
ints<-list()
counter1<-1;counter2<-1

#######

for(i in 1:(n_int-1)){
  df1<-as.data.frame(res[[i]])
  df1<-df1[,1:256]
  df2<-as.data.frame(res[[i+1]])
  df2<-df2[,1:256]
  
  # Clustering con BFS
  
  if(i==1){
    
    n_2 <- 5 # Numero de vecinos
    d_2 <- knn.dist(df1,k=n_2, algorithm= "kd_tree")
    index_2 <- knn.index(df1,k=n_2, algorithm= "kd_tree")
    
    # Paso I.2: Funcion de pesos
    
    heat_kernel <- function(x){
      t<- 1000000000
      h <- exp(-((x^2)/t))
    }
    
    HK_2 <- sapply(d_2,heat_kernel)
    HK_matrix_2 <- matrix(HK_2,nrow = nrow(df1),ncol = n)
    HK_2 <- as.vector(transpose(HK_matrix_2))
    
    # Paso I.3: Construccion de matrices D, W, y L
    
    # 1.3.1 Matriz de Adyacencia
    
    rowsSp1 <- rep(1:nrow(df1), each = n_2)
    colsSp1 <- as.vector(transpose(index_2))
    
    rowsSp2 <- c(rowsSp1,colsSp1)
    colsSp2 <- c(colsSp1,rowsSp1)
    
    W_sparse <- sparseMatrix(rowsSp2, colsSp2, x = round(rep(HK_2,2)),use.last.ij = TRUE)
    head(summary(W_sparse))
    
    g <- graph_from_adjacency_matrix(W_sparse, weighted=TRUE,mode="undirected") # Construccion de la grafica
    result1 <- graph.bfs(g,1,"all", unreachable = TRUE, order=TRUE, rank=TRUE, father=TRUE, pred=TRUE,succ=TRUE, dist=TRUE)
    df1$cluster1 <- as.integer(result1$order)
  
    #create columns in the original matrix to show which cluster they belong to
    df[dim(df)[2]+i]<-rep(0,dim(df)[1])
    df[row.names(df1),dim(df)[2]]<-as.integer(result1$order)
    
  }
  else{result1 <- result2              #use the results for the last iteration
  df1$cluster1 <- as.integer(result1$order) #this ensures that the cluster labels will be correct for the adj. matrix
  }
  
  n_2 <- 5 # Numero de vecinos
  d_2 <- knn.dist(df2,k=n_2, algorithm= "kd_tree")
  index_2 <- knn.index(df2,k=n_2, algorithm= "kd_tree")
  
  # Paso I.2: Funcion de pesos
  
  heat_kernel <- function(x){
    t<- 1000000000
    h <- exp(-((x^2)/t))
  }
  
  HK_2 <- sapply(d_2,heat_kernel)
  HK_matrix_2 <- matrix(HK_2,nrow = nrow(df2),ncol = n)
  HK_2 <- as.vector(transpose(HK_matrix_2))
  
  # Paso I.3: Construccion de matrices D, W, y L
  
  # 1.3.1 Matriz de Adyacencia
  
  rowsSp1 <- rep(1:nrow(df2), each = n_2)
  colsSp1 <- as.vector(transpose(index_2))
  
  rowsSp2 <- c(rowsSp1,colsSp1)
  colsSp2 <- c(colsSp1,rowsSp1)
  
  W_sparse <- sparseMatrix(rowsSp2, colsSp2, x = round(rep(HK_2,2)),use.last.ij = TRUE)
  head(summary(W_sparse))
  
  g <- graph_from_adjacency_matrix(W_sparse, weighted=TRUE,mode="undirected") # Construccion de la grafica
  is.connected(g)
  result2 <- graph.bfs(g,1,"all", unreachable = TRUE, order=TRUE, rank=TRUE, father=TRUE, pred=TRUE,succ=TRUE, dist=TRUE)
  df2$cluster2 <- as.integer(result2$order)
  
  #create columns in the original matrix to show which cluster they belong to
  df[dim(df)[2]+1]<-rep(0,dim(df)[1])
  df[row.names(df2),dim(df)[2]]<-as.integer(result2$order)
  
  intersection <- merge(df1,df2,all=TRUE)            #points in the intersection
  intersection[is.na(intersection)] <- 0
  ints[[i]]<-as.data.frame(unique(intersection[3:4]))               #list of all the clusters that intersect
  
}

#----------------------------- GENERATE ADJACENCY MATRIX -----------------------------

cadena=''
#for ( i in 1:length(ints)) {
#  for (j in 1:dim(ints[[i]][1])[1]) {
#    if (ints[[i]][j,1]!=0 && ints[[i]][j,2]!=0){
#      cadena<-paste(cadena,'c',i,ints[[i]][j,1],'-c',i+1,ints[[i]][j,2],',', sep="")
#    }
#  }
#}
#a<-substr(cadena,1,nchar(cadena)-1)
#print(a)

View(df)

#circulo
#g<-graph_from_literal(c11-c22,c11-c21,c21-c32,c22-c31,c32-c41,c31-c42,c42-c51,c41-c51)
#rayitas
#g<-graph_from_literal(c11-c21,c21-c31,c32-c41,c41-c51)
#otra que salio con el circulo
#g<-graph_from_literal(c11-c21,c21-c32,c21-c31,c32-c41,c31-c41,c41-c51)
g<-graph_from_literal(c21-c31,c31-c41,c42-c51,c41-c52,c51-c61,c52-c61)
plot(g)



df1<-as.data.frame(res[[1]])
df1<-df1[,1:256]
df2<-as.data.frame(res[[2]])
df2<-df2[,1:256]
