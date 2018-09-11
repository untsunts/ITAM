sum<-0
conteo_mat<-matrix(0,10,n_int1*n_int2)
myBigList<-list()
mySmallList<-list()
color<-(zip_train$X1)/10
n_2 <- 5 # Numero de vecinos
contador<-0
for(i in 1:n_int1)
{
  for(j in 1:n_int2){
    k<- 1
    vacio=FALSE
    if(nrow(res[[contador*i+j]])!=0){
      df1<-as.data.frame(res[[(contador)*i+j]])
      df1<-df1[,1:ncol(df1)-2]
      
      # Clustering con BFS
      
      while(!vacio){
        mySmallList<-list()
        d_2 <- knn.dist(df1,k=n_2, algorithm= "kd_tree") 
        index_2 <- knn.index(df1,k=n_2, algorithm= "kd_tree") 
        
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
        
        rowsSp1 <- rep(1:nrow(df1), each = n_2)
        colsSp1 <- as.vector(transpose(index_2))
        
        rowsSp2 <- c(rowsSp1,colsSp1)
        colsSp2 <- c(colsSp1,rowsSp1)
        
        W_sparse <- sparseMatrix(rowsSp2, colsSp2, x = round(rep(HK_2,2)),use.last.ij = TRUE)
        #head(summary(W_sparse))
        
        g <- graph_from_adjacency_matrix(W_sparse, weighted=TRUE,mode="undirected") # Construccion de la grafica
        result1 <- graph.bfs(g,1,"all", unreachable = FALSE, order=TRUE, rank=FALSE, father=FALSE, pred=FALSE,succ=FALSE, dist=FALSE)
        cl<-as.integer(result1$order)
        #l<-as.integer(j)
        #con<-is.connected(g)
        namek <- paste('cluster', k , sep='_')
        temp<-list(a=rownames(df1[cl,]))
        mySmallList[[namek]]<-temp
        df1 <- df1[!(cl), ]
        k<-k+1
        sum<-sum+1
        l<-nrow(df1)
        if(l==0)
        {
          vacio=TRUE
        }
      }
    }      else
    {
      namek <- paste('cluster', k , sep='_')
      temp<-list()
      mySmallList[[namek]]<-temp
      sum<-sum+1
    }
  }
  namei<-paste('inter',i,sep='_')
  #temp<-list(A=mySmallList)
  myBigList[[namei]]<-mySmallList
  contador<-contador+1
}

adj<-matrix(0,sum,sum)
adj2<-matrix(0,sum,sum)
color_v<-matrix(0,sum,2)
aux<-0
temp1<-myBigList[[1]]
for(i in 1:((n_int1*n_int2)-1))
{
  temp2<-myBigList[[i+1]]
  l1<-length(temp1)
  for(j in 1:l1)
  {
    color_v[j+aux,1]<-mean(color[as.numeric(temp1[[j]]$a)])
    color_v[j+aux,2]<-var(color[as.numeric(temp1[[j]]$a)])
    
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
conteo_mat<-matrix(0,sum,10)
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
conteo_mat[sum,10]<-length(temp2[[1]]$a)
conteo_mat_porc<-matrix(0,sum,10)
conteo_max<-rep(0,sum)
conteo_max2<-rep(0,sum)
conteo_aux<-conteo_mat
conteo_p<-rep(0,sum)
conteo_p2<-rep(0,sum)
for(i in 1:sum)
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
conteo_p[sum]<-1


