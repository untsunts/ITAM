#CREATING A TOY DATASET TWO LEVELS
minimo <- runif(1)*1
maximo <- 1+runif(1)*9
df1 <- data.frame(x1=seq(minimo,maximo,by=0.001))
df1$x2 <- c(rnorm(n=floor(dim(df1)[1]/2),mean=10,sd=2),  #let's make this variable induce some clusters
            rnorm(n=ceiling(dim(df1)[1]/2),mean=0,sd=1))

#ANOTHER TOY DATASET CIRCLE
angulos <- runif(n=1000)*2*pi
radios  <- runif(n=1000)/10+0.5
df2 <- data.frame(x1 =radios*cos(angulos) , x2 =radios*sin(angulos) )

#ANOTHER TOY DATASET SWISS ROLL
angulos <- runif(n=1000)*5*pi
df3 <- data.frame(x1 =angulos*cos(angulos) , x2 =angulos*sin(angulos) )


#----------------------------- IF YOU WANT KERNEL PCA FOR THE DATASET -----------------------------
#sigma <- 1
#kres <- kpca(~., data=df3,features=2,kernel="rbfdot",kpar = list(sigma = sigma))
#df4 <- as.data.frame(kres@rotated)

df <- df3       #choose a dataset
plot(df)

################### IMPORTANT
#BEFORE THIS SECTION IT IS IMPORTANT FOR THE DATA FRAME THAT WILL BE USED TO BE CALLED "df"
#ALSO I AM ASSUMING WE CAN TAKE "x1" AS A VARIABLE FROM "df" TO SUBSET
#THIS VARIABLE HAS TO BE NUMERIC AND NON CATEGORICAL
#I SUGGEST WORKING WITH THE TOY DATA SET FIRST AND THEN MOVING ON TO TRY OTHER ONES
####################

#----------------------------- PACKAGES -----------------------------

install.packages("fpc")
install.packages("kernlab")
install.packages("igraph")
library(fpc)
library(igraph)
library(kernlab)



#----------------------------- NECESSARY PARAMETERS -----------------------------
var_o <- df$x1    #variable we will use to make the overlapping subsets
#var_o <- df4$V1   #if we want to use kernel pca variable to cut
n_int <- 6       #number of intervals we want
p <- 0.2          #proportion of each interval that should overlap with the next
#parameters for dbscan
eps <- 0.7            #epsilon makes the number of clusters VERY unstable  !!!!!
p_noise <- 0.05       #

#----------------------------- CREATING THE INTERVALS -----------------------------

#this section will create a data frame in which we will construct overlapping intervals
intervals_centers <- seq(min(var_o),max(var_o),length=n_int)  #basic partition = centers
interval_length <- intervals_centers[2]-intervals_centers[1]  #to create the overlaps of p% of this length
intervals <- data.frame(centers=intervals_centers)            #create a data frame
#create the overlapping intervals  
intervals$min <- intervals_centers - (0.5+p)*interval_length                     
intervals$max <- intervals_centers + (0.5+p)*interval_length
#decent name for the intervals e.g    [5.34;6.53)     [6.19;7.39)
intervals$interval <- seq(1,n_int)
intervals$name <- with(intervals, sprintf("[%.2f;%.2f)",min,max))

#function that will split the variable according to the invervals
res <- lapply(split(intervals,intervals$interval), function(x){   
  return(df[var_o> x$min & var_o <= x$max,])     #res will be a list with each element res[i]
})                                                #being the points on the i'th subset

#res


#ITERATE EVERY ELEMENT OF THE LIST (res[i]) AND CLUSTERIZE INSIDE
ints<-list()
counter1<-1;counter2<-1

for(i in 1:(n_int-1)){
  df1<-as.data.frame(res[[i]])
  df2<-as.data.frame(res[[i+1]])
  
  if(i==1){
    MinPts <- p_noise*dim(df1)[1]
    result1<-(dbscan(df1,eps=eps,MinPts=MinPts,showplot = TRUE))
    df1$cluster1 <- result1$cluster
    
    #create columns in the original matrix to show which cluster they belong to
    df[dim(df)[2]+i]<-rep(0,dim(df)[1])
    df[row.names(df1),dim(df)[2]]<-result1$cluster
    
  }else{result1 <- result2              #use the results for the last iteration
        df1$cluster1 <- result1$cluster #this ensures that the cluster labels will be correct for the adj. matrix
  }
  
  MinPts <- p_noise*dim(df2)[1]
  result2<-(dbscan(df2,eps=eps,MinPts=MinPts,showplot = TRUE))
  df2$cluster2 <- result2$cluster
  
  #create columns in the original matrix to show which cluster they belong to
  df[dim(df)[2]+1]<-rep(0,dim(df)[1])
  df[row.names(df2),dim(df)[2]]<-result2$cluster
  
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