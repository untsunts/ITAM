library(FNN)
data<- query<- cbind(1:10, 1:10)

a<-knn.index(data, k=3)
knnx.index(data, query, k=5)
knnx.index(data, query, k=5, algo="kd_tree")

b<-bfs(make_ring(10) %du% make_ring(10), root=1, "out",
    order=TRUE, rank=TRUE, father=TRUE, pred=TRUE,
    succ=TRUE, dist=TRUE)


pts <- list(x = cars[,1], y = cars[,2])
e1 <- new.env()
e1$a <- 10
e1$b <- 20
as.list(e1)
