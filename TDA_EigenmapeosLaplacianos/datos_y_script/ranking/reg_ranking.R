

transmat <- read.csv("torneo_clausura.csv")[,-1]
n <- nrow(transmat)
alpha <- .85
newtransmat <-alpha*transmat + (1-alpha)*matrix(1/n, n, n)

library(expm)
newtransmat <- as.matrix(newtransmat) %^% 2

flow_mat <- matrix(0,n,n)
for(i in 1:(n-1)) for(j in (i+1):n) flow_mat[i,j] <- log(newtransmat[j,i]/newtransmat[i,j])
flow_mat <- flow_mat - t(flow_mat)

X <- matrix(0, ncol=n, nrow=n*(n-1)/2+1)
y <- numeric(n*(n-1)/2+1)

X[1, ] <- 1
y[1]  <- 0
row <- 2
for(i in 1:(n-1)) for(j in (i+1):n){
  X[row,i] <- -1
  X[row,j] <- 1
  y[row] <- flow_mat[i,j]
  row <- row+1
}

qr.solve(t(X)%*%X, t(X)%*%y)
t(X)%*%y/n

mod <- lm(y~X, data = data.frame(X,y))
summary(mod)


