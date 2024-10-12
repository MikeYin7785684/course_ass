library(mvtnorm)
Y_obs <- read.csv("CitySurvey.csv")
Y_obs[,1]<-log(Y_obs[,1])
rinwish <- function(n, nu0, iS0) {
    sL0 <- chol(iS0)
    S <- array(dim = c(dim(iS0), n))
    for (i in 1:n)
    {
        Z <- matrix(rnorm(nu0 * dim(iS0)[1]), nu0, dim(iS0)[1]) %*% sL0
        S[, , i] <- solve(t(Z) %*% Z)
    }
    return(S[, , 1:n])
}
Y_obs.mean <- mu_0 <- apply(Y_obs,2, mean)
n<-nrow(Y_obs)
S0 <- Rambda0 <- Sigma <- matrix(cov(Y_obs),nrow = 2)
nu0<-dim(Y_obs)[2]+2

THETA<-SIGMA<-NULL
for (s in 1:10000){
  Rambda_n<-solve(solve(Rambda0)+n*solve(Sigma))
  mu_n<-Rambda_n%*%(solve(Rambda0)%*%mu_0+n*solve(Sigma)%*%Y_obs.mean)
  theta<-rmvnorm(1,mu_n,Rambda_n)
    
  Sn<-S0+(t(Y_obs)-c(theta))%*%t(t(Y_obs)-c(theta))
  Sigma<-rinwish(1,nu0+n,solve(Sn))
  
  THETA<-rbind(THETA,theta)
  SIGMA<-rbind(SIGMA,c(Sigma))
}