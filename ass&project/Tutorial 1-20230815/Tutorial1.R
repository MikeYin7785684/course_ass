
#Q1
theta<-seq(0,1,0.001)
n<-20
y<-0


pdf("HW_Fig1a.pdf",family="Times",height=3.5,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,0.75,0))
par(mfrow=c(1,2))
a<-40
b<-400
d.prior<-dbeta(theta,a,b)

plot(theta,d.prior,type="n",ylim=c(0,30),
xlab="percentage infected in the population",ylab="",main="Beta(40,400) prior")
lines(theta,d.prior,lty=2,lwd=2,)

d.post<-dbeta(theta,a+y,b+n-y)
lines(theta,d.post,lty=1,lwd=2)
legend(0.6,10,c("prior","post"),lty=c(2,1),lwd=c(2,2))

a<-2
b<-2
d.prior<-dbeta(theta,a,b)
plot(theta,d.prior,type="n",ylim=c(0,30),
xlab="percentage infected in the population",ylab="",main="Beta(2,2) prior")
lines(theta,d.prior,lty=2,lwd=2)

d.post<-dbeta(theta,a+y,b+n-y)
lines(theta,d.post,lty=1,lwd=2)
legend(0.6,10,c("prior","post"),lty=c(2,1),lwd=c(2,2))
dev.off()

#Uniform priors
pdf("HW_Fig1b.pdf",family="Times",height=3.5,width=7)
par(mar=c(3,3,1,1),mgp=c(1.75,0.75,0))
par(mfrow=c(1,2))

#Unif(0,1) prior
a<-1
b<-1
d.prior<-dbeta(theta,a,b)

plot(theta,d.prior,type="n",ylim=c(0,25),
     xlab="percentage infected in the population",ylab="",main="Unif(0,1) prior")
lines(theta,d.prior,lty=2,lwd=2)

d.post<-dbeta(theta,a+y,b+n-y)
lines(theta,d.post,lty=1,lwd=2)
legend(0.6,10,c("prior","post"),lty=c(2,1),lwd=c(2,2))


#Unif(0.05,0.20) prior
theta.trunc<-seq(0.05,0.20,0.001)
d.prior<-dunif(theta.trunc,0.05,0.20)

plot(theta.trunc,d.prior,type="n",ylim=c(0,30),xlim=c(0,1),
     xlab="percentage infected in the population",ylab="",main="Unif(0.05,0.20) prior")
lines(theta.trunc,d.prior,lty=2,lwd=2)

#R function to generate draws from a truncated beta distribution
# dtbeta() code from https://rdrr.io/github/bahlolab/moimix/man/dtbeta.html
dtbeta <- function(x, shape1, shape2, lower = 0, upper = 1) {
  
  stopifnot(lower <= upper,shape2 > 0, shape1 > 0)
  tt <- rep(0, length(x))
  normalize.factor <- pbeta(upper, shape1, shape2) - pbeta(lower, shape1, shape2)
  tt[x >= lower & x <= upper] <- dbeta(x[x >= lower & x <= upper],
                                       shape1, shape2) / normalize.factor
  return(tt)
}



lines(x, dtbeta(theta.trunc,a+y,b+n-y,0.05,0.20),lwd=2,type="l",xaxs="i",yaxs="i",xlim=c(0,1),ylim=c(0,30),xlab=expression(theta),ylab=expression(f(y ~"|"~theta)))
legend(0.6,10,c("prior","post"),lty=c(2,1),lwd=c(2,2))


dev.off()

#Q2
#a) 
theta1<-1
theta2<-2
sigma<-2

y<-seq(-8,10,0.01)
py<-0.5*dnorm(y,theta1,sigma)+0.5*dnorm(y,theta2,sigma)
par(mfrow=c(1,1))
pdf("HW_Fig2.pdf")
plot(y,py,type="l",xlab="y",ylab="",xaxs="i",yaxs="i",bty="n",cex=2,yaxt="n",xaxt="n")
axis(1, at = c(-5,-4,-3,-2,-1, 0, 1,2,3,4,5,6,7,8,9,10))
dev.off()

#b)

0.5*dnorm(1,1,2)/(0.5*dnorm(1,1,2)+0.5*dnorm(1,2,2))

#c)
ptheta_y<-function(sigma)  0.5*dnorm(1,1,sigma)/(0.5*dnorm(1,1,sigma)+0.5*dnorm(1,2,sigma))

py<-0.5*dnorm(y,theta1,sigma)+0.5*dnorm(y,theta2,sigma)

pdf("HW_Fig3.pdf")
plot(seq(0.025,5,0.01),ptheta_y(seq(0.025,5,0.01)),xlab="sigma",ylab=expression(paste("p(",theta,"=1|y=1)")),type="l",lwd=3)
dev.off()

#Q4
p<-c(0,0.025,0.05,0.075,0.10)
g_p<-c(0.80, 0.10,0.05,0.035,0.015)
#a) 
n<-60
y<-55

g.post<-dbinom(y,n,1-p)*g_p
g.post<-g.post/sum(g.post)
g.post         
#b)
y<-c(58,  55 , 59 , 54 , 56 , 57 , 57 , 50 , 52 , 60)

g_p<-c(0.80, 0.10,0.05,0.035,0.015)

for (i in 1:10){
  g.post<-dbinom(y[i],n,1-p)*g_p
  g.post<-g.post/sum(g.post)
  g_p<-g.post #updated posterior becomes new prior
}
  
}  