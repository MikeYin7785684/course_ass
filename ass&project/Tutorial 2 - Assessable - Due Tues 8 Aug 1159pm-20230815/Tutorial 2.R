
#problem 2
# p r i o r parameters
#a)
a1<-120
b1<-10
a2<-12
b2<-1

ya<-c(12,9,12,14,13,13,15,8,15,6)
yb<-c(11,11,10,9,9,8,7,10,6,8,8,9,7)




theta_a.post<-rgamma(1000,a1+sum(ya),b1+length(ya))
theta_b.post<-rgamma(1000,a2+sum(yb),b2+length(yb))

qgamma(c(0.025,0.975),a1+sum(ya),b1+length(ya))
qgamma(c(0.025,0.975),a2+sum(yb),b2+length(yb))

#b)

n0<-seq(1,50,1)

a2<-12*n0
b2<-n0

pdf("HW_Pic9.pdf")
plot(n0,(a2+sum(yb))/(b2+length(yb)),ylab=expression(paste("E[", theta[B],"|",y[B],"]")),type="l",col="red",lwd=2,xlab=expression(n[0]),ylim=c(9,13))
abline(h=(a1+sum(ya))/(b1+length(ya)))
text(10,12,expression(paste("E[", theta[A],"|",y[A],"]")))
dev.off()
#c)
a1<-120
b1<-10
a2<-12
b2<-1

y<-0:20
d<-rbind(dnbinom(y ,(a2+sum(yb)) , mu=(a2+sum(yb)) / (b2+length(yb)) ),
dnbinom(y , (a1+sum(ya)+sum(yb)) , mu=(a1+sum(ya)+sum(yb)) / (b1+length(ya)+length(yb)) ))


pdf("HW_Pic3.pdf")
barplot(d,beside=TRUE,ylab="posterior predictive probability",xlab=expression(tilde(y)[B]))
legend(45,0.08,cex=0.8,c("independent prior","type A prior"),lty=c(1,1),col=c("black","grey"),lwd=c(3,3))
dev.off()

qnbinom(0.975,a1+sy,mu=(a1+sy)/(b1+n))
qnbinom(0.975,a2+sy,mu=(a2+sy)/(b2+n))

pdf("HW_Pic4.pdf")
theta<-0:20
d<-rbind(dgamma(theta ,a2,b2),
dgamma(theta, a1+sum(ya) , b1+length(ya)))
barplot(d,beside=TRUE,xlab=expression(theta),ylab=expression(paste("p(",theta[A],")")))
legend(45,0.2,cex=0.8,c("independent prior","type A prior"),lty=c(1,1),col=c("black","grey"),lwd=c(3,3))
dev.off()

theta<-0:20
d<-rbind(dgamma(theta ,a1+sy,b1+n),
dgamma(theta, a2+sy , b2+n))
barplot(d,beside=TRUE,xlab="y",ylab="prior probability")

n0<-seq(1,50,1)



#Problem 3
#a)
a<-2
b<-8
n<-43
y<-15
theta<-seq(0,1,0.01)
pdf("HW_Pic5.pdf")
par(mfrow=c(2,2))
plot(theta,dbeta(theta,a,b),type="l",ylab=expression(paste("p(",theta,")")),xlab=expression(theta),ylim=c(0,6))
plot(theta,choose(n,y)*theta^y*(1-theta)^(n-y),type="l",ylab=expression(paste("p(y|",theta,")")),xlab=expression(theta))
plot(theta,dbeta(theta,a+y,b+n-y),type="l",ylab=expression(paste("p(",theta,"|y)")),xlab=expression(theta))
plot(theta,dbeta(theta,a,b)* choose(n,y)*theta^y*(1-theta)^(n-y),type="l",ylab=expression(paste("p(",theta,"|y) (unscaled)")),xlab=expression(theta))
dev.off()

qbeta(0.025,a+y,b+n-y)
qbeta(0.975,a+y,b+n-y)

#b)
a<-8
b<-2
n<-43
y<-15
theta<-seq(0,1,0.01)
pdf("HW_Pic6.pdf")
par(mfrow=c(2,2))
plot(theta,dbeta(theta,a,b),type="l",ylab=expression(paste("p(",theta,")")),xlab=expression(theta),ylim=c(0,6))
plot(theta,choose(n,y)*theta^y*(1-theta)^(n-y),type="l",ylab=expression(paste("p(y|",theta,")")),xlab=expression(theta))
plot(theta,dbeta(theta,a+y,b+n-y),type="l",ylab=expression(paste("p(",theta,"|y)")),xlab=expression(theta))
plot(theta,dbeta(theta,a,b)* choose(n,y)*theta^y*(1-theta)^(n-y),type="l",ylab=expression(paste("p(",theta,"|y) (unscaled)")),xlab=expression(theta))
dev.off()

qbeta(0.025,a+y,b+n-y)
qbeta(0.975,a+y,b+n-y)

#c)
a1<-2
b1<-8
a2<-8
b2<-2


pdf("HW_Pic7.pdf")
p.theta<-0.75*dbeta(theta,a1,b1)+0.25*dbeta(theta,a2,b2)
plot(theta,p.theta,type="l",ylab=expression(paste("p(",theta,")")),xlab=expression(theta))
dev.off()
#or
p.theta<-0.25*gamma(10)/(gamma(2)*gamma(8))*(3*theta*(1-theta)^7+theta^7*(1-theta))
plot(theta,p.theta,type="l",ylab=expression(paste("p(",theta,")")),xlab=expression(theta))

#d) 
pdf("HW_Pic8.pdf")
w1<-0.75*gamma(a1+b1)/(gamma(a1)*gamma(b1))*(gamma(y+a1)*gamma(n-y+b1))/gamma(a1+b1+n)
w2<-0.25*gamma(a2+b2)/(gamma(a2)*gamma(b2))*(gamma(y+a2)*gamma(n-y+b2))/gamma(a2+b2+n)
p1<-w1/(w1+w2)
p2<-1-p1
plot(theta,p1*dbeta(theta,a1+y,n-y+b1)+p2*dbeta(theta,a2+y,n-y+b2),type="l",main="mix post.dist",col="blue",ylab=expression(paste("p(",theta,"|y)")),xlab=expression(theta),lwd=3,xaxt="n")
axis(1,at=seq(0,1,0.05))
abline(v=0.32)
abline(v=0.31)
dev.off()