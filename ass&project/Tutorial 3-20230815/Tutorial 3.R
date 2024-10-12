
#Problem 3
#a)
y_A<-c(12,9,12,14,13,13,15,8,15,6)
syA<-sum(y_A)
nA<-length(y_A)
y_B<-c(11,11,10,9,9,8,7,10,6,8,8,9,7)
syB<-sum(y_B)
nB<-length(y_B)

a1<-120
b1<-10
a2<-12
b2<-1

theta1.mc<-rgamma(10000,a1+syA,b1+nA)
theta2.mc<-rgamma(10000,a2+syB,b2+nB)
mean(theta1.mc>theta2.mc)

#b)

n0<-seq(0.25,40,0.25)
t.n<-length(n0)

result<-NULL
for (i in 1:t.n){
a2<-12*n0[i]
b2<-n0[i]
theta1.mc<-rgamma(10000,a1+syA,b1+nA)
theta2.mc<-rgamma(10000,a2+syB,b2+nB)
result<-c(result,mean(theta1.mc>theta2.mc))
}
pdf("HW_Fig2.pdf")
plot(n0,result,ylab=expression(paste("Pr(",theta[A]>theta[B],"|y)")))
dev.off()

#c)
count<-0
for(i in 1:10000){
yA.mc<-rpois(1,theta1.mc[i])
yB.mc<-rpois(1,theta2.mc[i])
count<-count+(yA.mc>yB.mc)*1
}
count/10000


n0<-seq(0.25,40,0.25)
t.n<-length(n0)
count.S<-NULL
for (i in 1:t.n){
a2<-12*n0[i]
b2<-n0[i]
theta1.mc<-rgamma(10000,a1+syA,b1+nA)
theta2.mc<-rgamma(10000,a2+syB,b2+nB)
count<-0
for(j in 1:10000){
yA.mc<-rpois(1,theta1.mc[j])
yB.mc<-rpois(1,theta2.mc[j])
count<-count+(yA.mc>yB.mc)*1
}
count.S<-c(count.S,count)	
}

pdf("HW_Fig3.pdf")
plot(n0,count.S/10000,ylab=expression(paste("Pr(",tilde(Y)[A]>tilde(Y)[B],"|y)")))
dev.off()

#Problem 4
#a)
yA.mc<-NULL
for (i in 1:1000){
theta1.mc<-rgamma(1,a1+syA,b1+nA)
yA.mc<-cbind(yA.mc,rpois(nA,theta1.mc))
}

t.s<-apply(yA.mc,2,mean)/apply(yA.mc,2,sd)
pdf("HW_Fig4.pdf")
hist(t.s,breaks=20)
abline(v=mean(y_A)/sd(y_A),lwd=2)
dev.off()
mean(t.s>mean(y_A)/sd(y_A))

#b)
a2<-12
b2<-1
yB.mc<-NULL
for (i in 1:1000){
theta2.mc<-rgamma(1,a2+syB,b2+nB)
yB.mc<-cbind(yB.mc,rpois(nB,theta2.mc))
}

t.s<-apply(yB.mc,2,mean)/apply(yB.mc,2,sd)
pdf("HW_Fig5.pdf")
hist(t.s,breaks=20)
abline(v=mean(y_B)/sd(y_B),lwd=2)
dev.off()
mean(t.s>mean(y_B)/sd(y_B))


#Problem 5
#a)
a1<-2
b1<-8
a2<-8
b2<-2
y<-15
n<-43

#plot
theta<-seq(0,1,0.01)
w1<-0.75*gamma(a1+b1)/(gamma(a1)*gamma(b1))*(gamma(y+a1)*gamma(n-y+b1))/gamma(a1+b1+n)
w2<-0.25*gamma(a2+b2)/(gamma(a2)*gamma(b2))*(gamma(y+a2)*gamma(n-y+b2))/gamma(a2+b2+n)
p1<-w1/(w1+w2)
p2<-1-p1
pdf("HW_Fig6.pdf")
plot(theta,p1*dbeta(theta,a1+y,n-y+b1)+p2*dbeta(theta,a2+y,n-y+b2),type="l",main="mix post.dist",col="blue",ylab="p(theta|y)",lwd=3,xaxt="n")
axis(1,at=seq(0,1,0.05))
dev.off()

post.theta<-function(theta) p1*dbeta(theta,a1+y,n-y+b1)+p2*dbeta(theta,a2+y,n-y+b2)


#discrete approximation
theta_0.025<-seq(0,0.25,0.01)
n_0.025<-length(theta_0.025)
theta_0.975<-seq(1,0.4,-0.01)
n_0.975<-length(theta_0.975)

d1<-post.theta(theta_0.025)*0.01 #*0.01 because increment size of theta_seq is 0.01
d2<-post.theta(theta_0.975)*0.01

cdf_0.025<-d1[1]
k<-0
for (i in 2:n_0.025){
if(k==0){
cdf_0.025<-c(cdf_0.025,cdf_0.025[i-1]+d1[i])
k<-(cdf_0.025[i]>=0.025)*1
} else {
k<-length(cdf_0.025)
}
}
theta_0.025[k]

cdf_0.975<-d2[1]
k<-0
for (i in 2:n_0.975){
if(k==0){
cdf_0.975<-c(cdf_0.975,cdf_0.975[i-1]+d2[i])
k<-(cdf_0.975[i]>=0.025)*1
} else {
k<-length(cdf_0.975)
}
}
theta_0.975[k]

#b)
theta.mc<-NULL
for (i in 1:1000){
x<-rbinom(1,1,p1)
if (x==1){
 theta <-rbeta(1,a1+y,n-y+b1)
} else {
 theta <-rbeta(1,a2+y,n-y+b2)	
}
theta.mc<-c(theta.mc,theta)
}

quantile(theta.mc,c(0.025,0.975))




