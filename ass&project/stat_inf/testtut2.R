gdp=read.csv("gdp.csv");
gdp[,5]=as.numeric(gdp[,5]);
gdp=na.omit(gdp);
gdp[,5]=log(gdp[,5])
gdplog=gdp[,5]
summary(gdplog)
below=quantile(gdplog,0.25)-1.5*IQR(gdplog)
above=quantile(gdplog,0.75)+1.5*IQR(gdplog)
outliers=gdp[(gdplog<below|gdplog>above),]
outliers
boxplot(gdplog)
mean(gdplog)
sd(gdplog)

#tut3


plot(x,y)
#MC method

#2d uniform pionts
n=100
x=runif(n)
y=1-2*runif(n)
cos2pix=cos(2*pi*x)
intcos2pix=ifelse(cos2pix>y,1,0)
2*sum(intcos2pix)/n-1

lines(x,y,'p','x')

n=1000
x=runif(n)
y=1-2*runif(n)
cos2pix=cos(2*pi*x)
intcos2pix=ifelse(cos2pix>y,1,0)
2*sum(intcos2pix)/n-1
lines(x,y,'p','.')

#1d uniform with binomial(1,p)
n=100
x=runif(n)
cos2pix=cos(2*pi*x)
posif=ifelse(cos2pix>0,1,-1)
intcos2pix=rbinom(n,1,abs(cos2pix))*posif
sum(intcos2pix)/n

n=1000
x=runif(n)
cos2pix=cos(2*pi*x)
posif=ifelse(cos2pix>0,1,-1)
intcos2pix=rbinom(n,1,abs(cos2pix))*posif
sum(intcos2pix)/n


## 3

#numeric method
n=100
x=seq(0+1/n,1,1/n)
y=cos(2*pi*(x)^2)
intcos2pix2=sum(y)/n
intcos2pix2

n=1000
x=seq(0+1/n,1,1/n)
y=cos(2*pi*(x)^2)
intcos2pix2=sum(y)/n
intcos2pix2


plot(x,y)
#MC method

#2d uniform pionts
n=100
x=runif(n)
y=1-2*runif(n)
cos2pix2=cos(2*pi*(x)^2)
intcos2pix2=ifelse(cos2pix2>y,1,0)
(2*sum(intcos2pix2)/n)-1


lines(x,y,'p','x')

n=1000
x=runif(n)
y=1-2*runif(n)
cos2pix2=cos(2*pi*(x)^2)
intcos2pix2=ifelse(cos2pix2>y,1,0)
(2*sum(intcos2pix2)/n)-1
lines(x,y,'p','.')


#1d uniform with binomial(1,p)
n=100
x=runif(n)
cos2pix2=cos(2*pi*(x^2))
posif=ifelse(cos2pix2>0,1,-1)
intcos2pix2=rbinom(n,1,abs(cos2pix2))*posif
sum(intcos2pix2)/n

n=1000
x=runif(n)
cos2pix2=cos(2*pi*x^2)
posif=ifelse(cos2pix2>0,1,-1)
intcos2pix2=rbinom(n,1,abs(cos2pix2))*posif
sum(intcos2pix2)/n



#midterm
cw<-function(s,n=1000,m,v){
#s,n,m,v stand for number of seats,stimulation number, the first m passengers spilled coffee on tickets and the v-th person who we inteested in 
suc=0#success number
prob=0
for(i in 1:n){
  cw_seat<-sample.int(s)#initial 
  cw_drop<-sample(cw_seat,m)#first m random pick
  if(all(cw_drop==cw_seat[1:m])){#if pick their own
    suc<-suc+1
  }
  else{
    idx=c()
    for (cwd in cw_drop){
    idx<-c(idx,which(cw_seat==cwd))
    }
    idx=sort(idx)
    if(v<idx[1]){#pick all after v
      suc<-suc+1
    }
    else{
    if( !(v %in% idx) ){#not pick v
    while (idx[1]<v & idx[1]!=1){
      t<-idx[1]+1
      rest_seat<-c(cw_seat[1],cw_seat[t:s])#jump to sitting wrong seat smallest 
      cw_drop<-sample(rest_seat,1)
      idx[1]<-which(cw_seat==cw_drop)
      idx<-sort(idx)
      if(idx[1]==1 | idx[1]>v)
      {
        suc<-suc+1
      }
    }
    }
  }
  }
  }
prob<-suc/n
return(prob)
}
cw(10,10000,1,10)
cw(10,10000,1,5)
cw(10,10000,2,10)

#mid 3
n<-100
theta<-3
x<-rexp(n,1/theta)+1
hist(x,xlab="X",main="Sample of X")
abline(v=mean(x),col="red")
abline(v=4,col="blue")

S<-1000
n<-100
alpha<-c()
theta<-c()
for(i in 1:S){
  theta<-3
  x<-rexp(n,1/theta)+1
  alpha<-c(alpha,min(x))
  theta<-c(theta,mean(x)-min(x))
}
hist(alpha,xlab=expression(paste("Value of ", alpha)),main=expression(paste("Sample of ", alpha)))
hist(x,xlab=expression(paste("Value of ", theta)),main=expression(paste("Value of ", theta)))

#mid4
da<-read.csv(file = "Desktop/course/STAT6027/data.csv")
wbar<-mean(da[,1])
n<-1000
theta=100
err=1
while(err>1e-3){
f<-theta/(1-exp(-theta))-wbar
fdash<-(1-exp(-theta)+theta*exp(-theta))/(1-exp(-theta))^2
err<-f/fdash
theta<- theta-err
}
theta

#mid4.4
fn<-function(w,theta){
  (-sum(w*log(theta)-theta-log(factorial(w))-log(1-exp(-theta))))
}
optim(par=100,fn=fn,w=da[,1],method = "BFGS")


#tut8
1-pnorm(qnorm(0.99)-sqrt(5))
((qnorm(0.99)-qnorm(0.05))^2)*2
 



#tut9
poisson_log_likelihood <- function(beta, x, y) {
  mu <- exp(beta[1] + beta[2]*x+beta[3]*x^2)  # Linear predictor
  -sum(dpois(y, lambda = mu, log = TRUE))  # Log-likelihood
}
data<-read.table("Data")
x<-na.omit(as.numeric(data$V2))
y<-na.omit(as.numeric(data$V1))
# ini_beta<-c(0,0,0)
fit<-optim(par=ini_beta,fn=poisson_log_likelihood,hessian = TRUE,x=x,y=y)
coefficients <- fit$par
coefficients
variance<-diag(solve(-fit$hessian))
variance

#tut10
data<-read.table('Data.txt')
data
x<-na.omit(as.numeric(data$V2))
y<-na.omit(as.numeric(data$V1))
ini_beta<-c(0,0,0)
poisson_log_likelihood_1<- function(beta, x, y) {
  mu <- exp(beta[1] + beta[2]*x+beta[3]*x^2)  # Linear predictor
  -sum(dpois(y, lambda = mu, log = TRUE))  # Log-likelihood
}
poisson_log_likelihood_0<- function(beta, x, y) {
  mu <- exp(beta[1] )  # Linear predictor
  -sum(dpois(y, lambda = mu, log = TRUE))  # Log-likelihood
}
fit_1<-optim(par=ini_beta,fn=poisson_log_likelihood_1,hessian = TRUE,x=x,y=y)
coefficients_1 <- fit_1$par
fit_0<-optim(par=ini_beta[1],fn=poisson_log_likelihood_0,hessian = TRUE,x=x,y=y)
coefficients_0 <- fit_0$par
chisq_t<- 2*(poisson_log_likelihood_0(coefficients_0,x,y)-poisson_log_likelihood_1(coefficients_1,x,y))
coefficients_0
coefficients_1
pchisq(chisq_t,2)

