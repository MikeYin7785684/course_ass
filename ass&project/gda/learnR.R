load('classdata.RData')

#tut2
ozone
class(ozone)
names(ozone)
summary(ozone)
xl=range(c(stamford,yonkers),na.rm=T)
par(mfrow=c(2,1))  # Two histograms on the same page
hist(stamford,breaks=seq(0,250,25),main="Histogram of Stamford Ozone",xlim=xl,col=0)
hist(yonkers,breaks=seq(0,250,25),main="Histogram of Yonkers Ozone",xlim=xl,col=0)
stamford=stamford[!is.na(stamford)]
yonkers=yonkers[!is.na(yonkers)]
plot(density(stamford,window="g",width=60),type='l',xlab="Ozone",ylab="Density",main="Stamford Ozone",xlim=xl)
plot(density(yonkers,window="g",width=60),type='l',xlab="Ozone",ylab="Density",main="Yonkers Ozone",xlim=xl)
qqoz=qqnorm(c(stamford,yonkers))
xl=range(qqoz$x)
yl=range(qqoz$y)
par(mfrow=c(1,2),pty="s")
qqnorm(stamford,xlim=xl,ylim=yl)
qqline(stamford);abline(h=mean(stamford),v=0,col="grey")
qqnorm(yonkers,xlim=xl,ylim=yl)
qqline(yonkers);abline(h=mean(yonkers),v=0,col="grey")
par(pty="m")
par(mfrow=c(1,1),pty="s")
qqplot(stamford,yonkers,main="Q-Q plot of Stamford and Yonkers data")
par(pty="s")
boxplot(stamford,yonkers,main="Ozone Distribution for Stamford and Yonkers",
        sub="Boxplot",ylab="Ozone",names=c("Stamford","Yonkers"))
newstam=stamford^(1/3)
newyonk=yonkers^(1/3)
hist(newstam,main="Histogram of Transformed Stamford Ozone",col=0)
hist(newyonk,main="Histogram of Transformed Yonkers Ozone",col=0)
par(mfrow=c(2,1),pty="m")  # Two histograms on the same page
hist(newstam,breaks=seq(2,7,1/2),main="Histogram of Transformed Stamford Ozone",col=0)
hist(newyonk,breaks=seq(2,7,1/2),main="Histogram of Transformed Yonkers Ozone",col=0)
xl=range(c(newstam,newyonk))
plot(density(newstam,window="g",width=2),type='l',xlab="Ozone",ylab="Density",
     main="Transformed Stamford Ozone",xlim=xl)
plot(density(newyonk,window="g",width=2),type='l',xlab="Ozone",ylab="Density",
     main="Transformed Yonkers Ozone",xlim=xl)
qqoz2=qqnorm(c(newstam,newyonk))
xl=range(qqoz2$x)
yl=range(qqoz2$y)
par(mfrow=c(1,2),pty="s")
qqnorm(newstam,xlim=xl,ylim=yl)
qqline(newstam)
abline(h=mean(newstam),v=0,col="grey")
qqnorm(newyonk,xlim=xl,ylim=yl)
qqline(newyonk)
abline(h=mean(newyonk),v=0,col="grey")
par(mfrow=c(1,1))
qqplot(newstam,newyonk,main="Q-Q plot of Stamford and Yonkers data")
par(pty="m")
boxplot(newstam,newyonk,main="Ozone Distribution for Transormed Stamford and Yonkers",
        sub="Boxplot",ylab="Ozone",names=c("Stamford","Yonkers"))
year1=nickel[,1]
year2=nickel[,2]
year3=nickel[,3]
year4=nickel[,4]
year5=nickel[,5]
xl=range(c(year1,year2,year3,year4,year5))
par(mfrow=c(1,5))
hist(year1,col=0,xlim=xl)
hist(year2,col=0,xlim=xl)
hist(year3,col=0,xlim=xl)
hist(year4,col=0,xlim=xl)
hist(year5,col=0,xlim=xl)
plot(density(year1,window="g",width=0.2),type='l',main="1977 nickels",xlim=xl)
plot(density(year2,window="g",width=0.2),type='l',main="1978 nickels",xlim=xl)
plot(density(year3,window="g",width=0.1),type='l',main="1979 nickels",xlim=xl)
plot(density(year4,window="g",width=0.1),type='l',main="1980 nickels",xlim=xl)
plot(density(year5,window="g",width=0.1),type='l',main="1981 nickels",xlim=xl)
par(mfrow=c(1,1))
boxplot(year1,year2,year3,year4,year5,main="Nickel weights for five years",
        sub="Boxplot",ylab="Weight (gms)",names=c("1977","1978","1979","1980","1981"))
abline(h=4.53,col="red",lwd=2)


#tut3
normal=antibody$normal
diabetic=antibody$diabetic
insulin=antibody$insulin

anti=c(normal,diabetic,insulin)
xl=range(anti)
par(mfrow=c(3,1))
hist(normal,main="Nitrogren Binding for Normal Group",probability=T,xlim=xl,col=0)
lines(density(normal,window="c",width=250),type="l")
hist(diabetic,main="Nitrogren Binding for Alloxan Group",probability=T,xlim=xl,col=0)
lines(density(diabetic,window="c",width=250),type="l")
hist(insulin,main="Nitrogren Binding for Insulin Group",probability=T,xlim=xl,col=0)
lines(density(insulin,window="c",width=250),type="l")

par(mfrow=c(1,3),pty="s")
qqnorm(normal);qqline(normal);abline(h=mean(normal),v=0,col="grey")
qqnorm(diabetic);qqline(diabetic);abline(h=mean(diabetic),v=0,col="grey")
qqnorm(insulin);qqline(insulin);abline(h=mean(insulin),v=0,col="grey")
par(pty="m")
anti=c(log(normal),log(diabetic),log(insulin))
xl=range(anti)
par(mfrow=c(3,1))
hist(log(normal),main="Nitrogren Binding for Normal Group",probability=T,xlim=xl,col=0)
lines(density(log(normal),window="c",width=5),type="l")
hist(log(diabetic),main="Nitrogren Binding for Alloxan Group",probability=T,xlim=xl,col=0)
lines(density(log(diabetic),window="c",width=5),type="l")
hist(log(insulin),main="Nitrogren Binding for Insulin Group",probability=T,xlim=xl,col=0)
lines(density(log(insulin),window="c",width=5),type="l")
anti=c(sqrt(normal),sqrt(diabetic),sqrt(insulin))
xl=range(anti)
par(mfrow=c(3,1))
hist(sqrt(normal),main="Nitrogren Binding for Normal Group",probability=T,xlim=xl,col=0)
lines(density(sqrt(normal),window="c",width=20),type="l")
hist(sqrt(diabetic),main="Nitrogren Binding for Alloxan Group",probability=T,xlim=xl,col=0)
lines(density(sqrt(diabetic),window="c",width=20),type="l")
hist(sqrt(insulin),main="Nitrogren Binding for Insulin Group",probability=T,xlim=xl,col=0)
lines(density(sqrt(insulin),window="c",width=20),type="l")

cube=function(x){
  x^(1/3)
}
anti=c(cube(normal),cube(diabetic),cube(insulin))
xl=range(anti)
par(mfrow=c(3,1))
hist(cube(normal),main="Nitrogren Binding for Normal Group",probability=T,xlim=xl,col=0)
lines(density(cube(normal),window="c",width=5),type="l")
hist(cube(diabetic),main="Nitrogren Binding for Alloxan Group",probability=T,xlim=xl,col=0)
lines(density(cube(diabetic),window="c",width=5),type="l")
hist(cube(insulin),main="Nitrogren Binding for Insulin Group",probability=T,xlim=xl,col=0)
lines(density(cube(insulin),window="c",width=5),type="l")

par(mfrow=c(1,3),pty="s")
qqnorm(normal);qqline(normal);abline(h=mean(normal),v=0,col="grey")
qqnorm(diabetic);qqline(diabetic);abline(h=mean(diabetic),v=0,col="grey")
qqnorm(insulin);qqline(insulin);abline(h=mean(insulin),v=0,col="grey")

qqnorm(log(normal));qqline(log(normal));abline(h=mean(log(normal)),v=0,col="grey")
qqnorm(log(diabetic));qqline(log(diabetic));abline(h=mean(log(diabetic)),v=0,col="grey")
qqnorm(log(insulin));qqline(log(insulin));abline(h=mean(log(insulin)),v=0,col="grey")

qqnorm(sqrt(normal));qqline(sqrt(normal));abline(h=mean(sqrt(normal)),v=0,col="grey")
qqnorm(sqrt(diabetic));qqline(sqrt(diabetic));abline(h=mean(sqrt(diabetic)),v=0,col="grey")
qqnorm(sqrt(insulin));qqline(sqrt(insulin));abline(h=mean(sqrt(insulin)),v=0,col="grey")

qqnorm(cube(normal));qqline(cube(normal));abline(h=mean(cube(normal)),v=0,col="grey")
qqnorm(cube(diabetic));qqline(cube(diabetic));abline(h=mean(cube(diabetic)),v=0,col="grey")
qqnorm(cube(insulin));qqline(cube(insulin));abline(h=mean(cube(insulin)),v=0,col="grey")

anti=c(sqrt(normal),sqrt(diabetic),sqrt(insulin))
par(mfrow=c(1,1))
boxplot(sqrt(normal),sqrt(diabetic),sqrt(insulin),main="Boxplots of Nitrogren Binding for three groups 
          (sqrt scale)",ylab="Binding Capacity",names=c("Normal","Diabetic","Insulin"),
        style.bxp="old")

#tut4
volt = test$volt
life = test$time
plot(volt,life,main="Time to breakdown at various voltages",xlab="Voltage",
     ylab="Lifetime")
loglife = log(life)
plot(volt,loglife,main="Time to breakdown at various voltages, log scale",
     xlab="voltage",ylab="log(lifetime)")
voltvals = unique(volt)
splits = 0
for(i in 1:(length(voltvals)-1)){
  splits[i]=(voltvals[i]+voltvals[i+1])/2
}
abline(v=splits)
medians=0
q1s=0
q3s=0
for(i in 1:length(voltvals)){
  bp=boxplot(loglife[volt==voltvals[i]],plot=F)
  medians[i]=bp$stats[3]
  q1s[i]=rev(bp$stats)[2]
  q3s[i]=rev(bp$stats)[4]
  Addbplots(voltvals[i],bp$stats,0.5)
}
lines(voltvals,medians,type="l")
lines(voltvals,q1s,lty=2)
lines(voltvals,q3s,lty=2)
plot(volt,loglife,main="Time to breakdown at various voltages, log scale",
     xlab="voltage",ylab="log(lifetime)")
lines(lowess(volt,loglife),type="l")
lines(ksmooth(volt,loglife,kernel="n",ban=5),type="l",col="red")
fitlife=lowess(volt,loglife)
reslife=loglife-approx(fitlife$x,fitlife$y,volt)$y
plot(volt,reslife,main="Lowess residuals for Log lifetime versus
 voltage",xlab="Voltage",ylab="Lowess Residuals")

par(pty="s")  
qqnorm(reslife/approx(lowess(volt,abs(reslife))$x,lowess(volt,abs(reslife))$y,volt)$y,
       main="Q-Q plot of Lowess Residuals from log lifetime versus voltage",
       ylab="Quantiles of Estimated Residuals")
qqline(reslife/approx(lowess(volt,abs(reslife))$x,lowess(volt,abs(reslife))$y,volt)$y)
abline(h=0,v=0,col="grey")
par(pty="m")
ibrary(MASS)                                                        
fit = rlm(loglife~volt)
plot(volt,fit$residuals,main="Residual plot from model fit",xlab="Voltage",
     ylab="Residuals")
plot(volt,abs(fit$residuals),main="Absolute residual plot from model fit",
     xlab="Voltage",ylab="Absolute Residuals")
lines(lowess(volt,abs(fit$residuals)),type="l")






#Tut7
library(MASS) 
load('classdata.RData')
plot(lynx,type="b",pch=1,main="Lynx Data",ylab="Number of
       Trappings",xlab="Time")
plot(sqrt(lynx),type="b",pch=1,main="sqrt(Lynx) Data",ylab="Number of
       Trappings",xlab="Time")
plot(log(lynx),type="b",pch=1,main="log(Lynx) Data",ylab="Number of 
       Trappings",xlab="Time")
Ident(sqrt(lynx),"sqrt(lynx) Data","Raw Data")
fin = Raic(sqrt(lynx))


v = Factor(c(rep(1:10,11),1,2,3,4))
v1 = cbind(1-apply(v,1,sum),v)
apply(v1,2,sum)

mod = rlm(sqrt(lynx)~v)

mod$coef

Ident(mod$resid,"sqrt(lynx) Data","Residuals from cyclic model")

fin = Raic(mod$resid)

c(fin$coef[1,2],fin$coef[2,2])
res = fin$resid[,2]
fv = mod$resid[-1]-res
Ident(res,"Residuals for sqrt(lynx) Data","Cycle and AR(2) fitted")

par(mfrow=c(2,2),oma=c(0,0,6,0))

plot(1:(length(lynx)-1),res,type="b",pch=1,xlab="Time",ylab="Residuals",
     main="Residual plot")

plot(fv,res,
     main="Residuals versus fitted values",
     ylab="Residuals",xlab="Fitted Values")

par(pty="s")
qqnorm(res,main="Quantile-Quantile plot",
       ylab="Residuals",xlab="Gaussian Quantiles")
qqline(res);abline(h=0,v=0,col="grey")
par(pty="m")

plot(fv,abs(res),
     main="Absolute Residuals versus fitted values",
     ylab="Absolute Residuals", xlab="Fitted Values")
lines(lowess(fv,abs(res)))

mtext("Residual plots from cycle and AR(2) fit for sqrt(lynx) data",outer=T,
      side=3,cex=1.5)

plot(co2,main="Raw scale")
plot(sqrt(co2),main="Square Root")
plot(log(co2),main="Log scale")
plot(co2^(1/3),main="Cube Root")
plot(co2^(1/4),main="Fourth root")
# Again, there does not seem much difference, visually, in terms of spread
h=stl(co2,s.window="periodic",t.window=41)
plot(h)
h=stl(co2^(1/4),s.window="periodic",t.window=41)
plot(h)
# Really very little difference

#project
names(otter)
otter$group
otter$season
insure
par(mfrow=c(4,2))
for(i in 2:ncol(insure)){
  plot(insure[,1],insure[,i],xlab = names(insure)[1],ylab = names(insure)[i])
}






# Load required packages
# Load required packages
library(leaflet)
library(tigris)
options(tigris_use_cache = TRUE)
install.packages("ggplot2")
library(ggplot2)
ggplot(data = insure, aes(x = long, y = lat, map_id = insure$Zip)) +
  geom_map(aes(fill = insure$Race), map = ) +
  expand_limits(your_map_data) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_void()


####
#colnames(insure)[colnames(insure) == "ZIP"] <- "ZIPCODE"
library(ggplot2)
library(sf)
library(dplyr)
library(ggmap)
insure<- data.frame(insure)
colnames(insure)[colnames(insure) == "Zip"] <- "zip"
insure$zip <- as.character(insure$zip)
chicago_zip_boundaries <- st_read("Boundaries_ZIP_Codes.geojson")
merged_data <- inner_join(chicago_zip_boundaries, insure, by = c("Zip" = "zip"))
ggplot() +
  geom_sf(data = merged_data, aes(fill = Race)) +
  scale_fill_gradient(low = "white", high = "black") +
  labs(title = "Race by ZIP Code in Chicago") +
  theme_minimal()

ggplot() +
  geom_sf(data = merged_data, aes(fill = Fire)) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Fire by ZIP Code in Chicago") +
  theme_minimal()



ggplot() +
  geom_sf(data = merged_data, aes(fill = Theft)) +
  scale_fill_gradient(low = "white", high = "black") +
  labs(title = "Theft by ZIP Code in Chicago") +
  theme_minimal()
ggplot() +
  geom_sf(data = merged_data, aes(fill = Age)) +
  scale_fill_gradient(low = "black", high = "white") +
  labs(title = "Age by ZIP Code in Chicago") +
  theme_minimal()
ggplot() +
  geom_sf(data = merged_data, aes(fill = Volun)) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Volun by ZIP Code in Chicago") +
  theme_minimal()
ggplot() +
  geom_sf(data = merged_data, aes(fill = Invol)) +
  scale_fill_gradient(low = "white", high = "green") +
  labs(title = "Invol by ZIP Code in Chicago") +
  theme_minimal()
ggplot() +
  geom_sf(data = merged_data, aes(fill = Income)) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Income by ZIP Code in Chicago") +
  theme_minimal()
insure


####
cement
library(rgl) 
plot3d(cbind(y,x[,1:2]))
library(Rcmdr)
scatter3d(y,x[,1],x[,2])
library(akima)
surf <- interp(x[,1],x[,2],y)
surf$z <-ifelse(is.na(surf$z),0,surf$z)
persp(surf$z/200)
contour(surf)
w <- sweep(x,2,apply(x,2,mean))
w <- w %*% diag(1/sqrt(diag(crossprod(w))))
z <- crossprod(w)
s <- eigen(z)
sqrt(s$values[1]/s$values)
round(s$vectors,2)
