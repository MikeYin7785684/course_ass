x1<-rnorm(25)
x1_tr<-x1[1:20]
x1_tt<-x1[20:25]
x2<-rnorm(25,x1,0.001)
x2_tr<-x2[1:20]
x2_tt<-x2[20:25]
y<- rnorm(25,3+x1+x2)
y_tr<-y[1:20]
y_tt<-y[20:25]
model1<-lm(y_tr ~ x1_tr + x2_tr)
train_data<-cbind(x1_tr,x2_tr)
test_data <- data.frame(x1_tr=x1_tt, x2_tr = x2_tt)
y_p1<-predict(model1,test_data)
errors1<-sum((y_tt-y_p1)^2)
errors1
model2<-glmnet(train_data, y_tr, lambda=.01)
y_p2<-predict(model2,as.matrix(test_data))
errors2<-sum((y_tt-y_p2)^2)
errors2
