library(rpart)


d=read.delim("breast_cancer_data.csv", sep = ",", header=T)
head(d)

set.seed(101)
d_perm=d[sample(dim(d)[1],dim(d)[1],replace=FALSE),]
d_val=d_perm[1:floor(.75*(dim(d)[1])),]
d_test=d_perm[(floor(.75*(dim(d)[1]))+1):(dim(d)[1]),]

m=rpart(diagnosis~.,data=d_val, method="class")
m.pred<-predict(m,type="class")
err.m.pred<-sum(m.pred!=d$diagnosis)/nrow(m)


mfull=rpart(diagnosis~.,data=d_val, method="class", 
          control = rpart.control(cp=0, minsplit = 1))
mfull.pred<-predict(mfull,d_val[gp==counter,],type="class")
err.mfull.pred<-sum(pred!=d_val$diagnosis[gp==counter])/
  sum(gp==counter)



numgp<-5 #number of folds
gp<-rep(1:numgp,length.out=dim(d_val)[1])


err.m<-NA*numeric(numgp)
for (counter in 1:numgp) {
  #fit the model on all of the data excluding one group
  m<-rpart(diagnosis~.,data=d_val[gp!=counter,],method="class")
  #get predictions for the left out group and get error rates
  pred<-predict(m,d_val[gp==counter,],type="class")
  err.m[counter]<-sum(pred!=d_val$diagnosis[gp==counter])/
    sum(gp==counter)
}
mean(err.m)

err.mfull<-NA*numeric(numgp)
for (counter in 1:numgp) {
  #fit the model on all of the data excluding one group
  mfull<-rpart(diagnosis~.,data=d_val[gp!=counter,],method="class", 
           control = rpart.control(cp=0, minsplit = 1))
  #get predictions for the left out group and get error rates
  pred<-predict(mfull,d_val[gp==counter,],type="class")
  err.mfull[counter]<-sum(pred!=d_val$diagnosis[gp==counter])/
    sum(gp==counter)
}
mean(err.mfull)











