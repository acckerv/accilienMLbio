d=read.delim("breast_cancer_data.csv")
head(d)

library(rpart)
set.seed(101)
d_perm=d[sample(dim(d)[1],dim(d)[1],replace=FALSE),]
d_val=d_perm[1:floor(.75*(dim(d)[1])),]
d_test=d_perm[(floor(.75*(dim(d)[1]))+1):(dim(d)[1]),]
m_d=rpart(diagnosis~.,data=d_val, method="class")
print(m_d)
plot(m_d, uniform = T, margin=0.1)
text(m_d, use.n=T, all=T, cex=0,8)
plot(m_d, uniform = T, margin=0.1)
text(m_d, use.n=T, all=T, cex=0,8)

rpart.control()
m_d=rpart(diagnosis~.,data=d_val, method="class", 
          control = rpart.control(cp=0, minsplit = 1))
print(m_d)


