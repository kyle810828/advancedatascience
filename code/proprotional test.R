setwd("~/Desktop/DS1001/Data/Indeed")
Indeed<-read.csv("Indeed.csv")
Indeed<-as.data.frame(Indeed)
setwd("~/Desktop/DS1001/Data/Glassdoor")
GlassDoor<-read.csv("GlassDoor.csv")
GlassDoor<-as.data.frame(GlassDoor)

testG<-GlassDoor[,c('R','Python','Math','C++','Statistics')]
testI<-Indeed[,c('R','Python','Math','C++','Statistics')]

#proportional test
proptest<-matrix(rep(NA,10),ncol=2)
for(i in 1:5){
  x<-prop.test(c(sum(testG[,i]),sum(testI[,i])),c(nrow(testG),nrow(testI)),correct=FALSE)
  proptest[i,]<-c(colnames(testG)[i],x$p.value)
}
proptest<-as.data.frame(proptest)
colnames(proptest)<-c("Skills","p-value")
## proportional test
kable(proptest)


