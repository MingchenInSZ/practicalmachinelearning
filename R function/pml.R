pml<-function()
{
  library(caret)
  curdir<-getwd()
  trainfile<-paste(curdir,"/pml-training.csv",sep="")
  testfile<-paste(curdir,"/pml-testing.csv",sep="")
  
  training <-read.table(trainfile,header=T,sep=",")
  testing <- read.table(testfile,header=T,sep=",")
  
  rs<-c()
  rsc<-c()
  for(name in names(training))
  {
    r<-c()
    for(var in as.list(training[name]))
    {
      r<-append(r,as.numeric(nchar(as.character(var))))    
    }
    rs<-append(rs,sum(r==0))
    rsc<-append(rsc,sum(is.na(training[name])))
  }
  inds<-c()
  for(i in 1:length(rs))
  {
    if(rs[i]>19622*0.7)
    {
      inds<-append(inds,i)
    }
  }
  indsc<-c()
  for(i in 1:length(rsc))
  {
    if(rsc[i]>19622*0.7)
    {
     indsc<-append(indsc,i) 
    }
  }
  lost_ind<-union(inds,indsc)# the most lost record column index
  lost_ind<-append(lost_ind,c(1:3))
  training<-subset(training,select=-lost_ind)
  testing<-subset(testing,select=-lost_ind)
  c_v<-createDataPartition(training$classe,p=0.3,list=F)
  traindata<-training[-c_v,]
  cross_valid<-training[c_v,]
  fit<-train(classe~.,data=traindata,method="gbm")
  pred<-predict(fit,newdata=cross_valid)
  
  print(sum(pred==cross_valid$classe)/dim(cross_valid)[1])
  p_test<-predict(fit,newdata=testing)
  
  p_test
}