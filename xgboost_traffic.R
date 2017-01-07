library(xgboost)
packageVersion("xgboost")
setwd("C:/Users/User/Desktop/Andy/20170107")
load("LTEextreaction.rdata.RData")
remains<-names(which(table(x$label)>=50))
h1<-x$label %in% remains

x<-x[h1,]

x[,15]=factor(x[,15])
n_class=length(levels(x[,15]))

vali_idx=sample(nrow(x), nrow(x)*0.2)


print(class(x))
for (i in 2:14){
    print(class(x[,i]))
}


train_x=as.matrix(sapply(x[-vali_idx,2:14], as.numeric))
train_y=as.integer(x[-vali_idx,15])-1

vali_x=as.matrix(sapply(x[vali_idx,2:14], as.numeric))
vali_y=as.integer(x[vali_idx,15])-1

dtrain<- xgb.DMatrix(train_x,label=(train_y))  
dval<- xgb.DMatrix(vali_x, label=(vali_y))  

param <- list(  objective           = "multi:softprob", 
                eta                 = 0.05 , # 0.06, #0.01,
                eval_metric         = "merror",
                max_depth           = 3, #changed from default of 15
                subsample           = 0.3, # 0.9
                num_class           = n_class
                
)
watchlist <- list(eval = dval,train=dtrain)


bst <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 3000, #300, #280, #125, #250, # changed from 300
                    verbose             = 1,
                    early.stop.round    = 250,
                    watchlist           = watchlist
)

xgb.DMatrix.save(dtrain, 'xgb.DMatrix.data')
xgb.save(bst, 'xgb.model')



mat=c(1,2,3,4,5,6,1,2,3,4,5,6,1)
dtest <-xgb.DMatrix(matrix(mat,nrow=1))
pred0 = predict(bst,dtest)

