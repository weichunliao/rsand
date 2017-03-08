gpredict = function(dftrain, dftest){
	

￼￼

}

setwd("/Users/weichun/Desktop/rsand/hw1_wdata")
dftrain = read.csv("df1_train.csv")
dftest1 = read.csv("df1_test1.csv")
dftest1y = read.csv("df1_test1y.csv")
dftrain = dftrain[1:200,]

dftrain_a = dftrain$life_post_consumer
mua = mean(dftrain_a)
dftrain_b = dftrain[ , -which(colnames(dftrain) %in% c("life_post_consumer"))]
mub = colMeans(dftrain_b)
s_ab = 

# s_ab = matrix(0, nrow=1, ncol=nfeature)
# s_bb=matrix(0, nrow=nfeature, ncol=nfeature)
# predict=rep(0, ntest))
￼

