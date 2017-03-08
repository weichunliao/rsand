library("MASS")
gpredict = function(dftrain, dftest = NULL){
	dftrain_a = dftrain$life_post_consumer
	mua = mean(dftrain_a)

	feature_names = colnames(dftest)
	dftrain_b = dftrain[c(feature_names)]
	mub = colMeans(dftrain_b)

}

setwd("/Users/weichun/Desktop/rsand/hw1_wdata")
dftrain = read.csv("df1_train.csv")
dftest1 = read.csv("df1_test1.csv")
dftest1y = read.csv("df1_test1y.csv")
dftrain = dftrain[1:200,]

k = ncol(dftrain-1) # number of feature varables (Xb)
n = nrow((dftrain)) # number of training subjects

dftrain_a = dftrain$life_post_consumer
mua = mean(dftrain_a)

dftrain_b = dftrain[ , -which(colnames(dftrain) %in% c("life_post_consumer"))]
mub = colMeans(dftrain_b)

n_mub = matrix(data=1, nrow=n) %*% t(as.matrix(mub))
s_ab = ((n-1)^(-1)) * t(as.matrix(dftrain_a-mua)) %*% as.matrix(dftrain_b-n_mub)
s_bb = ((n-1)^(-1)) * t(as.matrix(dftrain_b-n_mub)) %*% as.matrix(dftrain_b-n_mub)

n_test = nrow(dftest1)
n_mub_test = matrix(data=1,  nrow = n_test) %*% t(as.matrix(mub))

predict = mua + s_ab %*% ginv(s_bb) %*% t(as.matrix(dftest1 - n_mub_test))
mae1a = mean(abs(dftest1y[,1] - predict))
# predict=rep(0, ntest))


