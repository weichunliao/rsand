gpredict = function(dftrain = NULL, dftest = NULL){
	nfeature = ncol(dftest) # number of features
	nfeature_train = ncol(dftrain) - 1
	n = nrow(dftrain) # number of training observations
	n_test = nrow(dftest) # the number of testing observations

	if ( is.null(n) ) {
		return(NULL)
	} 
	if ( !is.null(dftest) ) {
		if ( nfeature != nfeature_train ) {
			return(NULL)
		}
	}

	dftrain_a = dftrain$life_post_consumer
	mua = mean(dftrain_a) # mean of Xa

	if ( is.null(dftest) ) {
		dftrain_b = dftrain[, -which(names(dftrain) %in% c("life_post_consumer"))]
	} else {
		feature_names = colnames(dftest)
		dftrain_b = dftrain[c(feature_names)]
	}
	mub = colMeans(dftrain_b) # means of Xb
	
	mat_mub = matrix(data=1, nrow=n) %*% t(as.matrix(mub))
	s_ab = ((n-1)^(-1)) * t(as.matrix(dftrain_a - mua)) %*% as.matrix(dftrain_b - mat_mub)
	s_bb = ((n-1)^(-1)) * t(as.matrix(dftrain_b - mat_mub)) %*% as.matrix(dftrain_b - mat_mub)

	if ( is.null(dftest) ) {
		predict = NULL
	} else {
		mat_mub_test = matrix(data=1,  nrow = n_test) %*% t(as.matrix(mub))
		predict = mua + s_ab %*% solve(s_bb) %*% t(as.matrix(dftest - mat_mub_test))
		predict = as.vector(predict)
	}
	rep = list(mua = mua, mub = mub, s_ab= s_ab, s_bb=s_bb, predict = predict)
	return(rep)
}

# setwd("/Users/weichun/Desktop/rsand/hw1_wdata")
# dftrain = read.csv("df1_train.csv")
# dftest1 = read.csv("df1_test1.csv")
# dftest1y = read.csv("df1_test1y.csv")
# dftrain = dftrain[1:200,]

# out = gpredict(dftrain, dftest1)
