filter_chisq = function(dstrain, ypos="pos", min_count=5, chi_threshold = 10^(-5)){






	if () {
		return ( list(colpos = NULL, colname = NULL, chistat = NULL) )
	}
	result = list(colpos = , colname = , chistat = )
	return (result)
}





#############
load('hw4ds1.rdata')
testfold = 1
dstrain1 = hw4ds1[-folds[[testfold]],] 


ypos="pos"
min_count=5
chi_threshold = 10^(-5)
######


chi_test = function(product, feature) {
	ppp = as.numeric( product == ypos )
	feature = as.numeric( feature > 0 )

	if (sum(feature) <= min_count ) {
		return (NA)
	}

	chi_value = chisq.test(ppp, feature, correct=FALSE, simulate.p.value = TRUE)

	return (as.numeric(chi_value$statistic))
}



feature_mat = dstrain1[,-1]
chi_test_values = unlist(apply(feature_mat, 2, function(x) chi_test(dstrain1[,1], x) ))
chi_test_values[which(chi_test_values < chi_threshold, arr.ind=T )] = NA
names(sort(chi_test_values, decreasing=T, na.last=NA))




#########

out1=filter_chisq(dstrain1)



