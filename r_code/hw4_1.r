filter_chisq = function(dstrain, ypos="pos", min_count=5, chi_threshold = 10^(-5)){
	chi_test = function(product, feature) {
		ppp = as.numeric( product == ypos )
		feature = as.numeric( feature > 0 )

		if (sum(feature) <= min_count ) {
			return (NA)
		}

		# ppp = factor(ppp, levels=c(0,1))
		# feature = factor(feature, levels=c(0,1))
		N = length(ppp)
		f1c1 = sum(ppp*feature)
		f0c1 = sum(ppp) - f1c1
		f1c0 = sum(feature) - f1c1
		f0c0 = N - f1c1 - f0c1 - f1c0

		contable = rbind(cbind(f1c1, f0c1), cbind(f1c0, f0c0))
		chi_value = chisq.test(contable, correct=FALSE)

		# chi_value = chisq.test(ppp, feature, correct=FALSE)
		# chi_value = chisq.test(x=as.factor(ppp), y=as.factor(feature), correct=FALSE)
		# chi_value = chisq.test(ppp, feature, correct=FALSE)

		# chi_value = chisq.test(ppp, feature, correct=FALSE, simulate.p.value = TRUE)

		return (as.numeric(chi_value$statistic))
	}

	feature_mat = dstrain[,-1]
	chi_test_values = unlist(apply(feature_mat, 2, function(x) chi_test(dstrain[,1], x) ))
	chi_test_values[which(chi_test_values < chi_threshold, arr.ind=T )] = NA

	result = sort(chi_test_values, decreasing=T, na.last=NA)

	if (length(result)==0) {
		return ( list(colpos = NULL, colname = NULL, chistat = NULL) )
	}

	colpos = as.numeric(sapply(names(result), function(x) which(x == colnames(dstrain), arr.ind=T)))
	result = list(colpos = colpos, colname = names(result), chistat = result)

	return (result)
}





#############
# load('hw4ds1.rdata')
# testfold = 1
# dstrain1 = hw4ds1[-folds[[testfold]],] 
# out1=filter_chisq(dstrain1)

# print(head(out1$colpos, n=15))
# print(head(out1$colname, n=15))
# print(head(out1$chistat, n=15))


# ##
# load('hw4ds1.rdata')
# testfold = 2
# dstrain1 = hw4ds1[-folds[[testfold]],] 
# out1=filter_chisq(dstrain1)

# #######
# dstrain = dstrain1
# ypos="pos"
# min_count=5
# chi_threshold = 10^(-5)
# ####

# chi_test = function(product, feature) {
# 	product = dstrain1[,1]
# 	feature = dstrain1[,4110]

# 	ppp = as.numeric( product == ypos )
# 	feature = as.numeric( feature > 0 )

# 	if (sum(feature) <= min_count ) {
# 		return (NA)
# 	}

# 	chi_value = chisq.test(factor(ppp, levels=c(0,1)), factor(feature, levels=c(0,1)), correct=FALSE)
# 	# cat(levels(as.factor(ppp)))
# 	# chi_value = chisq.test(ppp, feature, correct=FALSE, simulate.p.value = TRUE)

# 	return (as.numeric(chi_value$statistic))
# }



# feature_mat = dstrain[,-1]
# chi_test_values = unlist(apply(feature_mat[,1:5], 2, function(x) chi_test(dstrain[,1], x) ))
# chi_test_values[which(chi_test_values < chi_threshold, arr.ind=T )] = NA

# result = sort(chi_test_values, decreasing=T, na.last=NA)

# if (length(result)==0) {
# 	return ( list(colpos = NULL, colname = NULL, chistat = NULL) )
# }

# colpos = as.numeric(sapply(names(result), function(x) which(x == colnames(dstrain), arr.ind=T)))
# result = list(colpos = colpos, colname = names(result), chistat = result)



# #########

# out1=filter_chisq(dstrain1)



