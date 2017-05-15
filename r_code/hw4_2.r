filter_ig = function(dstrain, ypos="pos", min_count=5, ig_threshold=10^(-5)){

	infomation_gain = function(product, feature){
		# product = dstrain[,1]
		# feature = dstrain[,4115]

		ppp = as.numeric( product == ypos )
		feature = as.numeric( feature > 0 )

		if (sum(feature) <= min_count ) {
			return (NA)
		}

		p_threshold = 10^(-6)
		count_entropy = function(p_list) {
			ent = 0
			for (p in p_list) {
				if (p >= p_threshold & !is.nan(p)){
					ent = ent + (-1)*p*log2(p)
				}
			}
			return (ent)
		}

		N = length(ppp)
		# P_pos = sum(ppp)/N
		# P_n_pos = 1 - P_pos

		P_feature = sum(feature)/N
		P_n_feature = 1 - P_feature
		
		f1c1 = sum(ppp*feature)
		f0c1 = sum(ppp) - f1c1
		f1c0 = sum(feature) - f1c1
		f0c0 = N - f1c1 - f0c1 - f1c0

		P_f1c1 = f1c1/sum(feature)
		P_f1c0 = 1 - P_f1c1

		P_f0c1 = f0c1/(f0c1+f0c0)
		P_f0c0 = f0c0/(f0c1+f0c0)

		ig_value = P_feature*count_entropy(c(P_f1c1, P_f1c0)) + P_n_feature*count_entropy(c(P_f0c1, P_f0c0))
		return (ig_value)
	}

	feature_mat = dstrain[,-1]
	ig_values = unlist(apply(feature_mat, 2, function(x) infomation_gain(dstrain[,1], x) ))

	P_pos = sum(as.numeric(dstrain[,1]==ypos))/nrow(dstrain)
	H_x = (-1)*( P_pos*log2(P_pos) + (1-P_pos)*log2(1-P_pos) )

	ig_values = H_x - ig_values
	ig_values[which(ig_values < ig_threshold, arr.ind=T )] = NA

	result = sort(ig_values, decreasing=T, na.last=NA)

	if (length(result)==0) {
		return (list(colpos=NULL, colname=NULL, igvalue=NULL))
	}
	colpos = as.numeric(sapply(names(result), function(x) which(x == colnames(dstrain), arr.ind=T)))
	result = list(colpos=colpos, colname=names(result), igvalue=result)
	return (result)
}


########

# load('hw4ds1.rdata')
# testfold = 1
# dstrain1 = hw4ds1[-folds[[testfold]],]

# out1=filter_ig(dstrain1)
# print(head(out1$colpos, n=15))
# print(head(out1$colname, n=15))
# print(head(out1$igvalue, n=15))

# dstrain = dstrain1
# ypos="pos"
# min_count=5
# ig_threshold=10^(-5)




