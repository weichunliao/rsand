

### filter chisquare ####
# filter_chisq = function(dstrain, ypos="pos", min_count=5, chi_threshold = 1e-5) {
#     nugrams = ncol(dstrain) #number of unigram+1
#     chiall = rep(-1, nugrams) #the first column is always -1, and will not be selected.
#     yvec = as.numeric(dstrain[,1]==ypos)
#     options(warn = -1) #silence the warning
#     for(ii in 2:nugrams) {
#         tmp1=cbind(yvec, as.numeric(dstrain[,ii]>0))
#         tmp1a=table(tmp1[,1], tmp1[,2])
        
#         if(nrow(tmp1a)<2 | ncol(tmp1a)<2) {
#             #stop("tmp1a table dimension too small!")
#             chiall[ii] = 0
#         } else if(sum(tmp1[,2])<=min_count) {
#             chiall[ii] = 0
#             #cat("feature", ii, "count too low, skip\n")
#         } else {
#             tmp2=chisq.test(tmp1a, correct=FALSE)
#             chiall[ii] = tmp2$statistic
#         }    
#     }
#     options(warn = 0) #turn the warnings back on
#     o1 = order(chiall, decreasing=TRUE)
    
#     tmpind1 = chiall[o1] > chi_threshold
#     if(sum(tmpind1) ==0) {
#         #cat("We have not features selected. The maximum value of chisq test is ", max(chiall), "\n")
#         return(list(colpos = NULL, colname=NULL, chistat=NULL))
#     } else {
#         o2=o1[tmpind1]
#         retname = names(dstrain)[o2]
#         return(list(colpos = o2, colname=retname, chistat=chiall[o2]))
#     }
# }
#########


rf_carton = function(dsall, folds, testfold, vpos='pos', chi_threshold=0.1, grid_length=20, grid_type='loglinear', rfntree=500, debuglevel=0) {
	library('randomForest')
	# dsall = ds1
	# folds = cvfold
	# testfold = 1
	# vpos='pos'
	# chi_threshold=0.1
	# grid_length=20
	# grid_type='loglinear'
	# rfntree=500
	# deguglevel=0

	test_idx = folds[[testfold]]
	if (testfold == 1) {
		tune_idx = folds[[10]]
	} else {
		tune_idx = folds[[(testfold-1)]]
	}
	test_ds = dsall[test_idx,]
	train_ds = dsall[-test_idx,]
	tune_ds = dsall[tune_idx,]
	sub_train_ds = dsall[-c(test_idx, tune_idx),]

	#### tuning mtry #####
	f_select = filter_chisq(sub_train_ds, ypos=vpos, chi_threshold = chi_threshold)

	m_min=2
	m_max = length(f_select$colpos)
	if (grid_type == 'equal') {
		grids = unique(round(seq(m_min, m_max, length=grid_length )))
	} else if (grid_type == 'loglinear') {
		grids = unique(round(exp(seq(log(m_min), log(m_max), length=grid_length))))
	}

	count_f1 = function(p, r) {
		return (2*p*r/(p+r))
	}

	get_f1 = function(grids_idx) {
		temp = grids[grids_idx]
		output.forest = randomForest(x=sub_train_ds[, f_select$colpos], y=sub_train_ds[,1], xtest=tune_ds[,f_select$colpos], ytest=tune_ds[,1], ntree=rfntree, mtry=temp)
		con_mat = output.forest$test$confusion[,1:2]
		# cat(con_mat,"\n")
		precision = con_mat[vpos, vpos]/sum(con_mat[, vpos])
		recall = con_mat[vpos, vpos]/sum(con_mat[vpos, ])
		f1 = count_f1(precision, recall)
		# cat(f1)

		return (f1)
	}

	# set.seed(5555)
	# fff = vector(mode="numeric", length=0)
	# for (i in 1:length(grids)){
	# 	ff = get_f1(i)
	# 	fff = c(fff,ff)
	# }

	# set.seed(5555)
	f1s = sapply(seq(length(grids)), function(x) get_f1(x))
 
	best_mtry = grids[which.max(f1s)]
	#############
	
	### train ###
	fselect = filter_chisq(train_ds, ypos=vpos, chi_threshold = chi_threshold)

	output.forest = randomForest(x=train_ds[,fselect$colpos], y=train_ds[,1], xtest=test_ds[,fselect$colpos], ytest=test_ds[,1], ntree=rfntree, mtry=best_mtry)
	con_mat = output.forest$test$confusion[,1:2]
	test = list()
	test$precision = con_mat[vpos, vpos]/sum(con_mat[, vpos])
	test$recall = con_mat[vpos, vpos]/sum(con_mat[vpos, ])
	test$f1 = count_f1(test$precision, test$recall)

	#############

	return (list(mgrids=grids, f1_all=f1s , best_m=best_mtry , test=test , fselect=fselect))
}





###########



###########
# h
# set.seed(5555)
# rftest=rf_carton(ds1, cvfold, testfold=1, debuglevel=0)


