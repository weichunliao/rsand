

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


rf_carton = function(dsall, folds, testfold, vpos='pos', chi_threshold=0.1, grid_length=20, grid_type='loglinear', rfntree=500, deguglevel=0) {
	library('randomForest')
	dsall = ds1
	folds = cvfold
	testfold = 1
	vpos='pos'
	chi_threshold=0.1
	grid_length=20
	grid_type='loglinear'
	rfntree=500
	deguglevel=0

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

	feature_select = filter_chisq(sub_train_ds, ypos=vpos, chi_threshold = chi_threshold)

	m_min=2
	m_max = length(feature_select$colpos)
	if (grid_type == 'equal') {
		grids = unique(round(seq(m_min, m_max, length=grid_length )))
	} else if (grid_type == 'loglinear') {
		grids = unique(round(exp(seq(log(m_min), log(m_max), length=grid_length))))
	}


	return (list(mgrids=, f1_all= , best_m= , test= , fselect=))
}

randomForest
output.forest = randomForest(x=sub_train_ds[,-1],y=sub_train_ds[,1],xtest=tune_ds[,-1],ytest=[,], ntree=2, mtry=2)



###########



###########
load('hw5ds1.rdata')
set.seed(5555)
rftest=rf_carton(ds1, cvfold, testfold=1, debuglevel=0)


