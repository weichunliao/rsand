# this function is used for counting t-value
reg_tvalue = function(y, x) {
	y = matrix(y, ncol = 1)
	xmat = as.matrix(cbind(c(1), x))
	bhead = solve(t(xmat) %*% xmat, t(xmat) %*% y)
	yhead = xmat %*% bhead
	e1 = y - yhead
	var1 = sum(e1 * e1) / (length(e1) - 2)
	sigma2 = solve(t(xmat) %*% xmat) * var1
	t1 = bhead[2]/sqrt(sigma2[2,2])

	return (t1)
}
gen_utagmat = function(utagvec = NULL, price) {
	utag_count = sort(table(unlist(strsplit(utagvec, ','))), decreasing = T)

	threshold = 5
	utag_low_thresh_idx = utag_count < threshold
	# remove utag freq. below threshold
	utag_count_low_thresh = utag_count[!utag_low_thresh_idx]
	# check if there still remain some utag 
	if (length(utag_count_low_thresh) == 0) {
		const = matrix(c(1), nrow = length(price), ncol = 1)
		colnames(const) = c("constant")
		return (const)
	}
	# make dummy variable matrix after feature selection though utag freq.
	dummy_mat = t(sapply(strsplit(utagvec, ','), function(x) as.numeric(names(utag_count_low_thresh) %in% x)))
	colnames(dummy_mat) = names(utag_count_low_thresh)
	# count t-value for each utag
	dummy_tvalue = apply(dummy_mat, 2, function(x) reg_tvalue(price, x))
	names(dummy_tvalue) = names(utag_count_low_thresh)

	# sort t-value for each utag
	ordered_t_idx = order(abs(dummy_tvalue), decreasing = T)
	ordered_dummy_tvalue = dummy_tvalue[ordered_t_idx]
	tvalue_threshold = 1
	tvalue_thresh_idx = abs(ordered_dummy_tvalue) >= tvalue_threshold
	# remove utag tvalue below threshold
	final_feature = ordered_dummy_tvalue[tvalue_thresh_idx]
	# check if there still remain some utag
	if (length(final_feature) == 0) {
		const = matrix(c(1), nrow = length(price), ncol = 1)
		colnames(const) = c("constant")
		return (const)
	}

	# select the dummy variable after feature selection though t-value and add constant col 
	umat = cbind(constant = c(1), dummy_mat[,c(names(final_feature))])
	# sort the umat by the abs(t-value)
	colnames(umat)[2:ncol(umat)] = paste('user_', c(names(final_feature)),sep='')

	return (umat)
}
### using for test
load(file='rtb1_train.rdata')

rtb1_train = rtb1_train[1:300,]
umat1 = gen_utagmat(rtb1_train$user_tags, rtb1_train$paying_price)
head(umat1)
y = rtb1_train$paying_price
w = solve(t(umat1) %*% umat1, t(umat1) %*% y)
###

