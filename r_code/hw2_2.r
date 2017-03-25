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
	utag_thresh5_idx = utag_count < threshold
	utag_count_thresh5 = utag_count[!utag_thresh5_idx]
	if (length(utag_count_thresh5) == 0) {
		const = matrix(c(1), nrow = length(price), ncol = 1)
		colnames(const) = c("constant")
		return (const)
	}
	dummy_mat = t(sapply(strsplit(utagvec, ','), function(x) as.numeric(names(utag_count_thresh5) %in% x)))
	colnames(dummy_mat) = names(utag_count_thresh5)
	dummy_tvalue = apply(dummy_mat, 2, function(x) reg_tvalue(price, x))
	names(dummy_tvalue) = names(utag_count_thresh5)

	ordered_t_idx = order(abs(dummy_tvalue), decreasing = T)
	ordered_dummy_tvalue = dummy_tvalue[ordered_t_idx]
	tvalue_threshold = 1
	tvalue_thresh1_idx = abs(ordered_dummy_tvalue) > tvalue_threshold
	final_feature = ordered_dummy_tvalue[tvalue_thresh1_idx]
	if (length(final_feature) == 0) {
		const = matrix(c(1), nrow = length(price), ncol = 1)
		colnames(const) = c("constant")
		return (const)
	}

	umat = dummy_mat[,c(names(final_feature))]
	colnames(umat) = paste('user_', colnames(umat),sep='')
	final_umat = cbind(constant = c(1), umat)

	return (final_umat)
}
### using for test
# load(file='rtb1_train.rdata')

# rtb1_train = rtb1_train[1:300,]
# umat1 = gen_utagmat(rtb1_train$user_tags, rtb1_train$paying_price)
# head(umat1)
# y = rtb1_train$paying_price
# w = solve(t(umat1) %*% umat1, t(umat1) %*% y)
###

