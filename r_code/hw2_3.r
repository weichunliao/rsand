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

gen_uagentmat = function(uagentvec, price) {
	N = length(uagentvec)
	# define regex pattern
	regex_pattern = "([A-Za-z][A-Za-z0-9]{1,})"
	# string compare though regex pattern
	uagent_list = regmatches(uagentvec, gregexpr(regex_pattern, uagentvec))
	# keep only unique words in each row.
	uagent_list = lapply(uagent_list, unique)

	uagent_count = table(unlist(uagent_list))
	# target_uagent = c("Mozilla", "Windows", "NT", "AppleWebKit", "KHTML", "like", "Gecko", "Maxthon", "Chrome", "Safari")
	# uagent_count = uagent_count[target_uagent]
	# remove doc freq. lower than lower threshold
	lower_thresh = 10
	uagent_count_low_thresh = uagent_count[uagent_count >= lower_thresh ]
	# remove doc freq. higher than upper threshold
	upper_thresh = N/2
	uagent_count_final = uagent_count_low_thresh[uagent_count_low_thresh <= upper_thresh]
	# check if there still have some feature
	if (length(uagent_count_final) == 0) {
		const = matrix(c(1), nrow = length(price), ncol = 1)
		colnames(const) = c("constant")
		return (const)
	}
	# make dummy variable matrix after feature selection though doc freq.
	dummy_mat = t(sapply(uagent_list, function(x) as.numeric(names(uagent_count_final) %in% x)))
	colnames(dummy_mat) = names(uagent_count_final)
	# count t-value for each utag
	dummy_tvalue = apply(dummy_mat, 2, function(x) reg_tvalue(price, x))
	names(dummy_tvalue) = names(uagent_count_final)
	# order t-value by alphabetical order
	# dummy_tvalue = dummy_tvalue[order(names(dummy_tvalue))]
	dummy_tvalue = dummy_tvalue[order(names(dummy_tvalue), decreasing = T)]

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
	colnames(umat)[2:ncol(umat)] = paste('agent_', c(names(final_feature)),sep='')

	return (umat)
}

###using for test
# load(file='rtb1_train.rdata')
# rtb1_train = rtb1_train[1:1500,]
# y = rtb1_train$paying_price
# umat1 = gen_uagentmat(rtb1_train$user_agent,y)

# print(head(umat1))
# print(head(sort(colSums(umat1), decreasing=TRUE), n=10))
#  #remove linearly independent columns
#  qr1 = qr(umat1, tol =1e-7)
#  ind3 = qr1$pivot[1:qr1$rank]
#  rank0 = ncol(umat1)
#  if(qr1$rank < rank0) {
# 	cat("There are", rank0, "columns, but rank is only", qr1$rank,"\n")
# 	toremove = qr1$pivot[(qr1$rank+1):rank0]

# 	cat("list of features removed", toremove,"\n")

# 	tokeep = qr1$pivot[1:qr1$rank]
# 	umat1 = umat1[,tokeep]
# }
# w = solve(t(umat1) %*% umat1, t(umat1) %*% y)
# print(w)
# ###
# uagentvec = rtb1_train$user_agent
# price = rtb1_train$paying_price





