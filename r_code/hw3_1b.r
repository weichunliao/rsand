pgm_predict = function(amodel, testdata) {
	if (ncol(testdata) != 3) {
		return (NULL)
	}
	d_log_mv_nrom = function(x, phone_pos) {
		d = length(x)
		x_mu = as.matrix(x - phone_pos$mu1) #nrow=3 ncol=1
		# cat(nrow(x_mu), "lll")
		# prec = as.matrix(phone_pos$prec1)
		log_likelihood = -0.5*( d*log(2*pi) + phone_pos$detsig_log + (t(x_mu) %*% phone_pos$prec1 %*% x_mu) )
		return (as.numeric(log_likelihood))
	}
	class_prob = t(apply(testdata, 1, function(y) sapply(amodel, function(x)  d_log_mv_nrom(y, x) )))
	prediction = t(apply(class_prob, 1, which.max))

	return(as.numeric(prediction))
}

# ### use for testing data

# load('phonetrain.rdata')
# load('phonetest1.rdata')
# train2 = list()
# for(aclass in outclass) {
# 	train2[[aclass]] = traindata[[aclass]][1:500,]
# }
# pgm_train = function(outclass, alldata){
# 	outclass_summary = function(classdata) {
# 		mu1 = colMeans(classdata)
# 		sigma1 = cov(classdata)
# 		detsig_log = log(det(sigma1))
# 		N1 = nrow(classdata)

# 		result = list(mu1=mu1, sigma1=sigma1, prec1=solve(sigma1), detsig_log=detsig_log, N1=N1)
# 		return (result)
# 	}

# 	ans = lapply(alldata, function(x) outclass_summary(x))
# 	return (ans)
# }
# # model1=pgm_train(outclass, train2)
# model1=pgm_train(outclass, traindata)

# amodel = model1
# testdata = testds1_feature[1:50,]
# #

# #density of multivariante normal dist
# test = t(apply(testdata, 1, function(y) sapply(model1, function(x) dmvnorm(y, x$mu1, x$sigma1))))
# test = t(apply(testdata[1:50,], 1, function(x) sapply(model1, function(y) d_log_mv_nrom(x, y))))

# test2 = t(apply(test, 1, which.max))
# d_log_mv_nrom = function(x, phone_pos) {
# 	d = length(x)
# 	x_mu = as.matrix(x - phone_pos$mu1) #nrow=1 ncol=3
# 	log_likelihood = -0.5*( d*log(2*pi) + phone_pos$detsig_log + (x_mu %*% phone_pos$prec1 %*% t(x_mu)) )
# 	return (as.numeric(log_likelihood))
# }




# pred1=pgm_predict(model1, testds1_feature)

