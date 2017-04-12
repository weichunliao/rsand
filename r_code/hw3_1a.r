pgm_train = function(outclass, alldata){

	outclass_summary = function(classdata) {
		mu1 = colMeans(classdata)
		sigma1 = cov(classdata)
		detsig_log = log(det(sigma1))
		N1 = nrow(classdata)

		result = list(mu1=mu1, sigma1=sigma1, prec1=solve(sigma1), detsig_log=detsig_log, N1=N1)
		return (result)
	}

	ans = lapply(alldata, function(x) outclass_summary(x))
	return (ans)

}

# load('phonetrain.rdata')
# train2 = list()
# for(aclass in outclass) {
# 	train2[[aclass]] = traindata[[aclass]][1:500,]
# }
# alldata = train2

# model1=pgm_train(outclass, train2)

