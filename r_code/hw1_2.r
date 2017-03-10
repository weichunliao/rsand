mle_update = function(mu_old, s_old, n_old, x){
	n = n_old +1
	mu = mu_old + (1/n) * (x - mu_old)
	s = ((n_old/n) * s_old) + (1/n) * as.matrix(x-mu) %*% t(as.matrix(x-mu))

	result = list(mu = mu, s = s, n = n)
	return (result)
}



