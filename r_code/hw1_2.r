mle_update = function(mu_old, s_old, n_old, x){
	n = n_old +1
	mu = mu_old + (1/n) * (x - mu_old)
	s = (1/n) * (n_old*s_old + as.matrix(x - mu) %*% t(as.matrix(x-mu)) + n_old * as.matrix(mu - mu_old) %*% t(as.matrix(mu-mu_old)))

	result = list(mu = mu, s = s, n = n)
	return (result)
}

