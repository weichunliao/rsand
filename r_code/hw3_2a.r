logicreg_l2_train = function (y, xmat, lambda_rate = 0.0005, param_tol = 10^(-5), granditertol = 2, outitermax = 50, inneritermax = 20, debuglevel = 0) {
	N = nrow(xmat)
	nfeature = ncol(xmat)
	lambda = lambda_rate * N
	w = solve( diag(lambda, nfeature) + t(xmat)%*%xmat ) %*% t(xmat) %*% y
	for (i in c(1:outitermax)){
		w_converge = 20

		cj = 0
		for (j in c(1:inneritermax)){
			y_n = t((1+exp( -t(w) %*% t(xmat) ) )^(-1))
			hassian = t(xmat) %*% diag( as.numeric(y_n * (1-y_n)), N) %*% xmat + diag(lambda, nfeature)
			gradient_error = lambda * w + t(xmat) %*% ( y_n - y )
			w_new = w - solve(hassian) %*% gradient_error

			mean_abs_diff = mean(abs(w_new - w))
			w = w_new
			cj  = cj + 1
			if ( mean_abs_diff < param_tol ){
				w_converge = j
				break
			}
		}
		for (k in c(1:inneritermax)){
			y_n = t((1+exp( -t(w) %*% t(xmat) ) )^(-1))
			eigenvaules_A = eigen(t(xmat) %*% diag( as.numeric(y_n * (1-y_n)), N) %*% xmat)$values ####
			gamma = sum(eigenvaules_A /(lambda+eigenvaules_A))
			lambda_new = gamma/(t(w) %*% w)

			mean_abs_diff = mean(abs(lambda_new - lambda))
			lambda = as.numeric(lambda_new)
			if (mean_abs_diff < param_tol) {
				break
			}
		}
		if (cj <= granditertol) {
			break
		}
	}
	y_n = t((1+exp( -t(w) %*% t(xmat) ) )^(-1))
	Sn_inv = diag(lambda, nfeature) + t(xmat) %*% diag( as.numeric(y_n * (1-y_n)), N) %*% xmat
	Sn = solve(Sn_inv)
	w_sd = sqrt(diag( Sn ) )

	ans = list(w=w, w_sd=w_sd, lambda=lambda, M=nfeature, N=N)
	return (ans)
}

