lm_evmax = function(y, xmat) {

	N = nrow(xmat)
	M = ncol(xmat)
	lambda = 0.001*N
	w = solve( lambda*diag(M) + t(xmat)%*%xmat ) %*% t(xmat) %*% y
	e0 = y - xmat%*%w
	beta = as.numeric(N/(t(e0)%*%e0))
	alpha = as.numeric(lambda*beta)

	repeat{
		A = alpha*diag(M) + beta*t(xmat)%*%xmat
		mN =  beta*solve(A)%*%t(xmat)%*%y
		eigenvaules_A = eigen(beta*t(xmat)%*%xmat)$values ####

		gamma =	sum(eigenvaules_A /(alpha+eigenvaules_A))

		alpha_new = as.numeric(gamma/(t(mN)%*%mN))
		e1 = y - xmat%*%mN
		beta_new = as.numeric((N-gamma)/(t(e1)%*%e1))

		if( (abs(alpha - alpha_new) + abs(beta - beta_new)) < 10^(-5) ){
			break
		}
		else {
			alpha = alpha_new
			beta = beta_new
		}
	}

	ans = list(mN = mN, mNsd = sqrt(diag(solve(A))), alpha = alpha, beta=beta)
	return (ans)
}


### test if correct
# nfeat=20
# rtb3 = rtb2_train[1:(nfeat+1)]
# y=as.matrix(rtb3[,1])
# xmat = model.matrix(paying_price~., data=rtb3)
# lmev1 = lm_evmax(y, xmat)
# lmev1



