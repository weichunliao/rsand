hmm_predict = function(model, allsent, sepchar = " ", addsmooth = 1){
	#addsmooth
	tprior_count = model$tprior_count + addsmooth
	tseq_count = model$tseq_count + addsmooth
	#get prob
	tprior_prob = log(tprior_count/sum(tprior_count))
	tseq_prob = log(tseq_count/sum(tseq_count))
	rownames(tseq_prob) = c('S', 'B', 'M', 'E')
	colnames(tseq_prob) = c('S', 'B', 'M', 'E')

	ct_count_all = model$ct_count + addsmooth
	ct_prob_all = log(ct_count_all/sum(ct_count_all))

	outsent = vector('character')
	outtag = vector('character')
	for (sent in allsent) {
		temp = sent###############
		sen_int = utf8ToInt(temp)
		n_char = length(sen_int)
		if (n_char == 0) {
			outsent = c(outsent, "")
			outtag = c(outtag, "")
			next
		}
		
		ct_prob = ct_prob_all[sen_int,]
	#
		if (n_char == 1) {
			ct_prob = t(as.matrix(ct_prob))
		}
		n = n_char -1
		mu_f_to_z_mat = matrix(c(0), nrow=n_char, ncol=4)
		colnames(mu_f_to_z_mat) = c('S', 'B', 'M', 'E')
		phi_mat = matrix(c(''), nrow=n_char, ncol=4)
		colnames(phi_mat) = c('S', 'B', 'M', 'E')

		tag_type = c('S', 'B', 'M', 'E')
		f = tprior_prob
		# z = 0
		# print(temp)
		for (r_idx in c(1:n_char)) {
			g = ct_prob[r_idx,]
			z = f+g
			z = as.matrix(z)
			rownames(z) = tag_type
			temp_mat = t(sapply(tag_type, function(x) {return(tseq_prob[x, ] + z[x,])} ))
			mu_f_to_z_mat[r_idx,] = apply(temp_mat, 2, function(x) max(x))
			# print(r_idx)
			phi_mat[r_idx,] = apply(temp_mat, 2, function(x) names(which(x==max(x), arr.ind=T))[1])
			# phi_mat[r_idx,] = apply(temp_mat, 2, function(x) names(which(x==max(x))))
			f = as.matrix(mu_f_to_z_mat[r_idx,])
		}

		last_tag = rownames(z)[which.max(as.numeric(z))]
		tag_seq = c(last_tag)
		next_tag = last_tag
		if (n_char > 1) {
			for (r_idx in c(n:1)){
				next_tag = phi_mat[r_idx, next_tag]
				tag_seq = c(next_tag, tag_seq)
			}
		}
		
		char_list = unlist(strsplit(temp, ''))
		cut_str = char_list[1]
		if (n_char > 1) {
			for (char_idx in c(2:n_char)) {
				if ((tag_seq[char_idx] == "M") | (tag_seq[char_idx] == "E")) {
					cut_str = paste(cut_str, char_list[char_idx], sep="", collapse="")
				} else {
					cut_str = paste(cut_str, char_list[char_idx], sep=sepchar, collapse="")
				}	
			}
		}
		outsent = c(outsent, cut_str)
		tag_seq = paste(tag_seq, collapse="")
		outtag = c(outtag, tag_seq)
	}
	return (list(outsent=outsent, outtag=outtag))
}
# out3=hmm_predict(model1, test_sent[851:900])
# ### test

# load('cwsas_train_v2.rdata')
# # model1=hmm_train(train_sent$text2, train_sent$bmes_tag)
# allsent=sample_sent
# sepchar = " "
# addsmooth = 1


# model1=hmm_train(train_sent$text2, train_sent$bmes_tag)
# print(model1$tprior_count)

# print(model1$tseq_count)

# print(model1$ct_count[65290:65300,])
# print(colSums(model1$ct_count))
# model = model1

#
# out1=hmm_predict(model1, sample_sent)
# print(out1)



