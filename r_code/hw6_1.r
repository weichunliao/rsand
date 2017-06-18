hmm_train = function(sentvec, tagvec){

	count_t_prior = function(sen){
		temp = table(strsplit(sen, ""))
		return (as.matrix(c(temp['S'], temp['B'], temp['M'], temp['E'])))
	}

	all_tag = paste(tagvec, collapse="")
	tprior_count = count_t_prior(all_tag)

	count_t_seq = function(sen) {
		pattern_list = c('SS','SB','SM','SE','BS','BB','BM','BE','MS','MB','MM','ME','ES','EB','EM','EE')
		count_pattern_freq = function(sen, search){
			res = sum(sapply(1:(nchar(sen)-nchar(search)+1),function(i){substr(sen,i,i+(nchar(search)-1))==search}))
			return (res)
		}
		result = sapply(pattern_list, function(x) count_pattern_freq(sen,x))
		# print(sen)
		# print(result)
		return (result)
	}

	# tseq_count_vec = t(sapply(tagvec, function(x) count_t_seq(x)))
	tseq_count_vec = rowSums(sapply(tagvec, function(x) count_t_seq(x)))
	tseq_count = matrix(tseq_count_vec, nrow=4, ncol=4, byrow=TRUE)
	# rownames(tseq_count) = c('S', 'B', 'M', 'E')
	# colnames(tseq_count) = c('S', 'B', 'M', 'E')

	
	all_char_int = utf8ToInt(paste(sentvec, collapse=""))
	all_tag_list = unlist(strsplit(all_tag, ""))
	all_tag_int = rep(0, length(all_tag_list))

	all_tag_int[which(all_tag_list == 'S', arr.ind=TRUE)] = 1
	all_tag_int[which(all_tag_list == 'B', arr.ind=TRUE)] = 2
	all_tag_int[which(all_tag_list == 'M', arr.ind=TRUE)] = 3
	all_tag_int[which(all_tag_list == 'E', arr.ind=TRUE)] = 4
	ct_count_idx = cbind(all_char_int, all_tag_int)

	ct_count = matrix(c(0), nrow=70000, ncol=4)
	colnames(ct_count) = c('S', 'B', 'M', 'E')
	for (row in c(1:nrow(ct_count_idx))){
		r_idx = ct_count_idx[row, 1]
		c_idx = ct_count_idx[row, 2]
		ct_count[r_idx, c_idx] = ct_count[r_idx, c_idx] + 1
	}

	return (list(ct_count=ct_count , tseq_count=tseq_count , tprior_count=tprior_count))
}


#########
# load('cwsas_train_v2.rdata')
# sentvec = train_sent$text2[1:100]
# tagvec = train_sent$bmes_tag[1:100]

# ################# use for smaple one

# model_s1=hmm_train(train_sent$text2[1:100], train_sent$bmes_tag[1:100])
# print(model_s1$tprior_count)
# print(model_s1$tseq_count)
# print(model_s1$ct_count[65290:65300,])
# print(colSums(model_s1$ct_count))

##################

