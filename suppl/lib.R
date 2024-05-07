

calc_stat = function(df, cutoff_total = 10000, cutoff_single = 100, min_country = 0, neighbour = 20, all = FALSE) {

	total_cited = tapply(df[, 3], df[, 1], sum)
	total_citing = tapply(df[, 3], df[, 2], sum)

	N = sum(total_cited)

	df$total_cited = total_cited[df[, 1]]
	df$total_citing = total_citing[df[, 2]]
	df$global_citations = N
	df$p_cited = df[, 3]/total_cited[df[, 1]]
	df$p_citing = df[, 3]/total_citing[df[, 2]]
	df$p_global_cited = total_cited[df[, 1]]/N
	df$p_global_citing = total_citing[df[, 2]]/N

	df$expected = as.numeric(total_cited[df[, 1]])*total_citing[df[, 2]]/N
	df$var_hyper = total_cited[df[, 1]]*(total_citing[df[, 2]]/N)*((N - total_citing[df[, 2]])/N)*((N - total_cited[df[, 1]])/(N-1))
	df$p_hyper = phyper(df[, 3] - 1, total_cited[df[, 1]], N - total_cited[df[, 1]], total_citing[df[, 2]], lower.tail = FALSE)

	df$log2_fc = log2( (df[, 3])/(df$expected) )
	df$z_score = (df$citations - df$expected)/sqrt(df$var_hyper)

	cn = intersect(names(total_cited[total_cited > cutoff_total]), names(total_citing[total_citing > cutoff_total]))

	df = df[df[, 1] %in% cn & df[, 2] %in% cn, , drop = FALSE]

	cn1 = names(total_cited[total_cited > cutoff_total])
	cn2 = names(total_citing[total_citing > cutoff_total])

	df = df[df[, 1] %in% cn1 & df[, 2] %in% cn2, , drop = FALSE]

	df = df[df[, 3] >= cutoff_single, , drop = FALSE]

	tb1 = table(df[, 1])
	exclude1 = names(tb1[tb1 < min_country])
	tb2 = table(df[, 2])
	exclude2 = names(tb2[tb2 < min_country])
	exclude = union(exclude1, exclude2)

	df = df[!(df[, 1] %in% exclude | df[, 2] %in% exclude), , drop = FALSE]

	if(!all) {
		
		international = which(df[, 1] != df[, 2])
		df = df[international, ]

		df = df[order(df[, 3]), ]

	}

	df
}
