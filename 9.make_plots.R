

setwd("~/project/analysis/citation_analysis")


library(ggplot2)
library(ggrepel)
library(GetoptLong)
library(circlize)
library(ComplexHeatmap)
library(cowplot)
library(impute)

country_meta = read.table("processed/country_meta.tab", sep = "\t", quote = "", header = TRUE)
country_meta = country_meta[!duplicated(country_meta$standardName), ]
country_meta$subregion2 = ifelse(grepl(", ", country_meta$subregion), gsub("^.*, ", "", country_meta$subregion), NA)
country_meta$subregion = gsub(", .*$", "", country_meta$subregion)
rownames(country_meta) = country_meta$standardName
country_meta = country_meta[, c("officialName", "region", "subregion", "subregion2")]
head(country_meta)

##### Figure 3 #####
df = read.table("processed/num_cite_country_country.tab", quote = "", sep = "\t", header = TRUE)
n_cited = tapply(df[, 3], df[, 1], sum)
n_citing = tapply(df[, 3], df[, 2], sum)


# being cited
num = tapply(seq_len(nrow(df)), df[, 1], function(ind) {
    df2 = df[ind, , drop = FALSE]
    c(domestic = sum(df2[df2[, 1] == df2[, 2], 3]), total = sum(df2[, 3]))
})
num = do.call(rbind, num)
num = as.data.frame(num)
num$international = num$total - num$domestic
num$p_international = num$international/num$total
num$p_domestic = num$domestic/num$total
num = num[num$total > 10000, ]
num1 = num

# citing
num = tapply(seq_len(nrow(df)), df[, 2], function(ind) {
    df2 = df[ind, , drop = FALSE]
    c(domestic = sum(df2[df2[, 1] == df2[, 2], 3]), total = sum(df2[, 3]))
})
num$total = c(international = sum(sapply(num, function(x) x[1])), total = sum(sapply(num, function(x) x[2])))
num = do.call(rbind, num)
num = as.data.frame(num)
num$international = num$total - num$domestic
num$p_international = num$international/num$total
num$p_domestic = num$domestic/num$total
num = num[num$total > 10000, ]
num2 = num

cn = intersect(rownames(num1), rownames(num2))
num1 = num1[cn, ]
num2 = num2[cn, ]

tb = data.frame(
    country = cn,
    region = country_meta[cn, "region"],
    total_cited = num1$total,
    total_citing = num2$total,
    p_international_cited = num1$p_international,
    p_international_citing = num2$p_international
)

l = (tb$p_international_cited > 0.6 & tb$p_international_citing > 0.6)
p1 = ggplot(tb, aes(x = p_international_citing, y = p_international_cited, color = region)) + geom_point() +
    xlim(0.3, 1) + ylim(0.3, 1) + geom_abline(slope = 1, intercept = 0, col = "grey", linetype = 2) +
    labs(x = "Fraction of international references", y = "Fraction of international citations", color = "Region") +
    geom_text_repel(data = tb[!l, ], aes(label = country), max.overlaps = Inf, size = 3, show.legend = FALSE) +
    ggtitle("A")
p2 = ggplot(tb, aes(x = (total_citing+total_cited)/2, y = log2(p_international_cited/p_international_citing), color = region)) + geom_point() +
	labs(x = "Average of country's total numbers of citations and references", y = "Overall international net influence, log2(p_cited/p_citing)") +
	scale_x_log10(breaks = c(1e4, 1e5, 1e6, 1e7), labels = c("10K", "100K", "1M", "10M")) + geom_hline(yintercept = 0, lty = 2, color = "grey") + 
	geom_text_repel(data = tb[!l | tb$p_international_cited/tb$p_international_citing > 1, ], aes(label = country), max.overlaps = Inf, size = 3, show.legend = FALSE) +
	ggtitle("B")

p3 = ggplot(tb, aes(x = total_cited, y = p_international_cited, color = region)) + geom_point() + 
	labs(x = "Total number of citations", y = "Fraction of international citations") +
	scale_x_log10(breaks = c(1e4, 1e5, 1e6, 1e7), labels = c("10K", "100K", "1M", "10M")) + ylim(0.3, 1) + geom_smooth(data = tb[-c(11, 20, 80), ], aes(color = NULL), color = "black", linewidth = 0.5) +
	geom_text_repel(data = tb[!l, ], aes(label = country), max.overlaps = Inf, size = 3, show.legend = FALSE) +
	ggtitle("C")
p4 = ggplot(tb, aes(x = total_citing, y = p_international_citing, color = region)) + geom_point() +
	labs(x = "Total number of references", y = "Fraction of international references") +
	scale_x_log10(breaks = c(1e4, 1e5, 1e6, 1e7), labels = c("10K", "100K", "1M", "10M")) + ylim(0.3, 1) + geom_smooth(data = tb[-c(11, 80), ], aes(color = NULL), color = "black", linewidth = 0.5) +
	geom_text_repel(data = tb[!l, ], aes(label = country), max.overlaps = Inf, size = 3, show.legend = FALSE) +
	ggtitle("D")

p1 = p1 + theme(legend.position = c(0.02, 0.98), legend.justification = c("left", "top"))
p2 = p2 + theme(legend.position="none")
p3 = p3 + theme(legend.position="none")
p4 = p4 + theme(legend.position="none")


pdf("figures/figure3.pdf", width = 10, height = 10)
print(plot_grid(p1, p2, p3, p4, nrow = 2))
dev.off()



###### figure 4 ######

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

		citation_unique = sort(unique(df[, 3]))

		df$median = NA
		df$mad = NA
		df$mean = NA
		df$sd = NA
		for(v in citation_unique) {
			l = df$citation == v
			ind = which(l)
			if(sum(l) > 25) {
				df$median[l] = median(df$log2_fc[ind])
				df$mad[l] = mad(df$log2_fc[ind])
				df$mean[l] = mean(df$log2_fc[ind])
				df$sd[l] = sd(df$log2_fc[ind])
			} else {
				ind = which(l)
				if(length(ind) == 0) next
				ind = c(seq(ind[1] - round(neighbour/2), ind[1] - 1), ind)
				ind = ind[ind > 0]
				if(ind[length(ind)] < nrow(df)) {
					ind = c(ind, seq(ind[length(ind)]+1, min(nrow(df), ind[length(ind)]+round(neighbour/2))))
				}
				df$median[l] = median(df$log2_fc[ind])
				df$mad[l] = mad(df$log2_fc[ind])
				df$mean[l] = mean(df$log2_fc[ind])
				df$sd[l] = sd(df$log2_fc[ind])
			}
		}

		df$log2_fc_scaled = (df$log2_fc - df$mean)/df$sd
	}

	df
}

df = read.table("processed/num_cite_country_country.tab", quote = "", sep = "\t", header = TRUE)
df = calc_stat(df, cutoff_single = 100, all = TRUE)
df2 = calc_stat(df, cutoff_single = 100, all = FALSE)
write.csv(df, file = "figures/df_fold_enrichment.csv", row.names =FALSE)

df$cate = ifelse(df$country_cited == df$country_citing, "Domestic", ifelse(country_meta[df$country_cited, "region"]=="Africa" & country_meta[df$country_citing, "region"]== "Africa", "International\n  within Africa", "International"))
df$cate = factor(df$cate, levels = c("Domestic", "International\n  within Africa", "International"))
p1 = ggplot(df, aes(x = citations, y = log2_fc, col = cate)) + geom_point(size = 0.5) +
    scale_color_discrete() +
    scale_x_log10(breaks = c(1e2, 1e3, 1e4, 1e5, 1e6, 1e7), labels = c("100", "1K", "10K", "100K", "1M", "10M")) + labs(x = "Number of citations", y = "Enrichment scores") +
    labs(col = "Citation type") + 
    ggtitle("A")

rk1 = sapply(split(df, df$country_cited), function(x) {
	i = which(x$country_cited == x$country_citing)
	rank(-x$p_cited)[i]
})

rk2 = sapply(split(df, df$country_citing), function(x) {
	i = which(x$country_cited == x$country_citing)
	rank(-x$p_citing)[i]
})

df2 = df[df$country_cited == df$country_citing, ]
df2$rank_cited = rk1
df2$rank_citing = rk2

p2  = ggplot(df2, aes(x = citations, y = (rank_cited + rank_citing)/2 + runif(82, min = -0.05, max = 0.05))) + 
	geom_point() +
    scale_x_log10(breaks = c(1e3, 1e4, 1e5, 1e6, 1e7), labels = c("1K", "10K", "100K", "1M", "10M")) + ylim(1, 7) +
    labs(x = "Number of citations", y = "Rankings of the domestic citations from the fraction-based metrics") +
    ggtitle("B")

p1 = p1 + theme(legend.position = c(0.98, 0.98), legend.justification = c("right", "top"))

pdf("figures/figure4.pdf", width = 10, height = 5)
print(plot_grid(p1, p2, nrow = 1))
dev.off()




##### figure 5 ######
df = read.table("processed/num_cite_country_country.tab", quote = "", sep = "\t", header = TRUE)
df = calc_stat(df, cutoff_total = 10000, cutoff_single = 100, min_country = 25, all = FALSE)


m = xtabs(log2_fc ~ country_cited + country_citing, data = df)
class(m) = "matrix"

all_countries = rownames(m)

m2 = m
m2[m2 < 0] = 0
library(igraph)

set.seed(123)
g = graph_from_adjacency_matrix(m2, mode = "plus", weighted = TRUE)
cm = cluster_louvain(g, weight = E(g)$weight, resolution = 1.2)
communities(cm)
mem = membership(cm)

mem_name = mem
mem_name[mem == 1] = "Latin-America"
mem_name[mem == 2] = "West-1"
mem_name[mem == 3] = "Asia-2"
mem_name[mem == 4] = "Europe-3"
mem_name[mem == 5] = "Mid-East"
mem_name[mem == 6] = "Africa"
mem_name[mem == 7] = "US"

mem_name["Hong Kong"] = "Asia-1"
mem_name["Singapore"] = "Asia-1"
mem_name["Taiwan"] = "Asia-1"
mem_name["South Korea"] = "Asia-1"
mem_name["China"] = "Asia-1"
mem_name["Japan"] = "Asia-1"

mem_name["Spain"] = "Europe-2"
mem_name["Italy"] = "Europe-2"

split(names(mem_name), mem_name)

region_color = structure(RColorBrewer::brewer.pal(length(unique(country_meta$region)), "Set3"), names = unique(country_meta$region))
mem_color = structure(RColorBrewer::brewer.pal(length(setdiff(unique(mem_name), NA)), "Set1"), names = setdiff(unique(mem_name), NA))


robust_dist_pair = function(x, y, trim = 0.1) {
	x_mid = mean(x, na.rm = TRUE)
	y_mid = mean(y, na.rm = TRUE)

	dd = sqrt( (x-x_mid)^2 + (y-y_mid)^2 )

	q = quantile(dd, c(trim/2, 1-trim/2), na.rm = TRUE)
	l = dd >= q[1] & dd <= q[2]
	l[is.na(l)] = FALSE
	sqrt( sum( (x[l] - y[l])^2 ) )/sum(l)
}

robust_dist = function(m, trim = 0.1) {
	n = nrow(m)
	d = matrix(0, nrow = n, ncol = n)
	rownames(d) = colnames(d) = rownames(m)

	for(i in 1:(n-1)) {
		for(j in (i+1):n) {
			d[i, j] = d[j, i] = robust_dist_pair(m[i, ], m[j, ], trim = trim)
		}
	}

	as.dist(d)
}


row_slice_hc = hclust(as.dist(1-cor(do.call(cbind, tapply(1:nrow(m), mem[rownames(m)], function(ind) {
	colMeans(m[ind, , drop = FALSE], na.rm = TRUE)
})))))
col_slice_hc = hclust(as.dist(1-cor(do.call(cbind, tapply(1:ncol(m), mem[colnames(m)], function(ind) {
	colMeans(m[ind, , drop = FALSE], na.rm = TRUE)
})))))

m_with_NA = m
m_with_NA[m == 0] = NA

row_order = order.dendrogram(reorder(as.dendrogram(hclust(robust_dist(m_with_NA))), -rowMeans(m_with_NA, na.rm = TRUE)))
col_order = order.dendrogram(reorder(as.dendrogram(hclust(robust_dist(t(m_with_NA)))), -colMeans(m_with_NA, na.rm = TRUE)))

pdf("figures/figure5.pdf", width = 10, height = 10)
ht = Heatmap(m, name = "Enrichment score", #cluster_rows = hc, cluster_columns = hc,
	row_title = "Cited/influencing countries",
	column_title = "Citing/influenced countries", column_title_side = "bottom",
	col = colorRamp2(c(-2, 0, 2, 4), c("blue", "white", "orange", "red")),
	row_order = row_order, column_order = col_order, row_names_side = "left",
	row_names_gp = gpar(fontsize = 8),
	column_names_gp = gpar(fontsize = 8),
	layer_fun = function(j, i, x, y, w, h, fill) {
		l = pindex(m, i, j) == 0
		if(any(l)) grid.rect(x[l], y[l], w[l], h[l], gp = gpar(fill = "#DDDDDD", col = "#DDDDDD"))
	},
	right_annotation = rowAnnotation(Region = country_meta[rownames(m), "region"], 
									Group = mem_name[rownames(m)],
		                             "#cited" = anno_barplot(sqrt(as.vector(n_cited[rownames(m)])), ylim = c(0, 8000), axis_param = list(at = c(0,2000,4000,6000,8000), labels = c("0", "4M", "16M", "36M", "64M"))),
		                             col = list(Region = region_color, Group = mem_color)),
	top_annotation = HeatmapAnnotation(Region = country_meta[colnames(m), "region"], 
										Group = mem_name[colnames(m)],
		                               "#citing" = anno_barplot(sqrt(as.vector(n_citing[colnames(m)])), ylim = c(0, 8000), axis_param = list(at = c(0,2000,4000,6000,8000), labels = c("0", "4M", "16M", "36M", "64M"))),
		                               col = list(Region = region_color, Group = mem_color)),
	cluster_row_slices = FALSE, cluster_column_slices = FALSE,
	row_split = factor(as.character(mem[rownames(m)]), levels = row_slice_hc$labels[row_slice_hc$order]),
	column_split = factor(as.character(mem[colnames(m)]), levels = col_slice_hc$labels[col_slice_hc$order]),
	border = "black"

)
draw(ht, merge_legend = TRUE, column_title = "Enrichment of citations between countries",
	heatmap_legend_list = list(Legend(at = "Not available", legend_gp = gpar(fill = "#DDDDDD"))))
dev.off()



#####  reduce to smaller groups
mem_name2 = mem_name

n = length(setdiff(unique(mem_name2), NA))
sm = matrix(nrow = n, ncol = n)
rownames(sm) = colnames(sm) = setdiff(unique(mem_name2), NA)
for(x in rownames(sm)) {
	for(y in colnames(sm)) {
		l1 = mem_name2 == x; l1[is.na(l1)] = FALSE
		l2 = mem_name2 == y; l2[is.na(l2)] = FALSE
		sm[x, y] = mean(m_with_NA[l1, l2], na.rm = TRUE)
	}
}

diag(sm) = 0


Heatmap(sm)

sm2 = sm
sm2[sm2 < 0.15] = 0
sm2

for(i in 1:(nrow(sm2)-1)) {
	for(j in (i+1):nrow(sm2)) {
		if(sm2[i, j] > 0.1 & sm2[j, i] > 0.1) {
			cat("draw_link(\"", rownames(sm2)[i], "\", \"", colnames(sm2)[j], "\", ends = \"both\", angle = 15, col =\"red\")\n", sep = "")
		} else if(sm2[i, j] > 0.1) {
			cat("draw_link(\"", rownames(sm2)[i], "\", \"", colnames(sm2)[j], "\", ends = \"last\", angle = 15, col =\"red\")\n", sep = "")
		} else if(sm2[j, i] > 0.1) {
			cat("draw_link(\"", rownames(sm2)[j], "\", \"", colnames(sm2)[i], "\", ends = \"last\", angle = 15, col =\"red\")\n", sep = "")
		}
	}
}

sm2 = sm
sm2[sm2 > -0.15] = 0
sm2 = sm2[-c(2,9), -c(2,9)]
sm2 = abs(sm2)

for(i in 1:(nrow(sm2)-1)) {
	for(j in (i+1):nrow(sm2)) {
		if(sm2[i, j] > 0.1 & sm2[j, i] > 0.1) {
			cat("draw_link(\"", rownames(sm2)[i], "\", \"", colnames(sm2)[j], "\", ends = \"both\", angle = 90)\n", sep = "")
		} else if(sm2[i, j] > 0.1) {
			cat("draw_link(\"", rownames(sm2)[i], "\", \"", colnames(sm2)[j], "\", ends = \"last\", angle = 90)\n", sep = "")
		} else if(sm2[j, i] > 0.1) {
			cat("draw_link(\"", rownames(sm2)[j], "\", \"", colnames(sm2)[i], "\", ends = \"last\", angle = 90)\n", sep = "")
		}
	}
}



theta = seq(0, 360, length = 8)[-8] - 360/14
names(theta) = c("Asia-1", "Europe-2", "Europe-3", "Latin-America", "Africa", "Mid-East", "Asia-2")
loc =  circlize:::polar2Cartesian(cbind(theta, 1))

draw_link = function(x1, x2, ends = "both", angle = 90, col = "darkgreen") {

	theta1 = theta[x1]
	theta2 = theta[x2]

	loc = circlize:::polar2Cartesian(cbind(c(theta1, theta2), 1))
	
	len = sqrt( (loc[1,1] - loc[2,1])^2 + (loc[1,2]-loc[2,2])^2 )
	offset = convertWidth(unit(6, "mm"), "native", valueOnly = TRUE)

	loc2 = loc
	loc2[1, 1] = loc[1, 1] + offset/len*abs(loc[1,1] - loc[2,1])*ifelse(loc[1, 1] < loc[2, 1], 1, -1)
	loc2[2, 1] = loc[2, 1] - offset/len*abs(loc[1,1] - loc[2,1])*ifelse(loc[1, 1] < loc[2, 1], 1, -1)
	
	loc2[1, 2] = loc[1, 2] + offset/len*abs(loc[1,2] - loc[2,2])*ifelse(loc[1, 2] < loc[2, 2], 1, -1)
	loc2[2, 2] = loc[2, 2] - offset/len*abs(loc[1,2] - loc[2,2])*ifelse(loc[1, 2] < loc[2, 2], 1, -1)

	grid.lines(loc2[, 1], loc2[, 2], default.units = "native", 
		arrow = arrow(ends = "last", angle = angle, length = unit(scale_segment(sm[x1, x2])*ifelse(angle == 90, 0.5, 1), "mm")), 
		gp = gpar(col = col))
	if(ends == "both") {
		grid.lines(loc2[2:1, 1], loc2[2:1, 2], default.units = "native", 
		arrow = arrow(ends = "last", angle = angle, length = unit(scale_segment(sm[x2, x1])*ifelse(angle == 90, 0.5, 1), "mm")), 
		gp = gpar(col = col))
	}
}

scale_segment = function(x, min = 1, max = 5) {
	(abs(x) - 0.15)/(1.6-0.15)*(max-min) + min
}

plot_hc = function(hc, ann = TRUE, fontsize = 8) {
    x = order(hc$order)
    nobs = length(x)

    if(length(hc$labels) == 0) {
        hc$labels = as.character(seq_along(hc$order))
    }

    labels

    hang = max(hc$height)*0.05
    
    pushViewport(viewport(xscale = c(0.5, nobs + 0.5), yscale = c(-hang, max(hc$height)*1.1)))

    merge = hc$merge
    order = hc$order
    nr = nrow(merge)
    midpoint = numeric(nr)

    for(i in seq_len(nr)) {
        child1 = merge[i, 1]
        child2 = merge[i, 2]

        end = max(hc$height[i] - hang, 0)

        if(child1 < 0 && child2 < 0) { # both are leaves
            grid.segments(x[ -child1 ], 
                     hc$height[i],
                     x[ -child2 ], 
                     hc$height[i], default.units = "native")
            midpoint[i] = (x[ -child1 ] + x[ -child2 ])/2
 
            grid.segments(x[ -child1 ], hc$height[i], x[ -child1 ], end, default.units = "native")
            grid.text(hc$labels[-child1], x[ -child1 ], end - hang*0.4, default.units = "native", rot = 90, just = "right", gp = gpar(fontsize = fontsize))
            grid.segments(x[ -child2 ], hc$height[i], x[ -child2 ], end, default.units = "native")
            grid.text(hc$labels[-child2], x[ -child2 ], end - hang*0.4, default.units = "native", rot = 90, just = "right", gp = gpar(fontsize = fontsize))
        } else if(child1 < 0 && child2 > 0) {
            grid.segments(x[ -child1 ], 
                     hc$height[i],
                     midpoint[ child2 ], 
                     hc$height[i], default.units = "native")
            midpoint[i] = (x[ -child1 ] + midpoint[ child2 ])/2
            grid.segments(x[ -child1 ], hc$height[i], x[ -child1 ], end, default.units = "native")
            grid.text(hc$labels[-child1], x[ -child1 ], end - hang*0.4, default.units = "native", rot = 90, just = "right", gp = gpar(fontsize = fontsize))
            grid.segments(midpoint[ child2 ], hc$height[i], midpoint[ child2 ], hc$height[ child2 ], default.units = "native")
        } else if(merge[i, 1] > 0 && merge[i, 2] < 0) {
            grid.segments(midpoint[ child1 ], 
                     hc$height[i],
                     x[ -child2 ], 
                     hc$height[i], default.units = "native")
            midpoint[i] = (midpoint[ child1 ] + x[ -child2 ])/2
            grid.segments(midpoint[ child1 ], hc$height[i], midpoint[ child1 ], hc$height[ child1 ], default.units = "native")
            grid.segments(x[ -child2 ], hc$height[i], x[ -child2 ], end, default.units = "native")
            grid.text(hc$labels[-child2], x[ -child2 ], end - hang*0.4, default.units = "native", rot = 90, just = "right", gp = gpar(fontsize = fontsize))
        } else {
            grid.segments(midpoint[ child1 ], 
                     hc$height[i],
                     midpoint[ child2 ], 
                     hc$height[i], default.units = "native")
            midpoint[i] = (midpoint[ child1 ] + midpoint[ child2 ])/2
            grid.segments(midpoint[ child1 ], hc$height[i], midpoint[ child1 ], hc$height[ child1 ], default.units = "native")
            grid.segments(midpoint[ child2 ], hc$height[i], midpoint[ child2 ], hc$height[ child2 ], default.units = "native")
        }
    }
    if(ann) {
	    grid.annotation_axis(side = "left", gp = gpar(fontsize = 8))
	    grid.text("2 - enrichment score", x = unit(-6, "mm"), y = 0.5, rot = 90, gp = gpar(fontsize = fontsize+2), just = "bottom")
	}
    popViewport()
}

d = matrix(0, nrow = 9, ncol = 9)
rownames(d) = colnames(d) = rownames(sm)
for(i in 1:9) {
	for(j in 1:9) {
		d[j, i] = d[i, j] = 2 - sign(sm[i, j] + sm[j, i])*max(abs(c(sm[i, j], sm[j, i])))
	}
}
grid.newpage()
plot_hc(hclust(as.dist(d), method = "ward.D2"))


pdf("figures/figure6.pdf", width = 14, height = 3.5433)
grid.newpage()
pushViewport(viewport(width = unit(22, "cm"), height = unit(9, "cm")))

pushViewport(viewport(x = unit(5, "mm"), just = "left", width = unit(8, "cm"), height = unit(8, "cm"), xscale = c(-1.4, 1.4), yscale = c(-1.4, 1.4)))
grid.rect(gp = gpar(col = "black"))
grid.circle(x = loc[, 1], y = loc[, 2], default.units = "native", r = unit(5, "mm"))
grid.text(x = loc[, 1], y = loc[, 2], label = names(theta), default.units = "native", gp = gpar(fontsize = 8))

draw_link("Latin-America", "Asia-1", ends = "both", angle = 90)
draw_link("Asia-2", "Europe-2", ends = "last", angle = 90)
draw_link("Europe-3", "Asia-1", ends = "last", angle = 90)
draw_link("Asia-1", "Africa", ends = "both", angle = 90)
draw_link("Asia-1", "Europe-2", ends = "both", angle = 90)
draw_link("Africa", "Europe-2", ends = "both", angle = 90)

grid.text("World 2", x = unit(2, "mm"), y = unit(1, "npc") - unit(2, "mm"), just = c("left", "top"), gp = gpar(fontsize = 10))

popViewport()


pushViewport(viewport(x = unit(8+2, "cm"), height = unit(4, "cm"), just = "left", width = unit(2, "cm")))
grid.rect(gp = gpar(col = "black"))
grid.circle(x = c(0.5, 0.5), y = unit(0.5, "npc") + unit(c(1, -1), "cm"), r = unit(5, "mm"))
grid.text(x = c(0.5, 0.5), y = unit(0.5, "npc") + unit(c(1, -1), "cm"), label = c("West-1", "US"), gp = gpar(fontsize = 8))
grid.lines(unit(c(0.5, 0.5), "npc"), unit(0.5, "npc")+unit(c(0.4, -0.4), "cm"), arrow = arrow(ends = "last", angle = 90, length = unit(0.5*scale_segment(sm["West-1", "US"]), "mm")), gp = gpar(col = "darkgreen"))
grid.lines(unit(c(0.5, 0.5), "npc"), unit(0.5, "npc")-unit(c(0.4, -0.4), "cm"), arrow = arrow(ends = "last", angle = 90, length = unit(0.5*scale_segment(sm["US", "West-1"]), "mm")), gp = gpar(col = "darkgreen"))

grid.text("World 1", x = unit(0.5, "npc"), y = unit(1, "npc") + unit(2, "mm"), just = c("bottom"), gp = gpar(fontsize = 10))

popViewport()

grid.lines(unit(c(8.6, 9.9), "cm"), unit(c(0.5, 0.5), "npc"), arrow = arrow(ends = "last", angle = 90, length = unit(0.5*scale_segment(mean(sm[-c(2,9), c(2,9)])), "mm")), gp = gpar(col = "darkgreen"))
grid.lines(unit(rev(c(8.6, 9.9)), "cm"), unit(c(0.5, 0.5), "npc"), arrow = arrow(ends = "last", angle = 90, length = unit(0.5*scale_segment(mean(sm[c(2,9), -c(2,9)])), "mm")), gp = gpar(col = "darkgreen"))

pushViewport(viewport(x = unit(8+2+2+1.5, "cm"), just = "left", width = unit(8, "cm"), height = unit(8, "cm"), xscale = c(-1.4, 1.4), yscale = c(-1.4, 1.4)))
grid.circle(x = loc[, 1], y = loc[, 2], default.units = "native", r = unit(5, "mm"))
grid.text(x = loc[, 1], y = loc[, 2], label = names(theta), default.units = "native", gp = gpar(fontsize = 8))

draw_link("Latin-America", "Asia-2", ends = "both", angle = 15, col ="red")
draw_link("Latin-America", "Europe-3", ends = "both", angle = 15, col ="red")
draw_link("Latin-America", "Mid-East", ends = "both", angle = 15, col ="red")
draw_link("Latin-America", "Africa", ends = "both", angle = 15, col ="red")
draw_link("Europe-2", "Latin-America", ends = "last", angle = 15, col ="red")
draw_link("Asia-2", "Europe-3", ends = "both", angle = 15, col ="red")
draw_link("Asia-1", "Asia-2", ends = "last", angle = 15, col ="red")
draw_link("Asia-2", "Mid-East", ends = "both", angle = 15, col ="red")
draw_link("Asia-2", "Africa", ends = "both", angle = 15, col ="red")
draw_link("Europe-3", "Mid-East", ends = "both", angle = 15, col ="red")
draw_link("Europe-3", "Europe-2", ends = "both", angle = 15, col ="red")
draw_link("Asia-1", "Mid-East", ends = "last", angle = 15, col ="red")
draw_link("Mid-East", "Africa", ends = "both", angle = 15, col ="red")
draw_link("Europe-2", "Mid-East", ends = "last", angle = 15, col ="red")

popViewport()

lgd1 = Legend(at = c(-0.2, -0.4, -0.6, -0.8), graphics = list(
	function(x, y, w, h) {
		grid.lines(unit.c(x-0.5*h, x + 0.5*h), unit.c(y, y),  gp = gpar(col = "darkgreen"),
			arrow = arrow(ends = "last", angle = 90, length = unit(0.5*scale_segment(0.2), "mm"))
		)
	},
	function(x, y, w, h) {
		grid.lines(unit.c(x-0.5*h, x + 0.5*h), unit.c(y, y),  gp = gpar(col = "darkgreen"),
			arrow = arrow(ends = "last", angle = 90, length = unit(0.5*scale_segment(0.4), "mm"))
		)
	},
	function(x, y, w, h) {
		grid.lines(unit.c(x-0.5*h, x + 0.5*h), unit.c(y, y),  gp = gpar(col = "darkgreen"),
			arrow = arrow(ends = "last", angle = 90, length = unit(0.5*scale_segment(0.6), "mm"))
		)
	},
	function(x, y, w, h) {
		grid.lines(unit.c(x-0.5*h, x + 0.5*h), unit.c(y, y),  gp = gpar(col = "darkgreen"),
			arrow = arrow(ends = "last", angle = 90, length = unit(0.5*scale_segment(0.8), "mm"))
		)
	}
), column_gap = unit(2, "mm"), nrow = 1, labels_gp = gpar(fontsize = 7))

lgd2 = Legend(at = c(" 0.2", " 0.4", " 0.6", " 0.8"), graphics = list(
	function(x, y, w, h) {
		grid.lines(unit.c(x-0.5*h, x + 0.5*h), unit.c(y, y), gp = gpar(col = "red"),
			arrow = arrow(ends = "last", angle = 15, length = unit(scale_segment(0.2), "mm"))
		)
	},
	function(x, y, w, h) {
		grid.lines(unit.c(x-0.5*h, x + 0.5*h), unit.c(y, y),  gp = gpar(col = "red"),
			arrow = arrow(ends = "last", angle = 15, length = unit(scale_segment(0.4), "mm"))
		)
	},
	function(x, y, w, h) {
		grid.lines(unit.c(x-0.5*h, x + 0.5*h), unit.c(y, y), gp = gpar(col = "red"), 
			arrow = arrow(ends = "last", angle = 15, length = unit(scale_segment(0.6), "mm"))
		)
	},
	function(x, y, w, h) {
		grid.lines(unit.c(x-0.5*h, x + 0.5*h), unit.c(y, y), gp = gpar(col = "red"), 
			arrow = arrow(ends = "last", angle = 15, length = unit(scale_segment(0.8), "mm"))
		)
	}
), column_gap = unit(2, "mm"), nrow = 1, labels_gp = gpar(fontsize = 7))


lgd = packLegend(lgd1, lgd2, row_gap = unit(2, "mm"))
draw(lgd, x = unit(0.5, "npc") + unit(5, "mm"), y = unit(1, "cm"))

pushViewport(viewport(x = unit(8+2+2+1.5+9, "cm"), y = unit(1, "npc") - unit(10, "mm"), just = c("left", "top"), width = unit(5, "cm"), height = unit(6, "cm")))
plot_hc(hclust(as.dist(d), method = "ward.D2"))
popViewport()


popViewport()
dev.off()



### Figure 7

add_branch_color = function(dend, index = NULL, col = 2) {
	if(is.null(index)) {
		dend = dendrapply(dend, function(d) {
			attr(d, "edgePar") <- list(col = col)
			d
		})
	} else {
		d = dend[[index]]
		d = add_branch_color(d, col = col)
		dend[[index]] = d
	}

	dend
}


draw_cmdscale = function(nm, group, on = "rows", title = "") {
	if(on == "rows") {
         m_small = m_with_NA[nm, !colnames(m_with_NA) %in% c(nm, "United States")]
         # fit = prcomp_irlba(m3, n = 2)
         loc1 = cmdscale(robust_dist(m_small))
         all_nms = rownames(m_small)
     } else {
         m_small = m_with_NA[!rownames(m_with_NA) %in% c(nm, "United States"), nm]
         # fit = prcomp_irlba(t(m3), n = 2)
         loc1 = cmdscale(robust_dist(t(m_small)))
         all_nms = colnames(m_small)
     }

	loc1 = as.data.frame(loc1)[, 1:2]; colnames(loc1) = c("V1", "V2")
	loc1$label = all_nms
	loc1$group = group

	ggplot(loc1, aes(x = V1, y = V2, label = label, color = group)) + 
		geom_point() + geom_text_repel(max.overlaps = Inf, size = 4, show.legend = FALSE) +
		labs(x = "MDS, Dimension 1", y = "MDS, Dimension 2") +
		ggtitle(title) + theme(legend.position="none")
}


lt = split(names(mem), mem)  # 2, 4, 5

col_fun = colorRamp2(c(-2, 0, 2), c("blue", "white", "red"))

pl = list()
w = numeric(0)
h = numeric(0)

cn = lt[["2"]]
subm = m[cn, cn]
subm2 = subm
subm2[subm2 < 0] = 0
g2 = graph_from_adjacency_matrix(subm2, mode = "plus", weighted = TRUE)

set.seed(666)
mem2 = membership(cluster_louvain(g2, weight = sqrt(E(g2)$weight), resolution = 1.1))
mem2 = factor(mem2, levels = c(3, 1, 2))

ht = Heatmap(subm, name = "Enrichment score", #cluster_rows = hc, cluster_columns = hc,
	row_title = "Cited/influencing countries",
	column_title = "Citing/influenced countries", column_title_side = "bottom",
	col = col_fun,
	row_split = mem2, column_split = mem2,
	right_annotation = rowAnnotation(
		"#cited" = anno_barplot(sqrt(as.vector(n_cited[rownames(subm)])), axis_param = list(at = c(0, 1000, 2000, 3000), labels = c("0", "1M", "4M", "9M"))),
		show_legend = FALSE
	),
	top_annotation = HeatmapAnnotation(
		"#citing" = anno_barplot(sqrt(as.vector(n_citing[colnames(subm)])), axis_param = list(at = c(0, 1000, 2000), labels = c("0", "1M", "4M"))),
		show_legend = FALSE
	),
	layer_fun = function(j, i, x, y, w, h, fill) {
		l = pindex(subm, i, j) == 0
		if(any(l)) grid.rect(x[l], y[l], w[l], h[l], gp = gpar(fill = "#DDDDDD", col = "#DDDDDD"))
	},
	cluster_row_slices = FALSE, cluster_column_slices = FALSE,
	show_row_dend = FALSE, show_column_dend = FALSE,
	border = "black",
	width = unit(4, "mm")*nrow(subm),
	height = unit(4, "mm")*ncol(subm)
)

pdf("figures/figure7.pdf", width = 8, height = 7)

ht = draw(ht, column_title = "Influence map only in West-1 countries",
	heatmap_legend_list = list(Legend(at = "Not available", legend_gp = gpar(fill = "#DDDDDD"))))
draw(ht)
dev.off()



###### balance
library(GetoptLong)
country = "United States"

do_country = function(country, name, a1 = 1, a2 = 1, b1 = 3, b2 = 3) {

	x1 = m_with_NA[, country]
	x2 = m_with_NA[country, ]
	
	x2 = x2[names(x1)]

	l = x1 != 0 & x2 != 0 & !is.na(x1) & !is.na(x2) & (!names(x1) %in% mel[["Africa"]])
	x1 = x1[l]
	x2 = x2[l]

	max = 1.5
	
	pushViewport(viewport(xscale = c(-max, max), yscale = c(-max, max), width = unit(3, "cm"), height = unit(3, "cm")))
	
	grid.polygon(c(-(b1-a2), -(b1+a1), (b2-a1), (b2+a2))/1.414, 
				 c(-(a2+b1), -(b1-a1), (b2+a1), (b2-a2))/1.414,
				 default.units = "native",
				 gp = gpar(fill = "#EEEEEE", col = NA))
	grid.lines(c(-b1, b2)/1.414, c(-b1, b2)/1.414, default.units = "native", gp = gpar(col = "grey"))
	grid.abline(slop = 0, intercept = 0, gp = gpar(col = "grey", lty = 2))
	grid.lines(c(0, 0), c(-2/1.414, 2/1.414), default.units = "native", gp = gpar(col = "grey", lty = 2))
	grid.points(x1, x2, gp = gpar(col = mem_color[mem_name[names(x1)]]), pch = 16, size = unit(4, "pt"), default.units = "native")
	grid.text(ifelse(name == "Latin-America", "Latin-\nAmerica", name), x = (b2+(a2-a1)/2+0.2)/1.414, y = (b2+(a1-a2)/2+0.2)/1.414, default.units = "native", rot = -45, just = c("bottom"))
	
	df = do.call(rbind, lapply(1:2, function(k) {
		if(k == 1) {
			l = mem[names(x1)] %in% c(2, 7)
		} else {
			l = mem[names(x1)] %in% c(1, 3, 4, 5, 6)
		}
		# x = x1[l] + x2[l]
		# y = x2[l] - x1[l]
	    # fit = lm(y ~ x)
	    # slop = (1+fit$coefficients[2])/(1-fit$coefficients[2])
	    # intercept = fit$coefficients[1]/(1-fit$coefficients[2])
	    fit = rotate_lm(x2[l], x1[l])
	    slop = fit$coefficients[2]
	    intercept = fit$coefficients[1]
	    grid.lines(range(x1[l]), intercept + slop*range(x1[l]), default.units = "native", gp = gpar(col = k, lwd = 2))

	    data.frame(slop = slop, intercept = intercept, group = name, k = ifelse(k == 1, "world1", "world2"),
	    	balance = mean(x2[l] - x1[l]),
	    	r2 = fit$r2
	    )
	}))
	popViewport()
    
    df
}

rotate_lm = function(y, x) {
	fit = lm(y ~ x)

	d = cooks.distance(fit)
	l = d <= 0.5

	x = x[l]
	y = y[l]

	y2 = y - x
	x2 = y + x

	fit = lm(y2 ~ x2)

	slop = (1+fit$coefficients[2])/(1-fit$coefficients[2])
	intercept = fit$coefficients[1]/(1-fit$coefficients[2])

	r2 = 1 - sum((y - (intercept + slop*(x)))^2)/sum( (y - mean(y))^2 )

	list(coefficients = c(intercept, slop), r2 = r2)
}

mel = split(names(mem_name), mem_name)
lt = list()
pl = list()
pl[["US"]] = grid.grabExpr(lt[["US"]] <- do_country("United States", "US", a2 = 0.5, a1 = 1.2, b1 = 2.5, b2 = 1))
pl[["Netherlands"]] = grid.grabExpr(lt[["Netherlands"]] <- do_country("Netherlands", "Netherlands", a2 = 0.5, b1 = 1.7, b2 = 1.1))
pl[["Spain"]] = grid.grabExpr(lt[["Spain"]] <- do_country("Spain", "Spain", a2 = 0.6, b1 = 1.5, b2 = 1.5))
pl[["Poland"]] = grid.grabExpr(lt[["Poland"]] <- do_country("Poland", "Poland", a1 = 0.8,  b1 = 2))
pl[["Brazil"]] = grid.grabExpr(lt[["Brazil"]] <- do_country("Brazil", "Brazil", a2 = 1.2, a1 = 0.8, b1 = 2, b2 = 4))
pl[["Saudi Arabia"]] = grid.grabExpr(lt[["Saudi Arabia"]] <- do_country("Saudi Arabia", "Saudi Arabia", b2 = 5.3, b1 = 2, a1 = 1))
pl[["Thailand"]] = grid.grabExpr(lt[["Thailand"]] <- do_country("Thailand", "Thailand", b2 = 4.7, b1 = 2, a1 = 1, a2 = 1))
pl[["China"]] = grid.grabExpr(lt[["China"]] <- do_country("China", "China", b2 = 1.6, b1 = 2, a2 = 1))
pl[["Hong Kong"]] = grid.grabExpr(lt[["Hong Kong"]] <- do_country("Hong Kong", "Hong Kong", b2 = 2.5, b1 = 1.5, a2 = 0.8))


tbb = data.frame(
	slop1 = sapply(lt, function(x) x[1, "slop"]),
	intercept1 = sapply(lt, function(x) x[1, "intercept"]),
	balance1 = sapply(lt, function(x) x[1, "balance"]),
	r2_1 = sapply(lt, function(x) x[1, "r2"]),
	slop2 = sapply(lt, function(x) x[2, "slop"]),
	intercept2 = sapply(lt, function(x) x[2, "intercept"]),
	balance2 = sapply(lt, function(x) x[2, "balance"]),
	r2_2 = sapply(lt, function(x) x[2, "r2"])
)
rownames(tbb) = names(lt)

plot(tbb[, 1], tbb[, 3])
text(tbb[, 1], tbb[, 3], rownames(tbb))
abline(h = 1, col = "grey")
abline(v = 1, col = "grey")

tbb2 = apply(tbb, 2, scale)
rownames(tbb2) = rownames(tbb)


pdf("figures/figure8.pdf", width = 15, height = 6)
grid.newpage()
pushViewport(viewport(x = unit(5, "cm")))
grid.draw(pl[[1]])
popViewport()

pushViewport(viewport(x = unit(7.4, "cm")))
grid.draw(pl[[2]])
popViewport()

pushViewport(viewport(x = unit(9.8, "cm")))
grid.draw(pl[[3]])
popViewport()

pushViewport(viewport(x = unit(12, "cm")))
grid.draw(pl[[4]])
popViewport()

pushViewport(viewport(x = unit(14.9, "cm")))
grid.draw(pl[[5]])
popViewport()

pushViewport(viewport(x = unit(18.3, "cm")))
grid.draw(pl[[6]])
popViewport()

pushViewport(viewport(x = unit(21.4, "cm")))
grid.draw(pl[[7]])
popViewport()

pushViewport(viewport(x = unit(24.6, "cm")))
grid.draw(pl[[8]])
popViewport()

pushViewport(viewport(x = unit(27.8, "cm")))
grid.draw(pl[[9]])
popViewport()

lgd = Legend(title = "Region", title_position = "topleft", at = names(mem_color[-7]), type = "points", legend_gp = gpar(col = mem_color[-7]), nrow = 3)

draw(lgd, x = unit(2.3, "cm"), y = unit(13, "cm"), just = c("left", "top"))

grid.text("Scale on axes", x =  unit(10.2, "cm"), y = unit(13, "cm"), just = c("left", "top"), gp = gpar(fontsize = 10, fontface = "bold"))
grid.segments(unit(10.3, "cm"), unit(12.3, "cm"), unit(12.3, "cm"), unit(12.3, "cm"), gp = gpar(col = "#808080", lty = 1))
grid.segments(unit(c(10, 11, 12)+0.3, "cm"), unit(12.3, "cm"), unit(c(10, 11, 12)+0.3, "cm"), unit(12.4, "cm"), gp = gpar(col = "#808080", lty = 1))
grid.text(c(0, 1, 2), unit(c(10, 11, 12)+0.3, "cm"), unit(12, "cm"), gp = gpar(fontsize = 7, col = "#808080"), just = "top")

pushViewport(viewport(x = unit(30.5, "cm"), y = unit(13, "cm"), width = unit(5, "cm"), height = unit(6, "cm"), just = c("left", "top")))
plot_hc(hclust(dist(tbb2)), ann = FALSE, fontsize = 10)
popViewport()
dev.off()



lt = list()
for(country in names(mem_name[mem_name != "Africa"])) {
	foo = grid.grabExpr(lt[[country]] <- do_country(country, country, a2 = 0.5, a1 = 1.2, b1 = 2.5, b2 = 1))
}



# # degree distribution
# lt = split(names(mem_name), mem_name)

# country = lt[["Asia-1"]]
# tbl = list()
# for(c in all_countries) {
# 	foo = data.frame(nrow = 0, ncol = 2)
# 	colnames(foo) = c("V1", "V2")
# 	for(x in country) {
# 		try(foo <- rbind(foo, read.table(qq("processed/citations_split/citations_@{x}_to_@{c}.tab"))))
# 	}
# 	tbb = table(table(foo[, 1]))  # out-degree

# 	tbl[[c]] = data.frame(degree = as.numeric(names(tbb)), freq = as.vector(tbb))
# }

# fl1 = sapply(tbl, function(x) {
# 	fit = lm(log(x$freq) ~ log(x$degree))
# 	c("slop" = fit$coefficients[2], "intercept_x" = -fit$coefficients[1]/fit$coefficients[2], "intercept_y" = fit$coefficients[1])
# })

# tbl = list()
# for(c in all_countries) {
# 	foo = data.frame(nrow = 0, ncol = 2)
# 	colnames(foo) = c("V1", "V2")
# 	for(x in country) {
# 		try(foo <- rbind(foo, read.table(qq("processed/citations_split/citations_@{c}_to_@{x}.tab"))))
# 	}
# 	tbb = table(table(foo[, 2]))  # out-degree

# 	tbl[[c]] = data.frame(degree = as.numeric(names(tbb)), freq = as.vector(tbb))
# }

# fl2 = sapply(tbl, function(x) {
# 	fit = lm(log(x$freq) ~ log(x$degree))
# 	c("slop" = fit$coefficients[2], "intercept_x" = -fit$coefficients[1]/fit$coefficients[2], "intercept_y" = fit$coefficients[1])
# })


# #### change background
# df = read.table("processed/num_cite_country_country.tab", quote = "", sep = "\t", header = TRUE)
# df1 = calc_stat(df, all = FALSE); rownames(df1) = paste(df1[, 1], df1[, 2], sep = "-")
# df2 = df[df[, 1] != df[, 2], ]
# df2 = calc_stat(df2, all = FALSE); rownames(df2) = paste(df2[, 1], df2[, 2], sep = "-")

# cn = intersect(rownames(df1), rownames(df2))
# df1 = df1[cn, ]
# df2 = df2[cn, ]

# pdf("figures/figure9.pdf", width = 6, height = 6)
# l = df1$log2_fc- df2$log2_fc < -0.4
# plot(df1$log2_fc, df2$log2_fc, cex = ifelse(l, 0.8, 0.2), 
# 	col = add_transparency(ifelse(l&grepl("United States", rownames(df1)), "red", 
# 		      ifelse(l&grepl("China", rownames(df1)), "blue", 
# 		      	ifelse(l&grepl("Ethiopia", rownames(df1)), "orange", "black"))), 0.25),
# 	pch = ifelse(df1$country_cited == "United States", 4, 
# 		      ifelse(df1$country_cited == "China", 4,
# 		      	 ifelse(df1$country_cited == "Ethiopia", 4, 16))
# 		),
# 	xlab = "log2 fold enrichment / all citations", ylab = "log2 fold enrichment / international citations", main = "compare universe"
# )
# abline(v = 0, lty = 2, col = "grey")
# abline(h = 0, lty = 2, col = "grey")
# legend("topleft", cex = 0.7,
# 	legend = c("from United States", "on United States", "on China", "on Ethiopia", "others"), 
# 	col = c("red", "red", "blue", "orange", "black"), pch = c(16, 4, 4, 4, 16))
# dev.off()




# df = read.table("processed/num_cite_country_country.tab", quote = "", sep = "\t", header = TRUE)
# df = calc_stat(df, cutoff_single = 100, all = TRUE)

# l = df$country_cited == df$country_citing
# lm(log2(df$total_cited[l]) ~ log2(df$citations[l]))
# lm(log2(df$total_citing[l]) ~ log2(df$citations[l]))


# plot(df$total_citing[l], df$citations[l], log = "xy")

# lt1 = list()
# for(c in names(mem_name)) {
# 	l = df$country_cited == c & df$country_cited != df$country_citing
# 	x1 = df$citations[l]
# 	x2 = df$total_citing[l]

# 	plot(x1, x2, log = "xy", main = c, col = mem_color[mem_name[df$country_citing[l]]])
	

# 	ll = mem_name[df$country_citing[l]] %in% c("US", "West-1")
# 	fit1 = lm( log2(x2)[ll] ~ log2(x1)[ll] )
# 	su1 = summary(fit1)

# 	fit2 = lm( log2(x2)[!ll] ~ log2(x1)[!ll] )
# 	su2 = summary(fit2)
# 	lt1[[c]] = list("w1" = su1$coefficients,
# 		            "w2" = su2$coefficients)
# }

# lt2 = list()
# for(c in names(mem_name)) {
# 	l = df$country_citing == c & df$country_cited != df$country_citing
# 	x1 = df$citations[l]
# 	x2 = df$total_cited[l]

# 	plot(x1, x2, log = "xy", main = c, col = mem_color[mem_name[df$country_cited[l]]])
	

# 	ll = mem_name[df$country_cited[l]] %in% c("US", "West-1")
# 	fit1 = lm( log2(x2)[ll] ~ log2(x1)[ll] )
# 	su1 = summary(fit1)

# 	fit2 = lm( log2(x2)[!ll] ~ log2(x1)[!ll] )
# 	su2 = summary(fit2)
# 	lt2[[c]] = list("w1" = su1$coefficients,
# 		            "w2" = su2$coefficients)
# }

# lt3 = list()
# for(c in names(mem_name)) {
# 	l = df$country_citing == c & df$country_cited != df$country_citing
# 	x1 = df$citation[l]; names(x1) = df$country_cited[l]

# 	l = df$country_cited == c & df$country_cited != df$country_citing
# 	x2 = df$citation[l]; names(x2) = df$country_citing[l]

# 	cn = intersect(names(x1), names(x2))
# 	x1 = x1[cn]
# 	x2 = x2[cn]

# 	plot(x1, x2, log = "xy", main = c)
# 	fit = lm( log2(x1) ~ log2(x2) )
# 	su = summary(fit)
# 	lt3[[c]] = su$coefficients
# }



