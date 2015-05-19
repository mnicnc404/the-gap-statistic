require(cluster)
require(plyr)
require(ggplot2)

ln.wk = function(x, kb, method, ...)
{   require(cluster)
	set.seed(1)
	if (kb > 1) label = method(x, kb, ...)$cluster
    else lable = rep(1, nrow(x))
	return (log(sum(sapply(split(1:nrow(x), label), function(ii) {
				pair = x[ii, ]
				sum(dist(pair))/(2*nrow(pair))
			}))))
}   

ref.dist = function(x, pca)
{	xp = x
	if (pca)
	{	std.x = scale(as.matrix(x), center = T, scale = F)
		v = svd(std.x)$v
		xp = data.frame(std.x %*% v)
	}
    mins = apply(xp, 2, min)
	maxs = apply(xp, 2, max)
   	ref.dist = sapply(1:length(mins), function(f) runif(nrow(xp), min = mins[f], max = maxs[f]))
    if (pca) 
	{	ref.dist = data.frame(as.matrix(ref.dist) %*% t(v))
		print(head(ref.dist))
		ref.dist = sapply(1:length(ref.dist), function(f) ref.dist[,f] = ref.dist[,f] + mean(x[,f]))
		print(head(ref.dist))
		print(head(x))
	}
	return (ref.dist)
}



gap = function(data, max = 10, method = kmeans, pca = T, B = 50, ...)
{   require(cluster)
	x = data
	if (is.matrix(x)) x = data.frame(x) 
	ln.w = e.ln.w = se = rep(0, 10)
    for (b in 1:B)
	{	
	}
}
