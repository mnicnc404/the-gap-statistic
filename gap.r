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

est.ln.wk = function(x, pca, max)
{	xp = x
	if (pca)
	{	x = as.matrix(x)
		std.x = scale(x, center = T, scale = F)
		v = svd(std.x)
		xp = std.x %*% v		
		xp = data.frame(xp)
	}
    mins = apply(xp, 2, min)
	maxs = apply(xp, 2, max)
   	ref.dist = sapply(1:length(mins), function(f) runif(nrow(xp), min = mins[f], max = maxs[f]))
    if (pca)
	{	ref.dist = as.matrix(ref.dist)
		ref.dist = ref.dist %*% t(v)
		ref.dist = data.frame(ref.dist) 
	}

}

gap = function(data, max = 10, method = kmeans, pca = T, B = 50, ...)
{   require(cluster)
	x = data
	if (is.matrix(x)) x = data.frame(x) 
    
}
