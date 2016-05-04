ln.wk = function(x, label)
{	return (log(sum(sapply(split(1:nrow(x), label), function(ki) {
				pair = x[ki, ,drop = F]
				sum(dist(pair)^2)/(2*nrow(pair))
			}))))
}   

ref.dist = function(x, pc = T)
{	xp = x
	if (pc)
	{	std.x = scale(as.matrix(x), center = T, scale = F)
		v = svd(std.x)$v
		xp = data.frame(std.x %*% v)
	}
	mins = apply(xp, 2, min)
	maxs = apply(xp, 2, max)
	ref.dist = sapply(1:length(mins), function(f) runif(nrow(xp), min = mins[f], max = maxs[f]))
	if (pc) 
	{	ref.dist = data.frame(as.matrix(ref.dist) %*% t(v))
		## The following code does not make any difference if the method to calculate distance between two observations is (squared) Euclidean Distance. ##
		#ref.dist = sapply(1:length(ref.dist), function(f) ref.dist[,f] = ref.dist[,f] + mean(x[,f]))
	}
	return (ref.dist)
}

gap.stat = function(data, max = 10, clusFUN = kmeans, pc = T, B = 50, tibs = T, ...)
{	x = data
	is.hclust = as.character(substitute(clusFUN)) == "hclust"
	if (is.matrix(x)) x = data.frame(x) 
	ln.w = e.ln.w = s = oriGap = rep(0, max)
	e.ln.ws = matrix(0, B, max)
	if (is.hclust) 
	{	require(cluster)
		hc = hclust(dist(x), ...)
	}
	for (k in 1:max) ln.w[k] = ln.wk(x, if (is.hclust) cutree(hc, k) else clusFUN(x, k, ...)$cluster)
	for (b in 1:B)
	{	r = ref.dist(x, pc)
		if (is.hclust) r.hc = hclust(dist(r))
		for (k in 1:max) e.ln.ws[b, k] = ln.wk(r, if (is.hclust) cutree(r.hc, k) else clusFUN(r, k, ...)$cluster)
	}
	e.ln.w = colMeans(e.ln.ws)
	for (k in 1:max) s[k] = sqrt((1 + 1/B) * sum((e.ln.ws[,k] - mean(e.ln.ws[,k]))^2)/B)
	oriGap = e.ln.w - ln.w
	globalMax = tibsClus = which.max(oriGap)
	## The following instructions are the original cluster number selection method proposed by Tibshirani. ##
	## If we cannot find the smallest "k" where 1<k<max such that Gap(k) <= Gap(k+1) - s(k+1), k = max, which may implys that the maximal cluster specified is not big enough. ##
	for (k in 1:(max-1)) 
		 if (oriGap[k] >= (oriGap[k+1] - s[k+1]))
		 {	tibsClus = k
			break
		 }
	return (list(Tabs = cbind(logW = ln.w, ElogW = e.ln.w, S = s, Gap = oriGap, GapMinusS = oriGap - s), GlobalMaxClusterNum = globalMax, TibsClusterNum = tibsClus, ClusterNum = if(tibs) tibsClus else globalMax))
}
