ln.wk = function(x, kb = 1, method, ...)
{	label = rep(1, nrow(x))
	if (kb > 1) label = method(x, kb, ...)$cluster
	return (log(sum(sapply(split(1:nrow(x), label), function(ki) {
				pair = x[ki, ]
				sum(dist(pair))/(2*nrow(pair))
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
	#########
	## The following code does not make any difference if the method to calculate distance between two observations is Euclidean Distance. ##
    #if (pc) 
	#{	ref.dist = data.frame(as.matrix(ref.dist) %*% t(v))
	#	ref.dist = sapply(1:length(ref.dist), function(f) ref.dist[,f] = ref.dist[,f] + mean(x[,f]))
	#}
	########
	return (ref.dist)
}

gap = function(data, max = 10, method = kmeans, pc = T, B = 50, ...)
{	x = data
	if (is.matrix(x)) x = data.frame(x) 
	ln.w = e.ln.w = s = oriGap = rep(0, 10)
	e.ln.ws = matrix(0, B, max)
	for (k in 1:max) ln.w[k] = ln.wk(x, k, method, ...)
    for (b in 1:B)
	{	r = ref.dist(x, pc)
		for (k in 1:max) e.ln.ws[b, k] = ln.wk(r, k, method, ...)
	}
	e.ln.w = colMeans(e.ln.ws)
	for (k in 1:max) s[k] = sqrt((1 + 1/B) * sum((e.ln.ws[,k] - mean(e.ln.ws[,k]))^2)/B)
	oriGap = e.ln.w - ln.w
	globalMax = tibs = which.max(oriGap)
	## The following instructions are the original cluster number selection method proposed by Tibshirani. ##
	## If we cannot find a "k" where 1<k<max such that Gap(k) <= Gap(k+1) - s(k+1), k = max, which may implys that the maximal cluster specified is not big enough. ##
	for (k in 1:(max-1)) 
		 if (oriGap[k] >= (oriGap[k+1] - s[k+1]))
		 {	tibs = k
			break
		 }
	return (list(Tabs = (cbind(logW = ln.w, ElogW = e.ln.w, S = s, Gap = oriGap, GapMinusS = oriGap - s)), GlobalMaxClusterNum = globalMax, TibsClusterNum = tibs))
}
