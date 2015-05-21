CH.stat = function(data, max = 10, method = kmeans, ...)
{	x = data.frame(data)
	## CH(k=1) is not defined since there's a (k-1) in the denominator; even if we replace (k-1) by k, B(k=1) would be 0. The maximum would never occur at k=1. ##
	w = b = ch = rep(0, max)
	for (k in 2:max) 
	{	res = method(x, k, ...)
		w[k] = res$tot.withinss
		b[k] = res$betweenss
		ch[k] = (b[k]/(k-1))/(w[k]/(nrow(x)-k))
	}
	return (list(Tabs = cbind(w.k = w, b.k = b, ch.k = ch), ClusterNum = which.max(ch)))
}

KL.stat = function(data, max = 10, method = kmeans, ...)
{	x = data.frame(data)
	w = diff = rep(0, max+1)
	kl = rep(0, max)
	for (k in 1:(max+1)) w[k] = method(x, k, ...)$tot.withinss
	for (k in 2:(max+1)) diff[k] = w[k-1]*(k-1)^(2/length(x)) - w[k]*k^(2/length(x))
	for (k in 2:max) kl[k] = abs(diff[k]/diff[k+1])
	return (list(kl = kl, ClusterNum = which.max(kl)))
}

H.stat = function(data, max = 10, method = kmeans, ...)
{	x = data.frame(data)
	w = rep(0, max+1)
	h = rep(0, max)
	for (k in 1:(max+1)) w[k] = method(x, k, ...)$tot.withinss
	clus = 0
	for (k in 1:max) 
	{	h[k] = (w[k]/w[k+1] - 1)/(nrow(x) - k - 1)
		if (h[k] <= 10 & clus == 0) clus = k
	}
	if (clus == 0) clus = which.min(h)
	return (list(Hartigan = h, ClusterNum = clus))
}

s.stat = function(data, max = 10, method = kmeans, ...)
{	require(cluster)
	x = data.frame(data)
	s = rep(0, max)
	for (k in 2:max) s[k] = mean(silhouette(method(x, k ,...)$cluster, dist(x))[,3])
	return (list(AvgSilhouette = s, ClusterNum = which.max(s)))
}
