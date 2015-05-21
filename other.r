
CH = function(data, max = 10, method = kmeans, ...)
{	x = data
	if (is.matrix(x)) x = data.frame(x)
	## CH(k=1) is not defined since there's a (k-1) in the denominator; even if we replace (k-1) by k, B(k=1) would be 0. The maximum would never occur at k=1. ##
	w = b = ch = rep(0, max)
	for (k in 2:max) 
	{	res = method(x, k, ...)
		w[k] = res$tot.withinss
		b[k] = res$betweenss
		ch[k] = (b[k]/(k-1))/(w[k]/(nrow(x)-k))
	}
	return (list(Tabs = cbind(w.k = w, b.k = b, ch.k = ch), GlobalMaxClusterNum = which.max(ch)))
}
