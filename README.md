# The Gap Statistic

An implementation of the gap statistic in R. The gap statistic, proposed by Robert Tibshirani, Guenther Walther, and Trevor Hastie, is a method for estimating the number of clusters in a set of data. For details, please check the original paper ["Estimating the number of clusters in a data set via the gap statistic"](http://web.stanford.edu/~hastie/Papers/gap.pdf).

Originally it was a reproduction assignment assigned by my advisor. However, I found that the existing implementation [clusGap](http://stat.ethz.ch/R-manual/R-devel/library/cluster/html/clusGap.html) in the [cluster](http://cran.r-project.org/web/packages/cluster/index.html) package is not completely align to the original algorithm, so I decided to push my implementation onto github for those who are interested.

**Warning: the code is not robust.** If you encounter any problem with the code, please contact me at Github or [send me email](mnicnc404@gmail.com).

## What's the Difference?

### The Reference Distribution

The original paper proposed two ways to generate reference distribution:

> Unif: Generate each reference feature uniformly over the range of the observed values for that feature.

And:

> PC: Generate the reference features from a uniform distribution over a box aligned with the principal components of the data.

The [clusGap](http://stat.ethz.ch/R-manual/R-devel/library/cluster/html/clusGap.html) only provides the PC method, but the experiment results in the [paper](http://web.stanford.edu/~hastie/Papers/gap.pdf) says that the PC way does not always out perform the Unif method; sometimes the Unif way actually does better (see Table 1 of the [paper](http://web.stanford.edu/~hastie/Papers/gap.pdf)). 

I implemented both ways to generate the reference distribution. To decide which method to use, just determine the parameter `pc` when using the function. For instance:

		gap(iris[,-5], pc = F)

If we assign `pc` to `False`, the Unif way will be used; 
If we assign `pc` to `True`, the PC way will be used.

Note: There's another [implementation](https://github.com/echen/gap-statistic) implemented by [echen](https://github.com/echen) on Github. Only the Unif way is implemented in [echen](https://github.com/echen)'s code.

### Standard Deviation Calculation

The [paper](http://web.stanford.edu/~hastie/Papers/gap.pdf) says we should select a `k` where `1 < k < max` such that `Gap(k) <= Gap(k+1) - s(k+1)`, where s is some value multiplied by standard deviation of the within-group dispersion drawn from several samples. In [clusGap](http://stat.ethz.ch/R-manual/R-devel/library/cluster/html/clusGap.html), the built-in `var` function is used to calculate standard deviation (as we know, standard deviation is the square root of variance.) However, in the built-in `var` function, the denominator of the standard deviation is deducted by the degree of freedom (which is 1), which is different from the original algorithm (the denominator of the *standard deviation* proposed in the paper is not deducted by 1)!

Despite the fact that it may not cause big difference, I still decide to implement the original [paper](http://web.stanford.edu/~hastie/Papers/gap.pdf)'s version (not to use the built-in function `var`).

## Example

The usage is similar to [clusGap](http://stat.ethz.ch/R-manual/R-devel/library/cluster/html/clusGap.html). For instance:

		set.seed(1)
		x = c(rnorm(10, 0, 1), rnorm(10, 5, 1.5), rnorm(10, 10, 1))
		y = c(rnorm(10, 10, 1), rnorm(10, 5, 1), rnorm(10, 0, 1))
		data = cbind(x,y)
		gapStat(data = data, max = 10, method = kmeans, pc = T, B = 50, iter.max = 10)
		# The "iter.max" parameter is for the "kmeans" function.

Note that you only have to specify the data like `gapStat(data)`, which gives you the same result.
The result is:

		logW    ElogW          S         Gap   GapMinusS
		[1,] 3.932728 3.764804 0.06969463 -0.16792329 -0.23761792
		[2,] 3.190214 3.120146 0.06970189 -0.07006775 -0.13976964
		[3,] 2.477820 2.776090 0.06688969  0.29827007  0.23138038
		[4,] 2.327328 2.577955 0.07746779  0.25062745  0.17315966
		[5,] 2.215417 2.399859 0.07217596  0.18444261  0.11226665
		[6,] 2.050635 2.274811 0.07124480  0.22417568  0.15293088
		[7,] 1.974787 2.132784 0.08352412  0.15799618  0.07447206
		[8,] 1.821773 2.018260 0.08526062  0.19648702  0.11122640
		[9,] 1.664564 1.915915 0.09375918  0.25135046  0.15759129
		[10,] 1.569001 1.766674 0.10887759  0.19767347  0.08879588
		
		$GlobalMaxClusterNum
		[1] 3
		
		$TibsClusterNum
		[1] 3

You can `plot(data)` if you'd like to see the distribution of the generated data.
