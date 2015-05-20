# The Gap Statistic

An implementation of the gap statistic in R. The gap statistic, proposed by Robert Tibshirani, Guenther Walther, and Trevor Hastie, is a method for estimating the number of clusters in a set of data. For details, please check the original paper ["Estimating the number of clusters in a data set via the gap statistic"](http://web.stanford.edu/~hastie/Papers/gap.pdf).

Originally it was a reproduction assignment assigned by my advisor. However, I found that the existing implementation [clusGap](http://stat.ethz.ch/R-manual/R-devel/library/cluster/html/clusGap.html) in the [cluster](http://cran.r-project.org/web/packages/cluster/index.html) package is not completely align to the original algorithm, so I decided to push my implementation onto github for those who are interested.


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

