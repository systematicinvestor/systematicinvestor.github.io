---
layout: post
title: Correlation in Rcpp
comments: true
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](',base.url,'about) page.




There are many great examples of using Rcpp at [Rcpp Gallery](http://gallery.rcpp.org).
Below, I will use [RcppParallel](http://rcppcore.github.io/RcppParallel/) to speed computations of [correlation matrix](http://en.wikipedia.org/wiki/Correlation_and_dependence).




{% highlight r %}
#*****************************************************************
# Load historical end of day data
#*****************************************************************
library(SIT)
load.packages('quantmod')

filename = 'big.test.Rdata'

if(!file.exists(filename)) {
  tickers = nasdaq.100.components()
  tickers = sp500.components()$tickers

  data = env()
  getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, set.symbolnames = T, auto.assign = T)
  for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
  #print(bt.start.dates(data))

  bt.prep(data, align='keep.all', dates='2000::', fill.gaps=T)

  # remove ones with little history
  bt.prep.remove.symbols.min.history(data, 3*252)

  # show the ones removed
  print(setdiff(tickers,names(data$prices)))

  prices = data$prices
  save(prices, file=filename)
}

load(file=filename)

#*****************************************************************
# Setup
#*****************************************************************
ret = prices / mlag(prices) - 1
  ret = last(ret,252)
  ret = coredata(na.omit(ret))

#*****************************************************************
# Compute Correlation
#*****************************************************************
rtest = function(ret) {
	temp = cor(ret)
	temp[!lower.tri(temp)] = 0
	temp
}

r.cor = rtest(ret)
{% endhighlight %}

First let's implement correlation using Rcpp:

{% highlight cpp %}
#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::plugins(cpp11)]]

struct asset_info {
	double sum, sum2, stdev;
};

//[correlation matrix](http://en.wikipedia.org/wiki/Correlation_and_dependence).
// n,sX,sY,sXY,sX2,sY2
// cor = ( n * sXY - sX * sY ) / ( sqrt(n * sX2 - sX^2) * sqrt(n * sY2 - sY^2) )
inline asset_info compute_asset_info(const NumericMatrix& mat, 
	const int icol, const int rstart, const int rend) {
	double sum, sum2;
    sum = sum2 = 0;

	for (int r = rstart; r < rend; r++) {
		double d = mat(r, icol);
		sum += d;
		sum2 += pow(d,2);
	}

	asset_info res;
		res.sum = sum;
		res.sum2 = sum2;
		res.stdev = sqrt((rend-rstart) * sum2 - pow(sum, 2));
	return res;
}

inline NumericMatrix c_cor_helper(const NumericMatrix& mat, const int rstart, const int rend) {
	int nc = mat.ncol();
	int nperiod = rend - rstart;
	NumericMatrix rmat(nc, nc);

	vector<asset_info> info(nc);
	for (int c = 0; c < nc; c++)
		info[c] = compute_asset_info(mat, c, rstart, rend);

	for (int c1 = 0; c1 < nc; c1++) {
		for (int c2 = 0; c2 < c1; c2++) {
			double sXY = 0;

			for (int r = rstart; r < rend; r++)
				sXY += mat(r, c1) * mat(r, c2);

			rmat(c1, c2) = (nperiod * sXY - info[c1].sum * info[c2].sum) / (info[c1].stdev * info[c2].stdev);
		}
	}

	return rmat;
}

// [[Rcpp::export]]
NumericMatrix c_cor(NumericMatrix mat) {
	return c_cor_helper(mat, 0, mat.nrow());
}
{% endhighlight %}

Please save above code in the `correlation.cpp` file or download [correlation.cpp](/public/doc/correlation.cpp).

Next let's make sure it produces same results.


{% highlight r %}
#*****************************************************************
# Rcpp Correlation
#*****************************************************************
# make sure to install [Rtools on windows](http://cran.r-project.org/bin/windows/Rtools/)
load.packages('Rcpp')

# load Rcpp functions
sourceCpp('correlation.cpp')

r.cor = rtest(ret)
c.cor = c_cor(ret)
print(test.equality(r.cor, c.cor))
{% endhighlight %}



|item1 | item2 |equal |
|:-----|:------|:-----|
|r.cor |c.cor  |TRUE  |
    

The [RcppParallel](http://rcppcore.github.io/RcppParallel/) is an easy way to speed up
above computations. For more details about [RcppParallel](http://rcppcore.github.io/RcppParallel/)
I recommend following resources:

* [RcppParallel help site](http://rcppcore.github.io/RcppParallel/)
* [RcppParallel source code](https://github.com/RcppCore/RcppParallel)
* [RcppParallel introduction](http://dirk.eddelbuettel.com/blog/2014/07/16/#introducing_rcppparallel)
* [RcppParallel at Rcpp gallery](http://gallery.rcpp.org/articles/parallel-distance-matrix/)

Next let's implement correlation using [RcppParallel](http://rcppcore.github.io/RcppParallel/):

{% highlight cpp %}
/*------ Parallel version ------*/

// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
using namespace RcppParallel;

// pre-compute sum and stdev
struct cor_p1 : public Worker {
	const RMatrix<double> mat;
	const int rstart, rend, nperiod;

	RVector<double> rsum, rstdev;

	cor_p1(const NumericMatrix& mat, const int rstart, const int rend,
		NumericVector rsum, NumericVector rstdev)
		: mat(mat), rstart(rstart), rend(rend), nperiod(rend - rstart), rsum(rsum), rstdev(rstdev) { }

	void operator()(size_t begin, size_t end) {
		for (size_t c = begin; c < end; c++) {
			double sum, sum2;
			sum = sum2 = 0;

			for (int r = rstart; r < rend; r++) {
				double d = mat(r,c);
				sum += d;
				sum2 += pow(d,2);
			}

			rsum[c] = sum;
			rstdev[c] = sqrt(nperiod * sum2 - pow(sum,2));
		}
	}
};
// compute correlation
struct cor_p2 : public Worker {
	const RMatrix<double> mat;
	const int rstart, rend, nperiod;
	const RVector<double> sum, stdev;
    
	RMatrix<double> rmat;

	cor_p2(const NumericMatrix& mat, const int rstart, const int rend,
		const NumericVector& sum, const NumericVector& stdev, 
		NumericMatrix rmat)
		: mat(mat), rstart(rstart), rend(rend), nperiod(rend - rstart), sum(sum), stdev(stdev), rmat(rmat) {}

	void operator()(size_t begin, size_t end) {
		for (size_t c1 = begin; c1 < end; c1++) {
			for (size_t c2 = 0; c2 < c1; c2++) {
				double sXY = 0;
				for (int r = rstart; r < rend; r++)
					sXY += mat(r,c1) * mat(r,c2);

				rmat(c1,c2) = (nperiod * sXY - sum[c1] * sum[c2]) / (stdev[c1] * stdev[c2]);         
			}
		}
	}
};

inline NumericMatrix cp_cor_helper(const NumericMatrix& mat, const int rstart, const int rend) {
	int nc = mat.ncol();
	NumericVector rsum(nc), rstdev(nc);

	cor_p1 p1(mat, rstart, rend, rsum, rstdev);
	parallelFor(0, nc, p1);

	NumericMatrix rmat(nc, nc);

	cor_p2 p2(mat, rstart, rend, rsum, rstdev, rmat);
	parallelFor(0, nc, p2);

	return rmat;
}

// [[Rcpp::export]]
NumericMatrix cp_cor(NumericMatrix mat) {
	return cp_cor_helper(mat, 0, mat.nrow());
}
{% endhighlight %}

Please save above code in the `correlation.cpp` file or download [correlation.cpp](/public/doc/correlation.cpp).

Next let's make sure it produces same results and compare the run times.


{% highlight r %}
#*****************************************************************
# RcppParallel Correlation
#*****************************************************************
# make sure to install [Rtools on windows](http://cran.r-project.org/bin/windows/Rtools/)
load.packages('Rcpp')
load.packages('RcppParallel')
# [RcppParallel help site](http://rcppcore.github.io/RcppParallel/)
# [RcppParallel source code](https://github.com/RcppCore/RcppParallel)
# [RcppParallel introduction](http://dirk.eddelbuettel.com/blog/2014/07/16/#introducing_rcppparallel)
# [RcppParallel gallery](http://gallery.rcpp.org/articles/parallel-distance-matrix/)
# defaultNumThreads()

# load Rcpp functions
sourceCpp('correlation.cpp')

r.cor = rtest(ret)
c.cor = c_cor(ret)
cp.cor = cp_cor(ret)
print(test.equality(r.cor, c.cor, cp.cor, type='all'))
{% endhighlight %}



|item1 | item2 |equal |
|:-----|:------|:-----|
|r.cor |c.cor  |TRUE  |
|r.cor |cp.cor |TRUE  |
|c.cor |cp.cor |TRUE  |
    




{% highlight r %}
# compare performance
library(rbenchmark)
res <- benchmark(cor(ret),
                 c_cor(ret),
                 cp_cor(ret),
                 replications = 20,
                 order="relative")
print(res[,1:4])
{% endhighlight %}



|rownames(x) |test        | replications| elapsed| relative|
|:-----------|:-----------|------------:|-------:|--------:|
|3           |cp_cor(ret) |           20|    0.14|    1.000|
|2           |c_cor(ret)  |           20|    0.50|    3.571|
|1           |cor(ret)    |           20|    0.52|    3.714|
    

The [RcppParallel](http://rcppcore.github.io/RcppParallel/) version is about 3.5 times faster.


*(this report was produced on: 2015-04-11)*
