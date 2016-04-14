---
layout: post
title: Run Correlation in Rcpp
comments: true
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](',base.url,'about) page.




Let's continue with [Correlation in Rcpp](/Correlation-Rcpp) post and implement
running window Correlation functionality.

First, let's load historical prices for S&P 500:




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
# Run Correlation
#*****************************************************************
prices = prices[,1:5]

n = ncol(prices)
nperiod = nrow(prices)
ret = prices / mlag(prices) - 1

index =  (nperiod-20):nperiod
ret = ret[index,]
nperiod = nrow(ret)
nwindow = 15

rtest = function(ret, nwindow) {
	n = ncol(ret)
	nperiod = nrow(ret)
	out = array(NA, c(n,n,nperiod))
	index = !lower.tri(out[,,1])

	for(i in nwindow:nperiod) {
		temp = cor(coredata(ret[(i-nwindow+1):i,]))
		temp[index] = 0
		out[,,i] = temp
	}
	out
}

r.cor = rtest(ret, nwindow)
{% endhighlight %}

Next, let's implement a function using Rcpp. I will re-use code from
[Correlation in Rcpp](/Correlation-Rcpp) post.

{% highlight cpp %}
/*------ Running correlation ------*/
// [[Rcpp::export]]
NumericVector c_run_cor(NumericMatrix mat, int nwindow) {
	int nc = mat.ncol();
    int nperiod = mat.nrow();
	NumericVector ret = NumericVector(Dimension(nc, nc, nperiod));
	fill_n(ret.begin(), ((0 + nwindow - 1) * nc * nc), NA_REAL);
    
	for(int i = 0; i < (nperiod-nwindow+1); i++) {
		NumericMatrix cor = c_cor_helper(mat, i, i+nwindow);
		std::copy(cor.begin(), cor.end(), ret.begin() + ((i + nwindow - 1) * nc * nc));
	}
	return ret;
}

// [[Rcpp::export]]
NumericVector cp_run_cor(NumericMatrix mat, int nwindow) {
	int nc = mat.ncol();
	int nperiod = mat.nrow();
	NumericVector ret = NumericVector(Dimension(nc, nc, nperiod));
	fill_n(ret.begin(), ((0 + nwindow - 1) * nc * nc), NA_REAL);

	for (int i = 0; i < (nperiod - nwindow + 1); i++) {
		NumericMatrix cor = cp_cor_helper(mat, i, i + nwindow);
		std::copy(cor.begin(), cor.end(), ret.begin() + ((i + nwindow - 1) * nc * nc));
	}
	return ret;
}
{% endhighlight %}

Please save above code in the `correlation.cpp` file or download [correlation.cpp](/public/doc/correlation.cpp).

Please note that running window correlation requires lot's of memory to store results;
hence, you might want to add `--max-mem-size=8000M` to your `R` parameters.

Next let's make sure it produces same results.


{% highlight r %}
#*****************************************************************
# Run Rcpp Correlation
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


r.cor = rtest(ret, nwindow)
c.cor = c_run_cor(ret, nwindow)
cp.cor = cp_run_cor(ret, nwindow)
print(test.equality(r.cor, c.cor, cp.cor))
{% endhighlight %}



|item1 | item2 |equal |
|:-----|:------|:-----|
|r.cor |c.cor  |TRUE  |
|r.cor |cp.cor |TRUE  |
    

Please notice that we can optimize running window [correlation matrix](http://en.wikipedia.org/wiki/Correlation_and_dependence)
computations by initially pre-computing statistics and at each step just updating them.

Following Rcpp version implements this approach:

{% highlight cpp %}
//[correlation matrix](http://en.wikipedia.org/wiki/Correlation_and_dependence).
// n,sX,sY,sXY,sX2,sY2
// cor = ( n * sXY - sX * sY ) / ( sqrt(n * sX2 - sX^2) * sqrt(n * sY2 - sY^2) )

// [[Rcpp::export]]
NumericVector c_run_cor_smart(NumericMatrix mat, int nwindow) {
	int nc = mat.ncol();
	int nperiod = mat.nrow();
	NumericVector ret = NumericVector(Dimension(nc, nc, nperiod));
	fill_n(ret.begin(), ((0 + nwindow - 1) * nc * nc), NA_REAL);

	// pre compute first run
	NumericMatrix cor(nc, nc);
	NumericMatrix infoXY(nc, nc);

	vector<asset_info> info(nc);
	for (int c = 0; c < nc; c++)
		info[c] = compute_asset_info(mat, c, 0, nwindow);

	for (int c1 = 0; c1 < nc; c1++) {
		for (int c2 = 0; c2 < c1; c2++) {
			double sXY = 0;

			for (int r = 0; r < nwindow; r++)
				sXY += mat(r, c1) * mat(r, c2);

			infoXY(c1, c2) = sXY;
			cor(c1, c2) = (nwindow * sXY - info[c1].sum * info[c2].sum) / (info[c1].stdev * info[c2].stdev);
		}
	}
	std::copy(cor.begin(), cor.end(), ret.begin() + ((0 + nwindow - 1) * nc * nc));

	// for loop, append
	for (int i = 1; i < (nperiod - nwindow + 1); i++) {
		// update stats
		int rstart = i - 1; 
		int rend = i + nwindow - 1;
		for (int c = 0; c < nc; c++) {
			double d0 = mat(rstart, c);
			double d1 = mat(rend, c);
			info[c].sum += -d0 + d1;
			info[c].sum2 += -pow(d0, 2) + pow(d1, 2);
			info[c].stdev = sqrt(nwindow * info[c].sum2 - pow(info[c].sum, 2));
		}

		// compute cors
		for (int c1 = 0; c1 < nc; c1++) {
			double sX0 = mat(rstart, c1);
			double sX1 = mat(rend, c1);			
			for (int c2 = 0; c2 < c1; c2++) {
				double sY0 = mat(rstart, c2);
				double sY1 = mat(rend, c2);

				infoXY(c1, c2) += -sX0*sY0 + sX1*sY1;
				cor(c1, c2) = (nwindow * infoXY(c1, c2) - info[c1].sum * info[c2].sum) / (info[c1].stdev * info[c2].stdev);
			}
		}

		std::copy(cor.begin(), cor.end(), ret.begin() + ((i + nwindow - 1) * nc * nc));
	}
	return ret;
}
{% endhighlight %}

Please save above code in the `correlation.cpp` file or download [correlation.cpp](/public/doc/correlation.cpp).

Next let's implement same idea using [RcppParallel](http://rcppcore.github.io/RcppParallel/):

{% highlight cpp %}
// pre-compute sum and stdev
struct cor_smart_p1 : public Worker {
	const RMatrix<double> mat;
	int rstart, rend;
	const int nperiod;

	RVector<double> rsum, rsum2, rstdev;

	bool first_run;

	cor_smart_p1(const NumericMatrix& mat, const int rstart, const int rend,
		NumericVector rsum, NumericVector rsum2, NumericVector rstdev)
		: mat(mat), rstart(rstart), rend(rend), 
		nperiod(rend - rstart), rsum(rsum), rsum2(rsum2), rstdev(rstdev), first_run(true) { }

	void update(int i) {
		rstart = i;
		rend = rstart + nperiod;
		first_run = false;
	}
	void operator()(size_t begin, size_t end) {
		if (first_run)
			for (size_t c = begin; c < end; c++) {			
				double sum, sum2;
				sum = sum2 = 0;

				for (int r = rstart; r < rend; r++) {
					double d = mat(r, c);
					sum += d;
					sum2 += pow(d, 2);
				}

				rsum[c] = sum;
				rsum2[c] = sum2;
				rstdev[c] = sqrt(nperiod * sum2 - pow(sum, 2));
			} 
		else 
			for (size_t c = begin; c < end; c++) {
				double d0 = mat(rstart-1, c);
				double d1 = mat(rend-1, c);
				rsum[c] += -d0 + d1;
				rsum2[c] += -pow(d0, 2) + pow(d1, 2);
				rstdev[c] = sqrt(nperiod * rsum2[c] - pow(rsum[c], 2));
			}		
	}
};

// compute correlation
struct cor_smart_p2 : public Worker {
	const RMatrix<double> mat;
	int rstart, rend;
	const int nperiod;
	const RVector<double> sum, stdev;

	RMatrix<double> infoXY;
	RMatrix<double> rmat;

	bool first_run;

	cor_smart_p2(const NumericMatrix& mat, int rstart, int rend,
		const NumericVector& sum, const NumericVector& stdev,
		NumericMatrix infoXY, NumericMatrix rmat)
		: mat(mat), rstart(rstart), rend(rend), nperiod(rend - rstart), 
		sum(sum), stdev(stdev), infoXY(infoXY), rmat(rmat), first_run(true) { }

	void update(int i) {
		rstart = i;
		rend = rstart + nperiod;
		first_run = false;
	}
	void operator()(size_t begin, size_t end) {
		if (first_run)
			for (size_t c1 = begin; c1 < end; c1++) {
				for (size_t c2 = 0; c2 < c1; c2++) {
					double sXY = 0;
					for (int r = rstart; r < rend; r++)
						sXY += mat(r, c1) * mat(r, c2);

					infoXY(c1, c2) = sXY;
					rmat(c1, c2) = (nperiod * sXY - sum[c1] * sum[c2]) / (stdev[c1] * stdev[c2]);
				}
			}
		else
			for (size_t c1 = begin; c1 < end; c1++) {
				double sX0 = mat(rstart-1, c1);
				double sX1 = mat(rend-1, c1);
				for (size_t c2 = 0; c2 < c1; c2++) {
					double sY0 = mat(rstart-1, c2);
					double sY1 = mat(rend-1, c2);

					infoXY(c1, c2) += -sX0*sY0 + sX1*sY1;
					rmat(c1, c2) = (nperiod * infoXY(c1, c2) - sum[c1] * sum[c2]) / (stdev[c1] * stdev[c2]);
				}
			}
	}
};


// [[Rcpp::export]]
NumericVector cp_run_cor_smart(NumericMatrix mat, int nwindow) {
	int nc = mat.ncol();
	int nperiod = mat.nrow();
	NumericVector ret = NumericVector(Dimension(nc, nc, nperiod));
	fill_n(ret.begin(), ((0 + nwindow - 1) * nc * nc), NA_REAL);

	// pre compute first run
	NumericVector rsum(nc), rsum2(nc), rstdev(nc);

	cor_smart_p1 p1(mat, 0, nwindow, rsum, rsum2, rstdev);
	parallelFor(0, nc, p1);

	NumericMatrix cor(nc, nc);
	NumericMatrix infoXY(nc, nc);

	cor_smart_p2 p2(mat, 0, nwindow, rsum, rstdev, infoXY, cor);
	parallelFor(0, nc, p2);

	std::copy(cor.begin(), cor.end(), ret.begin() + ((0 + nwindow - 1) * nc * nc));

	// for loop, append
	for (int i = 1; i < (nperiod - nwindow + 1); i++) {
		// update stats
		p1.update(i);
		parallelFor(0, nc, p1);

		p2.update(i);
		parallelFor(0, nc, p2);

		std::copy(cor.begin(), cor.end(), ret.begin() + ((i + nwindow - 1) * nc * nc));
	}
	return ret;
}
{% endhighlight %}

Please save above code in the `correlation.cpp` file or download [correlation.cpp](/public/doc/correlation.cpp).


Next let's make sure it produces same results and compare the run times.


{% highlight r %}
#*****************************************************************
# Test on small example
#*****************************************************************
# load Rcpp functions
sourceCpp('correlation.cpp')


r.cor = rtest(ret, nwindow)
c.cor = c_run_cor(ret, nwindow)
cp.cor = cp_run_cor(ret, nwindow)
c.cor.smart = c_run_cor_smart(ret, nwindow)
cp.cor.smart = cp_run_cor_smart(ret, nwindow)

print(test.equality(r.cor, c.cor, cp.cor,c.cor.smart,cp.cor.smart))
{% endhighlight %}



|item1 | item2       |equal |
|:-----|:------------|:-----|
|r.cor |c.cor        |TRUE  |
|r.cor |cp.cor       |TRUE  |
|r.cor |c.cor.smart  |TRUE  |
|r.cor |cp.cor.smart |TRUE  |
    




{% highlight r %}
#*****************************************************************
# Test on large example
#*****************************************************************
load(file=filename)
n = ncol(prices)
nperiod = nrow(prices)
ret = prices / mlag(prices) - 1

index =  (nperiod-200):nperiod
ret = ret[index,]
nperiod = nrow(ret)
nwindow = 19


r.cor = rtest(ret, nwindow)
c.cor = c_run_cor(ret, nwindow)
cp.cor = cp_run_cor(ret, nwindow)
c.cor.smart = c_run_cor_smart(ret, nwindow)
cp.cor.smart = cp_run_cor_smart(ret, nwindow)

print(test.equality(r.cor,c.cor,cp.cor,c.cor.smart,cp.cor.smart))
{% endhighlight %}



|item1 | item2       |equal |
|:-----|:------------|:-----|
|r.cor |c.cor        |TRUE  |
|r.cor |cp.cor       |TRUE  |
|r.cor |c.cor.smart  |TRUE  |
|r.cor |cp.cor.smart |TRUE  |
    




{% highlight r %}
# free memory
env.del(spl('r.cor,c.cor,cp.cor,c.cor.smart,cp.cor.smart'), globalenv())
gc(T)


# compare performance
library(rbenchmark)
res <- benchmark(rtest(ret, nwindow),
			c_run_cor(ret, nwindow),
			cp_run_cor(ret, nwindow),
			c_run_cor_smart(ret, nwindow),
			cp_run_cor_smart(ret, nwindow),
			replications = 1,
			order="relative")
print(res[,1:4])
{% endhighlight %}



|rownames(x) |test                           | replications| elapsed| relative|
|:-----------|:------------------------------|------------:|-------:|--------:|
|5           |cp_run_cor_smart(ret, nwindow) |            1|    0.23|    1.000|
|3           |cp_run_cor(ret, nwindow)       |            1|    0.42|    1.826|
|4           |c_run_cor_smart(ret, nwindow)  |            1|    0.44|    1.913|
|2           |c_run_cor(ret, nwindow)        |            1|    0.67|    2.913|
|1           |rtest(ret, nwindow)            |            1|    3.03|   13.174|
    

The [RcppParallel](http://rcppcore.github.io/RcppParallel/) version is about 1.9 times faster.












*(this report was produced on: 2015-04-11)*
