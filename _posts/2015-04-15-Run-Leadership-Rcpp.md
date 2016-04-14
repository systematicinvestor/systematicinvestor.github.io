---
layout: post
title: Run Leadership in Rcpp
comments: true
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](',base.url,'about) page.




Let's continue with [Run Correlation in Rcpp](/Run-Correlation-Rcpp) post and implement
running window Leadership score based on lead-lag relationship [Detecting Leaders from Correlated Time Series D. Wu, Y. Ke, J. Yu, P. Yu, L. Chen]

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
  getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
  
	rm.index = which(sapply(ls(data), function(i) is.null(data[[i]])))
	if(any(rm.index)) env.del(names(rm.index), data)
  
  for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
  #print(bt.start.dates(data))

  bt.prep(data, align='keep.all', dates='2000::', fill.gaps=T)

  # remove ones with little history
  prices = data$prices
  bt.prep.remove.symbols(data, which(
   	count(prices, side = 2) < 3*252 | is.na(last(prices)) 
  ))
       
  # show the ones removed
  print(setdiff(tickers,names(data$prices)))

  prices = data$prices
  save(prices, file=filename)
}

load(file=filename)

#*****************************************************************
# Run Lead Lag Correlation
#*****************************************************************
prices = prices[,1:5]

n = ncol(prices)
nperiod = nrow(prices)
ret = prices / mlag(prices) - 1

index =  (nperiod-20):nperiod
ret = ret[index,]
nperiod = nrow(ret)
nwindow = 15

# lead - lag sample function
rtest = function(ret, nwindow) {
	nlag = floor(nwindow/2)
	n = ncol(ret)
	nperiod = nrow(ret)
	out = array(0, c(n,n,nperiod))
	index = !lower.tri(out[,,1])

	for(i in nwindow:nperiod) {
		temp = coredata(ret[(i-nwindow+1):i,])
		temp.out = matrix(0, n,n)
		
		for (c1 in 2:n) {
			data1 = temp[,c1]
			c2.index = 1:(c1-1)
			data2 = temp[,c2.index,drop=F]

			# if c1.cor : c2 -> c1 i.e. cor(data1[(lag+1):nwindow], data2[1:(nwindow-lag),])
			c1.cor = rep(0,ncol(data2))
			for (lag in 0:nlag) {
				cor.temp = cor(data1[(lag+1):nwindow], data2[1:(nwindow-lag),])
				c1.cor = c1.cor + pmax(cor.temp, 0)
			}
			c1.cor = c1.cor / (nlag + 1)
			
			# if c2.cor : c1 -> c2 i.e. cor(data1[1:(nwindow-lag)], data2[(lag+1):nwindow,])
			c2.cor = rep(0,ncol(data2))
			for (lag in 1:nlag) {
				cor.temp = cor(data1[1:(nwindow-lag)], data2[(lag+1):nwindow,])
				c2.cor = c2.cor + pmax(cor.temp, 0)
			}
			c2.cor = c2.cor / nlag
			
			# if c1.cor >= c2.cor hence c2 -> c1 i.e. cor(data1[(lag+1):nwindow], data2[1:(nwindow-lag),])
			index = c1.cor >= c2.cor
			if(any(index))
				temp.out[c2.index[index], c1] = c1.cor[index]
				
			# if c2.cor >= c1.cor hence c1 -> c2 i.e. cor(data1[1:(nwindow-lag)], data2[(lag+1):nwindow,])
			index = !index
			if(any(index))
				temp.out[c1, c2.index[index]] = c2.cor[index]							
		}			
		out[,,i] = temp.out
	}
	out
}

r.cor = rtest(ret, nwindow)
{% endhighlight %}

Next, let's implement a function using Rcpp. I will re-use code from
[Run Correlation in Rcpp](/Run-Correlation-Rcpp) post.

{% highlight cpp %}
#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::plugins(cpp11)]]


// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
using namespace RcppParallel;


/*------ pre-compute sum and stdev ------*/
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
	void do_first_run(size_t begin, size_t end) {
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
	}
	void do_update(size_t begin, size_t end) {
		for (size_t c = begin; c < end; c++) {
			double d0 = mat(rstart - 1, c);
			double d1 = mat(rend - 1, c);
			rsum[c] += -d0 + d1;
			rsum2[c] += -pow(d0, 2) + pow(d1, 2);
			rstdev[c] = sqrt(nperiod * rsum2[c] - pow(rsum[c], 2));
		}
	}
	void operator()(size_t begin, size_t end) {
		if (first_run)
			do_first_run(begin, end);
		else
			do_update(begin, end);
	}
};


/*------ Running Leadership ------*/
// compute correlation
struct leadership_smart_p2 : public Worker {
	const RMatrix<double> mat;
	int rstart, rend;
	const int nperiod, nc;
	const RVector<double> sum, sum2, stdev;

	vector<vector<double>>& infoXY;
	RMatrix<double> rmat;

	bool first_run;
	int nlag;

	vector<double> cor_mat;

	leadership_smart_p2(const NumericMatrix& mat, int rstart, int rend,
		const NumericVector& sum, const NumericVector& sum2, const NumericVector& stdev,
		vector<vector<double>>& infoXY, NumericMatrix rmat)
		: mat(mat), rstart(rstart), rend(rend), nperiod(rend - rstart), nc(mat.ncol()),
		sum(sum), sum2(sum2), stdev(stdev), infoXY(infoXY), rmat(rmat),
		first_run(true), nlag(floor(nperiod/2))
	{ }

	void update(int i) {
		rstart = i;
		rend = rstart + nperiod;
		first_run = false;
	}

	// first run: pre-compute all correlations
	void do_first_run(size_t begin, size_t end) {
		int xyi = floor(begin * (begin - 1) / 2);
		for (size_t c1 = begin; c1 < end; c1++) {
			for (size_t c2 = 0; c2 < c1; c2++, xyi++) {
				for (int lag = 0; lag <= nlag; lag++) {
					double sXY = 0;
					for (int r1 = rstart + lag, r2 = rstart; r1 < rend; r1++, r2++) {
						sXY += mat(r1, c1) * mat(r2, c2); // r1 always ends at rend
					}
					infoXY[lag][xyi] = sXY;
				}

				for (int lag = nlag+1; lag <= 2*nlag; lag++) {
					double sXY = 0;
					for (int r1 = rstart + lag - nlag, r2 = rstart; r1 < rend; r1++, r2++) {
						sXY += mat(r2, c1) * mat(r1, c2); // r1 always ends at rend
					}
					infoXY[lag][xyi] = sXY;
				}
			}
		}
	}

	// update run: use pre-compute correlations and sums to compute stats
	void do_update(size_t begin, size_t end) {
		int xyi = floor(begin * (begin - 1) / 2);
		for (size_t c1 = begin; c1 < end; c1++) {
			for (size_t c2 = 0; c2 < c1; c2++, xyi++) {
				double c1cor, c2cor;
				double r1sum, r1sum2, r1stdev;
				double r2sum, r2sum2, r2stdev;
				r1sum = sum[c1]; r1sum2 = sum2[c1];
				r2sum = sum[c2]; r2sum2 = sum2[c2];

if (!first_run) infoXY[0][xyi] += -mat(rstart - 1, c1) * mat(rstart - 1, c2) + mat(rend - 1, c1) * mat(rend - 1, c2);

				r1stdev = sqrt(nperiod * r1sum2 - pow(r1sum, 2));
				r2stdev = sqrt(nperiod * r2sum2 - pow(r2sum, 2));
				double temp = (nperiod * infoXY[0][xyi] - r1sum * r2sum) / (r1stdev * r2stdev);
				c1cor = max(temp, 0.0);
				
				int r20 = rstart - 1;
				int r11 = rend - 1;

				for (int lag = 1; lag <= nlag; lag++) {
					int r10 = rstart - 1 + lag;
					int r21 = rend - 1 - lag;
if (!first_run) infoXY[lag][xyi] += -mat(r10, c1) * mat(r20, c2) + mat(r11, c1) * mat(r21, c2);

					double d1 = mat(r10, c1);
					double d2 = mat(r21 + 1, c2);
					r1sum -= d1;
					r2sum -= d2;
					r1sum2 -= pow(d1, 2);
					r2sum2 -= pow(d2, 2);
					int nperiod1 = nperiod - lag;
					r1stdev = sqrt(nperiod1 * r1sum2 - pow(r1sum, 2));
					r2stdev = sqrt(nperiod1 * r2sum2 - pow(r2sum, 2));

					temp = (nperiod1 * infoXY[lag][xyi] - r1sum * r2sum) / (r1stdev * r2stdev);
					c1cor += max(temp, 0.0);
				}

				r1sum = sum[c1]; r1sum2 = sum2[c1];
				r2sum = sum[c2]; r2sum2 = sum2[c2];
				c2cor = 0;
				for (int lag = nlag + 1; lag <= 2 * nlag; lag++) {
					int r10 = rstart - 1 + lag - nlag;
					int r21 = rend - 1 - (lag - nlag);
if (!first_run) infoXY[lag][xyi] += -mat(r20, c1) * mat(r10, c2) + mat(r21, c1) * mat(r11, c2);

					double d1 = mat(r21 + 1, c1);
					double d2 = mat(r10, c2);
					r1sum -= d1;
					r2sum -= d2;
					r1sum2 -= pow(d1, 2);
					r2sum2 -= pow(d2, 2);
					int nperiod1 = nperiod - (lag - nlag);
					r1stdev = sqrt(nperiod1 * r1sum2 - pow(r1sum, 2));
					r2stdev = sqrt(nperiod1 * r2sum2 - pow(r2sum, 2));

					temp = (nperiod1 * infoXY[lag][xyi] - r1sum * r2sum) / (r1stdev * r2stdev);
					c2cor += max(temp, 0.0);
				}

				c1cor = c1cor / (nlag + 1);
				c2cor = c2cor / nlag;

				if (c1cor >= c2cor) {
					rmat(c2, c1) = c1cor;
					rmat(c1, c2) = 0.0;
				} else {
					rmat(c1, c2) = c2cor;
					rmat(c2, c1) = 0.0;
				}
			}
		}
	}

	// main entry
	void operator()(size_t begin, size_t end) {
		if (first_run) 
			do_first_run(begin, end);			
		do_update(begin, end);		
	}

};

// [[Rcpp::export]]
NumericVector cp_run_leadership_smart(NumericMatrix mat, int nwindow, bool runInParallel) {
	//bool runInParallel = true;
	int nc = mat.ncol();
	int nperiod = mat.nrow();
	NumericVector ret = NumericVector(Dimension(nc, nc, nperiod));
		fill_n(ret.begin(), ((0 + nwindow - 1) * nc * nc), 0.0);

	// pre-compute first run
	NumericVector rsum(nc), rsum2(nc), rstdev(nc);
	cor_smart_p1 p1(mat, 0, nwindow, rsum, rsum2, rstdev);
	if(runInParallel)
		parallelFor(0, nc, p1);
	else
		p1.do_first_run(0, nc);

	NumericMatrix cor(nc, nc);
		fill(cor.begin(), cor.end(), 0.0);
	int nlag = floor(nwindow / 2);

	vector<vector<double>> infoXY(2*nlag + 1, vector<double>( floor((nc-1)*nc / 2) ));
	leadership_smart_p2 p2(mat, 0, nwindow, rsum, rsum2, rstdev, infoXY, cor);

	if (runInParallel)
		parallelFor(1, nc, p2);
	else {
		p2.do_first_run(1, nc);
		p2.do_update(1, nc);
	}

	std::copy(cor.begin(), cor.end(), ret.begin() + ((0 + nwindow - 1) * nc * nc));

	// for loop and append
	for (int i = 1; i < (nperiod - nwindow + 1); i++) {
		p1.update(i);
		if (runInParallel)
			parallelFor(0, nc, p1);
		else
			p1.do_update(0, nc);

		p2.update(i);
		if (runInParallel)
			parallelFor(1, nc, p2);
		else
			p2.do_update(1, nc);

		std::copy(cor.begin(), cor.end(), ret.begin() + ((i + nwindow - 1) * nc * nc));
	}

	return ret;
}
{% endhighlight %}

Please save above code in the `lead.lag.correlation.cpp` file or download [lead.lag.correlation.cpp](/public/doc/lead.lag.correlation.cpp).

Please note that above code requires lot's of memory to store results;
hence, you might want to add `--max-mem-size=8000M` to your `R` parameters.

Next let's make sure it produces same results and compare the run times.


{% highlight r %}
#*****************************************************************
# Run Lead Lag Correlation
#*****************************************************************
# make sure to install [Rtools on windows](http://cran.r-project.org/bin/windows/Rtools/)
load.packages('Rcpp')
load.packages('RcppParallel')
# [RcppParallel help site](http://rcppcore.github.io/RcppParallel/)
# [RcppParallel source code](https://github.com/RcppCore/RcppParallel)
# [RcppParallel introduction](http://dirk.eddelbuettel.com/blog/2014/07/16/#introducing_rcppparallel)
# [RcppParallel gallery](http://gallery.rcpp.org/articles/parallel-distance-matrix/)
# defaultNumThreads()
#[High performance functions with Rcpp](http://adv-r.had.co.nz/Rcpp.html)

#*****************************************************************
# Test on small example
#*****************************************************************
# load Rcpp functions
sourceCpp('lead.lag.correlation.cpp')

r.cor = rtest(ret, nwindow)
c.cor = cp_run_leadership_smart(ret, nwindow, T)

print(test.equality(r.cor, c.cor))
{% endhighlight %}



|item1 | item2 |equal |
|:-----|:------|:-----|
|r.cor |c.cor  |TRUE  |
    




{% highlight r %}
print(max(abs(r.cor - c.cor), na.rm=T))
{% endhighlight %}



1.11022302462516e-16
    




{% highlight r %}
# compare performance
library(rbenchmark)
res <- benchmark(rtest(ret, nwindow),
			cp_run_leadership_smart(ret, nwindow,T),
			cp_run_leadership_smart(ret, nwindow,F),
			replications = 2,
			order="relative")
print(res[,1:4])
{% endhighlight %}



|rownames(x) |test                                     | replications| elapsed|relative |
|:-----------|:----------------------------------------|------------:|-------:|:--------|
|1           |rtest(ret, nwindow)                      |            2|    0.08|         |
|2           |cp_run_leadership_smart(ret, nwindow, T) |            2|    0.00|         |
|3           |cp_run_leadership_smart(ret, nwindow, F) |            2|    0.00|         |
    




{% highlight r %}
#*****************************************************************
# Test on large example
#*****************************************************************
load(file=filename)
prices = prices[,1:25]
n = ncol(prices)
nperiod = nrow(prices)
ret = prices / mlag(prices) - 1

index =  (nperiod-100):nperiod
ret = ret[index,]
nperiod = nrow(ret)
nwindow = 19

r.cor = rtest(ret, nwindow)
c.cor = cp_run_leadership_smart(ret, nwindow, T)

print(test.equality(r.cor, c.cor))
{% endhighlight %}



|item1 | item2 |equal |
|:-----|:------|:-----|
|r.cor |c.cor  |TRUE  |
    




{% highlight r %}
print(max(abs(r.cor - c.cor), na.rm=T))
{% endhighlight %}



1.02695629777827e-15
    




{% highlight r %}
# compare performance
library(rbenchmark)
res <- benchmark(rtest(ret, nwindow),
			cp_run_leadership_smart(ret, nwindow,T),
			cp_run_leadership_smart(ret, nwindow,F),
			replications = 2,
			order="relative")
print(res[,1:4])
{% endhighlight %}



|rownames(x) |test                                     | replications| elapsed|relative |
|:-----------|:----------------------------------------|------------:|-------:|:--------|
|1           |rtest(ret, nwindow)                      |            2|    8.26|         |
|2           |cp_run_leadership_smart(ret, nwindow, T) |            2|    0.00|         |
|3           |cp_run_leadership_smart(ret, nwindow, F) |            2|    0.01|         |
    




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

c.cor.F = cp_run_leadership_smart(ret, nwindow, F)
c.cor.T = cp_run_leadership_smart(ret, nwindow, T)

print(test.equality(c.cor.T, c.cor.F))
{% endhighlight %}



|item1   | item2  |equal |
|:-------|:-------|:-----|
|c.cor.T |c.cor.F |TRUE  |
    




{% highlight r %}
print(max(abs(c.cor.T - c.cor.F), na.rm=T))
{% endhighlight %}



0
    




{% highlight r %}
# compare performance
library(rbenchmark)
res <- benchmark(
			cp_run_leadership_smart(ret, nwindow,T),
			cp_run_leadership_smart(ret, nwindow,F),
			replications = 1,
			order="relative")
print(res[,1:4])
{% endhighlight %}



|rownames(x) |test                                     | replications| elapsed| relative|
|:-----------|:----------------------------------------|------------:|-------:|--------:|
|1           |cp_run_leadership_smart(ret, nwindow, T) |            1|    2.03|    1.000|
|2           |cp_run_leadership_smart(ret, nwindow, F) |            1|    9.71|    4.783|
    

The [RcppParallel](http://rcppcore.github.io/RcppParallel/) version is about 4 times faster.














*(this report was produced on: 2015-04-23)*
