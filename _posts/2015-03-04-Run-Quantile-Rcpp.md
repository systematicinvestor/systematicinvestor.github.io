---
layout: post
title: Run Quantile in Rcpp
comments: true
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.





In the [A 'Simple' Tactical Asset Allocation Portfolio with Percentile Channels (for Dummies)](https://cssanalytics.wordpress.com/2015/02/08/a-simple-tactical-asset-allocation-portfolio-with-percentile-channels-for-dummies/)
post, David Varadi discussed a Channel Breakout system that required to estimate moving window quantile.

This is a time consuming operation that can be speed up with runquantile function in caTools package.

Alternatively one can write a simple Rcpp function to speed up commutations If you are just starting 
with Rcpp, I found following resources extremely useful:

* [High performance functions with Rcpp chaoter in the Advanced R by Hadley Wickham](http://adv-r.had.co.nz/Rcpp.html)
* [Rcpp Gallery](http://gallery.rcpp.org/)

To get started on Windows you would need to install [Rtools](http://cran.r-project.org/bin/windows/Rtools/)

Below are sample functions to compute given quantile. The run_quantile0 function is basic and not
very efficient because it sorts all elements in each window. The second function, run_quantile
function takes advantage of previous order and only insert new element in the sorted array.

{% highlight cpp %}
#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::plugins(cpp11)]]

// quantile setup
struct quantile {
	int lo, hi, n, total;
	double hlo, hhi;
};
quantile make_quantile(int n, double prob) {
	quantile res;
	double index = (n - 1) * prob;
	res.lo = floor(index);
	res.hi = res.lo + 1;
	res.hhi = index - res.lo;
	res.hlo = 1 - res.hhi;
	return res;	
}

// index vector
vector<int> make_seq(int n) {
	vector<int> id(n);
	iota(id.begin(), id.end(), 0);
	return id;
}

// [[Rcpp::export]]
NumericVector run_quantile0(NumericVector x, int n, double prob) {
	auto sz = x.size();
	NumericVector res(sz);	
	
	// quantile setup
	auto q = make_quantile(n, prob);

	for(int i = 0; i < (sz-n+1); i++) {
		// can be made a lot faster by not re-sorting each time
		vector<double> z(&x[i], &x[i+n]);
		sort(z.begin(), z.end());    	
		res[i+n-1] = q.hlo * z[q.lo] + q.hhi * z[q.hi];  
	}
    
	// pad the first n-1 elements with NA
	fill(res.begin(), res.end()-sz+n-1, NA_REAL);
	return res;	
}

// [[Rcpp::export]]
NumericVector run_quantile(NumericVector x, int n, double prob) {
	auto sz = x.size();
	NumericVector res(sz);	
	
	// quantile setup
	auto q = make_quantile(n, prob);

	// index vector
	auto id = make_seq(n);
	
	for(int i = 0; i < (sz-n+1); i++) {
		if(i == 0)
			sort(id.begin(), id.end(), 
				[&](int a, int b) { return x[a] < x[b]; });
		else {
	    		// remove index (i-1)
		    	id.erase(find(id.begin(), id.end(), i-1));
		    	// insert keeping sorted order
	    		id.insert(lower_bound(id.begin(), id.end(), i+n-1, 
	    			[&](int a, int b) { return x[a] < x[b]; }), i+n-1);
		}    
		
		res[i+n-1] = q.hlo * x[id[q.lo]] + q.hhi * x[id[q.hi]];  
	}
    
	// pad the first n-1 elements with NA
	fill(res.begin(), res.end()-sz+n-1, NA_REAL);
	return res;	
}
{% endhighlight %}

Please save above code in the `quantile.cpp` file or download [quantile.cpp](/public/doc/quantile.cpp).

Next let's check for correctness and speed. 


{% highlight r %}
#*****************************************************************
# Rcpp Run Quantile
#*****************************************************************
# make sure to install Rtools on windows
# http://cran.r-project.org/bin/windows/Rtools/
library(SIT)
load.packages('quantmod')

load.packages('Rcpp')

# load run quantile functions
sourceCpp('quantile.cpp')

#*****************************************************************
# Test functionality and speed
#*****************************************************************
# inefficient R implementation
run_quantileR = function(x, n, probs) {
	out = c( rep(NA,(n-1)), sapply(n:len(x), function(i) quantile(x[(i- (n-1) ):i], probs) ))
	as.vector(out)
}	


# basic test
load.packages('microbenchmark')

n = 10
probs = 0.5
x = runif(100)

test1 = run_quantileR(x, n, probs)
test2 = run_quantile0(x, n, probs)
test3 = run_quantile(x, n, probs)

print(all.equal(test1, test2))
{% endhighlight %}



TRUE
    




{% highlight r %}
print(all.equal(test1, test3))
{% endhighlight %}



TRUE
    




{% highlight r %}
print(to.nice(summary(microbenchmark(
	run_quantileR(x, n, probs),
	run_quantile0(x, n, probs),
	run_quantile(x, n, probs),
	times = 10
)),0))
{% endhighlight %}



|   |expr                       |min    |lq     |mean   |median |uq     |max    |neval  |
|:--|:--------------------------|:------|:------|:------|:------|:------|:------|:------|
|1  |run_quantileR(x, n, probs) |13,666 |13,963 |14,566 |14,261 |15,060 |16,278 |    10 |
|2  |run_quantile0(x, n, probs) |    40 |    41 |    50 |    54 |    57 |    61 |    10 |
|3  |run_quantile(x, n, probs)  |    13 |    13 |    20 |    17 |    30 |    34 |    10 |
    

Second implementation, `run_quantile`, really shines over larger look back window


{% highlight r %}
n = 1000
probs = 0.9
x = runif(100000)

test1 = run_quantile0(x, n, probs)
test2 = run_quantile(x, n, probs)

print(all.equal(test1, test2))
{% endhighlight %}



TRUE
    




{% highlight r %}
print(to.nice(summary(microbenchmark(
	run_quantile0(x, n, probs),
	run_quantile(x, n, probs),
	times = 1
)),0))
{% endhighlight %}



|   |expr                       |min   |lq    |mean  |median |uq    |max   |neval |
|:--|:--------------------------|:-----|:-----|:-----|:------|:-----|:-----|:-----|
|1  |run_quantile0(x, n, probs) |5,570 |5,570 |5,570 |5,570  |5,570 |5,570 |    1 |
|2  |run_quantile(x, n, probs)  |   82 |   82 |   82 |   82  |   82 |   82 |    1 |
    

To be continued.


*(this report was produced on: 2015-03-07)*
