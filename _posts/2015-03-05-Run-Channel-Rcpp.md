---
layout: post
title: Run Channel in Rcpp
comments: true
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.





Let's continue with [Run Quantile in Rcpp](/Run-Quantile-Rcpp) post and implement
another concept David Varadi discussed in the [Conditional Percentile Channels](https://cssanalytics.wordpress.com/2015/02/20/conditional-percentile-channels/)
post.

Below is a Rcpp implementation of [Conditional Percentile Channels](https://cssanalytics.wordpress.com/2015/02/20/conditional-percentile-channels/)

{% highlight cpp %}
#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::plugins(cpp11)]]

// quantile setup
struct quantile {
	int lo, hi, n;
	double hlo, hhi;
};
quantile make_quantile(int n, double prob, bool high) {
	quantile res;
	double index = (n - 1) * prob;
	res.lo = floor(index);
	res.hi = res.lo + 1;
	res.hhi = index - res.lo;
	res.hlo = 1 - res.hhi;
	
	res.n = high ? n - res.hi : res.hi;
	return res;	
}

// index vector
vector<int> make_seq(int n) {
	vector<int> id(n);
	iota(id.begin(), id.end(), 0);
	return id;
}

// compute weighted average
double avg_quantile(vector<int> id, vector<int> id1, quantile q, 
	NumericVector x, int n, bool high
) {
	double temp, total; int j, k;
		
	// sort id
	for(k = 0; k < q.n; k++)
		id1[k] = k;

	int start = high ? q.hi : 0;
	int end = high ? n : q.n;

	sort(id1.begin(), id1.end(), 
		[&](int a, int b) { return id[a + start] < id[b + start]; });

	for(j = start, k =0, temp=0.0, total=0.0; j < end; j++, k++) {
		temp += x[id[j]] * (id1[k]+1) * (j - start + 1);
		total += (id1[k]+1) * (j - start + 1);
	}
	    	
	return temp  / total;	
}

// [[Rcpp::export]]
NumericVector run_quantile_weight(NumericVector x, int n, double low_prob, double high_prob) {
	auto sz = x.size();
	NumericMatrix res(sz, 2);
	
	// quantile setup
	auto qlo = make_quantile(n, low_prob, false);
	auto qhi = make_quantile(n, high_prob, true);

	// index vector
	auto id = make_seq(n);
	auto idlo = make_seq(qlo.n);
	auto idhi = make_seq(qhi.n);
				
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
        	
		res(i+n-1,0) =  avg_quantile(id, idlo, qlo, x, n, false);
		res(i+n-1,1) =  avg_quantile(id, idhi, qhi, x, n, true);
	}
    
	// pad the first n-1 elements with NA
	for (int i = 0; i < (n-1); i++) 
		res(i,0) = res(i,1) = NA_REAL;

	return res;	
}
{% endhighlight %}

Please save above code in the `channel.cpp` file or download [channel.cpp](/public/doc/channel.cpp).

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
sourceCpp('channel.cpp')

#*****************************************************************
# Inefficient R implementation
#*****************************************************************
run_quantile_weightR = function(x, n, low.prob, high.prob) {
	hi = floor((n-1) * high.prob) + 1 + 1
	hi.index = (hi:n) - (hi - 1)
	
	lo = floor((n-1) * low.prob)+ 1
	lo.index = 1:lo

	out = cbind( matrix(NA,nr=2,nc=n-1), sapply(n:len(x), function(i) {
		temp = x[(i- (n-1) ):i]
		index = sort.list(temp)

		# high		
		index1 = sort.list(index[hi:n])		
		h = sum(temp[index[hi:n]] * hi.index * index1) / sum(hi.index * index1)
		
		# low
		index1 = sort.list(index[1:lo])		
		l = sum(temp[index[1:lo]] * lo.index * index1) / sum(lo.index * index1)
		c(l,h)
	}
	))
	t(out)
}	

#*****************************************************************
# Test David's example
#*****************************************************************
print.helper = function(stats) {
	colnames(stats) = spl('low,high')
	print(stats[nrow(stats),,drop=F])
}

x = c(201:215,117,115,119,118,121)
n = len(x)
low.prob = 0.25
high.prob = 0.75
stats = run_quantile_weightR(x, n, low.prob, high.prob)
	print.helper(stats)
{% endhighlight %}



|      low|     high|
|--------:|--------:|
| 119.4906| 214.0909|
    




{% highlight r %}
stats = run_quantile_weight(x, n, low.prob, high.prob)
	print.helper(stats)
{% endhighlight %}



|      low|     high|
|--------:|--------:|
| 119.4906| 214.0909|
    




{% highlight r %}
x = c(1:15,117,115,119,118,121)
stats = run_quantile_weightR(x, n, low.prob, high.prob)
	print.helper(stats)
{% endhighlight %}



|      low|     high|
|--------:|--------:|
| 4.090909| 119.4906|
    




{% highlight r %}
stats = run_quantile_weight(x, n, low.prob, high.prob)
	print.helper(stats)
{% endhighlight %}



|      low|     high|
|--------:|--------:|
| 4.090909| 119.4906|
    




{% highlight r %}
#*****************************************************************
# Benchmark
#*****************************************************************
load.packages('microbenchmark')

n = 10
low.prob = 0.25
high.prob = 0.75
x = runif(100)

test1 = run_quantile_weightR(x, n, low.prob, high.prob)
test2 = run_quantile_weight(x, n, low.prob, high.prob)

print(all.equal(test1, test2))
{% endhighlight %}



TRUE
    




{% highlight r %}
print(to.nice(summary(microbenchmark(
	run_quantile_weightR(x, n, low.prob, high.prob),
	run_quantile_weight(x, n, low.prob, high.prob),
	times = 10
)),0))
{% endhighlight %}



|   |expr                                            |min    |lq     |mean   |median |uq     |max    |neval  |
|:--|:-----------------------------------------------|:------|:------|:------|:------|:------|:------|:------|
|1  |run_quantile_weightR(x, n, low.prob, high.prob) |10,911 |11,001 |11,657 |11,242 |12,256 |13,904 |    10 |
|2  |run_quantile_weight(x, n, low.prob, high.prob)  |    99 |   100 |   111 |   104 |   121 |   140 |    10 |
    

To be continued.


*(this report was produced on: 2015-03-07)*
