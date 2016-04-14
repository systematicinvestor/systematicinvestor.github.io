---
layout: post
title: Rcpp and RcppParallel Examples
comments: true
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.





Following are few examples of using Rcpp and RcppParallel. I found following
resources very helpful:

* [Rcpp Gallery](http://gallery.rcpp.org/)
* [RcppParallel help site](http://rcppcore.github.io/RcppParallel/)
* [RcppParallel source code](https://github.com/RcppCore/RcppParallel)
* [RcppParallel introduction](http://dirk.eddelbuettel.com/blog/2014/07/16/#introducing_rcppparallel)
* [RcppParallel at Rcpp gallery](http://gallery.rcpp.org/articles/parallel-distance-matrix/)

To get started on Windows you would need to install [Rtools](http://cran.r-project.org/bin/windows/Rtools/)

{% highlight cpp %}
#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
using namespace RcppParallel;

/*------ Example of proper way to pass by reference ------*/
//http://stackoverflow.com/questions/26234055/cohabitation-of-rcpparmadillo-and-rcppparallel
struct p_test_vector : public Worker {
	vector<double>& test_vec;
	p_test_vector(vector<double>& test_vec) : test_vec(test_vec) {}

	void operator()(size_t begin, size_t end) {
		for (size_t c1 = begin; c1 < end; c1++)
			test_vec[c1] = c1;
	}
};
// [[Rcpp::export]]
NumericVector cp_test_vector(int n) {
	vector<double> test(n);
	p_test_vector p(test);

	parallelFor(0, n, p);
	return wrap(test);
}

/*------ Example of copying multi-dimensional matrices ------*/
//http://lists.r-forge.r-project.org/pipermail/rcpp-devel/2011-September/002847.html
//http://lists.r-forge.r-project.org/pipermail/rcpp-devel/2013-August/006309.html
// [[Rcpp::export]]
NumericVector c_test_multi_array(NumericMatrix mat, int nwindow) {
	int nc = mat.ncol();
	int nperiod = mat.nrow();
	NumericVector ret = NumericVector(Dimension(nc, nc, nperiod));
	fill_n(ret.begin(), ((0 + nwindow - 1) * nc * nc), NA_REAL);

	for (int i = 0; i < (nperiod - nwindow + 1); i++) {
		NumericMatrix m(nc, nc);
		fill(m.begin(), m.end(), i);		
		copy(m.begin(), m.end(), ret.begin() + ((i + nwindow - 1) * nc * nc));
	}
	return ret;
}

/*------ Example of allocating list of matrices ------*/
//http://stackoverflow.com/questions/21663256/how-to-initialize-a-vector-of-vectors
// [[Rcpp::export]]
List c_test_list(int n) {
	vector<vector<double>> test(n, vector<double>(2*n));
	return wrap(test);
}

// [[Rcpp::export]]
List c_test_list1(int n) {
	vector<NumericMatrix> test(n, NumericMatrix(2 * n, 2 *n));
	return wrap(test);
}

/*** R

cp_test_vector(100)
c_test_multi_array(matrix(runif(20),nr=5), 3)

c_test_list(3)
c_test_list1(3)

*/
{% endhighlight %}

Please save above code in the `Rcpp_RcppParallel_examples.cpp` file or download [Rcpp_RcppParallel_examples.cpp](/public/doc/Rcpp_RcppParallel_examples.cpp).


{% highlight r %}
#*****************************************************************
# Rcpp Correlation
#*****************************************************************
# make sure to install [Rtools on windows](http://cran.r-project.org/bin/windows/Rtools/)
library(Rcpp)
library(RcppParallel)

# load functions
sourceCpp('Rcpp_RcppParallel_examples.cpp')
{% endhighlight %}


> cp_test_vector(100)
  [1]  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22
 [24] 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45
 [47] 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68
 [70] 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91
 [93] 92 93 94 95 96 97 98 99

> c_test_multi_array(matrix(runif(20), nr = 5), 3)
, , 1

     [,1] [,2] [,3] [,4]
[1,]   NA   NA   NA   NA
[2,]   NA   NA   NA   NA
[3,]   NA   NA   NA   NA
[4,]   NA   NA   NA   NA

, , 2

     [,1] [,2] [,3] [,4]
[1,]   NA   NA   NA   NA
[2,]   NA   NA   NA   NA
[3,]   NA   NA   NA   NA
[4,]   NA   NA   NA   NA

, , 3

     [,1] [,2] [,3] [,4]
[1,]    0    0    0    0
[2,]    0    0    0    0
[3,]    0    0    0    0
[4,]    0    0    0    0

, , 4

     [,1] [,2] [,3] [,4]
[1,]    1    1    1    1
[2,]    1    1    1    1
[3,]    1    1    1    1
[4,]    1    1    1    1

, , 5

     [,1] [,2] [,3] [,4]
[1,]    2    2    2    2
[2,]    2    2    2    2
[3,]    2    2    2    2
[4,]    2    2    2    2


> c_test_list(3)
[[1]]
[1] 0 0 0 0 0 0

[[2]]
[1] 0 0 0 0 0 0

[[3]]
[1] 0 0 0 0 0 0


> c_test_list1(3)
[[1]]
     [,1] [,2] [,3] [,4] [,5] [,6]
[1,]    0    0    0    0    0    0
[2,]    0    0    0    0    0    0
[3,]    0    0    0    0    0    0
[4,]    0    0    0    0    0    0
[5,]    0    0    0    0    0    0
[6,]    0    0    0    0    0    0

[[2]]
     [,1] [,2] [,3] [,4] [,5] [,6]
[1,]    0    0    0    0    0    0
[2,]    0    0    0    0    0    0
[3,]    0    0    0    0    0    0
[4,]    0    0    0    0    0    0
[5,]    0    0    0    0    0    0
[6,]    0    0    0    0    0    0

[[3]]
     [,1] [,2] [,3] [,4] [,5] [,6]
[1,]    0    0    0    0    0    0
[2,]    0    0    0    0    0    0
[3,]    0    0    0    0    0    0
[4,]    0    0    0    0    0    0
[5,]    0    0    0    0    0    0
[6,]    0    0    0    0    0    0



To be continued...


*(this report was produced on: 2015-04-19)*
