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