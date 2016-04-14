#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::plugins(cpp11)]]


// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
using namespace RcppParallel;


//[Standard deviation](http://mathforum.org/library/drmath/view/71724.html)
// sqrt((sX2 - sX^2/n)/(n-1))
struct run_sd_helper : public Worker {
	// input matrix to read from
	const RMatrix<double> mat;

	// internal variables
	int rstart, rend;
	const int nwindow, nwindow1;
	RVector<double> sum, sum2;

	// output matrix to write to
	RMatrix<double> rmat;

	// flag to indicate initialization
	bool first_run;

	// initialize from Rcpp input and output matrixes
	run_sd_helper(const NumericMatrix& mat, int rstart, int rend,
		NumericVector sum, NumericVector sum2, NumericMatrix rmat)
		: mat(mat), rstart(rstart), rend(rend), nwindow(rend - rstart), nwindow1(nwindow-1),
		sum(sum), sum2(sum2), rmat(rmat), first_run(true) { }

	// helper function to shift one period in time
	void update(int i) {
		rstart = i;
		rend = rstart + nwindow;
		first_run = false;
	}

	// do initialization, pre-compute sum and sum2
	void do_first_run(size_t begin, size_t end) {
		for (size_t c1 = begin; c1 < end; c1++) {
			double dsum, dsum2;
			dsum = dsum2 = 0;

			for (int r = rstart; r < rend; r++) {
				double d = mat(r, c1);
				dsum += d;
				dsum2 += pow(d, 2);
			}

			sum[c1] = dsum;
			sum2[c1] = dsum2;
			rmat(rend - 1, c1) = sqrt((dsum2 - pow(dsum, 2) / nwindow) / nwindow1);
		}
	}

	// do incremental update, no need to re-compute
	void do_update(size_t begin, size_t end) {
		for (size_t c1 = begin; c1 < end; c1++) {
			double d0 = mat(rstart - 1, c1);
			double d1 = mat(rend - 1, c1);
			sum[c1] += -d0 + d1;
			sum2[c1] += -pow(d0, 2) + pow(d1, 2);
			rmat(rend - 1, c1) = sqrt((sum2[c1] - pow(sum[c1], 2) / nwindow) / nwindow1);
		}
	}

	// main entry -function call operator that work for the specified range (begin/end)
	void operator()(size_t begin, size_t end) {
		if (first_run)
			do_first_run(begin, end);
		else
			do_update(begin, end);
	}
};

// [[Rcpp::export]]
NumericMatrix cp_run_sd(NumericMatrix mat, int nwindow) {
	// allocate the matrix we will return
	int nc = mat.ncol();
	int nperiod = mat.nrow();
	NumericMatrix rmat(nperiod, nc);

	// pad the first n-1 elements with NA
	for (int i = 0; i < nc; i++)
		fill_n(rmat.begin() + i*nperiod, nwindow - 1, NA_REAL);
	
	// pre-compute first run
	NumericVector rsum(nc), rsum2(nc);
	run_sd_helper p(mat, 0, nwindow, rsum, rsum2, rmat);
	parallelFor(0, nc, p);

	// for loop for each time period in parallel
	for (int i = 1; i < (nperiod - nwindow + 1); i++) {
		// update stats
		p.update(i);
		parallelFor(0, nc, p);
	}
	return rmat;
}
