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
