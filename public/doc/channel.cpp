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



