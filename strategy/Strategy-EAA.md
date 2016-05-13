---
layout: page
title: Elastic Asset Allocation Strategy
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.





The [Elastic Asset Allocation Strategy(EAA)](http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2543979)
backtest and live signal. For more details please see [SSRN paper](http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2543979)

The [EAA Strategy](http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2543979)
 uses a geometrical weighted average of the historical returns, volatilities
and correlations, using elasticities as weights:

> asset's attractivness = [return ^ wR] * [(1-correlation) ^ wC] / [volatility ^ wV] 

where:
---

* return is average asset's return over last year using different timeframes
* correlation is asset's correlation with equal weight, market, index
* volatility is asset's historical volatility
* wR, wC, wV are user elasticities





Load historical data from Yahoo Finance:



{% highlight r %}
#*****************************************************************
# Load historical data
#*****************************************************************
library(SIT)
load.packages('quantmod')

tickers = '
US.STOCKS = VTI + VTSMX
FOREIGN.STOCKS = VEU + FDIVX
EMERGING.MARKETS=EEM + VEIEX
US.10YR.GOV.BOND = IEF + VFITX
REAL.ESTATE = VNQ + VGSIX
COMMODITIES = DBC + CRB
CASH = BND + VBMFX
'

# load saved Proxies Raw Data, data.proxy.raw
load('data.proxy.raw.Rdata')

data <- new.env()

getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, raw.data = data.proxy.raw, auto.assign = T, set.symbolnames = T, getSymbols.fn = getSymbols.fn, calendar=calendar)
  for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na', dates='::')

print(last(data$prices))
{% endhighlight %}



|           | US.STOCKS| FOREIGN.STOCKS| EMERGING.MARKETS| US.10YR.GOV.BOND| REAL.ESTATE| COMMODITIES|  CASH|
|:----------|---------:|--------------:|----------------:|----------------:|-----------:|-----------:|-----:|
|2016-05-12 |    105.33|          43.19|            32.86|           110.47|       85.16|        14.5| 83.12|
    




{% highlight r %}
#*****************************************************************
# Setup
#*****************************************************************
data$universe = data$prices > 0
	# do not allocate to CASH
	data$universe$CASH = NA 

prices = data$prices * data$universe
	n = ncol(prices)
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): Date's serial number (1463036769) outside allowed range [367-109574], i.e. [January 1st, 1901-December 31st, 2199]
{% endhighlight %}



{% highlight text %}
## Error in date.ends.index(out, out$signal.timing): object 'out' not found
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): object 'period.ends' not found
{% endhighlight %}


Code Strategy Rules:



{% highlight r %}
#*****************************************************************
# Code Strategy
#******************************************************************
ret = diff(log(prices))

n.top = 3

mom.lookback = 80
vol.lookback = 12*22
cor.lookback = 12*22

weight=list(r=1, v=0, c=0.5, s=2)

hist.vol = sqrt(252) * bt.apply.matrix(ret, runSD, n = vol.lookback)

#mean of cumulative returns of 1, 3, 6, and 12 month periods
mom = (prices / mlag(prices, 22)-1 + prices / mlag(prices, 3*22)-1
+ prices / mlag(prices, 6*22)-1 + prices / mlag(prices, 12 * 22)-1)/22

mkt.ret = rowMeans(ret, na.rm=T)

#*****************************************************************
# Compute Correlation to Market
#******************************************************************
mkt.cor = data$weight * NA
for(i in period.ends[period.ends > cor.lookback]){
  index = (i - cor.lookback):i
  hist = ret[index,]
    include.index = !is.na(colSums(hist))
    
  if(any(include.index))
  	mkt.cor[i,include.index] = cor(hist[,include.index], mkt.ret[index], use='complete.obs',method='pearson')
}
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): object 'period.ends' not found
{% endhighlight %}



{% highlight r %}
avg.rank = (mom^weight$r * (1 - mkt.cor)^weight$c / hist.vol^weight$v) ^ weight$s
meta.rank = br.rank(avg.rank[period.ends,])
{% endhighlight %}



{% highlight text %}
## Error in `[.xts`(avg.rank, period.ends, ): object 'period.ends' not found
{% endhighlight %}



{% highlight r %}
#absolute momentum filter
weight = (meta.rank <= n.top)/rowSums(meta.rank <= n.top, na.rm=T) * (mom[period.ends,] > 0)
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): object 'meta.rank' not found
{% endhighlight %}



{% highlight r %}
# otherwise, it's weight is allocated to cash
weight$CASH = 1 - rowSums(weight,na.rm=T)
{% endhighlight %}



{% highlight text %}
## Error in rowSums(weight, na.rm = T): 'x' must be an array of at least two dimensions
{% endhighlight %}



{% highlight r %}
obj$weights$strategy = weight
{% endhighlight %}



{% highlight text %}
## Error in rowSums(weight, na.rm = T): 'x' must be an array of at least two dimensions
{% endhighlight %}



{% highlight text %}
## Error in lst(...): object 'models' not found
{% endhighlight %}



{% highlight text %}
## Error in lst(...): object 'models' not found
{% endhighlight %}




For your convenience, the 
[Strategy-EAA](/public/images/Strategy-EAA/Strategy-EAA.pdf)
report can also be downloaded and viewed the pdf format.



*(this report was produced on: 2016-05-13)*
