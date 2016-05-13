---
layout: page
title: Flexible Asset Allocation (FAA)
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.





The [Generalized Momentum and Flexible Asset Allocation (FAA): An Heuristic Approach by Wouter J. Keller and Hugo S.van Putten (2012)](http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2193735)
backtest and live signal. For more details please see:
 
* [Generalized Momentum and Flexible Asset Allocation (FAA): An Heuristic Approach by Wouter J. Keller and Hugo S.van Putten (2012)](http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2193735)
* [Flexible Asset Allocation](http://turnkeyanalyst.com/2013/01/flexible-asset-allocation/)
* [Asset Allocation Combining Momentum, Volatility, Correlation and Crash Protection](http://www.cxoadvisory.com/subscription-options/?wlfrom=%2F22480%2Fvolatility-effects%2Fasset-allocation-combining-momentum-volatility-correlation-and-crash-protectio%2F)

The [FAA Strategy](http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2193735)
allocates across Price, Volatility, and Correlation Momentum.





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
EMERGING>MARKETS=EEM + VEIEX
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



|           | US.STOCKS| FOREIGN.STOCKS| EMERGING>MARKETS| US.10YR.GOV.BOND| REAL.ESTATE| COMMODITIES|  CASH|
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
vol.lookback = 80
cor.lookback = 80

weight=c(1, 0.5, 0.5)

hist.vol = sqrt(252) * bt.apply.matrix(ret, runSD, n = vol.lookback)

mom = (prices / mlag(prices, mom.lookback) - 1)[period.ends,]
{% endhighlight %}



{% highlight text %}
## Error in `[.xts`((prices/mlag(prices, mom.lookback) - 1), period.ends, : object 'period.ends' not found
{% endhighlight %}



{% highlight r %}
#*****************************************************************
# Compute Average Correlation
#******************************************************************
avg.cor = data$weight * NA
for(i in period.ends[period.ends > cor.lookback]){
  hist = ret[(i - cor.lookback):i,]
    include.index = !is.na(colSums(hist))
  correlation = cor(hist[,include.index], use='complete.obs',method='pearson')
  avg.correlation = rowSums(correlation, na.rm=T)

  avg.cor[i,include.index] = avg.correlation
}
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): object 'period.ends' not found
{% endhighlight %}



{% highlight r %}
mom.rank = br.rank(mom)
cor.rank = br.rank(-avg.cor[period.ends,])
{% endhighlight %}



{% highlight text %}
## Error in `[.xts`(avg.cor, period.ends, ): object 'period.ends' not found
{% endhighlight %}



{% highlight r %}
vol.rank = br.rank(-hist.vol[period.ends,])
{% endhighlight %}



{% highlight text %}
## Error in `[.xts`(hist.vol, period.ends, ): object 'period.ends' not found
{% endhighlight %}



{% highlight r %}
avg.rank = weight[1]*mom.rank + weight[2]*vol.rank + weight[3]*cor.rank
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): object 'vol.rank' not found
{% endhighlight %}



{% highlight r %}
meta.rank = br.rank(-avg.rank)

#absolute momentum filter 
weight = (meta.rank <= n.top)/rowSums(meta.rank <= n.top, na.rm=T) * (mom > 0)

# cash logic
weight$CASH = 1 - rowSums(weight,na.rm=T)

obj$weights$strategy = weight
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): Date's serial number (1463036769) outside allowed range [367-109574], i.e. [January 1st, 1901-December 31st, 2199]
{% endhighlight %}



{% highlight text %}
## Error in lst(...): object 'models' not found
{% endhighlight %}



{% highlight text %}
## Error in lst(...): object 'models' not found
{% endhighlight %}




For your convenience, the 
[Strategy-FAA](/public/images/Strategy-FAA/Strategy-FAA.pdf)
report can also be downloaded and viewed the pdf format.





















*(this report was produced on: 2016-05-13)*
