---
layout: page
title: New 60/40
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.




The [New 60/40](https://systematicinvestor.wordpress.com/2012/08/07/the-new-6040/)
backtest and live signal. For more details please see:

* [You're Looking at the Wrong Number](http://gestaltu.blogspot.ca/2012/07/youre-looking-at-wrong-number.html)
* [The New 60/40](https://systematicinvestor.wordpress.com/2012/08/07/the-new-6040/)

The [New 60/40 Strategy](https://systematicinvestor.wordpress.com/2012/08/07/the-new-6040/)
allocates 60% risk to equities and 40% risk to long-term treasuries.

I think this strategy is missing the go to cash filter. For example of using cash filter please read
the [Quantitative Approach To Tactical Asset Allocation Strategy(QATAA) by Mebane T. Faber](http://mebfaber.com/timing-model/)

Another interesting observation is a spike in risk-parity in 2009-2010. I think this is related
[Volatility and "Crashing Up"](http://blog.thinknewfound.com/volatility-crashing/)


Following report is based on Monthly re-balancing, 
signal is generated one day before the month end,
and execution is done at close at the month end.





Load historical data from Yahoo Finance:



{% highlight r %}
#*****************************************************************
# Load historical data
#*****************************************************************
library(SIT)
load.packages('quantmod')

tickers = '
STOCK = SPY + VTSMX + VFINX
BOND = TLT + VUSTX
CASH = SHY + TB3Y
'

# load saved Proxies Raw Data, data.proxy.raw
load('data.proxy.raw.Rdata')

data <- new.env()

getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, raw.data = data.proxy.raw, auto.assign = T, set.symbolnames = T, getSymbols.fn = getSymbols.fn, calendar=calendar)
  for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na', dates='::')

print(last(data$prices))
{% endhighlight %}



|           |  STOCK|   BOND| CASH|
|:----------|------:|------:|----:|
|2016-05-12 | 206.56| 131.01|   85|
    




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
# Traditional, Dollar Weighted 40% Bonds & 60% Stock
#******************************************************************
target.allocation = NA * prices[1,]
	target.allocation$STOCK = 60/100
 target.allocation$BOND = 40/100

obj$weights$dollar.w.60.40 = rep.row(target.allocation, len(period.ends))
{% endhighlight %}



{% highlight text %}
## Error in len(period.ends): object 'period.ends' not found
{% endhighlight %}



{% highlight r %}
#*****************************************************************
# Risk Weighted 40% Bonds & 60% Stock
#******************************************************************
ret = diff(log(prices))
hist.vol = bt.apply.matrix(ret, runSD, n = 20)

# risk-parity
weight.risk = 1 / hist.vol
	weight.risk = weight.risk / rowSums(weight.risk, na.rm=T)

obj$weights$risk.w.60.40 = weight.risk[period.ends,]
{% endhighlight %}



{% highlight text %}
## Error in `[.xts`(weight.risk, period.ends, ): object 'period.ends' not found
{% endhighlight %}



{% highlight r %}
#*****************************************************************
# Cash Filter
#******************************************************************
# compute 10 month moving average
sma = bt.apply.matrix(prices, SMA, 200)

# go to cash if prices falls below 10 month moving average
go2cash = prices < sma
  go2cash = ifna(go2cash, T)[period.ends,]
{% endhighlight %}



{% highlight text %}
## Error in `[.xts`(ifna(go2cash, T), period.ends, ): object 'period.ends' not found
{% endhighlight %}



{% highlight r %}
weight = obj$weights$risk.w.60.40
	weight[go2cash] = 0
weight$CASH = 1 - rowSums(weight, na.rm=T)
{% endhighlight %}



{% highlight text %}
## Error in rowSums(weight, na.rm = T): 'x' must be an array of at least two dimensions
{% endhighlight %}



{% highlight r %}
obj$weights$risk.w.60.40.CASH = weight


weight[] = obj$weights$dollar.w.60.40
{% endhighlight %}



{% highlight text %}
## Error in weight[] = obj$weights$dollar.w.60.40: replacement has length zero
{% endhighlight %}



{% highlight r %}
	weight[go2cash] = 0
weight$CASH = 1 - rowSums(weight, na.rm=T)
{% endhighlight %}



{% highlight text %}
## Error in rowSums(weight, na.rm = T): 'x' must be an array of at least two dimensions
{% endhighlight %}



{% highlight r %}
obj$weights$dollar.w.60.40.CASH = weight

#*****************************************************************
# Scale Risk Weighted 40% Bonds & 60% Stock strategy to have 6% volatility
#****************************************************************** 
models = get.back.test(data, obj, input)
{% endhighlight %}



{% highlight text %}
## Error in rowSums(weight, na.rm = T): 'x' must be an array of at least two dimensions
{% endhighlight %}



{% highlight r %}
weight = target.vol.strategy(models$risk.w.60.40, ifna(weight.risk,0),
		target=6/100, lookback.len=21, max.portfolio.leverage=100/100)
{% endhighlight %}



{% highlight text %}
## Error in log(model$equity): non-numeric argument to mathematical function
{% endhighlight %}



{% highlight r %}
# invested not allocated to CASH
weight$CASH = 1 - rowSums(weight)
{% endhighlight %}



{% highlight text %}
## Error in rowSums(weight): 'x' must be an array of at least two dimensions
{% endhighlight %}



{% highlight r %}
obj$weights$risk.w.60.40.target6.cash = weight[period.ends,]
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): object 'period.ends' not found
{% endhighlight %}



{% highlight text %}
## Error in rowSums(weight, na.rm = T): 'x' must be an array of at least two dimensions
{% endhighlight %}



{% highlight text %}
## Error in out[[1]][[1]]: subscript out of bounds
{% endhighlight %}



{% highlight text %}
## Error in out[[1]][[1]]: subscript out of bounds
{% endhighlight %}


For your convenience, the 
[Strategy-NEW-60-40](/public/images/Strategy-NEW-60-40/Strategy-NEW-60-40.pdf)
report can also be downloaded and viewed the pdf format.







*(this report was produced on: 2016-05-13)*
