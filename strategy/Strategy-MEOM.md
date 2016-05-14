---
layout: page
title: Monthly End-of-the-Month Strategy
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.





The [Monthly End-of-the-Month Strategy (MEOM) by Quanting Dutchman](http://quantingdutchman.wordpress.com/2010/06/30/strategy-2-monthly-end-of-the-month-meom/)
backtest and live signal.

The [MEOM Strategy](http://quantingdutchman.wordpress.com/2010/06/30/strategy-2-monthly-end-of-the-month-meom/)
invests into the top 2 ETFs that are trading above a medium term moving avarage (WMA89) 
from the universe of 26 ETFs: 


* [DIA](http://finance.yahoo.com/q/hl?s=DIA)
* [EEM](http://finance.yahoo.com/q/hl?s=EEM)
* [EFA](http://finance.yahoo.com/q/hl?s=EFA)
* [EWH](http://finance.yahoo.com/q/hl?s=EWH)
* [EWJ](http://finance.yahoo.com/q/hl?s=EWJ)
* [EWT](http://finance.yahoo.com/q/hl?s=EWT)
* [EWZ](http://finance.yahoo.com/q/hl?s=EWZ)
* [FXI](http://finance.yahoo.com/q/hl?s=FXI)
* [GLD](http://finance.yahoo.com/q/hl?s=GLD)
* [GSG](http://finance.yahoo.com/q/hl?s=GSG)
* [IEF](http://finance.yahoo.com/q/hl?s=IEF)
* [ILF](http://finance.yahoo.com/q/hl?s=ILF)
* [IWM](http://finance.yahoo.com/q/hl?s=IWM)
* [IYR](http://finance.yahoo.com/q/hl?s=IYR)
* [QQQ](http://finance.yahoo.com/q/hl?s=QQQ)
* [SPY](http://finance.yahoo.com/q/hl?s=SPY)
* [VNQ](http://finance.yahoo.com/q/hl?s=VNQ)
* [XLB](http://finance.yahoo.com/q/hl?s=XLB)
* [XLE](http://finance.yahoo.com/q/hl?s=XLE)
* [XLF](http://finance.yahoo.com/q/hl?s=XLF)
* [XLI](http://finance.yahoo.com/q/hl?s=XLI)
* [XLP](http://finance.yahoo.com/q/hl?s=XLP)
* [XLU](http://finance.yahoo.com/q/hl?s=XLU)
* [XLV](http://finance.yahoo.com/q/hl?s=XLV)
* [XLY](http://finance.yahoo.com/q/hl?s=XLY)
* [XLK](http://finance.yahoo.com/q/hl?s=XLK)

The best asset is selected using following ranking criteria:

> Rank = MA( C/Ref(C,-2), 5 ) * Ref( MA( C/Ref(C,-2), 10 ), -5 )


Following report is based on Monthly re-balancing.
Signal is generated one day prior to the month end,
and execution is done at close at the month end.

The strategy enters positions in the top 2 ranked ETFs
on the last day of the month at the close, and
exits positions two days later at the close.





Load historical data from Yahoo Finance:



{% highlight r %}
#*****************************************************************
# Load historical data
#*****************************************************************
library(SIT)
load.packages('quantmod')

tickers = 'DIA,EEM,EFA,EWH,EWJ,EWT,EWZ,FXI,GLD,GSG,IEF,ILF,IWM,IYR,QQQ,SPY,VNQ,XLB,XLE,XLF,XLI,XLP,XLU,XLV,XLY,XLK'

data <- new.env()

getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T, set.symbolnames = T, getSymbols.fn = getSymbols.fn, calendar=calendar)
  for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', dates='1995::', fill.gaps=T)

print(last(data$prices))
{% endhighlight %}



|           |    DIA|   EEM|   EFA|  EWH|   EWJ|   EWT|  EWZ|   FXI|    GLD|   GSG|    IEF|   ILF|    IWM|   IYR|   QQQ|    SPY|  VNQ|   XLB|   XLE|   XLF|   XLI|   XLP|   XLU|   XLV|   XLY|   XLK|
|:----------|------:|-----:|-----:|----:|-----:|-----:|----:|-----:|------:|-----:|------:|-----:|------:|-----:|-----:|------:|----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|
|2016-05-13 | 175.48| 32.27| 56.97| 18.8| 11.53| 12.36| 27.8| 31.24| 121.71| 15.15| 110.89| 25.15| 109.64| 78.63| 105.5| 204.76| 84.4| 45.95| 64.99| 22.88| 55.05| 53.24| 49.39| 69.14| 78.11| 42.21|
    




{% highlight r %}
#*****************************************************************
# Setup
#*****************************************************************
data$universe = data$prices > 0
prices = data$prices * data$universe
	n = ncol(prices)
  nperiods = nrow(prices)
{% endhighlight %}



{% highlight text %}
## Error in eval(expr, envir, enclos): Date's serial number (1463123169) outside allowed range [367-109574], i.e. [January 1st, 1901-December 31st, 2199]
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
# BuyRule = C > WMA(C, 89)
buy.rule = prices > bt.apply.matrix(prices, function(x) { WMA(x, 89) } )		
  buy.rule = ifna(buy.rule, F)

# 2-day returns
ret2 = ifna(prices / mlag(prices, 2), 0)

# Rank2 = MA( C/Ref(C,-2), 5 ) * Ref( MA( C/Ref(C,-2), 10 ), -5 )
position.score = bt.apply.matrix(ret2, SMA, 5) * mlag( bt.apply.matrix(ret2, SMA, 10), 5)
	position.score[!buy.rule] = NA

period.ends2 = iif(period.ends + 2 > nperiods, nperiods, period.ends + 2)
{% endhighlight %}



{% highlight text %}
## Error in len(cond): object 'period.ends' not found
{% endhighlight %}



{% highlight r %}
weight = ntop(position.score[period.ends,], 2)
{% endhighlight %}



{% highlight text %}
## Error in `[.xts`(position.score, period.ends, ): object 'period.ends' not found
{% endhighlight %}



{% highlight r %}
models = list()

data$weight[] = NA
	data$weight[period.ends,] = 	weight
{% endhighlight %}



{% highlight text %}
## Error in `[.xts`(x, i, which.i = TRUE): object 'period.ends' not found
{% endhighlight %}



{% highlight r %}
	data$weight[period.ends2,] = 0		
{% endhighlight %}



{% highlight text %}
## Error in `[.xts`(x, i, which.i = TRUE): object 'period.ends2' not found
{% endhighlight %}



{% highlight r %}
models$strategy = bt.run.share(data, clean.signal=F, trade.summary=T, silent=T, do.lag = 1 + out$delayLen)		
{% endhighlight %}



{% highlight text %}
## Error in mlag(weight, do.lag - 1): object 'out' not found
{% endhighlight %}



{% highlight text %}
## Error in `*tmp*`[[1]]: subscript out of bounds
{% endhighlight %}



{% highlight text %}
## Error in out[[1]][[1]]: subscript out of bounds
{% endhighlight %}



{% highlight text %}
## Error in out[[1]][[1]]: subscript out of bounds
{% endhighlight %}



For your convenience, the 
[Strategy-MEOM](/public/images/Strategy-MEOM/Strategy-MEOM.pdf)
report can also be downloaded and viewed the pdf format.


For more details please check the
[MEOM strategy code at bt.meom.test() function in bt.test.r at github](https://github.com/systematicinvestor/SIT/blob/master/R/bt.test.r)



*(this report was produced on: 2016-05-14)*
