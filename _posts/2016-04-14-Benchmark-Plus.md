---
layout: post
title: Benchmark Plus
comments: true
---
To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.




The overlay strategy is the market neutral strategy that can be applied to benchmark to
improve benchmark's performance. The new strategy weights are equal to benchmark weights
plus the overlay weights.

Below I will present a very simple example. 
The Benchmark portfolio is a market cap weighted country portfolio.
The Overlay portfolio is long the top five momentum countries and is short 
the bottom five momentum countries.
The Benchmark Plus portfolio is the Benchmark portfolio combined with the Overlay portfolio.

I will use the 
[Betting Against Beta: Equity Factors, Monthly](https://www.aqr.com/library/data-sets/betting-against-beta-equity-factors-monthly)
historical data set to download:

* market excess returns, 
* lagged market capitalization,
* risk free rate 





To construct the Benchmark portfolio, a market cap weighted country portfolio, returns
please multiply market returns by normalized market capitalization weights



{% highlight r %}
#*****************************************************************
# construct market cap weighted benchmark, no need to lag market.cap; it is t-1
#****************************************************************** 
benchmark.weight = data$market.cap / rowSums(data$market.cap)
benchmark.ret = xts(rowSums(benchmark.weight * data$market), dates)
benchmark.equity = cumprod(1 + benchmark.ret)
{% endhighlight %}


To construct the Overlay portfolio, a market neutral portfolio that
is long the top five momentum countries and is short 
the bottom five momentum countries, please first ensure that overlay portfolio
weights satisfy that Benchmark portfolio weight plus the Overlay portfolio weight
are with in 0 and 1. Next normalize the Overlay portfolio weights to sum up to 0.
This insures the Benchmark Plus portfolio is fully invested.



{% highlight r %}
#*****************************************************************
# construct overlay
#****************************************************************** 
# rank countries based on 1 month return, overweight top 5 and underweight bottom 5
mom = mlag(data$mom)
	
# make sure that overlay + benchmark weights is always from 0 to 1 and sums up to 1
long.weight = iif(ntop(mom,5) > (1 - benchmark.weight), 1 - benchmark.weight, ntop(mom,5))
short.weight = iif(ntop(mom,5,F) > benchmark.weight, benchmark.weight, ntop(mom,5,F))

# scale long and short weights to offset each other
overlay.weight =  rowSums(short.weight)* long.weight / rowSums(long.weight) - short.weight
	overlay.weight[1,] = 0
{% endhighlight %}


Finally, to construct the Benchmark Plus portfolio, the Benchmark portfolio combined with the Overlay portfolio,
returns please multiply market returns by combined Benchmark portfolio weights and Overlay portfolio weights.



{% highlight r %}
#*****************************************************************
# construct strategy = benchmark + overlay
#****************************************************************** 
# please note weights are already lagged
strategy.weight = benchmark.weight + overlay.weight
strategy.ret = xts(rowSums(strategy.weight * data$market), dates)
strategy.equity = cumprod(1 + strategy.ret)
{% endhighlight %}


Now, I will construct:

* annualized tracking error - the annualized active risk
* [annualized information ratio](http://www.styleadvisor.com/content/information-ratio) - the annualized alpha divided by annualized active risk



{% highlight r %}
#*****************************************************************
# compute descriptive statistics
#****************************************************************** 
# make sure weights add up
# range(rowSums(abs(strategy.weight)))

tracking.error = sqrt(12) * sd(strategy.ret - benchmark.ret)

alpha = 12*(mean(strategy.ret) - mean(benchmark.ret))

information.ratio = alpha / tracking.error

print(data.frame(alpha, tracking.error, information.ratio))
{% endhighlight %}



|     alpha| tracking.error| information.ratio|
|---------:|--------------:|-----------------:|
| 0.0160963|      0.0309904|          0.519397|
    


I get 0.5 [annualized information ratio](http://www.styleadvisor.com/content/information-ratio)
and 3% tracking error; not a bad result for such a simple system.

Following are the descriptive statistics for both portfolios and historical performance chart.



{% highlight r %}
#*****************************************************************
# Create model and Reports
#****************************************************************** 
models = list()
models$benchmark = lst(ret = benchmark.ret, equity=benchmark.equity, weight=benchmark.weight)
models$strategy = lst(ret = strategy.ret, equity=strategy.equity, weight=strategy.weight)

plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)	    	
	mtext('Cumulative Performance', side = 2, line = 1)
{% endhighlight %}

![plot of chunk plot-7](/public/images/2016-04-14-Benchmark-Plus/plot-7-1.png) 

{% highlight r %}
print(plotbt.strategy.sidebyside(models, make.plot=F, return.table=T,perfromance.fn = engineering.returns.kpi))
{% endhighlight %}



|           |benchmark         |strategy          |
|:----------|:-----------------|:-----------------|
|Period     |Jan1986 - Feb2016 |Jan1986 - Feb2016 |
|Cagr       |8.25              |9.97              |
|Sharpe     |0.59              |0.68              |
|DVR        |0.52              |0.61              |
|R2         |0.88              |0.89              |
|Volatility |15.61             |15.77             |
|MaxDD      |-54.46            |-55.23            |
|Exposure   |100               |100               |
    


Following are the transition map plots for both strategies.
There is a bit of noise in the Benchmark Plus strategy due to momentum portfolio changes.


![plot of chunk plot-8](/public/images/2016-04-14-Benchmark-Plus/plot-8-1.png) 


Overall, the Benchmark Plus strategy maintains a similar risk profile as the original
Benchmark strategy and is able improve performance. I think for such a simple strategy
it works quite well. 




For your convenience, the [2016-04-14-Benchmark-Plus](https://github.com/systematicinvestor/systematicinvestor.github.io/blob/master/rposts/2016-04-14-Benchmark-Plus.r) post source code.

*(this report was produced on: 2016-04-15)*
