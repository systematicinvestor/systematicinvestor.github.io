---
layout: post
title: XIV Seasonality
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.




[XIV Seasonality  2 Days Prior to Expiration](https://evotrader.wordpress.com/2014/11/24/xiv-seasonality-2-days-prior-to-expiration/)
[Trading Volatility  XIV Seasonalities](http://webcache.googleusercontent.com/search?q=cache:3L-Llw_-f-AJ:www.tradingtheodds.com/2014/11/trading-volatility-xiv-seasonalities/+&cd=1&hl=en&ct=clnk)
[2014 Options Expiration Calendar](http://www.cboe.com/AboutCBOE/xcal2014.pdf)

Various plots:

* http://stats.stackexchange.com/questions/30858/how-to-calculate-cumulative-distribution-in-r
* http://stackoverflow.com/questions/1497539/fitting-a-density-curve-to-a-histogram-in-r



Load historical data for SPY and TLT, and align it, so that dates on both time series match. We also adjust data for stock splits and dividends.


{% highlight r %}
#*****************************************************************
# Load historical data
#*****************************************************************
library(SIT)
load.packages('quantmod')

tickers = spl('VIX=^VIX,VXX+VXX.LONG,XIV+XIV.LONG')

raw.data <- new.env()
    raw.data$VXX.LONG = make.stock.xts(read.xts("data/VXXlong.TXT", format='%Y-%m-%d'))
    raw.data$XIV.LONG = make.stock.xts(read.xts("data/XIVlong.TXT", format='%Y-%m-%d'))
    
data <- new.env()
getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, raw.data = raw.data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
#bt.start.dates(data)
bt.prep(data, align='remove.na')
{% endhighlight %}

Next let's compute statistics and trading signal.

{% highlight r %}
price = data$prices$XIV
ret = price / mlag(price) - 1

quick.summary <- function(data) {
cat('\n', 'N:', nrow(data), 'Mean:', mean(data, na.rm=T), '\n')
}

quick.summary(100*ret)
{% endhighlight %}


 N: 2693 Mean: 0.1781502 


{% highlight r %}
# VIX settles 30 days prior to SPY
key.date = map.spx.expiration(data$prices, offset=30) 
signals = list(T0=0)
for(i in 1:5) signals[[paste0('P',i)]] = -i
signals$P12 = -2:-1
signals = calendar.signal(key.date, signals)
 
quick.summary(100*ret)
{% endhighlight %}


 N: 2693 Mean: 0.1781502 


{% highlight r %}
quick.summary(100*ret[signals$T0])    
{% endhighlight %}


 N: 128 Mean: 0.1730392 


{% highlight r %}
for(i in 1:5) 
quick.summary(100*ret[ signals[[paste0('P',i)]] ])
{% endhighlight %}


 N: 128 Mean: 1.055512 

 N: 128 Mean: 0.4137429 

 N: 128 Mean: -0.1110872 

 N: 128 Mean: 0.4591647 

 N: 128 Mean: -0.363429 


{% highlight r %}
quick.summary(100*ret[signals$P12])
{% endhighlight %}


 N: 256 Mean: 0.7346273 


{% highlight r %}
quick.summary(100*ret[ifna(!signals$P12,T)])
{% endhighlight %}


 N: 2437 Mean: 0.1196699 


{% highlight r %}
hist(ret, 50, prob=TRUE, col="grey")
x = na.omit(as.vector(ret[signals$P12]))
lines(density(x), col="blue", lwd=2)
x = na.omit(as.vector(ret[ifna(!signals$P12,T)]))
lines(density(x), lty="dotted", col="darkgreen", lwd=2) 
{% endhighlight %}

![plot of chunk plot-3](/public/images/2014-11-25-XIV-Seasonality/plot-3-1.png) 

{% highlight r %}
x = na.omit(as.vector(ret))
plot(ecdf(x), col='gray')
x = na.omit(as.vector(ret[signals$P12]))
lines(ecdf(x), type='h', col='blue')
x = na.omit(as.vector(ret[ifna(!signals$P12,T)]))
lines(ecdf(x), col='green')
{% endhighlight %}

![plot of chunk plot-3](/public/images/2014-11-25-XIV-Seasonality/plot-3-2.png) 

{% highlight r %}
tickers = spl('SPY')

data <- new.env()
getSymbols.extra(tickers, src = 'yahoo', from = '1980-01-01', env = data, set.symbolnames = T, auto.assign = T)
for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all', fill.gaps = T)

#*****************************************************************
# Setup
#*****************************************************************
key.date = map.spx.expiration(data$prices, backfill=F, offset=-7)

universe = data$prices > 0

signals = list(T0=0)
for(i in 1:5) signals[[paste0('N',i)]] = 0:i

signals = calendar.signal(key.date, signals)
models = calendar.strategy(data, signals, universe = universe)
    
    strategy.performance.snapshoot(models, T)
{% endhighlight %}

![plot of chunk plot-3](/public/images/2014-11-25-XIV-Seasonality/plot-3-3.png) NULL


{% highlight r %}
    last.trades(models$T0)
{% endhighlight %}

![plot of chunk plot-3](/public/images/2014-11-25-XIV-Seasonality/plot-3-4.png) 




*(this report was produced on: 2014-12-07)*
