---
layout: post
title: Turn of the Month Seasonality
comments: true
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.




The [QuantDare](http://quantdare.wordpress.com) wrote a detailed post
about [Turn of the Month Seasonality for S&P 500](http://translate.google.com/translate?hl=en&sl=es&u=http://quantdare.wordpress.com/2014/10/20/seasonality-systems/)

Below I will try to adapt a code from the posts:



{% highlight r %}
#*****************************************************************
# Load historical data
#*****************************************************************
library(SIT)
load.packages('quantmod')

tickers = 'SPY+^GSPC'

data = new.env()
getSymbols.extra(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)

for(i in ls(data))
	data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)

bt.prep(data, align='remove.na')

#*****************************************************************
# Setup
#*****************************************************************
prices = data$prices

models = list()

universe = prices > 0

key.date.index = date.month.ends(data$dates, F)
key.date = NA * prices
	key.date[key.date.index,] = T	

#*****************************************************************
# Turn of the Month Seasonality
#*****************************************************************
signals = list()
for(i in 10:1) signals[[paste0('P',i)]] = -i	
signals$T0 = 0
for(i in 1:10) signals[[paste0('N',i)]] = i	
	signals = calendar.signal(key.date, signals)

models = calendar.strategy(data, signals, universe = universe)

#*****************************************************************
# Create Report
#*****************************************************************
print(plotbt.strategy.sidebyside(models, make.plot=F, return.table=T, perfromance.fn=engineering.returns.kpi))
{% endhighlight %}



|              |P10               |P9                |P8                |P7                |P6                |P5                |P4                |P3                |P2                |P1                |T0                |N1                |N2                |N3                |N4                |N5                |N6                |N7                |N8                |N9                |N10               |
|:-------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|:-----------------|
|Period        |Jan1980 - Mar2015 |Jan1980 - Mar2015 |Jan1980 - Mar2015 |Jan1980 - Mar2015 |Jan1980 - Mar2015 |Jan1980 - Mar2015 |Jan1980 - Mar2015 |Jan1980 - Mar2015 |Jan1980 - Mar2015 |Jan1980 - Mar2015 |Jan1980 - Mar2015 |Jan1980 - Mar2015 |Jan1980 - Mar2015 |Jan1980 - Mar2015 |Jan1980 - Mar2015 |Jan1980 - Mar2015 |Jan1980 - Mar2015 |Jan1980 - Mar2015 |Jan1980 - Mar2015 |Jan1980 - Mar2015 |Jan1980 - Mar2015 |
|Cagr          |1.9               |-0.65             |0.31              |0.34              |-0.93             |-0.65             |-0.07             |1.44              |0.8               |0.85              |0.58              |2.48              |1.31              |0.63              |-0.12             |-0.52             |-0.32             |-0.3              |-0.08             |1.56              |0.3               |
|Sharpe        |0.52              |-0.1              |0.1               |0.11              |-0.21             |-0.15             |0                 |0.37              |0.24              |0.23              |0.18              |0.59              |0.37              |0.19              |-0.01             |-0.12             |-0.07             |-0.06             |-0.01             |0.36              |0.1               |
|DVR           |0.47              |-0.05             |0                 |0                 |-0.19             |-0.05             |0                 |0.27              |0.18              |0.21              |0.04              |0.55              |0.29              |0.09              |0                 |-0.04             |-0.03             |-0.02             |0                 |0.29              |0.05              |
|R2            |0.9               |0.56              |0                 |0                 |0.88              |0.35              |0.13              |0.71              |0.74              |0.91              |0.2               |0.93              |0.79              |0.49              |0                 |0.32              |0.51              |0.32              |0.19              |0.8               |0.55              |
|Volatility    |3.76              |5.17              |3.89              |3.88              |3.98              |3.78              |3.86              |4.03              |3.49              |3.94              |3.61              |4.28              |3.69              |3.57              |4.04              |3.63              |3.77              |3.92              |3.47              |4.53              |3.72              |
|MaxDD         |-9.08             |-35.81            |-19.38            |-23.67            |-38.64            |-27.31            |-14.71            |-12.58            |-14.08            |-9.99             |-18.44            |-11.97            |-14.35            |-19.98            |-23.9             |-24.09            |-27.5             |-33.36            |-17.31            |-10.09            |-17.15            |
|Exposure      |4.75              |4.75              |4.75              |4.75              |4.75              |4.75              |4.75              |4.75              |4.75              |4.75              |4.75              |4.75              |4.75              |4.75              |4.75              |4.75              |4.75              |4.75              |4.75              |4.75              |4.75              |
|Win.Percent   |59.24             |53.32             |51.18             |48.58             |50.24             |48.1              |52.61             |53.55             |55.69             |54.5              |52.37             |59                |55.92             |52.84             |50.24             |48.82             |51.9              |50.95             |53.55             |57.82             |54.74             |
|Avg.Trade     |0.16              |-0.04             |0.03              |0.03              |-0.07             |-0.05             |0                 |0.13              |0.07              |0.08              |0.05              |0.21              |0.11              |0.06              |0                 |-0.04             |-0.02             |-0.02             |0                 |0.14              |0.03              |
|Profit.Factor |1.52              |0.87              |1.07              |1.08              |0.84              |0.87              |1                 |1.4               |1.2               |1.18              |1.12              |1.64              |1.34              |1.13              |0.98              |0.89              |0.91              |0.95              |0.98              |1.38              |1.07              |
|Num.Trades    |422               |422               |422               |422               |422               |422               |422               |422               |422               |422               |422               |422               |422               |422               |422               |422               |422               |422               |422               |422               |422               |
    




{% highlight r %}
# custom stats	
out = sapply(models, function(x) list(
	CAGR = 100*compute.cagr(x$equity),
	MD = 100*compute.max.drawdown(x$equity),
	Win = x$trade.summary$stats['win.prob', 'All'],
	Profit = x$trade.summary$stats['profitfactor', 'All']
))	
performance.barchart.helper(out, sort.performance = F)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2015-03-23-Month-Seasonality/plot-2-1.png) 

A few days before and a few days after the month end show great performance with low draw downs. 


*(this report was produced on: 2015-03-24)*
