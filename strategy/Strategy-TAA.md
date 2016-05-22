---
layout: page
title: Quantitative Approach To Tactical Asset Allocation Strategy
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.





The [Quantitative Approach To Tactical Asset Allocation Strategy(QATAA) by Mebane T. Faber](http://mebfaber.com/timing-model/)
backtest and live signal. For more details please see [SSRN paper](http://papers.ssrn.com/sol3/papers.cfm?abstract_id=962461)

The [QATAA Strategy](http://mebfaber.com/timing-model/)
allocates 20% across 5 asset classes:

* US Stocks
* Foreign Stocks
* US 10YR Government Bonds
* Real Estate
* Commodities

If asset is above it's 10 month moving average it gets 20% allocation; otherwise, it's weight is
allocated to cash. The re-balancing process is done Monthly.

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
US.STOCKS = VTI + VTSMX
FOREIGN.STOCKS = VEU + FDIVX
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



|           | US.STOCKS| FOREIGN.STOCKS| US.10YR.GOV.BOND| REAL.ESTATE| COMMODITIES|  CASH|
|:----------|---------:|--------------:|----------------:|-----------:|-----------:|-----:|
|2016-05-20 |    104.89|          42.87|           109.63|       82.04|       14.66| 82.67|
    




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





Code Strategy Rules:



{% highlight r %}
#*****************************************************************
# Code Strategy
#******************************************************************
sma = bt.apply.matrix(prices, SMA, 200)

# If asset is above it's 10 month moving average it gets 20% allocation
weight = iif(prices > sma, 20/100, 0)

# otherwise, it's weight is allocated to cash
weight$CASH = 1 - rowSums(weight)

obj$weights$strategy = weight[period.ends,]
{% endhighlight %}


![plot of chunk plot-6](/public/images/Strategy-TAA/plot-6-1.png) 

#Strategy Performance:
    




|              |strategy          |
|:-------------|:-----------------|
|Period        |May1996 - May2016 |
|Cagr          |9.22              |
|Sharpe        |1.14              |
|DVR           |1.12              |
|R2            |0.98              |
|Volatility    |8.01              |
|MaxDD         |-13.48            |
|Exposure      |99.72             |
|Win.Percent   |64.07             |
|Avg.Trade     |0.2               |
|Profit.Factor |2.04              |
|Num.Trades    |988               |
    


![plot of chunk plot-6](/public/images/Strategy-TAA/plot-6-2.png) 

#Monthly Results for strategy :
    




|     |Jan   |Feb   |Mar   |Apr   |May   |Jun   |Jul   |Aug   |Sep   |Oct   |Nov   |Dec   |Year  |MaxDD |
|:----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:-----|
|1996 |      |      |      |      |      |  1.3 |  0.2 | -0.2 |  1.7 |  2.2 |  1.7 | -0.9 |  6.2 | -1.8 |
|1997 |  0.2 |  0.0 | -0.6 |  0.9 |  4.0 |  2.2 |  3.8 | -2.2 |  5.0 | -1.8 | -0.2 |  1.5 | 13.3 | -5.0 |
|1998 |  1.0 |  2.0 |  2.5 |  0.3 |  0.0 |  1.1 | -0.1 | -4.7 |  2.3 | -0.6 |  0.3 |  2.1 |  6.0 | -7.0 |
|1999 |  1.4 | -2.7 |  1.9 |  2.5 | -1.9 |  3.1 | -0.6 |  1.0 |  1.2 |  0.3 |  3.2 |  4.7 | 14.8 | -3.6 |
|2000 | -1.2 |  3.1 |  1.8 | -1.3 |  0.9 |  3.4 |  0.3 |  2.1 | -1.2 | -1.2 |  2.4 |  2.2 | 11.9 | -4.2 |
|2001 |  1.1 |  0.1 | -0.6 | -0.1 |  0.9 |  1.5 |  1.4 |  1.6 |  0.2 |  0.8 | -1.6 | -0.2 |  5.3 | -3.3 |
|2002 |  0.1 |  1.1 | -0.2 | -0.1 |  0.8 |  0.8 | -2.5 |  2.1 |  1.3 | -0.8 | -0.4 |  3.3 |  5.7 | -5.2 |
|2003 |  1.5 |  2.3 | -1.8 |  0.7 |  5.3 |  1.2 |  1.1 |  2.3 |  1.3 |  2.9 |  1.7 |  4.4 | 25.5 | -4.0 |
|2004 |  2.4 |  3.1 |  1.7 | -5.1 |  1.0 |  0.3 | -0.4 |  2.8 |  1.3 |  2.5 |  2.6 |  1.8 | 14.4 | -7.1 |
|2005 | -2.0 |  3.0 | -0.5 | -0.7 |  1.1 |  2.1 |  3.4 |  1.2 |  1.0 | -3.0 |  2.4 |  2.4 | 10.6 | -4.4 |
|2006 |  4.4 | -0.9 |  2.5 |  1.7 | -2.3 |  0.7 |  1.5 |  1.3 |  0.2 |  2.8 |  2.5 | -0.4 | 14.8 | -7.4 |
|2007 |  1.7 | -0.4 |  0.5 |  1.8 |  1.0 | -2.0 | -0.2 |  0.6 |  4.0 |  3.7 | -0.9 |  1.2 | 11.2 | -5.6 |
|2008 |  0.2 |  2.5 |  0.3 |  0.5 |  0.5 | -1.5 | -1.8 | -0.5 | -2.4 | -2.6 |  4.7 |  5.1 |  4.7 |-13.1 |
|2009 | -2.5 | -0.6 |  1.5 | -0.2 |  0.1 |  0.0 |  4.5 |  3.5 |  3.4 | -0.8 |  4.5 |  1.4 | 15.6 | -4.9 |
|2010 | -4.3 |  2.9 |  4.3 |  2.4 | -6.3 | -1.1 |  2.6 |  1.3 |  0.8 |  3.3 | -1.4 |  5.2 |  9.4 | -9.8 |
|2011 |  1.9 |  3.1 |  0.2 |  4.0 | -1.4 | -2.2 |  0.9 | -3.3 | -2.1 | -0.2 | -0.8 |  1.3 |  0.9 |-13.5 |
|2012 |  2.7 |  0.3 |  0.8 |  0.3 | -6.2 |  1.8 |  1.3 |  1.6 |  0.6 | -1.3 |  0.6 |  1.5 |  3.8 | -7.7 |
|2013 |  2.5 | -0.7 |  1.5 |  2.9 | -2.4 | -2.1 |  1.5 | -2.8 |  2.9 |  2.0 | -0.6 |  0.4 |  5.0 | -8.2 |
|2014 | -0.9 |  2.3 |  0.1 |  1.3 |  1.3 |  1.4 | -1.7 |  2.2 | -3.0 |  3.1 |  1.4 |  0.4 |  8.0 | -3.1 |
|2015 |  2.6 | -0.7 |  0.1 | -0.3 | -0.2 | -2.4 |  0.8 | -1.4 |  0.8 | -0.2 | -0.3 | -0.3 | -1.7 | -6.2 |
|2016 |  0.2 |  0.9 |  2.2 | -0.4 | -0.5 |      |      |      |      |      |      |      |  2.5 | -2.0 |
|Avg  |  0.7 |  1.0 |  0.9 |  0.6 | -0.2 |  0.5 |  0.8 |  0.4 |  1.0 |  0.6 |  1.1 |  1.9 |  8.9 | -6.1 |
    


![plot of chunk plot-6](/public/images/Strategy-TAA/plot-6-3.png) ![plot of chunk plot-6](/public/images/Strategy-TAA/plot-6-4.png) 

#Trades for strategy :
    




|strategy         |weight |entry.date |exit.date  |nhold |entry.price |exit.price |return |
|:----------------|:------|:----------|:----------|:-----|:-----------|:----------|:------|
|US.STOCKS        |20     |2015-11-30 |2015-12-31 |31    |106.07      |103.81     |-0.42  |
|US.10YR.GOV.BOND |20     |2015-11-30 |2015-12-31 |31    |105.45      |104.97     |-0.09  |
|REAL.ESTATE      |20     |2015-11-30 |2015-12-31 |31    |78.3        |79.73      |0.37   |
|CASH             |40     |2015-11-30 |2015-12-31 |31    |80.57       |80.43      |-0.07  |
|REAL.ESTATE      |20     |2015-12-31 |2016-01-29 |29    |79.73       |76.99      |-0.69  |
|CASH             |80     |2015-12-31 |2016-01-29 |29    |80.43       |81.39      |0.96   |
|US.10YR.GOV.BOND |20     |2016-01-29 |2016-02-29 |31    |108.47      |110.08     |0.3    |
|CASH             |80     |2016-01-29 |2016-02-29 |31    |81.39       |82.09      |0.68   |
|US.10YR.GOV.BOND |20     |2016-02-29 |2016-03-31 |31    |110.08      |110.01     |-0.01  |
|REAL.ESTATE      |20     |2016-02-29 |2016-03-31 |31    |76.71       |83.8       |1.85   |
|CASH             |60     |2016-02-29 |2016-03-31 |31    |82.09       |82.65      |0.41   |
|US.STOCKS        |20     |2016-03-31 |2016-04-29 |29    |104.82      |105.51     |0.13   |
|US.10YR.GOV.BOND |20     |2016-03-31 |2016-04-29 |29    |110.01      |109.84     |-0.03  |
|REAL.ESTATE      |20     |2016-03-31 |2016-04-29 |29    |83.8        |81.83      |-0.47  |
|CASH             |40     |2016-03-31 |2016-04-29 |29    |82.65       |82.81      |0.08   |
|US.STOCKS        |20     |2016-04-29 |2016-05-20 |21    |105.51      |104.89     |-0.12  |
|FOREIGN.STOCKS   |20     |2016-04-29 |2016-05-20 |21    |44.06       |42.87      |-0.54  |
|US.10YR.GOV.BOND |20     |2016-04-29 |2016-05-20 |21    |109.84      |109.63     |-0.04  |
|REAL.ESTATE      |20     |2016-04-29 |2016-05-20 |21    |81.83       |82.04      |0.05   |
|COMMODITIES      |20     |2016-04-29 |2016-05-20 |21    |14.58       |14.66      |0.11   |
    




#Signals for strategy :
    




|           | US.STOCKS| FOREIGN.STOCKS| US.10YR.GOV.BOND| REAL.ESTATE| COMMODITIES| CASH|
|:----------|---------:|--------------:|----------------:|-----------:|-----------:|----:|
|2014-09-29 |        20|              0|               20|          20|           0|   40|
|2014-10-30 |        20|              0|               20|          20|           0|   40|
|2014-11-26 |        20|              0|               20|          20|           0|   40|
|2014-12-30 |        20|              0|               20|          20|           0|   40|
|2015-01-29 |        20|              0|               20|          20|           0|   40|
|2015-02-26 |        20|             20|               20|          20|           0|   20|
|2015-03-30 |        20|             20|               20|          20|           0|   20|
|2015-04-29 |        20|             20|               20|          20|           0|   20|
|2015-05-28 |        20|             20|               20|          20|           0|   20|
|2015-06-29 |        20|             20|                0|           0|           0|   60|
|2015-07-30 |        20|              0|                0|           0|           0|   80|
|2015-08-28 |         0|              0|               20|           0|           0|   80|
|2015-09-29 |         0|              0|               20|           0|           0|   80|
|2015-10-29 |        20|              0|               20|          20|           0|   40|
|2015-11-27 |        20|              0|               20|          20|           0|   40|
|2015-12-30 |         0|              0|                0|          20|           0|   80|
|2016-01-28 |         0|              0|               20|           0|           0|   80|
|2016-02-26 |         0|              0|               20|          20|           0|   60|
|2016-03-30 |        20|              0|               20|          20|           0|   40|
|2016-04-28 |        20|             20|               20|          20|          20|    0|
    







For your convenience, the 
[Strategy-TAA](/public/images/Strategy-TAA/Strategy-TAA.pdf)
report can also be downloaded and viewed the pdf format.





















*(this report was produced on: 2016-05-22)*
