---
layout: post
title: Appending Intraday Data
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.





There are times when I need to run a back-test before market close, to put trades
market on close. Unfortuantelly, Yahoo Finance updates their EOD files at the end of
the day, and using these files intraday will not have the price for today.

I made a very simple function that:

* downloads historical prices using `getSymbols` from Yahoo Finance
* downloads intraday snapshot from Yahoo Finance
* and appends intraday snapshot to historical prices

I.e.

{% highlight r %}
getSymbols.intraday <- function(Symbols, env, ...) {
	getSymbols(Symbols, env = env, ...)
	data.today = getQuote.yahoo.today(Symbols)
	bt.append.today(env, data.today)
}
{% endhighlight %}

Now, I can easily run a back-test at 3:30 using intraday prices from Yahoo Finance
and compute signal for today. For example:


{% highlight r %}
#*****************************************************************
# Load historical data
#*****************************************************************
library(SIT)
load.packages('quantmod')
tickers = spl('SPY,TLT,GLD,SHY')

data <- new.env()
getSymbols.intraday(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
{% endhighlight %}

Please note that I used `getSymbols.intraday` instead of `getSymbols` function above.
The data environment will contain both historical EOD prices and today's quotes


*(this report was produced on: 2015-01-02)*
