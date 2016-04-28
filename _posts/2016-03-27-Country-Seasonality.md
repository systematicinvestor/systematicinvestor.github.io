---
layout: post
title: Country Seasonality
comments: true
---
To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.




Meb Faber posted an interesting observation that if an asset or country is down 1/2/3 years in a row
it historically recovers in the next 1/2 years. Details at
[50% Returns Coming for Commodities and Emerging Markets?](http://mebfaber.com/2016/04/21/50-returns-coming-commodities-emerging-markets/)
post.

The test of this simple strategy for historical country returns is below.

Load historical country returns from AQR data library




{% highlight r %}
#*****************************************************************
# Read historical data from AQR library
# [Betting Against Beta: Equity Factors, Monthly](https://www.aqr.com/library/data-sets/betting-against-beta-equity-factors-monthly)
#******************************************************************
library(readxl)
library(SIT)
library(quantmod)

data = env()
data.set = 'betting-against-beta-equity-factors'

# monthly market returns in excess of t-bills
data$market.excess = load.aqr.data(data.set, 'monthly', 'MKT')

#monthly U.S. Treasury bill rates.
data$risk.free = load.aqr.data(data.set, 'monthly', 'RF', last.col2extract = 2)

# total market return
aqr.market = data$market.excess + as.vector(data$risk.free)

# compute equity 
equity = bt.apply.matrix(1+ifna(aqr.market,0),cumprod)
	equity[is.na(aqr.market)] = NA
	
year.ends = date.ends(equity, 'year')	

# create back-test environment with annual prices 
data = env()
for(n in names(aqr.market))
	data[[n]] = make.stock.xts(equity[year.ends,n])
bt.prep(data, align='keep.all', fill.gaps = F, dates='1986::')

plota.matplot( scale.one(data$prices), main='Asset Performance')
{% endhighlight %}

![plot of chunk plot-2](/public/images/2016-03-27-Country-Seasonality/plot-2-1.png) 


Look at historical stats for a strategy that invests in a country if is down 1/2/3 years in a row
and holds for the next 1/2/3 years



{% highlight r %}
#*****************************************************************
# Stats
#*****************************************************************
prices = data$prices
	n = ncol(prices)

ret = prices / mlag(prices) - 1
signal = ret < 0

signals = list(down1yr = signal, down2yr = signal & mlag(signal), down3yr=signal & mlag(signal) & mlag(signal,2))

# compute stats
make.stats(signals, ret)
{% endhighlight %}

![plot of chunk plot-3](/public/images/2016-03-27-Country-Seasonality/plot-3-1.png) 


The least frequent the event, the better its historical performance.
For example, after 3 years down there is on average a 30% bounce.
After 2 years down there is on average  a 20% bounce and 
after 1 year down there is on average a 15% bounce.

Next, create a strategy that invests in a country if is down 1/2/3 years in a row
and holds for the next 1/2/3 years



{% highlight r %}
#*****************************************************************
# Strategy
#*****************************************************************
models = run.models(signals, data)

strategy.performance.snapshoot(models, T)
{% endhighlight %}

![plot of chunk plot-4](/public/images/2016-03-27-Country-Seasonality/plot-4-1.png) NULL


Please note that exposure, the time strategy is invested, is small, around 30%, for 
the least frequent the events, i.e. 3 years down.

Overall, it is hard to compete with always invested, equal weight, strategy because
the timing signal, the number of years down, is not fully capturing all market movements.

**Below** is the same analysis for country historical returns sourced form 
Kenneth R. French - Data Library](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html)


![plot of chunk plot-5](/public/images/2016-03-27-Country-Seasonality/plot-5-1.png) 
![plot of chunk plot-6](/public/images/2016-03-27-Country-Seasonality/plot-6-1.png) 
![plot of chunk plot-7](/public/images/2016-03-27-Country-Seasonality/plot-7-1.png) NULL


The results are similar to the results obtained using country historical returns sourced from
[AQR - Betting Against Beta: Equity Factors, Monthly](https://www.aqr.com/library/data-sets/betting-against-beta-equity-factors-monthly)
data library.

These test were done based on absolute return being negative. 
The big question is if there is anything special about zero.
I.e. is it significant to test against returns being below zero,
or is it sufficient to test other values around zero.

For example, we may run the same tests using 1% as the trigger.
Alternatively the trigger value can be derived from recent market data
and adaptively adjusted as market enter new regimes.
 
 
 Supporting functions:
 ---
 


{% highlight r %}
#*****************************************************************
# Stats
#*****************************************************************
make.stats = function(signals, ret) {
	# compute stats
	stats = list()
	for(signal in names(signals)) {
		temp = signals[[signal]]
		stats[[paste(signal,'hold1yr')]] = coredata(ret)[ifna(mlag(temp),F)]
		stats[[paste(signal,'hold2yr')]] = 1/2 * coredata(ret + mlag(ret,-1))[ifna(mlag(temp),F)]
		stats[[paste(signal,'hold3yr')]] = 1/3 * coredata(ret + mlag(ret,-1) + + mlag(ret,-3))[ifna(mlag(temp),F)]
	}
	stats = lapply(stats,na.omit)
	# sapply(stats,mean) # mean

	# make a barplot
	par(mar = c(8, 4, 2, 1))
	boxplot(stats,las=2)
		abline(h=0,col='gray')
}

#*****************************************************************
# Strategy
#*****************************************************************
run.models = function(signals, data) {
	models = list()

	data$weight[] = NA
		data$weight[] = ntop(prices, n)
	models$equal.weight = bt.run(data, silent=T)

	for(signal in names(signals)) {
		temp = signals[[signal]]
		data$weight[] = NA
			data$weight[] = ntop(temp, n)
		models[[paste(signal,'hold1yr')]] = bt.run(data, silent=T)
		
		temp = signals[[signal]]
			temp = temp | mlag(temp)
		data$weight[] = NA
			data$weight[] = ntop(temp, n)
		models[[paste(signal,'hold2yr')]] = bt.run(data, silent=T)
		
		temp = signals[[signal]]
			temp = temp | mlag(temp) | mlag(temp,2)
		data$weight[] = NA
			data$weight[] = ntop(temp, n)
		models[[paste(signal,'hold3yr')]] = bt.run(data, silent=T)
	}

	models
}	
{% endhighlight %}



For your convenience, the [2016-03-27-Country-Seasonality](https://github.com/systematicinvestor/systematicinvestor.github.io/blob/master/rposts/2016-03-27-Country-Seasonality.r) post source code.

