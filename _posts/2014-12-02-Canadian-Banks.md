---
layout: post
title: Canadian Banks
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.




Snapshot of Canadian Banks



{% highlight r %}
	#*****************************************************************
	# Load historical data
	#*****************************************************************
	library(SIT)
	load.packages('quantmod')
	
	tickers = '
    BNS.TO, # Bank of Nova Scotia
    TD.TO,  # TD Bank
    BMO.TO, # Bank of Montreal
    RY.TO,  # Royal Bank of Canada
    CM.TO,  # CIBC
    NA.TO,  # National Bank of Canada
	'	
	
	data <- new.env()
	getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
	
	
	data.today = getQuote.yahoo.today(ls(data))
		print(data.today)
{% endhighlight %}



|Name              |Symbol |Time   |Date      |   Open|   High|    Low|  Close|  Volume| Yesterday|
|:-----------------|:------|:------|:---------|------:|------:|------:|------:|-------:|---------:|
|BANK OF MONTREAL  |BMO.TO |3:59pm |12/5/2014 |  80.74|  80.86|  79.47|  80.27| 2153570|     80.21|
|BANK OF NOVA SCOT |BNS.TO |3:59pm |12/5/2014 |  67.61|  67.62|  65.88|  66.20| 3167248|     67.58|
|CA  DIAN IMPERIAL |CM.TO  |4:15pm |12/5/2014 | 103.51| 104.04| 100.92| 102.33| 1805522|    103.52|
|  TIO  L BANK OF  |  .TO  |3:59pm |12/5/2014 |  50.60|  50.90|  49.50|  49.70| 2027435|     50.40|
|ROYAL BANK OF CAN |RY.TO  |3:59pm |12/5/2014 |  80.63|  80.87|  79.42|  80.40| 2859260|     80.63|
|TORONTO-DOMINION  |TD.TO  |3:59pm |12/5/2014 |  54.30|  54.69|  53.14|  54.25| 5111388|     54.03|
    




{% highlight r %}
	bt.append.today(data, data.today)
	
	
		for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
	bt.prep(data, align='remove.na')

	#*****************************************************************
	# Code Strategies
	#*****************************************************************
	prices = data$prices

	plota.matplot(scale.one(last(prices,60)))
{% endhighlight %}

![plot of chunk plot-2](/public/images/2014-12-02-Canadian-Banks/plot-2-1.png) 

{% highlight r %}
	plota.matplot(scale.one(prices))
{% endhighlight %}

![plot of chunk plot-2](/public/images/2014-12-02-Canadian-Banks/plot-2-2.png) 

{% highlight r %}
	print(last(prices, 20))
{% endhighlight %}



|           | BMO.TO| BNS.TO|  CM.TO|   .TO| RY.TO| TD.TO|
|:----------|------:|------:|------:|-----:|-----:|-----:|
|2014-11-07 |  81.27|  67.48| 103.15| 54.09| 81.08| 55.80|
|2014-11-10 |  81.40|  68.20| 103.56| 53.97| 81.58| 56.34|
|2014-11-12 |  81.77|  68.59| 103.83| 54.27| 82.25| 57.00|
|2014-11-13 |  82.30|  68.74| 104.59| 54.50| 82.15| 57.05|
|2014-11-14 |  82.19|  68.66| 104.45| 54.61| 82.21| 57.06|
|2014-11-17 |  81.94|  68.77| 104.55| 55.06| 82.23| 57.23|
|2014-11-18 |  82.14|  69.17| 104.67| 54.18| 82.14| 56.99|
|2014-11-19 |  82.55|  69.66| 104.78| 54.23| 82.90| 56.98|
|2014-11-20 |  82.81|  69.92| 104.83| 53.84| 83.04| 57.16|
|2014-11-21 |  82.75|  70.00| 104.82| 53.58| 82.53| 56.88|
|2014-11-24 |  83.20|  70.19| 104.29| 53.33| 82.47| 56.77|
|2014-11-25 |  83.23|  70.13| 105.33| 53.22| 82.32| 56.79|
|2014-11-26 |  83.59|  70.20| 106.12| 53.22| 82.79| 57.08|
|2014-11-27 |  83.88|  70.85| 106.48| 53.22| 83.33| 57.50|
|2014-11-28 |  83.86|  70.50| 106.49| 53.13| 83.16| 57.62|
|2014-12-01 |  83.29|  69.13| 106.10| 52.32| 82.78| 57.07|
|2014-12-02 |  81.42|  68.94| 106.48| 51.83| 81.42| 56.81|
|2014-12-03 |  81.01|  68.99| 107.16| 51.26| 81.59| 56.91|
|2014-12-04 |  80.21|  67.58| 103.52| 50.40| 80.63| 54.03|
|2014-12-05 |  80.27|  66.20| 102.33| 49.70| 80.40| 54.25|
    



*(this report was produced on: 2014-12-07)*
