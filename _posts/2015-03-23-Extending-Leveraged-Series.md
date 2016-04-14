---
layout: post
title: Extending-Leveraged-Series
comments: true
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.





We can use [extend.data](https://github.com/systematicinvestor/SIT/blob/master/R/data.r) function 
to extend time series using historical proxy. For example:
'EEM = extend.data(EEM, VEIEX, scale=T)`

The same procedure can be applied to extend leverage series. For example
[ProShares Ultra 20+ Year Treasury (UBT)](http://www.proshares.com/funds/ubt.html)
can be extended using properly leveraged [iShares 20+ Year Treasury Bond (TLT)](http://www.ishares.com/us/products/239454/ishares-20-year-treasury-bond-etf)
series.

Let's look at few examples:


{% highlight r %}
#*****************************************************************
# Load historical data
#*****************************************************************
library(SIT)
load.packages('quantmod')

tickers = spl('TMF,UBT,TLT')

data = new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)   

TLTx2 = create.leveraged(data$TLT, leverage=2)    
proxy.test(list(UBT=data$UBT, TLTx2=TLTx2),price.fn=Ad)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2015-03-23-Extending-Leveraged-Series/plot-2-1.png) 

|      |TLTx2 |UBT   |
|:-----|:-----|:-----|
|TLTx2 |      |99%   |
|      |      |      |
|Mean  |22.5% |22.2% |
|StDev |30.6% |30.1% |
    

The [create.leveraged](https://github.com/systematicinvestor/SIT/blob/master/R/data.r) function
simply multiplies out the daily series returns by the given leverage factor.

Another example is [Direxion Daily 20+ Yr Trsy Bull 3X ETF (TMF)](http://www.direxioninvestments.com/products/direxion-daily-20-year-treasury-bull-3x-etf); 
it is based on a different bond index than [iShares 20+ Year Treasury Bond (TLT)](http://www.ishares.com/us/products/239454/ishares-20-year-treasury-bond-etf),
so it is not a perfect proxy, but will do for our testing purposes.



{% highlight r %}
TLTx3 = create.leveraged(data$TLT, leverage=2)    
proxy.test(list(TMF=data$TMF, TLTx3=TLTx3),price.fn=Ad)
{% endhighlight %}

![plot of chunk plot-3](/public/images/2015-03-23-Extending-Leveraged-Series/plot-3-1.png) 

|      |TLTx3 |TMF   |
|:-----|:-----|:-----|
|TLTx3 |      |100%  |
|      |      |      |
|Mean  |17.2% |22.9% |
|StDev |31.0% |47.2% |
    

Finally let's plot the evolution of extended 1x, 2x, 3x leverage treasury bond ETFs.


{% highlight r %}
UBT = extend.data(data$UBT, create.leveraged(data$TLT, leverage=2), scale=T)
TMF = extend.data(data$TMF, create.leveraged(data$TLT, leverage=3), scale=T)    

proxy.test(list(TLT=data$TLT, UBT=UBT, TMF=TMF),price.fn=Ad)
{% endhighlight %}

![plot of chunk plot-4](/public/images/2015-03-23-Extending-Leveraged-Series/plot-4-1.png) 

|      |TLT   |TMF   |UBT   |
|:-----|:-----|:-----|:-----|
|TLT   |      |100%  |100%  |
|TMF   |      |      |100%  |
|      |      |      |      |
|Mean  | 8.7% |24.9% |17.4% |
|StDev |13.8% |41.9% |27.4% |
    


*(this report was produced on: 2015-03-24)*
