---
layout: post
title: Data Proxy - extending time series with proxies
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.




This page will hold collection of Proxies I collected to extend historical time series.

Note: there are more examples at
[Composing Synthetic Prices For Extended Historical ETF Data](http://indexswingtrader.blogspot.ca/2014/05/composing-synthetic-prices-for-extended.html)



<!-- open collapse div, http://stackoverflow.com/questions/15917463/embedding-markdown-in-jekyll-html 
<a href="#self" class="acollapse">(hide)</a>

<button>-</button>hide
<div markdown="1">

<button>+</button>show
<div markdown="1" style="display:none;">   

<input type="button" value="-">Commodities:
-->




<input type="button" class="btn btn-sm" value="-">Commodities:
----

<div markdown="1">

{% highlight r %}
    #*****************************************************************
    # Load external data
    #******************************************************************   
    library(SIT)
    load.packages('quantmod')  

raw.data <- new.env()
    
    # TRJ_CRB file was downloaded from the 
    # http://www.corecommodityllc.com/CoreIndexes.aspx
    # select TR/CC-CRB Index-Total Return and click "See Chart"
    # on Chart page click "Download to Spreadsheet" link
    # copy TR_CC-CRB, downloaded file, to data folder
    temp = extract.table.from.webpage( join(readLines("data/TR_CC-CRB")), 'EODValue' )
    temp = join( apply(temp, 1, join, ','), '\n' )
    raw.data$CRB = make.stock.xts( read.xts(temp, format='%m/%d/%y' ) )
     
    # prfmdata.csv file was downloaded from the http://www.crbequityindexes.com/indexdata-form.php
    # for "TR/J CRB Global Commodity Equity Index", "Total Return", "All Dates"
    raw.data$CRB_2 = make.stock.xts( read.xts('data/prfmdata.csv', format='%m/%d/%Y' ) )

    #*****************************************************************
    # Load historical data
    #******************************************************************   
    load.packages('quantmod')  
        
    tickers = spl('GSG,DBC')
    data = new.env()
    
    data$CRB = raw.data$CRB
    data$CRB_2 = raw.data$CRB_2
    
    getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)   
        for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
{% endhighlight %}

Look at historical start date for each series:
 

{% highlight r %}
print(bt.start.dates(data))
{% endhighlight %}



|      |Start      |
|:-----|:----------|
|CRB   |1994-01-03 |
|CRB_2 |1999-12-31 |
|DBC   |2006-02-06 |
|GSG   |2006-07-21 |
    

There are 2 sources of historical commodity index data. Let's compare them with commodity ETFs.


{% highlight r %}
    proxy.test(data)
{% endhighlight %}

![plot of chunk plot-4](/public/images/2014-11-14-Data-Proxy/plot-4-1.png) 

|      |CRB   |CRB_2 |DBC   |GSG   |
|:-----|:-----|:-----|:-----|:-----|
|CRB   |      |66%   |89%   |88%   |
|CRB_2 |      |      |66%   |63%   |
|DBC   |      |      |      |90%   |
|      |      |      |      |      |
|Mean  |-0.1% | 9.5% | 0.9% |-4.4% |
|StDev |19.1% |26.6% |21.0% |25.0% |
    

The historical commodity index data from [crbequityindexes](http://www.crbequityindexes.com/indexdata-form.php)
, that I denoted CRB_2, looks very different over the common interval for all proxies


{% highlight r %}
    proxy.overlay.plot(data)
{% endhighlight %}

![plot of chunk plot-5](/public/images/2014-11-14-Data-Proxy/plot-5-1.png) 

On the all history chart CRB_2 is also different.



{% highlight r %}
    proxy.prices(data)  
{% endhighlight %}

![plot of chunk plot-6](/public/images/2014-11-14-Data-Proxy/plot-6-1.png) 

|      |CRB Price |CRB Total |CRB_2 Price |CRB_2 Total |DBC Price |DBC Total |GSG Price |GSG Total |
|:-----|:---------|:---------|:-----------|:-----------|:---------|:---------|:---------|:---------|
|Mean  | 8.2%     | 8.2%     |11.5%       |11.5%       |-0.8%     |-0.8%     |-7.5%     |-7.5%     |
|StDev |16.3%     |16.3%     |21.5%       |21.5%       |20.9%     |20.9%     |25.0%     |25.0%     |
    

Quick glance at historical time series does not show anything abnormal between Price and Adjusted Price series. 
    

{% highlight r %}
    tickers = spl('DBC, DBC.CRB=DBC+CRB')
    proxy.map(data, tickers)      
{% endhighlight %}

![plot of chunk plot-7](/public/images/2014-11-14-Data-Proxy/plot-7-1.png) 





</div>
            
Please use `CRB` to extend Commodities.


        
<input type="button" class="btn btn-sm" value="-">REIT ex-U.S.: 
---- 
(Dow Jones Global **ex-U.S.** Real Estate Securities Index)


<div markdown="1">
    

{% highlight r %}
create.proxy = function(tickers, proxy.map.tickers, raw.data = new.env()) {
    #*****************************************************************
    # Load historical data
    #******************************************************************   
    load.packages('quantmod')  
    
    tickers = spl(tickers)
    data = new.env()
    
    getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, raw.data = raw.data, auto.assign = T)    
        for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
                         
    #*****************************************************************
    # Compare
    #******************************************************************
    print(bt.start.dates(data))
    
    proxy.test(data)    
    
    proxy.overlay.plot(data)   
    
    proxy.prices(data)

    tickers = spl(proxy.map.tickers)
    proxy.map(data, tickers)
}
    

	create.proxy('RWX,VNQ,VGSIX', 'RWX, RWX.VNQ=RWX+VNQ, RWX.VNQ.VGSIX=RWX+VNQ+VGSIX')
{% endhighlight %}



|      |Start      |
|:-----|:----------|
|VGSIX |1996-06-28 |
|VNQ   |2004-09-29 |
|RWX   |2007-03-02 |
    


![plot of chunk plot-9](/public/images/2014-11-14-Data-Proxy/plot-9-1.png) 

|      |RWX   |VGSIX |VNQ   |
|:-----|:-----|:-----|:-----|
|RWX   |      |66%   |67%   |
|VGSIX |      |      |99%   |
|      |      |      |      |
|Mean  | 3.3% |12.6% |12.9% |
|StDev |25.4% |40.2% |39.1% |
    


![plot of chunk plot-9](/public/images/2014-11-14-Data-Proxy/plot-9-2.png) ![plot of chunk plot-9](/public/images/2014-11-14-Data-Proxy/plot-9-3.png) 

|      |RWX Price |RWX Total |VGSIX Price |VGSIX Total |VNQ Price |VNQ Total |
|:-----|:---------|:---------|:-----------|:-----------|:---------|:---------|
|Mean  | 3.3%     | 3.3%     |14.4%       |14.4%       |15.9%     |15.9%     |
|StDev |25.4%     |25.4%     |28.1%       |28.1%       |35.1%     |35.1%     |
    


![plot of chunk plot-9](/public/images/2014-11-14-Data-Proxy/plot-9-4.png) 
</div>

Please use `VNQ` and `VGSIX` to extend REIT ex-U.S.


> Aside comparison of RWO vs. RWX:
[RWO vs. RWX: Head-To-Head ETF Comparison](http://etfdb.com/tool/etf-comparison/RWO-RWX/)
    
    
<input type="button" class="btn btn-sm" value="+">Global REIT:
----
(Dow Jones Global Select Real Estate Securities Index)

<div markdown="1" style="display:none;">   

{% highlight r %}
	create.proxy('IYR,VGSIX,RWO', 'RWO, RWO.IYR=RWO+IYR, RWO.IYR.VGSIX=RWO+IYR+VGSIX')
{% endhighlight %}



|      |Start      |
|:-----|:----------|
|VGSIX |1996-06-28 |
|IYR   |2000-06-19 |
|RWO   |2008-05-22 |
    


![plot of chunk plot-10](/public/images/2014-11-14-Data-Proxy/plot-10-1.png) 

|      |IYR   |RWO   |VGSIX |
|:-----|:-----|:-----|:-----|
|IYR   |      |85%   |99%   |
|RWO   |      |      |85%   |
|      |      |      |      |
|Mean  |14.3% | 8.9% |16.2% |
|StDev |38.9% |29.8% |41.8% |
    


![plot of chunk plot-10](/public/images/2014-11-14-Data-Proxy/plot-10-2.png) ![plot of chunk plot-10](/public/images/2014-11-14-Data-Proxy/plot-10-3.png) 

|      |IYR Price |IYR Total |RWO Price |RWO Total |VGSIX Price |VGSIX Total |
|:-----|:---------|:---------|:---------|:---------|:-----------|:-----------|
|Mean  |14.5%     |14.5%     | 8.9%     | 8.9%     |14.4%       |14.4%       |
|StDev |29.6%     |29.6%     |29.8%     |29.8%     |28.1%       |28.1%       |
    


![plot of chunk plot-10](/public/images/2014-11-14-Data-Proxy/plot-10-4.png) 
</div>

Please use `IYR` and `VGSIX` to extend Global REIT.





<input type="button" class="btn btn-sm" value="+">CASH:
----

<div markdown="1" style="display:none;">   

{% highlight r %}
	#--------------------------------   
	# load 3-Month Treasury Bill from FRED (BIL)
	filename = 'data/TB3M.Rdata'
	if(!file.exists(filename)) {
		TB3M = quantmod::getSymbols('DTB3', src='FRED', auto.assign = FALSE)
		save(TB3M, file=filename)
	}
	load(file=filename)
	TB3M[] = ifna.prev(TB3M)
	#compute.raw.annual.factor(TB3M)
	raw.data$TB3M = make.stock.xts(processTBill(TB3M, timetomaturity = 1/4, 261))
	#--------------------------------   

	create.proxy('BIL,TB3M', 'BIL, BIL.TB3M=BIL+TB3M', raw.data)
{% endhighlight %}



|     |Start      |
|:----|:----------|
|TB3M |1954-01-04 |
|BIL  |2007-05-30 |
    


![plot of chunk plot-11](/public/images/2014-11-14-Data-Proxy/plot-11-1.png) 

|      |BIL  |TB3M |
|:-----|:----|:----|
|BIL   |     |26%  |
|      |     |     |
|Mean  |0.6% |0.7% |
|StDev |0.7% |0.3% |
    


![plot of chunk plot-11](/public/images/2014-11-14-Data-Proxy/plot-11-2.png) ![plot of chunk plot-11](/public/images/2014-11-14-Data-Proxy/plot-11-3.png) 

|      |BIL Price |BIL Total |TB3M Price |TB3M Total |
|:-----|:---------|:---------|:----------|:----------|
|Mean  |0.6%      |0.6%      |4.3%       |4.3%       |
|StDev |0.7%      |0.7%      |0.4%       |0.4%       |
    


![plot of chunk plot-11](/public/images/2014-11-14-Data-Proxy/plot-11-4.png) 

{% highlight r %}
	#--------------------------------   
	# load 3 years t-bill from FRED (BIL)
	filename = 'data/TB3Y.Rdata'
	if(!file.exists(filename)) {
		TB3Y = quantmod::getSymbols('DGS3', src='FRED', auto.assign = FALSE)
		save(TB3Y, file=filename)
	}
	load(file=filename)
	TB3Y[] = ifna.prev(TB3Y)
	#compute.raw.annual.factor(TB3Y)
	raw.data$TB3Y = make.stock.xts(processTBill(TB3Y, timetomaturity = 3, 261))
	#--------------------------------   

	create.proxy('SHY,TB3Y', 'SHY, SHY.TB3Y=SHY+TB3Y', raw.data)
{% endhighlight %}



|     |Start      |
|:----|:----------|
|TB3Y |1962-01-02 |
|SHY  |2002-07-31 |
    


![plot of chunk plot-11](/public/images/2014-11-14-Data-Proxy/plot-11-5.png) 

|      |SHY  |TB3Y |
|:-----|:----|:----|
|SHY   |     |87%  |
|      |     |     |
|Mean  |2.2% |2.5% |
|StDev |1.5% |2.7% |
    


![plot of chunk plot-11](/public/images/2014-11-14-Data-Proxy/plot-11-6.png) ![plot of chunk plot-11](/public/images/2014-11-14-Data-Proxy/plot-11-7.png) 

|      |SHY Price |SHY Total |TB3Y Price |TB3Y Total |
|:-----|:---------|:---------|:----------|:----------|
|Mean  |2.2%      |2.2%      |5.7%       |5.7%       |
|StDev |1.5%      |1.5%      |3.3%       |3.3%       |
    


![plot of chunk plot-11](/public/images/2014-11-14-Data-Proxy/plot-11-8.png) 

{% highlight r %}
	#--------------------------------   
	# load 10 years t-bill from FRED (BIL)
	filename = 'data/TB10Y.Rdata'
	if(!file.exists(filename)) {
		TB10Y = quantmod::getSymbols('DGS10', src='FRED', auto.assign = FALSE)
		save(TB10Y, file=filename)
	}
	load(file=filename)
	TB10Y[] = ifna.prev(TB10Y)
	#compute.raw.annual.factor(TB10Y)
	raw.data$TB10Y = make.stock.xts(processTBill(TB10Y, timetomaturity = 10, 261))
	#--------------------------------   

	create.proxy('IEF,VFITX,TB10Y', 'IEF, IEF.VFITX=IEF+VFITX, IEF.VFITX.TB10Y=IEF+VFITX+TB10Y', raw.data)
{% endhighlight %}



|      |Start      |
|:-----|:----------|
|TB10Y |1962-01-02 |
|VFITX |1996-06-20 |
|IEF   |2002-07-30 |
    


![plot of chunk plot-11](/public/images/2014-11-14-Data-Proxy/plot-11-9.png) 

|      |IEF  |TB10Y |VFITX |
|:-----|:----|:-----|:-----|
|IEF   |     |96%   |93%   |
|TB10Y |     |      |92%   |
|      |     |      |      |
|Mean  |6.0% |5.9%  |4.7%  |
|StDev |7.0% |9.4%  |5.1%  |
    


![plot of chunk plot-11](/public/images/2014-11-14-Data-Proxy/plot-11-10.png) ![plot of chunk plot-11](/public/images/2014-11-14-Data-Proxy/plot-11-11.png) 

|      |IEF Price |IEF Total |TB10Y Price |TB10Y Total |VFITX Price |VFITX Total |
|:-----|:---------|:---------|:-----------|:-----------|:-----------|:-----------|
|Mean  |6.0%      |6.0%      |6.8%        |6.8%        |5.9%        |5.9%        |
|StDev |7.0%      |7.0%      |9.6%        |9.6%        |5.2%        |5.2%        |
    


![plot of chunk plot-11](/public/images/2014-11-14-Data-Proxy/plot-11-12.png) 

{% highlight r %}
	#--------------------------------   
	# load 20 years t-bill from FRED (BIL)
	filename = 'data/TB20Y.Rdata'
	if(!file.exists(filename)) {
		TB20Y = quantmod::getSymbols('GS20', src='FRED', auto.assign = FALSE)
		save(TB20Y, file=filename)
	}
	load(file=filename)
  print(TB20Y[is.na(TB20Y)])
{% endhighlight %}



|           | GS20|
|:----------|----:|
|1987-01-01 |     |
|1987-02-01 |     |
|1987-03-01 |     |
|1987-04-01 |     |
|1987-05-01 |     |
|1987-06-01 |     |
|1987-07-01 |     |
|1987-08-01 |     |
|1987-09-01 |     |
|1987-10-01 |     |
|1987-11-01 |     |
|1987-12-01 |     |
|1988-01-01 |     |
|1988-02-01 |     |
|1988-03-01 |     |
|1988-04-01 |     |
|1988-05-01 |     |
|1988-06-01 |     |
|1988-07-01 |     |
|1988-08-01 |     |
|1988-09-01 |     |
|1988-10-01 |     |
|1988-11-01 |     |
|1988-12-01 |     |
|1989-01-01 |     |
|1989-02-01 |     |
|1989-03-01 |     |
|1989-04-01 |     |
|1989-05-01 |     |
|1989-06-01 |     |
|1989-07-01 |     |
|1989-08-01 |     |
|1989-09-01 |     |
|1989-10-01 |     |
|1989-11-01 |     |
|1989-12-01 |     |
|1990-01-01 |     |
|1990-02-01 |     |
|1990-03-01 |     |
|1990-04-01 |     |
|1990-05-01 |     |
|1990-06-01 |     |
|1990-07-01 |     |
|1990-08-01 |     |
|1990-09-01 |     |
|1990-10-01 |     |
|1990-11-01 |     |
|1990-12-01 |     |
|1991-01-01 |     |
|1991-02-01 |     |
|1991-03-01 |     |
|1991-04-01 |     |
|1991-05-01 |     |
|1991-06-01 |     |
|1991-07-01 |     |
|1991-08-01 |     |
|1991-09-01 |     |
|1991-10-01 |     |
|1991-11-01 |     |
|1991-12-01 |     |
|1992-01-01 |     |
|1992-02-01 |     |
|1992-03-01 |     |
|1992-04-01 |     |
|1992-05-01 |     |
|1992-06-01 |     |
|1992-07-01 |     |
|1992-08-01 |     |
|1992-09-01 |     |
|1992-10-01 |     |
|1992-11-01 |     |
|1992-12-01 |     |
|1993-01-01 |     |
|1993-02-01 |     |
|1993-03-01 |     |
|1993-04-01 |     |
|1993-05-01 |     |
|1993-06-01 |     |
|1993-07-01 |     |
|1993-08-01 |     |
|1993-09-01 |     |
    




{% highlight r %}
	TB20Y[] = ifna.prev(TB20Y)
  # alternative, linear approximate missing data
  #TB20Y[] = approx(1:nrow(TB20Y), as.vector(TB20Y), 1:nrow(TB20Y), method='linear')$y  
  
	#compute.raw.annual.factor(TB10Y)
	raw.data$TB20Y = make.stock.xts(processTBill(TB20Y, timetomaturity = 20, 12))
	#--------------------------------   

	create.proxy('TLT,VUSTX,TB20Y', 'TLT, TLT.VUSTX=TLT+VUSTX, TLT.VUSTX.TB20Y=TLT+VUSTX+TB20Y', raw.data)
{% endhighlight %}



|      |Start      |
|:-----|:----------|
|TB20Y |1953-04-01 |
|VUSTX |1989-12-14 |
|TLT   |2002-07-30 |
    


![plot of chunk plot-11](/public/images/2014-11-14-Data-Proxy/plot-11-13.png) 

|      |TB20Y  |TLT    |VUSTX  |
|:-----|:------|:------|:------|
|TB20Y |       |76%    |71%    |
|TLT   |       |       |99%    |
|      |       |       |       |
|Mean  |313.3% |267.3% |244.5% |
|StDev | 90.2% | 74.2% | 61.2% |
    


![plot of chunk plot-11](/public/images/2014-11-14-Data-Proxy/plot-11-14.png) ![plot of chunk plot-11](/public/images/2014-11-14-Data-Proxy/plot-11-15.png) 

|      |TB20Y Price |TB20Y Total |TLT Price |TLT Total |VUSTX Price |VUSTX Total |
|:-----|:-----------|:-----------|:---------|:---------|:-----------|:-----------|
|Mean  |155.4%      |155.4%      |  8.8%    |  8.8%    |  8.4%      |  8.4%      |
|StDev | 70.3%      | 70.3%      | 13.8%    | 13.8%    | 10.5%      | 10.5%      |
    


![plot of chunk plot-11](/public/images/2014-11-14-Data-Proxy/plot-11-16.png) 

Please note, there are missing observations in GS20 from 1987 to 1993, but VUSTX only begins
in Dec 1989. Hence, the overlap is missing 3 years of observations: 1987, 1988, 1989.
In this years we assumed that GS20 was constant at 7.28 as reported in 1986-12-01 7.28.

Another problem with monthly data is the date used to record each observation. These
are most likely month-end observations, but they are recorded as the first day of the month.

</div>


<input type="button" class="btn btn-sm" value="+">GOLD:
----

<div markdown="1" style="display:none;">   

{% highlight r %}
	#--------------------------------
	filename = 'data/GOLD.Rdata'
	if(!file.exists(filename)) {
		GOLD = bundes.bank.data.gold()
		save(GOLD, file=filename)
	}
	load(file=filename)
	raw.data$GOLD = make.stock.xts(GOLD)
	#--------------------------------

	create.proxy('GLD,GOLD', 'GLD, GLD.GOLD=GLD+GOLD', raw.data)		
{% endhighlight %}



|     |Start      |
|:----|:----------|
|GOLD |1968-04-01 |
|GLD  |2004-11-18 |
    


![plot of chunk plot-12](/public/images/2014-11-14-Data-Proxy/plot-12-1.png) 

|      |GLD   |GOLD  |
|:-----|:-----|:-----|
|GLD   |      |61%   |
|      |      |      |
|Mean  |11.6% |12.2% |
|StDev |20.6% |20.4% |
    


![plot of chunk plot-12](/public/images/2014-11-14-Data-Proxy/plot-12-2.png) ![plot of chunk plot-12](/public/images/2014-11-14-Data-Proxy/plot-12-3.png) 

|      |GLD Price |GLD Total |GOLD Price |GOLD Total |
|:-----|:---------|:---------|:----------|:----------|
|Mean  |11.1%     |11.1%     | 9.4%      | 9.4%      |
|StDev |20.3%     |20.3%     |20.1%      |20.1%      |
    


![plot of chunk plot-12](/public/images/2014-11-14-Data-Proxy/plot-12-4.png) 
</div>



{% highlight r %}
	#--------------------------------
	# FTSE NAREIT U.S. Real Estate Index monthly total return series
	# http://returns.reit.com/returns/MonthlyHistoricalReturns.xls
	# https://r-forge.r-project.org/scm/viewvc.php/pkg/FinancialInstrument/inst/parser/download.NAREIT.R?view=markup&root=blotter
	filename = 'data/NAREIT.xls'
	if(!file.exists(filename)) {
		url = 'http://returns.reit.com/returns/MonthlyHistoricalReturns.xls'
		download.file(url, filename,  mode = 'wb')
	}
	
	load.packages('gdata')
	temp = read.xls(filename, pattern='Date', sheet='Index Data', stringsAsFactors=FALSE)
	index = as.numeric(gsub(',','',temp$Index))
	# monthly data, with 1-month-year format
	NAREIT = make.xts(index, as.Date(paste(1,temp$Date),'%d %b-%y')) 
	raw.data$NAREIT = make.stock.xts(NAREIT)
	#--------------------------------
{% endhighlight %}




Let's save these proxies in **data.proxy.Rdata** for convience to use later on
----

Syntax to specify tickers in `getSymbols.extra` function:

* Basic : XLY
* Rename: BOND=TLT
* Extend: XLB+RYBIX
* Mix above: XLB=XLB+RYBIX+FSDPX+FSCHX+PRNEX+DREVX
* TLT;MTM + IEF => TLT = TLT + IEF and MTM = MTM + IEF
* BOND = TLT;MTM + IEF => BOND = TLT + IEF, TLT = TLT + IEF, and MTM = MTM + IEF
* BOND;US.BOND = TLT;MTM + IEF => BOND = TLT + IEF, US.BOND = TLT + IEF, TLT = TLT + IEF, and MTM = MTM + IEF
* BOND = [TLT] + IEF => BOND = TLT + IEF, and TLT = TLT + IEF

* use comma or new line to separate entries
* lines starting with `#` symbol or empty lines are skipped **Make sure not to use commas in comments**

,message=T, warning=T


{% highlight r %}
tickers = '
COM = DBC;GSG + CRB

RExUS = [RWX] + VNQ + VGSIX
RE = [RWX] + VNQ + VGSIX
RE.US = [ICF] + VGSIX

EMER.EQ = [EEM] + VEIEX
EMER.FI = [EMB] + PREMX

GOLD = [GLD] + GOLD,
US.CASH = [BIL] + TB3M,
SHY + TB3Y,

US.HY = [HYG] + VWEHX

# Bonds
US.BOND = [AGG] + VBMFX
INTL.BOND = [BWX] + BEGBX

JAPAN.EQ = [EWJ] + FJPNX
EUROPE.EQ = [IEV] + FIEUX
US.SMCAP = IWM;VB + NAESX
TECH.EQ = [QQQ] + ^NDX
US.EQ = [VTI] + VTSMX + VFINX
US.MID = [VO] + VIMSX
EAFE = [EFA] + VDMIX + VGTSX

MID.TR = [IEF] + VFITX
CORP.FI = [LQD] + VWESX
TIPS = [TIP] + VIPSX + LSGSX
LONG.TR = [TLT] + VUSTX
'


data.proxy <- new.env()
getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data.proxy, raw.data = raw.data, auto.assign = T)

data.proxy.raw = raw.data
save(data.proxy.raw, file='data/data.proxy.raw.Rdata',compress='gzip') 
save(data.proxy, file='data/data.proxy.Rdata',compress='gzip') 
{% endhighlight %}

To use saved proxy data to extend historical time series, use `extend.data.proxy` function:


{% highlight r %}
tickers = spl('BIL')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)

print(bt.start.dates(data))
{% endhighlight %}



|    |Start      |
|:---|:----------|
|BIL |2007-05-30 |
    




{% highlight r %}
extend.data.proxy(data, proxy.filename = 'data/data.proxy.Rdata')

print(bt.start.dates(data))
{% endhighlight %}



|    |Start      |
|:---|:----------|
|BIL |1954-01-04 |
    













*(this report was produced on: 2015-03-19)*
