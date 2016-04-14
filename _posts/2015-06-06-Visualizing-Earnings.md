---
layout: post
title: Visualizing Earnings
comments: true
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](',base.url,'about) page.





I created a very simple function to download [Earnings Announcements form Zacks](http://www.zacks.com/stock/research/IBM/earnings-announcements).
The [zacks.info() function in data.r at github](https://github.com/systematicinvestor/SIT/blob/master/R/data.r)

Below I want to demonstrate how it can be used:



{% highlight r %}
#*****************************************************************
# Load historical data
#*****************************************************************
library(SIT)
load.packages('quantmod')

ticker = 'IBM'
data = env()

getSymbols.extra(ticker, src = 'yahoo', from = '1970-01-01', env = data,  set.symbolnames = T, auto.assign = T)
	for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na', fill.gaps = T)

data = data[[ticker]]
	
#*****************************************************************
# Load events
#*****************************************************************
events = zacks.info(ticker)

#*****************************************************************
# Highlight each quarterly earnings
#*****************************************************************
highlight = match(index(data), as.Date(events$earnings$Date, '%m/%d/%Y'))
	highlight = make.xts(highlight, index(data))	
	dates = highlight[!is.na(highlight)]
	highlight[] = ifna.prev(highlight)
	

x = data['2012::']
highlight = highlight['2012::']

plota(x)
	col = col.add.alpha(highlight, 20)
	plota.x.highlight(highlight, highlight != 0, col)
	plota.lines(x)
	plota.legend(ticker)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2015-06-06-Visualizing-Earnings/plot-2-1.png) 

{% highlight r %}
#*****************************************************************
# Compute daily volatility for each quarter
#*****************************************************************
ret = diff(log(Cl(x)))

hist.vol = 100 * sqrt(252) * tapply(ret, highlight, sd, na.rm=T)

temp = data.frame(date = index(dates), col=coredata(dates), hist.vol = hist.vol[match(dates, names(hist.vol))])
	temp = na.omit(temp)

par(mar = c(6, 4, 2, 2))
barplot(temp$hist.vol, col=col.add.alpha(temp$col, 20), names.arg=temp$date, las=2)
{% endhighlight %}

![plot of chunk plot-2](/public/images/2015-06-06-Visualizing-Earnings/plot-2-2.png) 

{% highlight r %}
#*****************************************************************
# Compare daily volatility from 2 days prior to earnings date to 2 days after 
# the earnings date versus all other days
#*****************************************************************
index = which(!is.na( match(index(ret), index(dates)) ))
index = c(index - 1, index -2, index, index + 1, index + 2)
index = index[index > 0 & index <= len(ret)]

temp = list(
	'Around Earnings' = sd(ret[index], na.rm=T),
	'All Other Days' = sd(ret[-index], na.rm=T)
)

temp = t(data.frame(temp))
	colnames(temp) = 'Historical Volatility'
	print(to.nice(100 * sqrt(252) * temp))
{% endhighlight %}



|                |Historical Volatility |
|:---------------|:---------------------|
|Around.Earnings |37.37                 |
|All.Other.Days  |14.69                 |
    




{% highlight r %}
#*****************************************************************
# Print info downloaded from Zacks
#*****************************************************************
for(n in ls(events)) {
	print('#', n)
	print(apply(events[[n]],2,remove.tags))
}
{% endhighlight %}



# dividends
    




|   |Payable Date |Amount |Announcement Date |Ex-Div Date |
|:--|:------------|:------|:-----------------|:-----------|
|1  |6/10/2015    |$1.30  |4/28/2015         |5/6/2015    |
|2  |3/10/2015    |$1.10  |1/27/2015         |2/6/2015    |
|3  |12/10/2014   |$1.10  |10/28/2014        |11/6/2014   |
|4  |9/10/2014    |$1.10  |7/29/2014         |8/6/2014    |
|5  |6/10/2014    |$1.10  |4/29/2014         |5/7/2014    |
|6  |3/10/2014    |$0.95  |1/28/2014         |2/6/2014    |
|7  |12/10/2013   |$0.95  |10/29/2013        |11/6/2013   |
|8  |9/10/2013    |$0.95  |7/30/2013         |8/7/2013    |
|9  |6/10/2013    |$0.95  |4/30/2013         |5/8/2013    |
|10 |3/9/2013     |$0.85  |1/29/2013         |2/6/2013    |
|11 |12/10/2012   |$0.85  |10/31/2012        |11/7/2012   |
|12 |9/10/2012    |$0.85  |7/31/2012         |8/8/2012    |
|13 |6/9/2012     |$0.85  |4/25/2012         |5/8/2012    |
|14 |3/10/2012    |$0.75  |2/1/2012          |2/8/2012    |
|15 |12/10/2011   |$0.75  |10/26/2011        |11/8/2011   |
|16 |9/10/2011    |$0.75  |7/27/2011         |8/8/2011    |
|17 |6/10/2011    |$0.75  |4/27/2011         |5/6/2011    |
|18 |3/10/2011    |$0.65  |1/26/2011         |2/8/2011    |
|19 |12/10/2010   |$0.65  |10/27/2010        |11/8/2010   |
|20 |9/10/2010    |$0.65  |7/28/2010         |8/6/2010    |
|21 |6/10/2010    |$0.65  |4/28/2010         |5/6/2010    |
|22 |3/10/2010    |$0.55  |1/28/2010         |2/8/2010    |
|23 |12/10/2009   |$0.55  |10/28/2009        |11/6/2009   |
|24 |9/10/2009    |$0.55  |7/29/2009         |8/6/2009    |
    




# earnings
    




|   |Date       |Period Ending |Estimate |Reported |Surprise       |Time        |
|:--|:----------|:-------------|:--------|:--------|:--------------|:-----------|
|1  |7/20/2015  |6/2015        |$3.79    |--       |--             |After Close |
|2  |4/20/2015  |3/2015        |$2.84    |$2.91    |0.07 (2.46%)   |After Close |
|3  |1/20/2015  |12/2014       |$5.41    |$5.81    |0.40 (7.39%)   |After Close |
|4  |10/20/2014 |9/2014        |$4.30    |$3.68    |-0.62 (14.42%) |After Close |
|5  |7/17/2014  |6/2014        |$4.31    |$4.32    |0.01 (0.23%)   |After Close |
|6  |4/16/2014  |3/2014        |$2.54    |$2.54    |0.00 (0.00%)   |After Close |
|7  |1/21/2014  |12/2013       |$6.01    |$6.13    |0.12 (2.00%)   |After Close |
|8  |10/16/2013 |9/2013        |$3.95    |$3.99    |0.04 (1.01%)   |After Close |
|9  |7/17/2013  |6/2013        |$3.78    |$3.91    |0.13 (3.44%)   |After Close |
|10 |4/18/2013  |3/2013        |$3.06    |$3.00    |-0.06 (1.96%)  |After Close |
|11 |1/22/2013  |12/2012       |$5.25    |$5.39    |0.14 (2.67%)   |After Close |
|12 |10/16/2012 |9/2012        |$3.62    |$3.62    |0.00 (0.00%)   |After Close |
|13 |7/18/2012  |6/2012        |$3.42    |$3.51    |0.09 (2.63%)   |After Close |
|14 |4/17/2012  |3/2012        |$2.64    |$2.78    |0.14 (5.30%)   |After Close |
|15 |1/19/2012  |12/2011       |$4.62    |$4.62    |0.00 (0.00%)   |After Close |
|16 |10/17/2011 |9/2011        |$3.22    |$3.28    |0.06 (1.86%)   |After Close |
|17 |7/18/2011  |6/2011        |$3.01    |$3.09    |0.08 (2.66%)   |After Close |
|18 |4/19/2011  |3/2011        |$2.29    |$2.31    |0.02 (0.87%)   |After Close |
|19 |1/18/2011  |12/2010       |$4.08    |$4.18    |0.10 (2.45%)   |After Close |
|20 |10/18/2010 |9/2010        |$2.75    |--       |--             |After Close |
|21 |7/19/2010  |6/2010        |$2.58    |--       |--             |After Close |
|22 |4/19/2010  |3/2010        |$1.94    |--       |--             |After Close |
|23 |1/19/2010  |12/2009       |$3.47    |--       |--             |After Close |
|24 |10/15/2009 |9/2009        |$2.39    |--       |--             |After Close |
|25 |7/16/2009  |6/2009        |$2.03    |--       |--             |After Close |
    




# guidance
    




|   |Date       |Average Estimate |Estimate Range  |
|:--|:----------|:----------------|:---------------|
|1  |4/20/2015  |$15.85           |$15.35 - $16.25 |
|2  |1/20/2015  |$16.45           |$15.17 - $18.55 |
|3  |10/20/2014 |$17.78           |$16.25 - $18.06 |
|4  |4/16/2014  |$17.90           |$17.45 - $18.15 |
|5  |1/21/2014  |$17.97           |$17.60 - $18.25 |
|6  |10/16/2013 |$16.87           |$16.73 - $16.96 |
|7  |7/17/2013  |$16.61           |$16.00 - $16.77 |
|8  |4/18/2013  |$16.74           |$16.61 - $16.85 |
|9  |1/22/2013  |$16.58           |$15.21 - $17.00 |
|10 |10/16/2012 |$15.14           |$15.02 - $15.30 |
|11 |7/18/2012  |$15.01           |$14.50 - $15.15 |
|12 |4/17/2012  |$14.87           |$14.18 - $15.23 |
|13 |1/19/2012  |$14.81           |$14.59 - $15.02 |
|14 |10/17/2011 |$13.31           |$13.18 - $13.50 |
|15 |7/18/2011  |$13.22           |$13.11 - $13.43 |
|16 |4/19/2011  |$13.06           |$12.85 - $13.22 |
|17 |1/18/2011  |$12.61           |$12.40 - $12.82 |
|18 |10/18/2010 |$11.28           |$11.08 - $11.40 |
|19 |7/19/2010  |$11.26           |$11.20 - $11.35 |
|20 |4/19/2010  |$11.07           |$9.61 - $11.28  |
|21 |1/19/2010  |$10.91           |$10.49 - $11.25 |
|22 |10/15/2009 |$9.78            |$9.69 - $10.00  |
|23 |9/8/2009   |$10.67           |$10.26 - $11.05 |
|24 |7/16/2009  |$9.19            |$8.72 - $9.50   |
    




# revisions
    




|   |Date      |Period Ending  |Previous |Current |Analyst Name |Analyst Firm                   |
|:--|:---------|:--------------|:--------|:-------|:------------|:------------------------------|
|1  |4/21/2015 |Dec 2016 (FY)  |$16.51   |$16.56  |--           |Cantor Fitzgerald & Co&hellip; |
|2  |4/21/2015 |Dec 2016 (FY)  |$15.23   |$15.90  |--           |Jefferies & Company            |
|3  |4/21/2015 |Dec 2016 (FY)  |$16.60   |$16.63  |--           |                               |
|4  |4/21/2015 |Mar 2016 (Q)   |$3.02    |$2.83   |--           |                               |
|5  |4/21/2015 |Mar 2016 (Q)   |$2.82    |$2.99   |--           |Cantor Fitzgerald & Co&hellip; |
|6  |4/21/2015 |Mar 2016 (Q)   |$3.25    |$3.12   |--           |Calyon Securities (USA&hellip; |
|7  |4/21/2015 |Mar 2016 (Q)   |$2.71    |$3.00   |--           |Jefferies & Company            |
|8  |4/21/2015 |Dec 2015 (Q)   |$5.34    |$5.31   |--           |Calyon Securities (USA&hellip; |
|9  |4/21/2015 |Dec 2015 (Q)   |$5.19    |$5.38   |--           |Jefferies & Company            |
|10 |4/21/2015 |Dec 2015 (Q)   |$5.78    |$5.48   |--           |Cantor Fitzgerald & Co&hellip; |
|11 |4/21/2015 |Dec 2015 (Q)   |$5.27    |$5.50   |--           |                               |
|12 |4/21/2015 |Dec 2015 (FY)  |$15.35   |$15.75  |--           |Jefferies & Company            |
|13 |4/21/2015 |Dec 2015 (FY)  |$15.66   |$15.91  |--           |                               |
|14 |4/21/2015 |Dec 2015 (FY)  |$15.85   |$15.88  |--           |Cantor Fitzgerald & Co&hellip; |
|15 |4/21/2015 |Sep 2015 (Q)   |$3.62    |$3.63   |--           |Calyon Securities (USA&hellip; |
|16 |4/21/2015 |Sep 2015 (Q)   |$3.66    |$3.74   |--           |Jefferies & Company            |
|17 |4/21/2015 |Sep 2015 (Q)   |$3.65    |$3.72   |--           |Cantor Fitzgerald & Co&hellip; |
|18 |4/21/2015 |Sep 2015 (Q)   |$3.64    |$3.76   |--           |                               |
|19 |4/21/2015 |Jun 2015 (Q)   |$3.96    |$3.79   |--           |                               |
|20 |4/21/2015 |Jun 2015 (Q)   |$3.78    |$3.81   |--           |Cantor Fitzgerald & Co&hellip; |
|21 |4/21/2015 |Jun 2015 (Q)   |$3.76    |$3.78   |--           |Jefferies & Company            |
|22 |4/21/2015 |Jun 2015 (Q)   |$3.82    |$3.90   |--           |Calyon Securities (USA&hellip; |
|23 |4/20/2015 |Mar 2016 (Q)   |$3.19    |$3.20   |--           |                               |
|24 |4/20/2015 |Dec 2015 (Q)   |$5.65    |$5.62   |--           |                               |
|25 |4/20/2015 |Dec 2015 (FY)  |$16.00   |$16.15  |--           |                               |
|26 |4/20/2015 |Sep 2015 (Q)   |$3.74    |$3.86   |--           |                               |
|27 |4/20/2015 |Jun 2015 (Q)   |$3.77    |$3.75   |--           |                               |
|28 |4/17/2015 |Mar 2015 (Q)   |$3.20    |$2.81   |--           |                               |
|29 |4/17/2015 |Mar 2015 (Q)   |$3.01    |$2.79   |--           |Jefferies & Company            |
|30 |4/16/2015 |Dec 2017 (FY)  |$18.10   |$17.35  |--           |Calyon Securities (USA&hellip; |
|31 |4/16/2015 |Dec 2016 (FY)  |$17.30   |$16.50  |--           |Calyon Securities (USA&hellip; |
|32 |4/16/2015 |Dec 2015 (FY)  |$16.00   |$15.75  |--           |Calyon Securities (USA&hellip; |
|33 |4/16/2015 |Mar 2015 (Q)   |$2.89    |$2.97   |--           |Calyon Securities (USA&hellip; |
|34 |4/14/2015 |Mar 2015 (Q)   |$2.84    |$2.83   |--           |                               |
|35 |1/20/2015 |Dec 2014 (FY)  |$16.14   |$16.05  |--           |Calyon Securities (USA&hellip; |
|36 |1/15/2015 |Dec 2014 (FY)  |--       |$16.24  |--           |                               |
|37 |1/12/2015 |Dec 2014 (FY)  |$16.08   |$16.09  |--           |Jefferies & Company            |
    




# splits
    




No Records Found
    




# webcasts
    




|   |Date       |Event            |Webcast |Transcript |Time    |
|:--|:----------|:----------------|:-------|:----------|:-------|
|1  |7/20/2015  |Q2 Earnings Call |        |--         |4:30 PM |
|2  |4/20/2015  |Q1 Earnings Call |        |--         |4:30 PM |
|3  |1/20/2015  |Q4 Earnings Call |        |Open       |4:30 PM |
|4  |1/20/2015  |Q4 Earnings Call |        |Open       |4:30 PM |
|5  |10/20/2014 |Q3 Earnings Call |        |Open       |4:30 PM |
|6  |10/20/2014 |Q3 Earnings Call |        |Open       |4:30 PM |
|7  |7/17/2014  |Q2 Earnings Call |        |--         |4:30 PM |
|8  |4/16/2014  |Q1 Earnings Call |        |Open       |4:30 PM |
|9  |1/21/2014  |Q4 Earnings Call |        |Open       |4:30 PM |
|10 |10/16/2013 |Q3 Earnings Call |        |Open       |4:30 AM |
|11 |7/17/2013  |Q2 Earnings Call |        |Open       |4:30 PM |
|12 |7/17/2013  |Q2 Earnings Call |        |Open       |4:30 PM |
|13 |4/18/2013  |Q1 Earnings Call |        |Open       |4:30 PM |
|14 |4/18/2013  |Q1 Earnings Call |        |Open       |4:30 PM |
|15 |1/22/2013  |Q4 Earnings Call |        |Open       |4:30 PM |
|16 |10/16/2012 |Q3 Earnings Call |        |Open       |4:30 PM |
|17 |7/18/2012  |Q2 Earnings Call |        |Open       |4:30 PM |
|18 |4/17/2012  |Q1 Earnings Call |        |Open       |4:30 PM |
|19 |1/19/2012  |Q4 Earnings Call |        |Open       |4:30 PM |
|20 |10/17/2011 |Q3 Earnings Call |        |Open       |4:30 PM |
|21 |7/18/2011  |Q2 Earnings Call |        |Open       |4:30 PM |
|22 |4/19/2011  |Q1 Earnings Call |        |Open       |4:30 PM |
|23 |1/18/2011  |Q4 Earnings Call |        |Open       |4:30 PM |
|24 |10/18/2010 |Q3 Earnings Call |        |Open       |4:30 PM |
|25 |7/19/2010  |Q2 Earnings Call |        |Open       |4:30 PM |
|26 |4/19/2010  |Q1 Earnings Call |        |Open       |4:30 PM |
|27 |1/19/2010  |Q4 Earnings Call |        |Open       |4:30 PM |
|28 |10/15/2009 |Q3 Earnings Call |        |Open       |4:30 PM |
|29 |7/16/2009  |Q2 Earnings Call |        |Open       |4:30 PM |
|30 |4/20/2009  |Q1 Earnings Call |        |Open       |4:30 PM |
|31 |7/17/2008  |Q2 Earnings Call |        |Open       |4:30 PM |
|32 |4/16/2008  |Q1 Earnings Call |--      |--         |4:30 PM |
|33 |10/16/2007 |Q3 Earnings Call |        |--         |4:30 PM |
|34 |7/18/2007  |Q2 Earnings Call |        |--         |4:30 PM |
    

As always, I'm very amazed with [R](http://www.r-project.org/). 
It took us just 100 lines of code to generate all this useful info.



*(this report was produced on: 2015-06-05)*
