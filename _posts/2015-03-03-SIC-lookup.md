---
layout: post
title: SIC lookup
comments: true
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.





[MKTSTK](https://mktstk.wordpress.com) posted steps to get [SIC industry code](https://mktstk.wordpress.com/2015/03/03/sic-lookup-by-stock-symbol/)
from [SEC's EDGAR website](https://www.sec.gov/edgar/searchedgar/companysearch.html)

Following their example, I added a simple function to extract data from [SEC's EDGAR website](https://www.sec.gov/edgar/searchedgar/companysearch.html)



{% highlight r %}
#*****************************************************************
# Load historical data
#*****************************************************************
library(SIT)
load.packages('quantmod')

ticker = 'AAPL'
info = edgar.info(ticker)

print(info$sic)
{% endhighlight %}



3571
ELECTRONIC COMPUTERS
    




{% highlight r %}
print(info$company)
{% endhighlight %}



|Company Info                                                  |
|:-------------------------------------------------------------|
|APPLE INC CIK#: 0000320193 (see all company filings)          |
|SIC: 3571 - ELECTRONIC COMPUTERS                              |
|State location: CA                                            |
|State of Inc.: CA                                             |
|Fiscal Year End: 0927                                         |
|formerly: APPLE COMPUTER INC (filings through 2007-01-04)     |
|formerly: APPLE COMPUTER INC/ FA (filings through 1997-07-28) |
|(Assistant Director Office: 3)                                |
|Get insider transactions for this issuer.                     |
    




{% highlight r %}
print(info$mailing)
{% endhighlight %}



|Mailing Address    |
|:------------------|
|ONE INFINITE LOOP  |
|CUPERTINO CA 95014 |
    




{% highlight r %}
print(info$business)
{% endhighlight %}



|Business Address   |
|:------------------|
|ONE INFINITE LOOP  |
|CUPERTINO CA 95014 |
|(408) 996-1010     |
    




{% highlight r %}
print(info$fillings)
{% endhighlight %}



|Filings  |Format                     |Description                                                                                                                         |Filing Date |File/Film Number    |
|:--------|:--------------------------|:-----------------------------------------------------------------------------------------------------------------------------------|:-----------|:-------------------|
|SD       |Documents                  |Acc-no: 0001193125-15-045292(34 Act) Size: 338 KB                                                                                   |2015-02-12  |001-3674315601802   |
|SC 13G   |Documents                  |Statement of acquisition of beneficial ownership by individualsAcc-no: 0000932471-15-003679(34 Act) Size: 39 KB                     |2015-02-10  |005-3363215591143   |
|8-K      |Documents                  |Current report, items 8.01 and 9.01Acc-no: 0001193125-15-039270(34 Act) Size: 394 KB                                                |2015-02-09  |001-3674315589180   |
|424B2    |Documents                  |Prospectus [Rule 424(b)(2)]Acc-no: 0001193125-15-031599(33 Act) Size: 435 KB                                                        |2015-02-03  |333-18819115571551  |
|FWP      |Documents                  |Filing under Securities Act Rules 163/433 of free writing prospectusesAcc-no: 0001193125-15-030106(34 Act) Size: 37 KB              |2015-02-03  |333-18819115569012  |
|SC 13G/A |Documents                  |[Amend]Statement of acquisition of beneficial ownership by individualsAcc-no: 0001086364-15-001457(34 Act) Size: 13 KB              |2015-02-02  |005-3363215565636   |
|424B2    |Documents                  |Prospectus [Rule 424(b)(2)]Acc-no: 0001193125-15-028777(33 Act) Size: 412 KB                                                        |2015-02-02  |333-18819115564954  |
|8-K      |Documents Interactive Data |Current report, items 8.01 and 9.01Acc-no: 0001193125-15-023732(34 Act) Size: 11 MB                                                 |2015-01-28  |001-3674315555430   |
|10-Q     |Documents Interactive Data |Quarterly report [Sections 13 or 15(d)]Acc-no: 0001193125-15-023697(34 Act) Size: 7 MB                                              |2015-01-28  |001-3674315555294   |
|8-K      |Documents                  |Current report, items 2.02 and 9.01Acc-no: 0001193125-15-021857(34 Act) Size: 143 KB                                                |2015-01-27  |001-3674315552085   |
|DEFA14A  |Documents                  |Additional definitive proxy soliciting materials and Rule 14(a)(12) materialAcc-no: 0001193125-15-017614(34 Act) Size: 865 KB       |2015-01-22  |001-3674315542585   |
|DEF 14A  |Documents                  |Other definitive proxy statementsAcc-no: 0001193125-15-017607(34 Act) Size: 1 MB                                                    |2015-01-22  |001-3674315542570   |
|NO ACT   |Documents                  |[Paper]No Action LetterAcc-no: 9999999997-15-000140(34 Act) Size: 1 KB                                                              |2014-12-30  |001-3674314008453   |
|NO ACT   |Documents                  |[Paper]No Action LetterAcc-no: 9999999997-15-000139(34 Act) Size: 1 KB                                                              |2014-12-29  |001-3674314008451   |
|NO ACT   |Documents                  |[Paper]No Action LetterAcc-no: 9999999997-14-016001(34 Act) Size: 1 KB                                                              |2014-12-11  |001-3674314008711   |
|NO ACT   |Documents                  |[Paper]No Action LetterAcc-no: 9999999997-14-015701(34 Act) Size: 1 KB                                                              |2014-12-04  |001-3674314008607   |
|CERTNYS  |Documents                  |[Paper]Certification by the New York Stock Exchange approving securities for listingAcc-no: 9999999997-14-015004(34 Act) Size: 1 KB |2014-11-13  |001-3674314007839   |
|8-A12B   |Documents                  |Registration of securities [Section 12(b)]Acc-no: 0001193125-14-409218(34 Act) Size: 18 KB                                          |2014-11-12  |001-36743141213987  |
|8-K      |Documents                  |Current report, items 8.01 and 9.01Acc-no: 0001193125-14-406296(34 Act) Size: 295 KB                                                |2014-11-10  |000-10030141209609  |
|424B2    |Documents                  |Prospectus [Rule 424(b)(2)]Acc-no: 0001193125-14-398779(33 Act) Size: 442 KB                                                        |2014-11-05  |333-188191141197794 |
|FWP      |Documents                  |Filing under Securities Act Rules 163/433 of free writing prospectusesAcc-no: 0001193125-14-395979(34 Act) Size: 20 KB              |2014-11-04  |333-188191141192688 |
|424B2    |Documents                  |Prospectus [Rule 424(b)(2)]Acc-no: 0001193125-14-394996(33 Act) Size: 431 KB                                                        |2014-11-04  |333-188191141190986 |
|10-K     |Documents Interactive Data |Annual report [Section 13 and 15(d), not S-K Item 405]Acc-no: 0001193125-14-383437(34 Act) Size: 12 MB                              |2014-10-27  |000-10030141175110  |
|8-K      |Documents                  |Current report, items 2.02 and 9.01Acc-no: 0001193125-14-376361(34 Act) Size: 168 KB                                                |2014-10-20  |000-10030141164188  |
|8-K      |Documents                  |Current report, items 8.01 and 9.01Acc-no: 0001193125-14-277193(34 Act) Size: 29 KB                                                 |2014-07-23  |000-1003014989234   |
|10-Q     |Documents Interactive Data |Quarterly report [Sections 13 or 15(d)]Acc-no: 0001193125-14-277160(34 Act) Size: 9 MB                                              |2014-07-23  |000-1003014989142   |
|8-K      |Documents                  |Current report, items 2.02 and 9.01Acc-no: 0001193125-14-275598(34 Act) Size: 212 KB                                                |2014-07-22  |000-1003014986891   |
|8-K      |Documents                  |Current report, item 5.02Acc-no: 0001193125-14-271698(34 Act) Size: 14 KB                                                           |2014-07-17  |000-1003014980728   |
|8-K      |Documents                  |Current report, items 5.03 and 9.01Acc-no: 0001193125-14-228798(34 Act) Size: 21 KB                                                 |2014-06-06  |000-1003014896955   |
|8-K      |Documents                  |Current report, item 5.02Acc-no: 0001181431-14-021923(34 Act) Size: 13 KB                                                           |2014-05-30  |000-1003014881425   |
|SD       |Documents                  |Acc-no: 0001193125-14-217311(34 Act) Size: 103 KB                                                                                   |2014-05-29  |000-1003014876438   |
|8-K      |Documents                  |Current report, items 8.01 and 9.01Acc-no: 0001193125-14-184969(34 Act) Size: 513 KB                                                |2014-05-06  |000-1003014817467   |
|424B2    |Documents                  |Prospectus [Rule 424(b)(2)]Acc-no: 0001193125-14-172859(33 Act) Size: 440 KB                                                        |2014-04-30  |333-18819114800336  |
|FWP      |Documents                  |Filing under Securities Act Rules 163/433 of free writing prospectusesAcc-no: 0001193125-14-168389(34 Act) Size: 54 KB              |2014-04-30  |333-18819114795638  |
|424B2    |Documents                  |Prospectus [Rule 424(b)(2)]Acc-no: 0001193125-14-164929(33 Act) Size: 419 KB                                                        |2014-04-29  |333-18819114791270  |
|S-8      |Documents                  |Securities to be offered to employees in employee benefit plansAcc-no: 0001193125-14-160171(33 Act) Size: 157 KB                    |2014-04-25  |333-19550914786592  |
|10-Q     |Documents Interactive Data |Quarterly report [Sections 13 or 15(d)]Acc-no: 0001193125-14-157311(34 Act) Size: 8 MB                                              |2014-04-24  |000-1003014782284   |
|8-K      |Documents                  |Current report, items 7.01 and 9.01Acc-no: 0001193125-14-154883(34 Act) Size: 26 KB                                                 |2014-04-23  |000-1003014779103   |
|8-K      |Documents                  |Current report, items 2.02 and 9.01Acc-no: 0001193125-14-154871(34 Act) Size: 213 KB                                                |2014-04-23  |000-1003014779088   |
|8-K      |Documents                  |Current report, items 5.02, 5.03, 5.07, and 9.01Acc-no: 0001193125-14-084697(34 Act) Size: 367 KB                                   |2014-03-05  |000-1003014670080   |
    




{% highlight r %}
#*****************************************************************
# Extract events
#*****************************************************************
events = aggregate(info$fillings[,'Filings'], list(info$fillings[,'Filing Date']),
	function(x) join(unique(x),'\n'))
	
dates = as.Date(events[,1], format='%Y-%m-%d')
events = events[,2]

#*****************************************************************
# Load historical data
#*****************************************************************
data = new.env()
getSymbols(ticker, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
  for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)

# plot prices
plota(data$AAPL['2015::'], type='ohlc', col=col.add.alpha('blue', 50))
	plota.legend(join(c(ticker, info$sic),' - '), 'blue', data$AAPL)
  
	# add overlay events  
	plota.text(data$AAPL[dates], events, cex=0.8) 
{% endhighlight %}

![plot of chunk plot-2](/public/images/2015-03-03-SIC-lookup/plot-2-1.png) 


*(this report was produced on: 2015-03-05)*
