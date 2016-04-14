---
layout: post
title: Applied Portfolio VaR Decomposition
comments: true
---


To install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT) please visit [About](/about) page.





Pawel Lachowicz published very good tutorial at [Applied Portfolio VaR Decomposition. (1) Marginal and Component VaR.](http://www.quantatrisk.com/2015/01/18/applied-portfolio-value-at-risk-decomposition-1-marginal-and-component-var/).

Additional info:
---

* [Backtesting Asset Allocation portfolios](https://systematicinvestor.wordpress.com/2012/03/19/backtesting-asset-allocation-portfolios/)
* [Unproxying weight constraints](http://www.portfolioprobe.com/2011/04/13/unproxying-weight-constraints/)

Below I will try to adapt his code:



{% highlight r %}
#*****************************************************************
# Load historical data
#*****************************************************************
library(SIT)
load.packages('quantmod')
tickers = spl('AAPL,DIS,IBM,JNJ,KO,NKE,TXN')

data <- new.env()
	data$symbolnames = tickers
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na')

#*****************************************************************
# Setup
#*****************************************************************
prices = last(data$prices, 3*252)
	n = ncol(prices)
 last.price = last(prices)
 last.date = index(last(prices))
rets = prices / mlag(prices) - 1
#rets = diff(log(prices))
		rets = coredata(na.omit(rets))

shares = list(AAPL = 530, DIS = 1050, IBM = 150, JNJ = 12700, KO = 3100, NKE = 3500, TXN = 19650)
	shares = unlist(shares[colnames(prices)])

capital = sum(last.price * shares)
exposure = last.price * shares
weight = exposure / capital

#*****************************************************************
# Compute VaR
#*****************************************************************
compute.VaR = function(rets = NULL, cov.mat = cov(rets), 
	weight = rep(1/nrow(cov.mat), nrow(cov.mat)), 
 	capital = 1, con.levl = 0.95, date = Sys.Date()) 
 {	
	z.stat = qnorm(con.levl)
 	weight = as.vector(weight)
 
	asset.vol = abs(weight) * sqrt(diag(cov.mat))
	asset.VaR = z.stat * asset.vol
	undiversified.VaR = sum(asset.VaR)
 
	port.vol = sqrt(t(weight) %*% cov.mat %*% weight)[1]
 	port.VaR = z.stat * port.vol
 	beta = (cov.mat %*% weight) / port.vol^2
 
 	marginal.VaR = z.stat * (cov.mat %*% weight) / port.vol
	component.VaR = marginal.VaR * weight
 
 list(con.levl = con.levl,
 	date = date,
  	n = nrow(cov.mat),
  	capital = capital,
  	weight = weight,
  	tickers = colnames(cov.mat),

		 undiversified.VaR = capital * undiversified.VaR,
  	port.vol = sqrt(252) * port.vol,
  	port.VaR = capital * port.VaR,
  	asset.VaR = capital * asset.VaR,
  	component.VaR = capital * component.VaR,
  	component.VaR.percent = component.VaR / port.VaR,
  	marginal.VaR = marginal.VaR
 )
}

#*****************************************************************
# Helper function to display VaR
#*****************************************************************
summary.VaR = function(data.VaR, capital = 1) {
	plot.data = list(
	'Portfolio Risk Report as of' = format(data.VaR$date, '%d-%b-%Y'),
 	'VaR estimation at' = paste(to.percent(data.VaR$con.levl), 'confidence level'),
 	'Number of assets:' = data.VaR$n,
 	'Current exposure:' = to.cash(capital * data.VaR$capital),
 	'Portfolio VaR (undiversified):' = to.cash(capital * data.VaR$undiversified.VaR),
 	'Portfolio VaR (diversified):' = to.cash(capital * data.VaR$port.VaR),
 	'Component VaR [individual VaR]' = ''
 	)
 
	plot.data1 = cbind( 
 	to.percent(data.VaR$component.VaR.percent),
  	to.cash(capital * data.VaR$component.VaR),
  	to.cash(capital * data.VaR$asset.VaR)
  	)
 
	rbind(cbind(sapply(plot.data,identity),'',''), plot.data1)
}


#*****************************************************************
# Compute VaR
#*****************************************************************
base.VaR = compute.VaR(rets, weight = weight, capital = capital, date = last.date)
print(summary.VaR(base.VaR))
{% endhighlight %}



|Portfolio Risk Report as of    |20-Jan-2015          |           |           |
|:------------------------------|:--------------------|:----------|:----------|
|VaR estimation at              |95% confidence level |           |           |
|Number of assets:              |7                    |           |           |
|Current exposure:              |$2,985,822           |           |           |
|Portfolio VaR (undiversified): |$53,389.94           |           |           |
|Portfolio VaR (diversified):   |$39,584.48           |           |           |
|Component VaR [individual VaR] |                     |           |           |
|AAPL                           | 1.39%               |$   550.53 |$ 1,611.14 |
|DIS                            | 2.84%               |$ 1,122.53 |$ 1,831.47 |
|IBM                            | 0.48%               |$   190.06 |$   426.25 |
|JNJ                            |32.53%               |$12,878.04 |$16,899.04 |
|KO                             | 2.50%               |$   987.73 |$ 2,024.83 |
|NKE                            | 9.77%               |$ 3,865.66 |$ 7,352.19 |
|TXN                            |50.50%               |$19,989.94 |$23,245.03 |
    




{% highlight r %}
# compute min risk portfolio
ia = create.ia(rets)
	ia$cov = cov(rets)
load.packages('quadprog')
weight = min.var.portfolio(ia, create.basic.constraints(n))

min.risk.VaR = compute.VaR(rets, weight = weight, capital = capital, date = last.date)
print(summary.VaR(min.risk.VaR))
{% endhighlight %}



|Portfolio Risk Report as of    |20-Jan-2015          |           |           |
|:------------------------------|:--------------------|:----------|:----------|
|VaR estimation at              |95% confidence level |           |           |
|Number of assets:              |7                    |           |           |
|Current exposure:              |$2,985,822           |           |           |
|Portfolio VaR (undiversified): |$48,371.92           |           |           |
|Portfolio VaR (diversified):   |$32,915.71           |           |           |
|Component VaR [individual VaR] |                     |           |           |
|AAPL                           | 6.77%               |$ 2,227.32 |$ 5,649.27 |
|DIS                            | 1.79%               |$   588.23 |$   982.39 |
|IBM                            |14.93%               |$ 4,914.08 |$ 8,070.68 |
|JNJ                            |43.53%               |$14,329.55 |$17,075.95 |
|KO                             |25.24%               |$ 8,306.86 |$11,403.60 |
|NKE                            | 7.75%               |$ 2,549.66 |$ 5,190.02 |
|TXN                            | 0.00%               |$     0.00 |$     0.00 |
    




{% highlight r %}
#*****************************************************************
# Helper function to display VaR comparison for two portfolios
#*****************************************************************
compare.VaR = function(base.VaR, test.VaR) {
	plot.data = list(
	'Original position' = to.percent(base.VaR$weight),
	'Marginal VaR' = to.cash(base.VaR$marginal.VaR,5),
	'New position' = to.percent(test.VaR$weight),
	'Marginal VaR' = to.cash(test.VaR$marginal.VaR,5)
	)

	plot.data1 = list(
	'Portfolio VaR' = to.cash(c(base.VaR$port.VaR, test.VaR$port.VaR)),
	' ' = c('', to.percent(test.VaR$port.VaR / base.VaR$port.VaR - 1)),
	'Annulized Vol' = to.percent(c(base.VaR$port.vol, test.VaR$port.vol))
	)
 
	plot.data1 = sapply(plot.data1,identity)
 	plot.data1 = col.name2row(plot.data1)
	
	rbind(cbind(base.VaR$ticker, sapply(plot.data,identity)),
		cbind(plot.data1[1,], plot.data1[2,],'',plot.data1[3,],'')
		)
}


print('Risk-Minimising Position scenario:')
{% endhighlight %}



Risk-Minimising Position scenario:
    




{% highlight r %}
print(compare.VaR(base.VaR, min.risk.VaR))
{% endhighlight %}



|              |Original position |Marginal VaR |New position |Marginal VaR |
|:-------------|:-----------------|:------------|:------------|:------------|
|AAPL          | 1.93%            |$0.00955     | 6.77%       |$0.01102     |
|DIS           | 3.33%            |$0.01128     | 1.79%       |$0.01102     |
|IBM           | 0.79%            |$0.00807     |14.93%       |$0.01102     |
|JNJ           |43.08%            |$0.01001     |43.53%       |$0.01102     |
|KO            | 4.48%            |$0.00738     |25.24%       |$0.01102     |
|NKE           |10.97%            |$0.01180     | 7.75%       |$0.01102     |
|TXN           |35.41%            |$0.01891     | 0.00%       |$0.01110     |
|Portfolio VaR |$39,584.48        |             |$32,915.71   |             |
|              |                  |             |-16.85%      |             |
|Annulized Vol |12.79%            |             |10.64%       |             |
    



*(this report was produced on: 2015-01-22)*
