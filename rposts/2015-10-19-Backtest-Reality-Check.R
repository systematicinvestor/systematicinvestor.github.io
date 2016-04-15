#' ---
#' layout: post
#' title: Back-test Reality Check
#' comments: true
#' ---
#' 
#' The purpose of a back-test is to show a realistic historical picture of strategy performance. 
#' One might use back-test results and corresponding statistics to judge whether a strategy is suitable one.
#' Hence, it is best to structure a back-test to be as realistic as possible in order to avoid unpleasant 
#' surprises and have solid foundation for selecting a suitable strategy.
#' 
#' First strategy outline: the strategy is the strategic equal weight allocation
#' across following 5 stocks: MMM, AA, CAT, KO, HPQ. I selected these stocks from Dow Jones Industrial Average.
#' The allocation is updated monthly and back-test starts on Jan 1st, 1970 with $100,000 initial capital.
#' 
#' Let's start with the most simple back-test setup and incrementally add features to make it more realistic.
#' 
#' The most simple setup is to multiply weights vector by daily returns, based on adjusted prices,
#' to compute daily returns for the strategy. Please see below the equity line for the strategy (r.ew)
#' 

#+ echo=T
#*****************************************************************
# Load historical data
#*****************************************************************
library(SIT)
load.packages('quantmod')

tickers = 'MMM, AA, CAT, KO, HPQ'

data = env()
getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, set.symbolnames = T, auto.assign = T)
	# copy unadjusted prices
	data.raw = env(data)

	# adjusted prices
	for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na', fill.gaps = T)

#*****************************************************************
# Setup
#*****************************************************************
prices = data$prices
	n = ncol(prices)
	nperiods = nrow(prices)

period.ends = date.ends(prices,'months')
	  
models = list()
	
commission = list(cps = 0.01, fixed = 10.0, percentage = 0.0)
	
weights = rep.row(rep(1/n, n), len(period.ends))
	
#*****************************************************************
# r.ew
#******************************************************************
data$weight[] = NA
	data$weight[period.ends,] = weights
models$r.ew = bt.run(data, silent=T, trade.summary=T)

#*****************************************************************
# Create Report
#****************************************************************** 
print('#Dividend and Split Adjusted Asset Performance')
plota.matplot(scale.one(data$prices),main='Asset Performance')

plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)	    	
	mtext('Cumulative Performance', side = 2, line = 1)

print(plotbt.strategy.sidebyside(models, make.plot=F, return.table=T,perfromance.fn = engineering.returns.kpi))

#' 
#' There is a problem with above approach, it assumes that weights stay constant through out the month, or
#' alternatively that we re-balance strategy daily to the target allocation. However, in reality, we invest
#' at the end of the month and update allocations at the end of the next month. The proper solution is to
#' compute share allocation at the end of the month and update shares at the end of the next month (s.ew)
#' 

#+ echo=T
#*****************************************************************
# s.ew
#******************************************************************
data$weight[] = NA
	data$weight[period.ends,] = weights
models$s.ew = bt.run.share.ex(data, clean.signal=F, silent=T, trade.summary=T)

#*****************************************************************
# Create Report
#****************************************************************** 
plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)	    	
	mtext('Cumulative Performance', side = 2, line = 1)

print(plotbt.strategy.sidebyside(models, make.plot=F, return.table=T,perfromance.fn = engineering.returns.kpi))

#' 
#' One of the missing features of above approach is commissions. In reality, every time we make a transaction,
#' brokerage charge commissions. Let's add following commission structure: $10 fixed per transaction, plus
#' 1c per share (s.ew.com)
#' 

#+ echo=T
#*****************************************************************
# s.ew.com
#******************************************************************
data$weight[] = NA
	data$weight[period.ends,] = weights
models$s.ew.com = bt.run.share.ex(data, clean.signal=F, silent=T, commission=commission, trade.summary=T)

#*****************************************************************
# Create Report
#****************************************************************** 
plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)	    	
	mtext('Cumulative Performance', side = 2, line = 1)

print(plotbt.strategy.sidebyside(models, make.plot=F, return.table=T,perfromance.fn = engineering.returns.kpi))

#' 
#' Another missing feature of above approach is round lot share allocation. In reality, we don't acquire fractional
#' shares, most of the time we buy shares in round lots. Let's add 100 shares round lot requirement (s.ew.com.lot) 
#' 

#+ echo=T
#*****************************************************************
# s.ew.com.lot
#******************************************************************
data$weight[] = NA
	data$weight[period.ends,] = weights
models$s.ew.com.lot = bt.run.share.ex(data, clean.signal=F, silent=T, commission=commission, trade.summary=T,
	lot.size=100
)

#*****************************************************************
# Create Report
#****************************************************************** 
plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)	    	
	mtext('Cumulative Performance', side = 2, line = 1)

print(plotbt.strategy.sidebyside(models, make.plot=F, return.table=T,perfromance.fn = engineering.returns.kpi))

#' 	
#' Another missing feature of above approach is turnover control. In reality, we don't blindly
#' re-balance to new allocation, but instead evaluate the cost of re-balance and tracking error, and
#' only re-balance when needed.  Let's re-balance only if total absolute discrepancy between
#' current allocation and target allocation is greater than 5% (s.ew.com.lot.turnover) 
#' 

#+ echo=T
#*****************************************************************
# s.ew.com.lot.turnover
#******************************************************************
data$weight[] = NA
	data$weight[period.ends,] = weights
models$s.ew.com.lot.turnover = bt.run.share.ex(data, clean.signal=F, silent=T, commission=commission, trade.summary=T,
	lot.size=100,
	control = list(round.lot = list(select = 'minimum.turnover', diff.target = 5/100))
)

#*****************************************************************
# Create Report
#****************************************************************** 
plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)	    	
	mtext('Cumulative Performance', side = 2, line = 1)

print(plotbt.strategy.sidebyside(models, make.plot=F, return.table=T,perfromance.fn = engineering.returns.kpi))

#' 	
#' Another erroneous feature of above approach is automatic reinvestment of dividends. The back-test so far was
#' based on split and dividend adjusted prices. In reality, the dividends are deposited into account as cash and
#' allocated during next re-balance. Let's switch to raw, un-adjusted, prices and properly incorporate historical
#' splits and dividends into back-test (s.ew.com.lot.turnover.unadjusted)
#' 

#+ echo=T
#*****************************************************************
# For each asset, append dividend and split columns
#****************************************************************** 	
data = env()
getSymbols.extra(tickers, src = 'yahoo', from = '1970-01-01', env = data, set.symbolnames = T, auto.assign = T)
	data.raw = data
#bt.unadjusted.add.div.split(data.raw)
bt.unadjusted.add.div.split(data.raw, infer.div.split.from.adjusted=T)
	
bt.prep(data.raw, align='remove.na', fill.gaps = T)

#*****************************************************************
# s.ew.com.lot.turnover.unadjusted
#******************************************************************
data.raw$weight[] = NA
	data.raw$weight[period.ends,] = weights
models$s.ew.com.lot.turnover.unadjusted = bt.run.share.ex(data.raw, clean.signal=F, silent=T, commission=commission, trade.summary=T,
	lot.size=100,
	control = list(round.lot = list(select = 'minimum.turnover', diff.target = 5/100)),
	adjusted = F
)

#*****************************************************************
# Create Report
#****************************************************************** 
plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)	    	
	mtext('Cumulative Performance', side = 2, line = 1)

print(plotbt.strategy.sidebyside(models, make.plot=F, return.table=T,perfromance.fn = engineering.returns.kpi))

#' 
#' Another missing feature of above approach is taxes. In reality, unless you invest in tax
#' sheltered account, the taxes are due at the end of the year. Let's add tax event to the back-test
#' on the last day in April each year (s.ew.com.lot.turnover.unadjusted.tax)
#' 

#+ echo=T
#*****************************************************************
# s.ew.com.lot.turnover.unadjusted.tax
#******************************************************************
data.raw$weight[] = NA
	data.raw$weight[period.ends,] = weights
models$s.ew.com.lot.turnover.unadjusted.tax = bt.run.share.ex(data.raw, clean.signal=F, silent=T, commission=commission, trade.summary=T,
	lot.size=100,
	control = list(round.lot = list(select = 'minimum.turnover', diff.target = 5/100)),
	adjusted = F,
	# enable taxes
	tax.control = default.tax.control(),
	cashflow.control = list(
		taxes = list(
			#cashflows = event.at(prices, 'year', offset=60),
			cashflows = event.at(prices, period.ends = custom.date.bus('last day in Apr', prices, 'UnitedStates/NYSE'), offset=0),
			cashflow.fn = tax.cashflows,
			invest = 'update',
			type = 'fee.rebate'
		)
	)
)



#*****************************************************************
# Create Report
#****************************************************************** 
plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)	    	
	mtext('Cumulative Performance', side = 2, line = 1)

print(plotbt.strategy.sidebyside(models, make.plot=F, return.table=T,perfromance.fn = engineering.returns.kpi))

print('#Average Annual Portfolio Turnover')
layout(1)
barplot.with.labels(sapply(models, compute.turnover, data), 'Average Annual Portfolio Turnover')


#+ echo=T
m = models$s.ew.com.lot.turnover.unadjusted.tax

# aside plots
#plotbt.transition.map(m$weight, 'Tax')
#plota(make.xts(m$value,data$dates), type='l')
	

print('#Events for s.ew.com.lot.turnover.unadjusted.tax:')
print(to.nice(mlast(bt.make.trade.event.summary.table(m), 20),0))

# aside summaries 
#print(mlast(bt.make.cashflow.event.summary.table(m), 20))
#print(look.at.taxes(m)['2015'])
#print(tax.summary(m))

#' 	
#' I feel a lot more comfortable with latest version of back-test result and corresponding statistics
#' because it resembles reality.
#' 
#' There are still more issues that one might want to incorporate into their back-test settings. Here are few ideas:
#' 
#' * if allocation is based on the signal, unlike the sample strategy above, you might want to add execution lag.
#' i.e. signal is generated on the second to last day of the month and execution takes place on the last day of the month
#' 
#' * consider various cash flows over the life of the back-test. for example, 
#' 	+ a young investor, in his 20's, contributes 5% of initial capital each year for the first 10 years, 
#' 	+ next there are small withdrawals of 1% of initial capital each year for the next 30 years to cover family expenses, 
#' 	+ finally, in retirement stage the withdrawals raise to 5% of portfolio equity each year
#' 
#' In conclusion, do not blindly trust the back-test numbers and corresponding statistics, consider if back-test is actually
#' a good simulation of real portfolio performance. 
#' 
#' 
#' Please note that the supporting code for this post is still in development. If you want to experiment at your own
#' risk, please sign up for a beta testing by filling up the contact form.
#' 





