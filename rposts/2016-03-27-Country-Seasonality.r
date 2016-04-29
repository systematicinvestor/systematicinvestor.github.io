#' ---
#' layout: post
#' title: Country Seasonality
#' comments: true
#' ---
#'
#' Meb Faber posted an interesting observation that if an asset or country is down 1/2/3 years in a row
#' it historically recovers in the next 1/2 years. Details at
#' [50% Returns Coming for Commodities and Emerging Markets?](http://mebfaber.com/2016/04/21/50-returns-coming-commodities-emerging-markets/)
#' post.
#' 
#' The test of this simple strategy for historical country returns is below.
#'
#' Load historical country returns from AQR data library
#'

#+ chunk1,echo=F, ref.label="chunk2"


#+ echo=T
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

#'
#' Look at historical stats for a strategy that invests in a country if is down 1/2/3 years in a row
#' and holds for the next 1/2/3 years
#'

#+ echo=T
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


#' 
#' The least frequent the event, the better its historical performance.
#' For example, after 3 years down there is on average a 30% bounce.
#' After 2 years down there is on average  a 20% bounce and 
#' after 1 year down there is on average a 15% bounce.
#'
#' Next, create a strategy that invests in a country if is down 1/2/3 years in a row
#' and holds for the next 1/2/3 years
#'

#+ echo=T
#*****************************************************************
# Strategy
#*****************************************************************
models = run.models(signals, data)


#' 
#' Please note that exposure, the time strategy is invested, is small, around 30%, for 
#' the least frequent the events, i.e. 3 years down.
#' 
#' Overall, it is hard to compete with always invested, equal weight, strategy because
#' the timing signal, the number of years down, is not fully capturing all market movements.
#' 
#' **Below** is the same analysis for country historical returns sourced form 
#' Kenneth R. French - Data Library](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html)
#'

#+ echo=F, eval=T
#*****************************************************************
# Read historical data from Kenneth R. French - Data Library
# [U.S. Research Returns Data](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors.zip)
# [International Research Returns Data](http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_International_Countries.zip)
#******************************************************************
data.set = 'F-F_Research_Data_Factors'
data = get.fama.french.data(data.set, 'months')
	US.Mkt.RF = data[[1]]$Mkt.RF
	US.RF = data[[1]]$RF

data.set = 'F-F_International_Countries'
data = get.fama.french.data(data.set, 'months', file.suffix='')

temp = env()
for(n in ls(data))
	temp[[n]] = make.stock.xts(cumprod(1+data[[n]][[1]]$.Mkt/100))
temp$US = make.stock.xts(cumprod(1+(US.Mkt.RF + US.RF)/100))

bt.prep(temp, align='keep.all', fill.gaps = F, dates='1975::')

data = bt.change.periodicity(temp,'years') 

plota.matplot( scale.one(bt.apply.matrix(data$prices,ifna.prev)), main='Asset Performance')

#+ echo=F, eval=T
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

#+ echo=F, eval=T
#*****************************************************************
# Strategy
#*****************************************************************
models = run.models(signals, data)


#' 
#' The results are similar to the results obtained using country historical returns sourced from
#' [AQR - Betting Against Beta: Equity Factors, Monthly](https://www.aqr.com/library/data-sets/betting-against-beta-equity-factors-monthly)
#' data library.
#'
#' These test were done based on absolute return being negative. 
#' The big question is if there is anything special about zero.
#' I.e. is it significant to test against returns being below zero,
#' or is it sufficient to test other values around zero.
#'
#' For example, we may run the same tests using 1% as the trigger.
#' Alternatively the trigger value can be derived from recent market data
#' and adaptively adjusted as market enter new regimes.
#'  


#'  
#'  Supporting functions:
#'  ---
#'  

#+ chunk2, echo=T, eval=T
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
	models$equal.weight = bt.run(data, trade.summary=T, silent=T)

	for(signal in names(signals)) {
		temp = signals[[signal]]
		data$weight[] = NA
			data$weight[] = ntop(temp, n)
		models[[paste(signal,'hold1yr')]] = bt.run(data, trade.summary=T, silent=T)
		
		temp = signals[[signal]]
			temp = temp | mlag(temp)
		data$weight[] = NA
			data$weight[] = ntop(temp, n)
		models[[paste(signal,'hold2yr')]] = bt.run(data, trade.summary=T, silent=T)
		
		temp = signals[[signal]]
			temp = temp | mlag(temp) | mlag(temp,2)
		data$weight[] = NA
			data$weight[] = ntop(temp, n)
		models[[paste(signal,'hold3yr')]] = bt.run(data, trade.summary=T, silent=T)
	}

	# create report
	plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL)	    	
		mtext('Cumulative Performance', side = 2, line = 1)
	
	print(plotbt.strategy.sidebyside(models, make.plot=F, return.table=T,perfromance.fn = engineering.returns.kpi))
	
	models
}	







#+ echo=F, eval=F
# Aside NOT USED
# create a pdf report with summary stats
pdf(file = 'Report.pdf', width=8.5, height=11)     
   	strategy.performance.snapshoot(models,data = data,
   	control = list(main = T,    comparison = T, transition = T, monthly = T)
   	)
dev.off()

#+ echo=F, eval=F
# save signal and return files
write.xts(signals$down1yr, 'down1yr.csv')
write.xts(ret, 'ret.csv')

