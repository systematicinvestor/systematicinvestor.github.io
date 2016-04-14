---
layout: post
title: HomeBank - free and open source personal finance software
---

I came across [HomeBank](http://homebank.free.fr/)
while looking for[Free and open source personal finance software](http://www.finiki.org/wiki/Free_and_open_source_personal_finance_software) 
to analyze / reconcile my bank and credit card statements.

At first I was not able to import my bank statements, but once this hurdle was over, please see 
more below, it is a very useful / functional program to see where do you spend the money.
It come very handy during a Christmas's shopping.

At first I was not able to import my bank statements into the [HomeBank](http://homebank.free.fr/)
program. The bank statement is a CSV file with date, memo, debit, credit, balance fields.

I wrote tiny R script to reformat this CSV file into format that [HomeBank](http://homebank.free.fr/)
understands. I.e. [CSV file format](http://homebank.free.fr/help/misc-csvformat.html)

{% highlight r %}
library(SIT)
library(quantmod)
library(data.table)

data.file = 'mybank.file.csv'

data = fread(data.file, stringsAsFactors = F, sep = ',', header = FALSE, autostart = 1)
		setnames(data, spl('date,desc,debit,credit,balance'))

dates = as.Date(data$date, format='%m/%d/%Y')

data = data.frame(
	dates = format(dates, '%m-%d-%y'),
	amount = iif(is.na(data$debit), data$credit, -data$debit),
	desc = data$desc
)

out = data.frame(date = data$date, 
	paymode = '',
	info = '',
	payee = '',
	memo = data$desc,
	amount = data$amount,
	category = '',
	tags = '')

write.table(out, file = 'HomeBank.mybank.csv', quote = F, sep = ';', row.names = F, col.names = F)
{% endhighlight %}

Finally, I had to configure [Assignments](http://homebank.free.fr/help/dlg-assi.html) to automatically
map most of my transactions to income/expense categories. All done.

Please have a look a the [HomeBank User manual](http://homebank.free.fr/help/index.html)
for all wonderful features of this amazing program.  