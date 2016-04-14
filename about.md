---
layout: page
title: About
permalink: about/
---


This is a new home for [SystematicInvestor blog](https://systematicinvestor.wordpress.com/)
it was previously hosted at [Wordpress](https://systematicinvestor.wordpress.com/). 
I'm making this move to simplify / automate / ease / speed up process of sharing my ideas / blogging.


This blog is a collection of my thoughts that I want to share without any warranties or
guarantees. For more details, please visit [About Blog](/About-Blog/) post.

This blog is hosted on [GitHub Pages](https://pages.github.com/) and is written using 
[R Markdown](http://rmarkdown.rstudio.com/). For more details, please visit [Setup Steps](/Steps/) post.


I use [R](http://www.r-project.org/) for most of my research and created a Toolbox to help
test new ideas. I.e. [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT)
is a collection of tools that I use in my investment research.

There are two ways to load [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT)
functionality:

Please make sure that `curl` package is already installed and if not, please execute 
{% highlight r %}install.packages('curl', repos = 'http://cran.r-project.org'){% endhighlight %}


* First method is to install [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT)
package

{% highlight r %}
###############################################################################
# Install Systematic Investor Toolbox (SIT) package
# github.com/systematicinvestor/SIT
###############################################################################

# please first install SIT.date
devtools::install_github('systematicinvestor/SIT.date')

library(curl)
curl_download('https://github.com/systematicinvestor/SIT/raw/master/SIT.tar.gz', 'sit',mode = 'wb',quiet=T)
install.packages('sit', repos = NULL, type='source')
{% endhighlight %}

and next load [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT)
library

{% highlight r %}
library(SIT)
{% endhighlight %}

* Second method is to download source code and load it into R session

{% highlight r %}
###############################################################################
# Load Systematic Investor Toolbox (SIT)
# github.com/systematicinvestor/SIT
###############################################################################
library(curl)
con = gzcon(curl('https://github.com/systematicinvestor/SIT/raw/master/sit.gz','rb'))
	source(con)
close(con)
{% endhighlight %}


Both methods are equivalent. If you want to keep [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT)
always up-to-date second method is way to go. If you want to just to get [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT)
first method is easier because you only download it once, and later on just load
[Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT)
with `library(SIT)` command.
 
 
Once [Systematic Investor Toolbox (SIT)](https://github.com/systematicinvestor/SIT)
is loaded, using either of two methods above, you can for example test sample functionality
of `plota.test()` function.

{% highlight r %} 
###############################################################################
# Example Usage:
###############################################################################
# Run a plot test
plota.test()
{% endhighlight %}




In some of my coding I also use a mixture of these two methods. See below:

{% highlight r %}
###############################################################################
# Load Systematic Investor Toolbox (SIT)
# github.com/systematicinvestor/SIT
###############################################################################
library(curl)
if(!file.exists('../sit'))
	curl_download('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', '../sit',mode = 'wb',quiet=T)
con = gzcon(file('../sit', 'rb'))
	source(con)
close(con)
{% endhighlight %}


<p class="rss-subscribe">subscribe <a href="{{ "feed.xml" | prepend: site.baseurl }}">via RSS</a></p>