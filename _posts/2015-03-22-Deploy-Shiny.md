---
layout: post
title: Deploy Shiny Application
comments: true
---


[Shiny](http://shiny.rstudio.com/) is a very convenient way to add interface to your R script / application.

The next big question is how to share your [Shiny](http://shiny.rstudio.com/) application with clients.
Probably the easiest way is to host your [Shiny](http://shiny.rstudio.com/) application on the
[Shiny Server](http://www.rstudio.com/products/shiny/shiny-server/). But let's say the application
is computationally intensive, it uses client's proprietary data, and you want to create a stand-alone
version that can be run locally on the client's computer.

Following are few tutorials with step by step instructions to package your 
[Shiny](http://shiny.rstudio.com/) application into stand-alone application:

* [Packaging your Shiny App as an Windows desktop app](http://blog.analytixware.com/2014/03/packaging-your-shiny-app-as-windows.html)
* [Deploying Desktop Apps with R](http://oddhypothesis.blogspot.ca/2014/04/deploying-self-contained-r-apps-to.html)
* [A Simple Shiny App for Monitoring Trading Strategies  Part II](http://www.thertrader.com/2014/08/07/a-simple-shiny-app-for-monitoring-trading-strategies-part-ii/)

Basically, you need to get:

* [Portable R](http://sourceforge.net/projects/rportable/) and
* [Portable Chrome](http://portableapps.com/apps/internet/google_chrome_portable) or
* [Portable Firefox](http://portableapps.com/apps/internet/firefox_portable)

And follow setup steps outline in the posts.

There is also a **BIG question** of including / distributing proprietary code / algorithms.
It is not possible to hide R code, so you might resolve to using:

* [Rcpp](http://www.rcpp.org/) to code your proprietary algorithms and compile binaries or
* [Rturbo/P](http://www.ptechnologies.org/) to convert your proprietary R algorithms and create
[Rturbo/P](http://www.ptechnologies.org/) binaries




#Aside
If you want to play with [Shiny Server](http://www.rstudio.com/products/shiny/shiny-server/)
on Windows, following is a good start:

* [Can i host a shiny app on a windows machine?](http://stackoverflow.com/questions/16052441/can-i-host-a-shiny-app-on-a-windows-machine)
* [leondutoit/shiny-server-on-ubuntu](https://github.com/leondutoit/shiny-server-on-ubuntu)

And more info for Docker and Vagran:

* [Setting up a development environment using Docker and Vagrant](http://blog.zenika.com/index.php?post/2014/10/07/Setting-up-a-development-environment-using-Docker-and-Vagrant)
* [Docker for Windows](http://docs.docker.com/installation/windows/)
* [boot2docker](https://github.com/boot2docker/boot2docker)
* [boot2docker releases](https://github.com/boot2docker/windows-installer/releases)


