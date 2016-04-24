#' ---
#' layout: post
#' title: SIT.blog - Wrappers to ease and automate blog posting
#' comments: true
#' ---
#' 
#' 
#' SIT.blog package is the collection of functions to ease and automate blog posting.
#' 
#' The blog posts are created in the R script with special Markdown formating.
#' The blog R script is converted to the RMarkdown format using `knitr::spin()`
#' 
#' 
#' Check out following introduction to spin command:
#' ----
#' 
#' * [Knitr's best hidden gem: spin](http://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/)
#' * [Quick reporting: build a report based on an R script](http://yihui.name/knitr/demo/stitch/)
#'   + [knitr-spin.R source R file with Rmarkdown formating](https://raw.githubusercontent.com/yihui/knitr/master/inst/examples/knitr-spin.R)
#'   + [knitr-spin.Rmd resulting Rmarkdown file](https://raw.githubusercontent.com/yihui/knitr/master/inst/examples/knitr-spin.Rmd)
#' 
#' 
#' You can also check R scripts used to generate latest blog post at [http://github.com/systematicinvestor/systematicinvestor.github.io/tree/master/rposts](http://github.com/systematicinvestor/systematicinvestor.github.io/tree/master/rposts)
#' 
#' I use following process to to ease and automate blog posting:
#' ----
#' 
#' 1. Write blog post in R script with special Markdown formating. For example [Benchmark Plus](https://github.com/systematicinvestor/systematicinvestor.github.io/blob/master/rposts/2016-04-14-Benchmark-Plus.r) post.
#' 
#' 2. Execute following command:
#' 

#+ echo=T,eval=F
library(knitr)
library(SIT.blog)
 
# generate markdown post
run.posts('2016-02-28-Post.r')

#' 
#' 3. Once I'm happy with results, execute following command:
#' 

#+ echo=T,eval=F
run.posts('2016-02-28-Post.r', move.post = T, blog.folder = 'myblog')

#' 
#' This command will run the blog post and copy final markdown and corresponding images to the blog folder you specify.
#' 
#' Alternatively, you can run the same command to process all blog post in the current folder by execute following command:
#' 

#+ echo=T,eval=F
# process all post(s) that start with number(s)- and end with r
run.posts('^\\d*-.*.r$')

#' 
#' Please let me know if stumble upon any bugs.
#' 
#' The [SIT.blog](https://github.com/systematicinvestor/SIT.blog) is available at
#' [github.com/systematicinvestor/SIT.blog](https://github.com/systematicinvestor/SIT.blog)
#' 
#' Please install with `devtools::install_github('systematicinvestor/SIT.blog')`
#' 