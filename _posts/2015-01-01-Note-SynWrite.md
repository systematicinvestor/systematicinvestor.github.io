---
layout: post
title: SynWrite - very powerfull, free source code editor
---

I came across [SynWrite](http://www.uvviewsoft.com/synwrite/) editor and found it
extremely useful for writing Markdown and RMarkdown documents.

You can download [SynWrite at SourceForge](http://sourceforge.net/projects/synwrite/) or 
latest build can be downloaded directly from [UvViewSoft SynWrite page](http://www.uvviewsoft.com/synwrite/).

To setup R and RMarkdown I had to make a few small adjustment to default installation:

* install R Lexer: Options -> Add-on Manager -> Install: R Lexer
* add dot(.) as valid word character to R Lexer: Options -> Customize -> Lexer Overrides -> R Lexer -> Additional word character

I also had to create RMarkdown Lexer; there is a tutorial at Readme\TutorialLexer1.odt that I used as template

* Export Markdown Lexer
* Create New Lexer: Options -> Customize Lexer Library -> Add New Lexer
* Import Markdown Lexer and set file extension to Rmd
* In the Sub-Lexer tab, select R Lexer and
* Start Condition: ^```\{r.*\}
* End Condition: ^```
* Select a different background for R Lexer


