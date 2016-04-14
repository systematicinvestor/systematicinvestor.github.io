---
layout: post
title: Shiny App Compact Definition
comments: true
---


Following is an Idea to define [Shiny](http://shiny.rstudio.com/) application as
one structure instead of splitting Server and UI parts.

For example, a [signle file shiny app](http://shiny.rstudio.com/articles/single-file.html)

{% highlight r %}
server <- function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs), col = 'darkgray', border = 'white')
  })
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs", "Number of observations:", min = 10, max = 500, value = 100)
    ),
    mainPanel(plotOutput("distPlot"))
  )
)
{% endhighlight %}

Can be coded using following syntax. I.e. tags are started with **

{% highlight r %}
**shiny
**sidebar
**slider='obs' 'Number of observations:' min=10 max=500 value=100
**main
**plot=distPlot
function={
    hist(rnorm(input$obs), col = 'darkgray', border = 'white')
  }
**shiny
{% endhighlight %}

[Another example](http://rmarkdown.rstudio.com/authoring_embedded_shiny.html)

{% highlight r %}
**shiny
**fluid
**select=region, 'Region:', choices = colnames(WorldPhones))
**plot=phonePlot function={
      barplot(WorldPhones[,input$region]*1000, 
              ylab = "Number of Telephones", xlab = "Year")
    }
**shiny
{% endhighlight %}

It is just an idea, there is no working parser.

[Sample Parser Idea](https://github.com/nin-jin/tree.d)