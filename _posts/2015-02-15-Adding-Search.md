---
layout: post
title: Adding Search
comments: true
---


Once the blog / web site has sufficient content, Searching functionality is very critical.
There are a few options for adding a Search functionality to blog hosted on 
[GitHub pages](https://pages.github.com/):

* [Google Custom Search](https://www.google.ca/cse/)
* [Static JSON file](http://martin-thoma.com/jekyll-and-git/#tocAnchor-1-14)
* [Integartion with IndexDen](http://jshum.github.io/blog/2013/01/21/adding-search-to-jekyll/)

For this blog, I decide to take another path and leverage [search functionlity of GitHub](https://developer.github.com/v3/search/). 
The content for this blog is written in Markdown and is stored in [GitHub repository](https://github.com/systematicinvestor/systematicinvestor.github.io).
So let's search the Markdown code in the repository and translate urls of matches on to generated
[blog site](http://systematicinvestor.github.io/).

Following is a sample HTML and Javascript code that searches GitHub repository:

{% highlight html %}
<p>What are you looking for?</p>
<input id="search" type="text" value="asset"/>
<button id="searchBtn">Search</button>
<div id="output"></div>
{% endhighlight %}

{% highlight javascript %}
$("#searchBtn").bind("click", function() {
  var repo = "systematicinvestor/systematicinvestor.github.io"
  var search = $("#search").val();
  var url = "https://api.github.com/search/code?q='" + search + "' in:file language:markdown repo:"+repo+"&sort=indexed&order=desc"

	$.ajax({
	    beforeSend: function(request) {
	        request.setRequestHeader("Accept", "application/vnd.github.v3.text-match+json");
	    },
	    dataType: "json",
	    url: url,
	    success: function(data) {
	        var searchList = $("<ul />");
	        $.each(data.items, function(index, item) {
	            if(item.name) {
	                $("<li>" + item.name + "<br>" + item.text_matches[0].fragment + "</li>")
	                .appendTo(searchList);
	            }
	        });
	        $("#output").html(searchList);
		}
	});	
});
{% endhighlight %}

It is very light weight and heavily relies on  [search functionlity of GitHub](https://developer.github.com/v3/search/).

Please note that GitHub search is powered by Sunspot, and Partial word searches are not supported by Sunspot.