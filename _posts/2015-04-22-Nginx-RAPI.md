---
layout: post
title: Nginx RApi
comments: true
---

I put [Lua Rclient Library for Nginx and complete setup at GitHub repository for your convenience](https://github.com/systematicinvestor/lua-rclient/blob/master/nginx.zip)


I want to share a very simple and light way to setup custom api to R functions using [Nginx server](http://wiki.nginx.org/Main)

There are of course more complete solutions available, for example using:

* [Shiny by RStudio](http://shiny.rstudio.com/)
* [OpenCPU by Jeroen Ooms](https://www.opencpu.org/)


You will need [Nginx for Windows](http://nginx-win.ecsds.eu/) from [http://nginx-win.ecsds.eu/](http://nginx-win.ecsds.eu/)
This is a compiled version of [Nginx server](http://wiki.nginx.org/Main) that includes support for [Lua](http://wiki.nginx.org/HttpLuaModule)
I.e. [OpenResty](http://openresty.org/)

I used the [Nginx image processing server with OpenResty and Lua](http://leafo.net/posts/creating_an_image_server.html)
as the guide to make this setup.

I want to design following API:

* `http://localhost:8080/rapi/calc?1+2+3` will return 6=1+2+3
* `http://localhost:8080/rapi/D?x*x` will return derivative of x*x = x + x 


First, let's create a [Nginx configuration](http://wiki.nginx.org/Main) in `conf\nginx.conf` file:

{% highlight nginx %}
# These three directives should be tweaked for production
error_log stderr notice;
daemon off;
events { }

http {
	include       mime.types; 

	# rclient
	lua_package_path 'lualibs/?.lua;'; 

	server {
		listen 8080;

		# examples
		# http://localhost:8080/rapi/calc/23+92
		# http://localhost:8080/rapi/deriv/x*x    
		location ~ ^/rapi/(?<program>[^/]+)/(?<param>.*)$ {
			content_by_lua_file lualibs/serve_rapi.lua;
		}
	}

	server {
		listen       80;

		location / {
			root   html;
			index  index.html index.htm;		
		}
	}
}
{% endhighlight %}

Next we need to process `http://localhost:8080/rapi` R API requests in the `lualibs/serve_rapi.lua` file:

{% highlight lua %}
local program, param =
ngx.var.program, ngx.var.param

-- helper function to indicate not valid request
local function return_not_found(msg)
  ngx.status = ngx.HTTP_NOT_FOUND
  ngx.header["Access-Control-Allow-Origin"] = "*"
  ngx.header["Content-type"] = "text/html"
  ngx.say(msg or "not found")
  ngx.exit(0)
end

if not(program == "calc" or program == "deriv") then
  return_not_found("invalid program. only calc and deriv are supported")
end

local R = require "rclient"  
local r = R.connect()
r.text1 = param

local status, exception
-- catch errors
status, exception = pcall(function() 
  if program == "calc" then
  	r "text1 <- eval(parse(text=text1))"
  else
  	r "text1 <- deparse(D(parse(text=text1),'x'))"
  end
end)


local res
if not status then
	res  = exception
else
	res  = unpack(r.text1)
end

status = r.disconnect

--[How do I add Access-Control-Allow-Origin in NGINX?](http://serverfault.com/questions/162429/how-do-i-add-access-control-allow-origin-in-nginx)
ngx.header["Access-Control-Allow-Origin"] = "*"
ngx.header["Content-Type"] = "text/plain"
ngx.say("param: " .. param)
ngx.say("result: " .. res)
ngx.exit(0)
{% endhighlight %}

Now it is time to run a first test. 

Please start Rserve, following the example from [Lua Rclient Library](http://www.scilua.org/rclient.html)
{% highlight r %}
library("Rserve")
Rserve(args="--no-save")
{% endhighlight %}

I usually start Rserve with a batch file:
`Rterm.exe -e "library('Rserve');Rserve(args='--no-save')"`

Next, start [Nginx server](http://wiki.nginx.org/Main) and try following URL in your browser:

* `http://localhost:8080/rapi/calc?1+2+3` you should return 6
* `http://localhost:8080/rapi/D?x*x` you should return x + x

Now we can setup a simple html page to access above RAPI:

{% highlight html %}
<html>
<head>
<meta content="text/html;charset=utf-8" http-equiv="Content-Type">
<script type="text/javascript" src="jquery-1.11.2.min.js"></script>
</head>
<body>
<h3>Sample Nginx RApi</h3>
<div id="input">
    <select id="operation">
        <option value="calc">Calculator</option>
        <option value="deriv">Derivative</option>
    </select> 
    <br><br>
    <span>Please enter your expression and click Value</span>
    <br><br>
    <input id="expression" type="text" value="1+2" onkeydown="if (event.keyCode == 13) doValue()" />
    <button id="valueBtn" onclick="doValue()">Value</button>
</div>
<br><br>
<div id="output"></div>
  
<script type="text/javascript">
function doValue() {
    var url = "http://localhost:8080/rapi/" + $("#operation").val() + "/" + $("#expression").val();
    $.get(url, function (data) {
        //[How do I replace all line breaks in a string with <br /> tags?](http://stackoverflow.com/questions/784539/how-do-i-replace-all-line-breaks-in-a-string-with-br-tags)
        $("#output").html(data.replace(/(?:\r\n|\r|\n)/g, '<br />'));
    });
}
</script> 
<br>
<br>
</body>
</html>
{% endhighlight %}

This page can be accessed by going to `http://localhost/`

I put [Lua Rclient Library for Nginx and complete setup at GitHub repository for your convenience](https://github.com/systematicinvestor/lua-rclient/blob/master/nginx.zip)

It adds support for native [OpenResty](http://openresty.org/) sockets (i.e. 
[cosocket api](http://wiki.nginx.org/HttpLuaModule#ngx.socket.tcp))
instead of using [LuaSocket](http://w3.impa.br/%7Ediego/software/luasocket/)
following steps done at [pgmoon is a PostgreSQL client library](https://github.com/leafo/pgmoon)










Aside:
---

[Lua Rclient Library](http://www.scilua.org/rclient.html) at GitHub:

* https://github.com/stepelu/lua-rclient
* https://github.com/jucor/rclient/
	

To Debug Lua script, i highly recommend getting [ZeroBrane Studio](http://studio.zerobrane.com/)
Following are sample tutorials:

* [debugging-openresty-nginx-lua-scripts-with-zerobrane-studio](http://notebook.kulchenko.com/zerobrane/debugging-openresty-nginx-lua-scripts-with-zerobrane-studio)
* [debugging-lua-web-applications-using-zerobrane-studio-and-xavante](http://www.juliengilli.com/2013/01/03/debugging-lua-web-applications-using-zerobrane-studio-and-xavante/)



For example you can try following script

{% highlight lua %}
local R = require "rclient"
  
local r = R.connect()
  
-- Lua --> R:
r.myvec  = { 1,2,3 } -- Array.
r.text1 = '1+2'
r.text2 = 'x*x'
  
-- Execute R commands and evaluate expression as in R interpreter:
r "text1 <- eval(parse(text=text1))"
r "text2 <- deparse(D(parse(text=text2),'x'))"
r "text1" --> [1] 3
r "text2" --> [1] x + x

local status = r.disconnect
{% endhighlight %}


You might also want to try LuaJIT:

* [Get LuaJIT for windows](http://www.scilua.org/get.html)
* [LuaJIT profiler](http://www.freelists.org/post/luajit/LuaJIT-21-Profiler-released)


Interesting Lua project [castl](http://java-hackers.com/p/PaulBernier/castl)

I also recommend installing [Gow - The lightweight alternative to Cygwin](https://github.com/bmatzelle/gow/wiki)
to have easy access to curl and other useful utilities.
