---
layout: page
title: Strategy
permalink: strategy/
---

Following are automated Strategy back-test and signal reports,
based on strategies discussed in blogs and at SSRN, that are automatically updated each night.

Please note that there in NO guarantee that these strategies will updated every night. I.e. if there
is a power outage or internet service interruption, the strategies pages will updated once power and 
internet service is restored.

Also please note that there is NO guarantee that historical performance is a good indication  of
future returns. Please use your own judgment and back-tests before investing. Following strategies 
are presented and updated only for educational purpose.

<ul>
{% for post in site.pages %}
	{% if post.url contains 'Strategy-' %}
	
	 {% assign report_name = post.url | split:'/' | last | remove:'.html' %}	
<li><a href="{{ BASE_PATH }}{{ post.url }}">{{ post.title }}</a>
<a href="{{ BASE_PATH }}{{ '/public/images/' | append:report_name | append:'/' | append:report_name | append:'.pdf' }}">[PDF]</a></li>
	{% endif %}
{% endfor %}
</ul>
