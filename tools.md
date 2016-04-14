---
layout: page
title: Tools
permalink: tools/
---

Following are automated researcg reports that are created each night:

<ul>
{% for post in site.pages %}
	{% if post.url contains 'Tool-' %}
	
	 {% assign report_name = post.url | split:'/' | last | remove:'.html' %}	
<li><a href="{{ BASE_PATH }}{{ post.url }}">{{ post.title }}</a>
<a href="{{ BASE_PATH }}{{ '/public/images/' | append:report_name | append:'/' | append:report_name | append:'.pdf' }}">[PDF]</a></li>
	{% endif %}
{% endfor %}
</ul>
