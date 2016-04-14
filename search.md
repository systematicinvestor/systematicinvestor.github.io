---
layout: page
title: Search
permalink: search/
---



<p>What are you looking for?</p>    

<input id="search" type="text" value="" class="form-control" onkeydown="if (event.keyCode == 13) doSearch()"/>

<div>
<input id="searchBtn" type="submit" class="btn" onclick="doSearch()" value="Search">
</div>

<div id="output"></div>

<script type="text/javascript" src="{{ site.baseurl }}public/js/search-min.js"></script>