---
title: "johplotR"
author: "Johannes Emmerling"
date: "6 June 2016"
output: html_document
---
johplotR - various default plots and special tools to create graphs and figures based on WITCH model runs.

Started in 2014 to have quick and reproducible sets of graphs for diagnostics, inspection, and publication. Happy to share and have people contributing to improve further.
Note that it is work in progress!!

## Main Functions and their arguments

### get_witch_variable
 
1.	variable_name, 	“K_EN”
2.	variable_name_save=variable_name, 	“Cars”
3.	additional_set="na",	“jreal“
4.	additional_set_id="na", 	“trad_cars” OR “all” for loading all elements (“all” does only load data, not produce the graph), or “sum” for summing over all elements of additional_set
5.	convert=1, 	1  to convert units (multiplying
6.	unit="", 	“GtCO2” (for charts y-axis)
7.	aggregation="regional", 	“regional” | “global_sum” | “global_mean”	
8.	bar="", 	“region” | “set”  if you want bar chart (use “regional” agg,)
9.	bar_x="time", 	“time” | “file2100”  over time or files/scenarios for one year
10.	bar_y="value", 	“value” | “share”
11.	bar_setvalues=".", 	c(“trad_cars”, “hybrid”)
12.	bar_colors=region_palette)	c(“black”, “blue”)
 
### witchmap
1.	variable_report, 	“K_EN”
2.	file_report, 	“results_ssp2_bau”
3.	t_report=20,	20 for 2100
4.	scale_min=0,	minimum of scale (0 for min of data)
5.	scale_max=0,	maximum of scale (0 for min of data)
6.	map_name="map"	“Temperature in 2100 in the BAU”
7.	map_legend="Legend"	Title of legend with colours	
8.	graphdir = "./"	folder to store the “map_name”.png file 
 
### saveplot
1.	plotname
2.	width=7
3.	height=5
4.	text_size=6



