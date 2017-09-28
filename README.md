witchplotR - ploting tools for the WITCH model

Started in 2014 by Johannes Emmerling to have quick and reproducible sets of graphs for diagnostics, inspection, and publication. Happy to share and have people contributing to improve further.
Note that it is work in progress!!


## Main Functions and their arguments

gdxcompaR

  ShinyApp based dynamic comparison tool for multipleGDX files
  Simply run gdxcompaR.R in the main directory specifying you WITCH and results folders in this file.


get_witch_variable
 
	variable_name, 	“K_EN”
	variable_name_save=variable_name, 	“Cars”
	additional_set="na",	“jreal“
	additional_set_id="na", 	“trad_cars” OR “all” for loading all elements (“all” does only load data, not produce the graph), or “sum” for summing over all elements of additional_set
	convert=1, 	1  to convert units (multiplying
	unit="", 	“GtCO2” (for charts y-axis)
	aggregation="regional", 	“regional” | “global_sum” | “global_mean”	
	bar="", 	“region” | “set”  if you want bar chart (use “regional” agg,)
	bar_x="time", 	“time” | “file2100”  over time or files/scenarios for one year
	bar_y="value", 	“value” | “share”
	bar_setvalues=".", 	c(“trad_cars”, “hybrid”)
	bar_colors=region_palette)	c(“black”, “blue”)
 
witchmap

	variable_report, 	“K_EN”
	file_report, 	“results_ssp2_bau”
	t_report=20,	20 for 2100
	scale_min=0,	minimum of scale (0 for min of data)
	scale_max=0,	maximum of scale (0 for min of data)
	map_name="map"	“Temperature in 2100 in the BAU”
	map_legend="Legend"	Title of legend with colours	
	graphdir = "./"	folder to store the “map_name”.png file 
 
saveplot

	plotname
	width=7
	height=5
	text_size=6
	
Primary_Energy_Mix

Primary_Energy_Mix_Regional

Electricity_Mix

Intensity_Plot

Carbon_Price

Sectoral_Emissions

Electricity_Mix_Regional



