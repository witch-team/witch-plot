#main directory
main_directory = "C:/Users/Emmerling/Documents/Dropbox/Professional/FEEM/WITCH_CODING/witch/"
#main_directory = "C:/Users/Emmerling/Documents/Dropbox/Professional/FEEM/ADVANCE/WP3/"
#main_directory = "U:/CCSD/VARIE/WITCH/SSP/"


#all directoried with trailing slash "/"!
pathdir = c("SSPv14/", "SSPv15trunk/") 
pathdir = "ADB/"  #with trailing slash /

removepattern="results_ssp2_"    #results_ssp2_bau_damages_" #"results_"
restrict_files = "results_ssp2." #"."

pathdir = paste0(main_directory, pathdir)
graphdir = paste0(pathdir[1], "graphs/") #/graphs/ in first folder if multiple folders
#graphdir = "SSPcompare/"

#Name scenarios (otherwise it takes gdx filename)
scenlist <- c("REF", "INDC_2C", "INDC_2C_TRADE", "INDC", "INDC_TRADE", "OPT_2C")


yearmin=1990
yearmax = 2050


#Initialize default options, load all witch and other functions
source('functions/witch_functions.R')



#Main part, get data plots etc.
get_witch_variable("Q", "GDP", "iq", "y", 1, "T$", "global_sum", scenarios=c("REF"))
get_witch_variable("Q_BAU", "GDP_BAU", "iq", "y", 1, "T$", "global_sum")
get_witch_variable("Q_EMI", "CO2_Emissions", "e", "co2", 3.67, "GtCO2", "global_sum")
get_witch_variable("TEMP", "Temperature", "m", "atm", 1, "°C", "global_mean")
get_witch_variable("Q_EN", "SEN", "j", "el", 0.0036, "EJ", "global_sum")
get_witch_variable("Q_EMI", "CCS_Emissions", "e", "ccs", 3.67, "GtCO2", "global_sum")
get_witch_variable("tpes", "tpes", "na", "na", 0.0036, "1", "global_sum")
get_witch_variable("Q_EN", "Final_Energy", "j", "en", 0.0036, "EJ", "global_sum")
get_witch_variable("Q_PES", "Coal_PES", "f", "coal", 0.0036, "EJ", "global_sum")
get_witch_variable("Q_OUT", "Coal_OUT", "f", "coal", 0.0036, "EJ", "global_sum")
get_witch_variable("Q_PES", "Coal", "f", "coal", 0.0036, "EJ", "global_sum")
get_witch_variable("Q_PES", "Gas", "f", "gas", 0.0036, "EJ", "global_sum")
get_witch_variable("Q_PES", "Oil", "f", "oil", 0.0036, "EJ", "global_sum")
#get_witch_variable("K_EN", "Cars", "jreal", "all", 1, "Mio. vehicles", "all", bar="set", bar_x="time", bar_y="value", bar_setvalues=c("trad_cars", "plg_hybrid", "hybrid", "edv"), bar_colors=c("grey", "green", "blue", "yellow"))
get_witch_variable("Q_PES", "Oil", "f", "oil", 0.0036, "EJ", "global_sum")
get_witch_variable("Q_EMI", "CO2_Emissions", "e", "co2", 3.67, "GtCO2", "global_sum")
get_witch_variable("Q_OUT", "Coal_cumulative", "f", "coal", 0.0036, "EJ", "global_sum", cumulative=TRUE)
get_witch_variable("Q_OUT", "Oil_cumulative", "f", "oil", 0.0036, "EJ", "global_sum", cumulative=TRUE)
get_witch_variable("Q_OUT", "Gas_cumulative", "f", "gas", 0.0036, "EJ", "global_sum", cumulative=TRUE)
get_witch_variable("MCOST_PES", "price", "f", "coal", 1000, "1", "global_mean")
get_witch_variable("Q_EMI", "CO2_Emissions", "e", "co2", 3.67, "GtCO2", "global_sum")



#some special graphs maybe special regions

regions_plotgrid = c("china", "india", "sasia", "easia", "indonesia")


get_witch_variable("tpes", "Primary_Energy", "na", "na", 0.0036, "EJ", "regional")
ggplot(subset(tpes, t<=10 & n %in% regions_plotgrid)) + geom_line(stat="identity", size=1.2, aes(ttoyear(t),value, color=file)) + facet_wrap( ~ n, scales = "free", switch=NULL, ncol=length(regions_plotgrid)) + ylab("EJ") + xlab("") + guides(color=guide_legend(title=NULL, nrow = 1)) + theme(legend.position="bottom")
saveplot("Primary Energy Regional")


#Special Graphs
Primary_Energy_Mix(PES_y = "value") 
Primary_Energy_Mix_Regional(PES_y = "value", regions = regions_plotgrid, years = seq(1990, 2050, 5), plot_type = "area")
Electricity_Mix(Electricity_y = "value")

years_el = seq(1990, 2050, 1)
years_el[16] <- 0
years_el[21] <- 0
Electricity_Mix_Regional(Electricity_y = "value", regions = regions_plotgrid, years = years_el, plot_type = "area")

Intensity_Plot(year=2050, region=c(regions_plotgrid, "WORLD"), year0=2010)
Sectoral_Emissions(regions=regions_plotgrid)


Primary_Energy_Mix_Regional(PES_y = "value", regions = regions_plotgrid, plot_type = "bar", years = c(2005, 2030, 2050), plot_name="PES Mix Share") 




Policy_Cost(discount_rate=3, regions=regions_plotgrid, bauscen = "REF", show_numbers=TRUE, tmax=10)



#Maps


#Map of WITCH regional aggregation
#witchmap(tfpn, file_report="BAU", t_report=5, mapcolor="Blues", map_name="WITCH Regions", region_id="witch14eu", plot_witch_regions=TRUE)




source('functions/close_functions.R') #finishes PDF, shows welfare



