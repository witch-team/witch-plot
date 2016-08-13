#main directory
main_directory = "C:/Users/Emmerling/Documents/Dropbox/Professional/FEEM/WITCH_CODING/witch/"
#main_directory = "C:\\Users\\Emmerling\\Documents\\Dropbox\\Professional\\FEEM\\EnergyIntensity\\Modeling\\"

#creating data Excel files creates a problem with old zip!!!
Sys.setenv(R_ZIPCMD= "C:/apps/Rtools/bin/zip")   

#all directoried with trailing slash "/"!
pathdir = c("DIAG/") #can be multiple directories

removepattern="results_"  
restrict_files = "DIAG" #"."
exclude_files = "report_"

pathdir = paste0(main_directory, pathdir)
graphdir = paste0(pathdir[1], "graphs/") #/graphs/ in first folder if multiple folders


#Name scenarios (otherwise it takes gdx filename)
#scenlist <- c("REF", "INDC_2C", "INDC_2C_TRADE", "INDC", "INDC_TRADE", "OPT_2C")
# select which scenarios are used,potentially change the order
#scenplot_global_order <- c(5,3,1)

yearmin=1990
yearmax = 2100

#Initialize default options, load all witch and other functions
source('functions/witch_functions.R')



#Main part, get data plots etc.
Global_Emissions(show_ar5=TRUE, ar5_budget=2000) #Global GHG Emissions

get_witch_variable("Q", "GDP", "iq", "y", 1, "T$", "global_sum")
get_witch_variable("Q_BAU", "GDP_BAU", "iq", "y", 1, "T$", "global_sum")
get_witch_variable("Q_EMI", "CO2_Emissions", "e", "co2", 3.67, "GtCO2", "global_sum")
get_witch_variable("TEMP", "Temperature", "m", "atm", 1, "°C", "global_mean")
get_witch_variable("Q_EN", "SEN", "j", "el", 0.0036, "EJ", "global_sum")
get_witch_variable("Q_EMI", "CCS_Emissions", "e", "ccs", 3.67, "GtCO2", "global_sum")
get_witch_variable("tpes", "tpes", "na", "na", 0.0036, "1", "global_sum")
get_witch_variable("Q_EN", "Final_Energy", "j", "en", 0.0036, "EJ", "global_sum")
get_witch_variable("Q_PES", "Coal_PES", "f", "coal", 0.0036, "EJ", "global_sum")
get_witch_variable("Q_OUT", "Coal_OUT", "f", "coal", 0.0036, "EJ", "global_sum")
get_witch_variable("Q_PES", "Gas", "f", "gas", 0.0036, "EJ", "global_sum")
get_witch_variable("Q_PES", "Oil", "f", "oil", 0.0036, "EJ", "global_sum")
get_witch_variable("Q_EMI", "CO2_Emissions", "e", "co2", 3.67, "GtCO2", "global_sum")
get_witch_variable("Q_OUT", "Coal_cumulative", "f", "coal", 0.0036, "EJ", "global_sum", cumulative=TRUE)
get_witch_variable("Q_OUT", "Oil_cumulative", "f", "oil", 0.0036, "EJ", "global_sum", cumulative=TRUE)
get_witch_variable("Q_OUT", "Gas_cumulative", "f", "gas", 0.0036, "EJ", "global_sum", cumulative=TRUE)
get_witch_variable("MCOST_PES", "price", "f", "coal", 1000, "1", "global_mean")
get_witch_variable("Q_EMI", "CO2_Emissions", "e", "co2", 3.67, "GtCO2", "global_sum")
get_witch_variable("SRM", "SRM_regional", "na", "na", 1, "TgS", "regional")
get_witch_variable("OMEGA", "Damages", "na", "na", 1, "%", "regional")


#calibration
#get_witch_variable("tpes_kali", "TPES_global", "na", "na", 0.0036, "EJ", "global_sum")
#get_witch_variable("tpes_kali", "TPES", "na", "na", 0.0036, "EJ", "regional")
#get_witch_variable("ei_kali", "TPES", "na", "na", 1, "MJ/$", "regional")
#ggplot(tpes_kali) + geom_line(stat="identity", size=1.2, aes(ttoyear(t),value, color=n)) + facet_wrap( ~ file) + ylab("EJ") + xlab("")  + scale_colour_manual(values = region_palette) + theme(legend.position="bottom")
#saveplot("PES compare calibrations")
#ggplot(ei_kali) + geom_line(stat="identity", size=1.2, aes(ttoyear(t),value, color=n)) + facet_wrap( ~ file) + ylab("MJ/$") + xlab("") + scale_colour_manual(values = region_palette) + theme(legend.position="bottom")
saveplot("EI compare calibrations")




#Special Graphs
regions_plotgrid = c("china", "india", "sasia", "easia", "indonesia")

Primary_Energy_Mix(PES_y = "value") 
Primary_Energy_Mix_Regional(PES_y = "value", regions = regions_plotgrid, years = seq(1990, 2050, 5), plot_type = "area")
Electricity_Mix(Electricity_y = "value")
Electricity_Mix_Regional(Electricity_y = "value", regions = regions_plotgrid, years = seq(1990, 2050, 1), plot_type = "area")
Intensity_Plot(year=2050, region=c(regions_plotgrid, "WORLD"), year0=2010)
Sectoral_Emissions(regions=regions_plotgrid)
Policy_Cost(discount_rate=5, regions=regions_plotgrid, bauscen = "bau", show_numbers=TRUE, tmax=10)

get_globiom_variables(regions=regions_plotgrid, varplot="ForestCover", varname="Forest Cover", varunit="%")  #plots forest cover



Mitigation_Decomposition(regions=regions_plotgrid, scenario_stringency_order = c("DIAG-Base", "DIAG-C30-gr5"), scen_short=c("Base", "C30-gr5"), plotname="Mitigation Decomposition")

Carbon_Budget(regions=regions_plotgrid, scenario="DIAG-C30-gr5", plotname="CO2 FFI Emissions Asia and RoW")



#Map of WITCH regional aggregation
#witchmap(tfpn, file_report="BAU", t_report=5, mapcolor="Blues", map_name="WITCH Regions", region_id="witch14eu", plot_witch_regions=TRUE)

source('functions/close_functions.R') #finishes PDF, shows welfare



