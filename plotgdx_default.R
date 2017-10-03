rm(list = ls())
#all directoried with trailing slash "/"!
#Where you're WITCH code is located
witch_folder = "C:/Users/Emmerling/Documents/Dropbox/Professional/FEEM/WITCH_CODING/witch/"
#main directory of your results files
main_directory <- "C:/Users/Emmerling/Documents/Dropbox/Professional/FEEM/WITCH_CODING/witch/"
#main_directory = "U:\\SCAMBIO\\submission_cdlinks\\"

#all directoried with trailing slash "/"!
subdir = c("emi_cap/") #can be multiple directories




removepattern = c("results_", "all_data_temp_") 
restrict_files = "" #"."
exclude_files = "report"

#Name scenarios (otherwise it takes gdx filename)
#scenlist <- c("REF", "INDC_2C", "INDC_2C_TRADE", "INDC", "INDC_TRADE", "OPT_2C")
# select which scenarios are used,potentially change the order, by default, all scenarios are used
#scenplot_global_order <- c(5,3,1)

#Special focus regions to report for
#regions_focus = c("china", "india", "sasia", "easia", "indonesia")

yearmin=1850
yearmax = 2100

#Initialize default options, load all witch and other functions
source('functions/witch_functions.R')



#gdxcompaR including historical data
#gdxcompaR_static("Q_EN", additional_set="j", additional_set_id="el", convert=.0036, unit="EJ", regions=witch_regions)
#gdxcompaR_static("Q_OUT", additional_set="f", additional_set_id="oil", convert=.0036, unit="EJ", regions=witch_regions)
#Run gdxcompaR ShinyApp, unit, and conversion factor in gdxcompaR/server.R
library(shiny);runApp(appDir = "gdxcompaR")
#get_witch_variable("Q", "GDP", "iq", "y", 1, "T$", "global_sum")
#get_witch_variable("Q_EMI", "CO2_Emissions", "e", "co2leak", 3.67, "GtCO2", "global_sum")

stop("Just load everything")


#Main part, get data plots etc.
Plot_Global_Emissions(show_ar5=TRUE, ar5_budget=1180, bauscen = "ssp2_bau") #Global GHG Emissions (AR5 2000 is CB of 2 degrees, 1180GtCO2 for "likely 2deg")

get_witch_variable("carbonprice", "Carbon Price", "na", "na", aggregation =  "global_mean")
get_witch_variable("Q", "GDP", "iq", "y", aggregation = "global_sum")
get_witch_variable("Q", "GDP", "iq", "y", aggregation = "regional")
get_witch_variable("Q_EMI", "CO2_Emissions", "e", "co2", aggregation = "global_sum")
get_witch_variable("TEMP", "Temperature", "m", "atm", aggregation = "global_mean")

get_witch_variable("Q_EMI", "CCS_Emissions", "e", "ccs", aggregation = "global_sum")
get_witch_variable("Q_EMI", "CCS_Emissions_Stored", "e", "ccs", aggregation = "global_sum", cumulative = T)
get_witch_variable("tpes", "tpes", "na", "na", aggregation = "global_sum")

get_witch_variable("Q_OUT", "Oil_Extraction", "f", "oil", aggregation = "regional")

Energy_Trade(fuel = "oil")
Energy_Prices(unit = "boe", scenplot = scenlist)

#calibration
get_witch_variable("tpes", "TPES_global", "na", "na", aggregation = "global_sum")
get_witch_variable("ykali", "GDP_global", "na", "na", aggregation = "global_sum")
ei_global <- tpes; ei_global$value <- ei_global$value/ykali$value
ggplot(ei_global,aes(ttoyear(t),value,colour=file)) + geom_line(stat="identity") + xlab("") + ylab("") + theme(legend.position="bottom", legend.box = "horizontal")
saveplot("EI Global original SSPs")

#now (better) based on new WITCH

get_witch_variable("ei_global", "Energy Intensity (global)", "na", "na", 1, "MJ/$", unit_conversion="global_mean")
Intensity_Plot(year=2050, region=c(regions_focus, "WORLD"), year0=2010)
#ggplot(tpes_kali) + geom_line(stat="identity", size=1.2, aes(ttoyear(t),value, color=n)) + facet_wrap( ~ file) + ylab("EJ") + xlab("")  + scale_colour_manual(values = region_palette) + theme(legend.position="bottom")
#saveplot("PES compare calibrations")
#ggplot(ei_kali) + geom_line(stat="identity", size=1.2, aes(ttoyear(t),value, color=n)) + facet_wrap( ~ file) + ylab("MJ/$") + xlab("") + scale_colour_manual(values = region_palette) + theme(legend.position="bottom")
#saveplot("EI compare calibrations")




Primary_Energy_Mix(PES_y = "value") 
Primary_Energy_Mix_Regional(PES_y = "value", regions = regions_focus, years = seq(1990, 2050, 5), plot_type = "area")
Electricity_Mix(Electricity_y = "value")
Electricity_Mix_Regional(Electricity_y = "value", regions = regions_focus, years = seq(1990, 2050, 1), plot_type = "area")
Intensity_Plot(year=2050, region=c(regions_focus, "WORLD"), year0=2010)
Sectoral_Emissions(regions=regions_focus)
Policy_Cost(discount_rate=5, regions=regions_focus, bauscen = "bau", show_numbers=TRUE, tmax=10)




Mitigation_Decomposition(regions=regions_focus, scenario_stringency_order = c("DIAG-Base", "DIAG-C30-gr5"), scen_short=c("Base", "C30-gr5"), plotname="Mitigation Decomposition")

Carbon_Budget(regions=regions_focus, scenario="DIAG-C30-gr5", plotname="CO2 FFI Emissions Asia and RoW")

Investment_Plot(regions=regions_focus)

Energy_Prices(scenplot = scenlist)

#Map of WITCH regional aggregation
#get_witch_simple("tfpn")
#witchmap(tfpn, file_report=scenlist[1], t_report=5, mapcolor="Blues", map_name="WITCH Regions", plot_witch_regions=TRUE)

source('functions/close_functions.R') #finishes PDF, shows welfare





























