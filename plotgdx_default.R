rm(list = ls())
#all directoried with trailing slash "/"!
#Where you're WITCH code is located
witch_folder = "../witch_fresh/"
#main directory of your results files
main_directory <- witch_folder # by default, the witch source folder
#main_directory <- "C:/Users/Emmerling/Documents/Dropbox (CMCC)/FEEM/WITCH_CODING/witch-cdlinks/"
#all directoried with trailing slash "/"!
subdir = c("") #can be multiple directories


removepattern = c("results_", "_calib", "_bau", "ssp2_") 
restrict_files = "results_" #"."
exclude_files = "db"

#Name scenarios (otherwise it takes gdx filename)
#scenlist <- c("REF", "INDC_2C", "INDC_2C_TRADE", "INDC", "INDC_TRADE", "OPT_2C")
#Select which scenarios are used,potentially change the order, by default, all scenarios are used
#scenplot_global_order <- c(5,3,1)
#Special focus regions to report for
#regions_focus = c("china", "india", "sasia", "easia", "indonesia")

yearmin=1980
yearmax = 2100

#Initialize default options, load all witch and other functions
source('functions/witch_functions.R')


#gdxcompaR (Standard gdxcompaR based on typical variables, otherwise edit in gdxcompaR/server.R)
runApp(appDir = "gdxcompaR")

stop("Just load everything")


#Main part, get data plots etc.
Plot_Global_Emissions(show_ar5=TRUE, ar5_budget=1180, bauscen = "ssp2_bau") #Global GHG Emissions (AR5 2000 is CB of 2 degrees, 1180GtCO2 for "likely 2deg")
get_witch_variable("carbonprice", "Carbon Price", "na", "na", aggregation =  "global_mean")
get_witch_variable("Q", "GDP", "iq", "y", aggregation = "global_sum")
get_witch_variable("Q", "GDP", "iq", "y", aggregation = "regional")
get_witch_variable("Q_EMI", "CO2_Emissions", "e", "co2", aggregation = "global_sum", cumulative = T)
get_witch_variable("TEMP", "Temperature", "m", "atm", aggregation = "global_mean")
get_witch_variable("Q_EMI", "CCS_Emissions", "e", "ccs", aggregation = "global_sum")
get_witch_variable("Q_EMI", "CCS_Emissions_Stored", "e", "ccs", aggregation = "global_sum", cumulative = T)
get_witch_variable("tpes", "tpes", "na", "na", aggregation = "global_sum")
get_witch_variable("Q_OUT", "Oil_Extraction", "f", "oil", aggregation = "regional")



# Energy Special Plot
regions_focus <- c("china")
Energy_Trade(fuelplot = "oil")
Energy_Prices(unit = "boe", scenplot = scenlist)
Primary_Energy_Mix(PES_y = "value") 
Primary_Energy_Mix_Regional(PES_y = "value", regions = regions_focus, years = seq(1990, 2050, 5), plot_type = "area")
Electricity_Mix(Electricity_y = "value")
Electricity_Mix_Regional(Electricity_y = "value", regions = regions_focus, years = seq(1990, 2050, 1), plot_type = "area")


#Mitigation_Decomposition(regions=regions_focus, scenario_stringency_order = c("DIAG-Base", "DIAG-C30-gr5"), scen_short=c("Base", "C30-gr5"), plotname="Mitigation Decomposition")
Investment_Plot(regions=regions_focus)
Sectoral_Emissions(regions=regions_focus)
Policy_Cost(discount_rate=5, regions=regions_focus, bauscen = "ssp2_bau", show_numbers=TRUE, tmax=10)



source('functions/close_functions.R') #finishes PDF, shows welfare


#export multiple variables as time series panel dataset "witch_dataset_long.csv"
#write_witch_data_csv(c("l", "ykali", "education", "quantiles", "migration", "TEMP_REGION", "PRECIP_REGION"), years = seq(1960, 2100, 20))
#Full ISO3 dataset (based on "build" folder)
#write_witch_historical_iso3_dataset()
























