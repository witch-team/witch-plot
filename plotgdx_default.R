rm(list = ls())
witch_folder = "../witch" #Where you're WITCH code is located
#main directory of your results files
main_directory <- witch_folder # by default, the witch source folder
#main_directory <- "C:/Users/Emmerling/Documents/Dropbox (CMCC)/EIEE/WITCH_CODING/WITCH_RUNS_2018/submission_cdlinks/2019_04_15"
subdir = c("") #can be multiple directories

restrict_files = c("results_") #to all scenarios matching partly one or more of its arguments
removepattern = c("results_", "_calib") 
restrict_files = c("results_") #"."
exclude_files = c("db_", "all_data_temp_")
removepattern = c("results_", "_calib") 

yearmin = 1980
yearmax = 2100

#If you want to have significant separations or parts of file names, specify file_separate <- c(type="first|last|separate", sep="_", names="c("file_new"))
#file_separate <- c("last", "_", c("specification"))
#Name scenarios (otherwise it takes gdx filename)
#scenlist <- c("REF", "INDC_2C", "INDC_2C_TRADE", "INDC", "INDC_TRADE", "OPT_2C")
#Select which scenarios are used,potentially change the order, by default, all scenarios are used
#scenplot_global_order <- c(5,3,1)

#Initialize default options, load all witch and other functions
source('functions/witch_functions.R')

#Just have a look at global welfare
get_witch_simple("utility_cebge_global"); print("Global welfare in all scenarios:"); print(utility_cebge_global)

#gdxcompaR (Standard gdxcompaR based on typical variables, otherwise edit in gdxcompaR/server.R)
runApp(appDir = "gdxcompaR")

stop("Just load everything")


get_witch_simple("I")
get_witch_simple("I_EN")
get_witch_simple("I_RD")
Inv <- rbind(I_EN, I_RD, fill=T)
Inv <- rbind(Inv, I, fill=T)
ggplot(Inv %>% group_by(t, file, pathdir) %>% summarize(value=sum(value))) + geom_line(aes(t, value*1e3, color=file))
Investment_Plot(regions="World")


diagnostics_plots() #Basic diagnostic plots

#Main part, get data plots etc.
Plot_Global_Emissions(show_ar5=TRUE, ar5_budget=1180, bauscen = "bau")
get_witch_variable("carbonprice", "Carbon Price", "na", "na", aggregation =  "global_mean")
get_witch_variable("Q", "GDP", "iq", "y", aggregation = "global_sum")
get_witch_variable("Q", "GDP", "iq", "y", aggregation = "regional")
get_witch_variable("Q_EMI", "CO2_Emissions", "e", "co2", aggregation = "global_sum", cumulative = T)
get_witch_variable("TEMP", "Temperature", "m", "atm", aggregation = "global_mean")
get_witch_variable("Q_EMI", "CCS_Emissions", "e", "ccs", aggregation = "global_sum")
get_witch_variable("Q_EMI", "CCS_Emissions_Stored", "e", "ccs", aggregation = "global_sum", cumulative = T)
get_witch_variable("tpes", "tpes", "na", "na", aggregation = "global_sum")
get_witch_variable("Q_OUT", "Oil_Extraction", "f", "oil", aggregation = "regional")




#Special Plots:
#Special focus regions to report for
regions_focus <- c("World")

Energy_Trade(fuelplot = "oil")
Primary_Energy_Mix(PES_y = "value", regions = regions_focus, years = seq(1990, 2100, 5), plot_type = "area")
Electricity_Mix(Electricity_y = "value", regions = regions_focus, years = seq(1990, 2100, 5), plot_type = "area")

Intensity_Plot(years = c(2050,2100), regions="World", year0=2010, scenplot = scenlist)
Global_Emissions_Stacked(scenario = scenlist[1])

#Mitigation_Decomposition(regions=regions_focus, scenario_stringency_order = c("DIAG-Base", "DIAG-C30-gr5"), scen_short=c("Base", "C30-gr5"), plotname="Mitigation Decomposition")
Investment_Plot(regions=regions_focus)
Sectoral_Emissions(regions=regions_focus)
Policy_Cost(discount_rate=5, regions=regions_focus, bauscen = "bau", show_numbers=TRUE, tmax=10)

#Impacts and SCC
SCC_plot(regions = "World")
#Climate plot
climate_plot()

#Impact Map
t_map = 20; bau_scen = scenlist[1]
get_witch_simple("Q")
impact_map_data <- Q %>% filter(iq=="y" & t==t_map) %>% group_by(n, pathdir) %>% mutate(value = -((value/sum(value[file==bau_scen]))-1)*100) %>% filter(is.finite(value))
witchmap(impact_map_data, file_report=scenlist[2], t_report=t_map, mapcolor="Reds", map_name="Impact Map", map_legend = "GDP loss [%]")

#Export multiple variables as time series panel dataset "witch_dataset_long.csv"
#write_witch_data_csv(c("l", "ykali"), years = seq(1960, 2100, 20))

























