rm(list = ls())
witch_folder = "../witch" #Where you're WITCH code is located
#main directory of your results files
main_directory <- witch_folder # by default, the witch source folder
#main_directory <- "C:/Users/Emmerling/Documents/Dropbox (CMCC)/EIEE/WITCH_CODING/WITCH_RUNS_2018/submission_cdlinks/2019_04_15"
subdir = c("") #can be multiple directories


restrict_files = c("results_") #to all scenarios matching partly one of its arguments
exclude_files = c("db_")
removepattern = c("results_", "_calib", "all_data_temp_ela_bau_y")

yearmin = 1980
yearmax = 2100

#If you want to have significant separations or parts of file names, specify file_separate <- c(type="first|last|separate", sep="_", names="c("file_new"))
#file_separate <- c("last", "_", c("specification"))
#Name scenarios (otherwise it takes gdx filename)
#scenlist <- c("REF", "INDC_2C", "INDC_2C_TRADE", "INDC", "INDC_TRADE", "OPT_2C")
#Select which scenarios are used,potentially change the order, by default, all scenarios are used
#scenplot_global_order <- c(5,3,1)

#Initialize default options, load all witch and other functionsget
source('R/witch_functions.R')

#gdxcompaR (Standard gdxcompaR based on typical variables, otherwise edit in gdxcompaR/server.R)
runApp(appDir = "gdxcompaR/witch")

stop("Just load everything")


get_witch_simple("MCOST_INV", check_calibration = T, force_reload = T)
ggplot(MCOST_INV %>% filter(jreal %in% c("elpv", "elcsp", "elwindon", "elwindoff") & t<=10 & n=="usa")) + geom_line(aes(ttoyear(t), value*1e3, color=file)) +facet_grid(pathdir ~ jreal, ncol=1) + xlab("") + ylab("Capital cost [$/kW]")



utility_cebge_global <- utility_cebge_global %>% mutate(growth_rate=as.numeric(gsub("R",3.4,str_sub(file,-1,-1))), cb=str_sub(file,-6,-4))
ggplot(utility_cebge_global) + geom_point(aes(growth_rate, value, color=cb)) + geom_line(aes(growth_rate, value, color=cb)) + scale_x_continuous(limits = c(0,10), breaks=seq(0,10,1)) + ylab("Welfare (global CEBGE)")
saveplot("CEBGE")

#implicit carbon price in cooperative
#1e3/c2co2
# get_witch_simple("eqq_emi_co2ffi_c_world", field = "m", force_reload = T)
# get_witch_simple("eqq_y_c_world", field = "m", force_reload = T)
# setnames(eqq_y_c_world, "value", "marg_cons")
# eqq_emi_co2ffi_c_world$marg_cons <- eqq_y_c_world$marg_cons
# eqq_emi_co2ffi_c_world$co2price <- -1e3/(44/12)*eqq_emi_co2ffi_c_world$value/eqq_emi_co2ffi_c_world$marg_cons
# ggplot(eqq_emi_co2ffi_c_world %>% filter(ttoyear(t)<=2100)) + geom_line(aes(ttoyear(t), co2price, color=file))
#Carbon budget
get_witch_simple("Q_EMI")
Q_EMI <- Q_EMI  %>% mutate(growth_rate=as.numeric(gsub("R",3.4,str_sub(file,-1,-1))), cb=str_sub(file,-6,-4))
ggplot(Q_EMI %>% filter(e=="co2" & t>=4 & t<=20) %>% group_by(file) %>% summarize(cb=sum(value)*5*44/12)) + geom_bar(aes(file, cb, fill=file), stat = "identity") + ylab("Carbon budget 2018-2100")
# compare all gases
ggplot(Q_EMI %>% filter(e %in% c("co2", "co2ffi", "co2lu", "ch4", "n2o") & t>=4 & t<=20) %>% group_by(cb, growth_rate, e) %>% summarize(cbact=sum(value)*5*44/12)) + geom_bar(aes(growth_rate, cbact, fill=cb), stat = "identity") + ylab("Carbon budget 2018-2102") + facet_grid(e ~ cb) + theme(legend.position = "none")
saveplot("Compare all gases")
# 2100:
ggplot(Q_EMI %>% filter(e=="co2" & t>=4 & ttoyear(t)<=2100) %>% group_by(file,cb,growth_rate) %>% summarize(cbactual=sum(value)*5*44/12)) + geom_bar(aes(growth_rate, cbactual, fill=cb), stat = "identity") + ylab("Carbon budget 2018-2003, all CO2") + facet_grid(. ~ cb) + theme(legend.position = "none")
saveplot("Actual Carbon Budget")
#CO2 over time
ggplot(Q_EMI %>% filter(e=="co2" & ttoyear(t)>=2000) %>% group_by(t, cb, growth_rate) %>% summarize(co2emi=sum(value)*44/12)) + geom_line(aes(ttoyear(t), co2emi, color=as.factor(growth_rate)), stat = "identity") + ylab("CO2 Emissions") + facet_grid(cb ~ .) + theme(legend.position = "bottom") + xlab("")
saveplot("Emission Profile")
#Carbon Price
get_witch_simple("carbonprice")
carbonprice <- carbonprice %>% mutate(growth_rate=as.numeric(gsub("R",3.4,str_sub(file,-1,-1))), cb=str_sub(file,-6,-4))
ggplot(carbonprice %>% group_by(t, cb,growth_rate, file) %>% filter(ttoyear(t)<=2100) %>% summarize(ctax=mean(value)*1e3/(44/12))) + geom_line(aes(ttoyear(t), ctax, color=cb), stat = "identity") + ylab("Carbon price [$/tCO2eq]") + facet_grid(cb ~ growth_rate)
saveplot("Carbon price")



netzeroyear <- Q_EMI %>% filter(e=="co2" & ttoyear(t)>=2015 & ttoyear(t) <=2100)  %>% mutate(year=ttoyear(t)) %>% select(-t) %>% group_by(year, cb, growth_rate) %>% summarize(co2emi=sum(value)*44/12) %>% ungroup() %>% group_by(cb, growth_rate) %>% summarize(cbactual=sum(co2emi*5), net0year=min(2100,min(year[co2emi<0.1])))
ggplot(netzeroyear) + geom_point(aes(growth_rate, net0year, color=cb)) + geom_line(aes(growth_rate, net0year, color=cb)) + xlab("") + scale_x_continuous(limits = c(0,10), breaks=seq(0,10,1))
saveplot("Net zero year")                                                                                    

netzeroyear <- merge(netzeroyear, utility_cebge_global %>% select(-file,-pathdir,-t), by = c("cb", "growth_rate"), all=T)
ggplot(netzeroyear) + geom_point(aes(net0year, value, color=cb)) + geom_line(aes(net0year, value, color=cb)) + ylab("Welfare (global CEBGE)")
saveplot("Net0year welfare")
ggplot(netzeroyear) + geom_line(aes(growth_rate, value*((cbactual-390)/as.numeric(cb))^(-2.2), color=cb)) + scale_x_continuous(limits = c(0,10), breaks=seq(0,10,1)) + ylab("Welfare (global CEBGE)")
saveplot("welfare correction")






diagnostics_plots() #Basic diagnostic plots

#Main part, get data plots etc.
Plot_Global_Emissions(bauscen = "bau")
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

























