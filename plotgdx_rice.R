rm(list = ls())
witch_folder = "../RICE50x" #Where you're RICE/DICE/RICE50x code is located
#main directory of your results files
main_folder <- witch_folder # by default, the witch source folder
subdir = c("") #can be multiple directories

reg_id = "ed57" #for historical data folder
year0 = 2015
tstep = 5

restrict_files = c("results_") #to all scenarios matching partly at least one of its arguments
exclude_files = c("")
removepattern = c("")

yearmin = 1980
yearmax = 2300

#Initialize default options, load all witch and other functionsget
source('R/witch_functions.R')

#mapping of variables to historical and validation statistics and unit conversion to WITCH units
map_var_hist <- fread("varname_model, set_model, element_model, var_witch, set_witch, element_witch, conv
Y, , , SOCECON, *, gdp-ppp, 1
E, , , Q_EMI, e, co2, 44/12
EIND, , , Q_EMI, e, co2ffi, 44/12
ELAND, , , Q_EMI, e, co2lu, 44/12
pop, , , l, , , 1e3
K, , , K, g, fg, 1
I, , , I, g, fg, 1
")
#compute numerical conversion factor
map_var_hist <- map_var_hist %>% rowwise() %>% mutate(conv=eval(parse(text = conv))) %>% as.data.table()


#gdxcompaR(Standard gdxcompaR based on typical variables, otherwise edit in gdxcompaR/server.R)
runApp(appDir = "gdxcompaR/rice")

get_plot_witch("E")
Y <- get_witch("Y")
