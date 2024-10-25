rm(list = ls())
witch_folder = "../fidelio" #Where you're RICE/DICE/RICE50x code is located (FIDELIO is in EUR of 2010) (EUR2010 to USD2005 = 1.33/1.10774)
#main directory of your results files
main_folder <- witch_folder # by default, the witch source folder
subdir = c("") #can be multiple directories

reg_id = "fidelio46" #for historical data folder
year0 = 2015
tstep = 1

restrict_files = c("results") #to all scenarios matching partly one of its arguments
exclude_files = c("")
removepattern = c("")

yearmin = 1980
yearmax = 2050

#Initialize default options, load all witch and other functionsget
source('R/witch_functions.R')

#mapping of variables to historical and validation statistics and unit conversion from WITCH historical to model
map_var_hist <- fread("varname_model, set_model, element_model, var_witch, set_witch, element_witch, conv
Y, , , SOCECON, *, gdp-ppp, 1
E, , , Q_EMI, e, co2, 1/(44/12)
EIND, , , Q_EMI, e, co2ffi, 1/(44/12)
ELAND, , , Q_EMI, e, co2lu, 1/(44/12)
pop, , , l, , , 1e3
K, , , K, g, fg, 1
I, , , I, g, fg, 1
GDP_VA_VAL_t, , , ykali, , , 1e6/1.09
")
#compute numerical conversion factor
map_var_hist <- map_var_hist %>% rowwise() %>% mutate(conv=eval(parse(text = conv))) %>% as.data.table()


#gdxcompaR(Standard gdxcompaR based on typical variables, otherwise edit in gdxcompaR/server.R)
runApp(appDir = "gdxcompaR/fidelio")

get_plot_witch("GDP_VA_VAL_t")
get_witch("GDP_VA_VAL_t")
