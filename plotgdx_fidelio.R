rm(list = ls())
witch_folder = "../FIDELIO" #Where you're RICE/DICE/RICE50x code is located (FIDELIO is in EUR of 2010) (EUR2010 to USD2005 = 1.33/1.10774)
#main directory of your results files
main_directory <- witch_folder # by default, the witch source folder
subdir = c("") #can be multiple directories

reg_id = "fidelio36" #for historical data folder
year0 = 2010
tstep = 1

restrict_files = c("") #to all scenarios matching partly one of its arguments
exclude_files = c("")
removepattern = c("")

yearmin = 1980
yearmax = 2050

#Initialize default options, load all witch and other functionsget
source('R/witch_functions.R')

#mapping of variables to historical and validation statistics and unit conversion from model units to WITCH units (will be shown in WITCH units by default!!)
map_var_hist <- fread("varname_model, set_model, element_model, var_witch, set_witch, element_witch, conv
Y, , , SOCECON, *, gdp-ppp, 1
E, , , Q_EMI, e, co2, 0.2727273
EIND, , , Q_EMI, e, co2ffi, 0.2727273
ELAND, , , Q_EMI, e, co2lu, 0.2727273
pop, , , l, , , 1e-3
K, , , K, g, fg, 1
I, , , I, g, fg, 1
GDP_VA_VAL_t, , , ykali, , , 1.200643e-06
")

#gdxcompaR(Standard gdxcompaR based on typical variables, otherwise edit in gdxcompaR/server.R)
runApp(appDir = "gdxcompaR/fidelio")

get_plot_witch("GDP_VA_VAL_t")
get_witch("GDP_VA_VAL_t")
