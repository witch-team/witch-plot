rm(list = ls())
witch_folder = "../RICE50x" #Where you're RICE50x code is located
#main directory of your results files
main_directory <- witch_folder # by default, the witch source folder
subdir = c("") #can be multiple directories

region_id = "ed57"
year0 = 2015
tstep = 5

restrict_files = c("") #to all scenarios matching partly one of its arguments
exclude_files = c("")
removepattern = c("results_")

yearmin = 1980
yearmax = 2300

#If you want to have significant separations or parts of file names, specify file_separate <- c(type="first|last|separate", sep="_", names="c("file_new"))
#file_separate <- c("last", "_", c("specification"))
#Name scenarios (otherwise it takes gdx filename)
#scenlist <- c("REF", "INDC_2C", "INDC_2C_TRADE", "INDC", "INDC_TRADE", "OPT_2C")
#Select which scenarios are used,potentially change the order, by default, all scenarios are used
#scenplot_global_order <- c(5,3,1)

#Initialize default options, load all witch and other functionsget
source('functions/witch_functions.R')

#mapping of variables to historical and validation statistics and unit conversion to WITCH units
map_var_hist <- fread("varname_model, set_model, element_model, var_witch, set_witch, element_witch, conv
Y, , , SOCECON, *, gdp-ppp, 1
E, , , Q_EMI, e, co2, 0.2727273
pop, , , l, , , 1e-3
K, , , K, g, fg, 1
I, , , I, g, fg, 1
")

#gdxcompaR(Standard gdxcompaR based on typical variables, otherwise edit in gdxcompaR/server.R)
runApp(appDir = "gdxcompaR/rice50x")

get_witch_variable("E")
get_witch_simple("Y")
get_witch_simple("pop")









