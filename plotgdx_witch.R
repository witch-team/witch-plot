rm(list = ls())
witch_folder = "../witch" #Where you're WITCH code is located
witch_folder = "/home/lolow/Sync/02-WITCH/report/newbranch/navigate_merge/" #Where you're WITCH code is located
#witch_folder = "D:/SyncThing/02-WITCH/report/newbranch" #Where you're WITCH code is located
#main directory of your results files
main_folder <- witch_folder # by default, the witch source folder
#main_folder <- "C:/Users/Emmerling/Documents/Dropbox (CMCC)/EIEE/WITCH_CODING/WITCH_RUNS_2018/submission_cdlinks/2019_04_15"
subdir = c("") #can be multiple directories


restrict_files = c("") #to all scenarios matching partly one of its arguments
exclude_files = c("")
removepattern = c("")

yearmin = 1980
yearmax = 2100

#If you want to have significant separations or parts of file names, specify file_separate <- c(type="first|last|separate", sep="_", names="c("file_new"))
#file_separate <- c("last", "_", c("specification"))
#Name scenarios (also subsets to the ones given (otherwise it takes gdx filename) as a mapping
#scenlist <- c("results_ssp2_asia_curpol"="Current policies")

#c(lsf.str()) #show all available functions

#Initialize default options, load all witch and other functionsget
source('R/witch_functions.R')

#gdxcompaR (Standard gdxcompaR based on typical variables, otherwise edit in gdxcompaR/server.R)
runApp(appDir = "gdxcompaR/witch")
