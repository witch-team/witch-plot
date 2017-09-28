rm(list = ls())
#all directoried with trailing slash "/"!
#Where you're WITCH code is located
witch_folder = "C:/Users/Emmerling/Documents/Dropbox/Professional/FEEM/WITCH_CODING/witch/"
#main directory of your results files
main_directory <- "C:/Users/Emmerling/Documents/Dropbox/Professional/FEEM/WITCH_CODING/witch/"
#all directoried with trailing slash "/"!
subdir = c("subdir/", "subdir2/") #can be multiple directories, if same filenames
#select files
removepattern = c("results_", "all_data_temp_") 
restrict_files = "" #"."
exclude_files = "report"
#Initialize default options, load all witch and other functions
source('functions/witch_functions.R')
library(shiny);runApp(appDir = "gdxcompaR")




























