# Main script to launch WITCH-PLOT for WITCH
# rm(list = ls()) # Should not be used. Better to restart the session in Session > Restart R

library(tracer) # Temporary load for debugging

# Folder definitions

# WITCH code folder to get model information
witch_folder <- "../witch" 

# Main directory and sub-directories of your results files
main_directory <- witch_folder
# subdir can be multiple directories (default is main directory)
subdir <- c("") 

# Filter results files
restrict_files <- c("") # To all scenarios matching partly one of its arguments
exclude_files <- c("")
removepattern <- c("")

yearmin = 1980
yearmax = 2100

#If you want to have significant separations or parts of file names, specify file_separate <- c(type="first|last|separate", sep="_", names="c("file_new"))
#file_separate <- c("last", "_", c("specification"))
#Name scenarios (also subsets to the ones given (otherwise it takes gdx filename) as a mapping
#scenlist <- c("results_ssp2_asia_curpol"="Current policies")

#Initialize default options, load all witch and other functionsget
source('R/witch_functions.R')

#gdxcompaR (Standard gdxcompaR based on typical variables, otherwise edit in gdxcompaR/server.R)
shiny::runApp(appDir = "gdxcompaR/witch")