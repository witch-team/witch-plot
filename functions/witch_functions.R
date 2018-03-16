#Author: Johannes Emmerling
#v1: February 09th, 2016

pathdir = paste0(main_directory, subdir)
#Specify directory for graphs and data to be saved: by default: /graphs/ in the folder
graphdir = if(length(pathdir)>1){paste0(main_directory, "graphs/") }else{paste0(pathdir, "graphs/")}

#check if directory valid
if(any(!dir.exists(pathdir))){stop("Please check the main directory and sub directory, including the trailing slash!")}

suppressPackageStartupMessages
source('functions/get_libraries.R')
pkgs <- c('data.table', 'stringr', 'docopt', 'countrycode', 'gdata', 'taRifx', 'reshape2', 'ggplot2', 'scales', 'RColorBrewer', 'plyr', 'openxlsx', 'gsubfn')
res <- lapply(pkgs, require_package)
require_gdxtools()

#In case creating data Excel files creates a problem with old zip!!!
#Sys.setenv(R_ZIPCMD= "C:/apps/Rtools/bin/zip")   

## Local Options ##
create_all_figures_pdf = FALSE
figure_format="png"
export_plotdata = FALSE  #save data for each graph also as EXCEL files
historical = TRUE  #add historical data where available
check_calibration=FALSE  # don't add historical data but rather have them overlap to check calibration
line2005 = FALSE #adds line at 2005 to show history from model data
theme_set(theme_bw())
show_numbers_2100 = FALSE
ssp_grid = FALSE
legend_position="bottom"    # "none", "bottom", or "right"


## End of Local Options ##



#load basic functions
source('functions/auxiliary_functions.R')
source('functions/witch_load_and_plot.R')

filelist = gsub(".gdx","",list.files(path=pathdir[1], full.names = FALSE, pattern="*.gdx", recursive = FALSE))
filelist = filelist[str_detect(filelist, restrict_files)]
filelist = filelist[!str_detect(filelist, exclude_files)]
if(length(filelist)==0){stop("No GDX files found.")}
if(!exists("scenlist")){.tmp <- filelist; for(rm in removepattern){.tmp <- gsub(rm,"",.tmp)}; scenlist <- .tmp;}
if(!exists("scenplot_global_order")){scenplot_global_order = seq(1:length(scenlist))}
print("GDX Files:")
print(filelist)
print("Scenario names:")
print(scenlist)
print("Scenarios actually used:")
filelist <- filelist[scenplot_global_order]
scenlist <- scenlist[scenplot_global_order]
print(scenlist)

#create directory for graphs
if (!dir.exists(graphdir)){dir.create(graphdir)} 
if(file.exists(paste(graphdir, "all_figures.pdf", sep=""))){(file.remove(paste(graphdir, "all_figures.pdf", sep="")))}
if(create_all_figures_pdf){pdf(paste(graphdir, "all_figures.pdf", sep=""), paper="a4r")}

#Palettes for WITCH regions and regional aggregation
get_witch_simple("conf")
region_id <- subset(conf, file==scenlist[1] & pathdir==pathdir[1] & V1=="regions")$V2
get_witch_simple("n")
get_witch_simple("t"); t_model<-unique(t$t)
witch_regions <- subset(n, file==scenlist[1] & pathdir==pathdir[1])$V1
if(!exists("display_regions")){display_regions <- witch_regions}
region_palette_rainbow <- setNames(rainbow(length(witch_regions)), witch_regions) #just in case have a fall back colour
region_palette_specific <- c(usa="darkblue",Usa="darkblue",oldeuro="blue", neweuro="cornflowerblue",kosau="darkgreen",Kosau="darkgreen",cajaz="chartreuse4",Cajaz="chartreuse4",te="gold2",Te="gold2",mena="darkgoldenrod4",Mena="darkgoldenrod4",ssa="goldenrod",Ssa="goldenrod",sasia="darkorange2","South Asia"="darkorange2",china="deeppink3",PRC="deeppink3",easia="orangered",ESEAP="orangered",laca="#fbb714",Laca="#fbb714",india="#fbf003",India="#fbf003",europe="blue",Europe="blue",indonesia="darkseagreen",Indonesia="darkseagreen",Rest_of_World="grey48",chinaw="darkorange",chinac="darkorange2",chinae="darkorange4",italy="green",mexico="slateblue2",brazil="tomato4",canada="blueviolet",jpnkor="lightsalmon3",oceania="forestgreen",southafrica="indianred3",seasia="orangered",World="black")
region_palette <- region_palette_specific #c(region_palette_specific, region_palette_rainbow)
if(!exists("regions_focus")){regions_focus <- witch_regions}
print(paste("Regional aggregation:", region_id))
witch_colors_alphabetic <- c("chartreuse4", "deeppink3", "orangered", "khaki1", "darkgreen", "gold", "darkgoldenrod4", "cornflowerblue", "blue", "darkorange2", "goldenrod", "gold2", "darkblue")


#load specialized functions
source('functions/special_plots.R')
source('functions/map_functions.R')
source('functions/energy_mix.R')
source('functions/policy_cost.R')
source('functions/add_historical_values.R')
source('functions/gdxcompaR_static.R')
source('functions/export_variables.R')




