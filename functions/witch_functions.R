#Author: Johannes Emmerling
#v1: February 09th, 2016
suppressPackageStartupMessages


source('functions/get_libraries.R')
pkgs <- c('data.table', 'stringr', 'docopt', 'countrycode', 'gdata', 'taRifx', 'reshape2', 'ggplot2', 'scales', 'RColorBrewer', 'plyr')
res <- lapply(pkgs, require_package)
require_gdxtools()


witch_folder = "C:/Users/Emmerling/Documents/Dropbox/Professional/FEEM/WITCH_CODING/witch/"



#Local Options
create_all_figures_pdf = FALSE
historical = TRUE  #add historical data where available
line2005 = FALSE #adds line at 2005 to show history from model data


#load basic functions
source('functions/auxiliary_functions.R')
source('functions/witch_load_and_plot.R')






filelist = gsub(".gdx","",list.files(path=pathdir[1], full.names = FALSE, pattern=removepattern, recursive = FALSE))
filelist = filelist[str_detect(filelist, restrict_files)]
if(!exists("scenlist")){scenlist <- gsub(removepattern,"",filelist)}
if(!exists("scenplot_global_order")){scenplot_global_order = seq(1:length(scenlist))}
print("GDX Files:")
print(filelist)
print("Scenario names:")
print(scenlist)
print("Scenarios actually used:")
print(scenlist[scenplot_global_order])

#now only consider the ones used
filelist <- filelist[scenplot_global_order]
scenlist <- scenlist[scenplot_global_order]

#readkey()



# set basic parameters by default
theme_set(theme_bw())
show_numbers_2100 = FALSE
ssp_grid = FALSE
legend_position="bottom"    # "none", "bottom", or "right"

if (!dir.exists(graphdir)){dir.create(graphdir)}


usd_deflator = 1
usd_deflator = 108.686/91.987  #2014 USD



#regions:
get_witch_simple("conf")
region_id <- subset(conf, file==scenlist[1] & V1=="regions")$V2








if(file.exists(paste(graphdir, "all_figures.pdf", sep=""))){(file.remove(paste(graphdir, "all_figures.pdf", sep="")))}
if(create_all_figures_pdf){pdf(paste(graphdir, "all_figures.pdf", sep=""), paper="a4r")}






#Palettes for WITCH regions
get_witch_simple("n")
get_witch_simple("t"); t_model<-unique(t$t)
witch_regions <- subset(n, file==scenlist[1])$V1
region_palette <- palette(rainbow(length(witch_regions)))
region_palette <- c(usa="darkblue", oldeuro="blue", neweuro="cornflowerblue", kosau="darkgreen", cajaz="chartreuse4", te="gold2", mena="darkgoldenrod4", ssa="goldenrod", sasia="darkorange2", china="deeppink3", easia="orangered", laca="gold", india="khaki1", europe="blue", indonesia="brown")


#specialized functions
source('functions/special_plots.R')
source('functions/map_functions.R')
source('functions/energy_mix.R')
source('functions/policy_cost.R')
source('functions/add_historical_values.R')



