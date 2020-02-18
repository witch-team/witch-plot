#Author: Johannes Emmerling
#v1: February 09th, 2016
witch_folder <- normalizePath(witch_folder)
main_directory <- normalizePath(main_directory)

fullpathdir = file.path(main_directory, subdir)
#Specify directory for graphs and data to be saved: by default: /graphs/ in the folder
graphdir = if(length(fullpathdir)>1){file.path(main_directory, "graphs") }else{file.path(fullpathdir, "graphs")}

#check if directory valid
if(any(!dir.exists(fullpathdir))){stop("Please check the main directory and sub directory!")}
if(!dir.exists(witch_folder)){stop("Please check your witch directory!")}

source('functions/get_libraries.R')
pkgs <- c('data.table', 'stringr', 'docopt', 'countrycode', 'taRifx', 'ggplot2', 'ggpubr', 'scales', 'RColorBrewer', 'dplyr', 'openxlsx', 'gsubfn', 'tidyr', 'rlang', 'shiny', 'shinythemes', 'rworldmap', 'plotly', 'purrr')
res <- lapply(pkgs, require_package)
require_gdxtools()

#In case creating data Excel files creates a problem with old zip!!!
#Sys.setenv(R_ZIPCMD= "C:/apps/Rtools/bin/zip")   

## Local Options ##
figure_format="png"
historical = TRUE  #add historical data where available
theme_set(theme_bw())
show_numbers_2100 = FALSE
ssp_grid = FALSE
legend_position="bottom"    # "none", "bottom", or "right"
write_plotdata_csv = F #if true, saves data of plot as csv file
yearmin = 1980
yearmax = 2100
varlist_combine_old_new_j <- c("Q_EN", "K_EN", "I_EN", "Q_IN")  #variables for which to combine old and new j technologies


## End of Local Options ##



#load basic functions
source('functions/auxiliary_functions.R')
source('functions/witch_load_and_plot.R')

filelist = gsub(".gdx","",list.files(path=fullpathdir[1], full.names = FALSE, pattern="*.gdx", recursive = FALSE))
if(restrict_files[1]!="") {
  for(i in 1:length(restrict_files)){
    .filelist_res = filelist[apply(outer(filelist, restrict_files[i], str_detect), 1, all)]
    if(i==1) .filelist_res_all <- .filelist_res else .filelist_res_all <- c(.filelist_res_all, .filelist_res)
  }
  filelist <- unique(.filelist_res_all)
}
filelist = filelist[!str_detect(filelist, paste(exclude_files, collapse = '|'))]
if(length(filelist)==0){stop("No GDX files found.")}
if(!exists("scenlist")){scenlist <- gsub(paste(removepattern, collapse="|"), "", filelist)}
if(!exists("scenplot_global_order")){scenplot_global_order = seq(1:length(scenlist))}
print("GDX Files:")
print(filelist)
print("Scenario names:")
print(scenlist)
print("Scenarios actually used:")
filelist <- filelist[scenplot_global_order]
scenlist <- scenlist[scenplot_global_order]
print(scenlist)

#file to separate check
if(exists("file_separate")) file_group_columns <- c("file", unname(file_separate[3:length(file_separate)])) else file_group_columns <- "file"


#Palettes for WITCH regions and regional aggregation
if(!exists("region_id")){
get_witch_simple("conf")
if(!(exists("conf"))) stop("No conf set found. Please specify region_i = x manually!")
if(length(unique(subset(conf, V1=="regions")$V2))>1) print("Be careful: not all results files were run with the same regional aggregation!")
region_id <- subset(conf, file==scenlist[1] & pathdir==basename(fullpathdir[1]) & V1=="regions")$V2
}
get_witch_simple("n")
witch_regions <- unique(n$V1)

if(!exists("year0")) year0 = 2005; tstep = 5;

if(!exists("display_regions")){display_regions <- witch_regions}
region_palette <- setNames(rainbow(length(witch_regions)), witch_regions) #just in case have a fall back colour
region_palette_specific <- c(usa="darkblue",Usa="darkblue",oldeuro="blue", neweuro="cornflowerblue",kosau="darkgreen",Kosau="darkgreen",cajaz="chartreuse4",Cajaz="chartreuse4",te="gold2",Te="gold2",mena="darkgoldenrod4",Mena="darkgoldenrod4",ssa="goldenrod",Ssa="goldenrod",sasia="darkorange2","South Asia"="darkorange2",china="deeppink3",PRC="deeppink3",easia="orangered",ESEAP="orangered",laca="#fbb714",Laca="#fbb714",india="#fbf003",India="#fbf003",europe="blue",Europe="blue",indonesia="lightsalmon3",Indonesia="lightsalmon3",Rest_of_World="grey48",chinaw="darkorange",chinac="darkorange2",chinae="darkorange4",italy="green",mexico="slateblue2",brazil="tomato4",canada="blueviolet",jpnkor="darkseagreen",oceania="forestgreen",southafrica="indianred3",seasia="orangered",World="black", "Global Pool"="black")
if(exists("conf")) region_palette <- region_palette_specific #c(region_palette_specific, region_palette_rainbow)
if(!exists("regions_focus")){regions_focus <- witch_regions}
print(paste("Regional aggregation:", region_id))
witch_colors_alphabetic <- c("chartreuse4", "deeppink3", "orangered", "khaki1", "darkgreen", "gold", "darkgoldenrod4", "cornflowerblue", "blue", "darkorange2", "goldenrod", "gold2", "darkblue")

witch_name_short <- function(witch_name){
  witch_name  <- gsub("indonesia", "IDN", witch_name)
  witch_name_shortened <- substr(toupper(witch_name), 1, 3)
  witch_name_shortened <- gsub("MEN", "MEA", witch_name_shortened)
  witch_name_shortened <- gsub("SOU", "ZAF", witch_name_shortened)
  witch_name_shortened <- gsub("CHI", "CHN", witch_name_shortened)
  witch_name_shortened <- gsub("TE", "TEC", witch_name_shortened)
  return(witch_name_shortened)
}
region_palette_specific_short <- region_palette_specific; names(region_palette_specific_short) <- witch_name_short(names(region_palette_specific))

witch_region_names <-"n,longname
canada,Canada
jpnkor,Japan-Korea
oceania,Oceania
indonesia,Indonesia
southafrica,South Africa
brazil,Brazil
mexico,Mexico
china,China
india,India
te,Transition Economies
ssa,Sub-Saharan Africa
laca,Latin America-Caribbean
sasia,South Asia
seasia,South East Asia
mena,Middle East-North Africa
europe,Europe
usa,United States of America
easia,East Asia
kosau,South-Korea and Australia
cajaz,Canada Japan New-Zealand,
neweuro,Eastern Europe
oldeuro,Western Europe"
witch_region_names <- read.table(textConnection(witch_region_names), sep=",", head=T, dec=".")
region_palette_longnames <- region_palette
names(region_palette_longnames) <- mapvalues(names(region_palette), as.character(witch_region_names$n), paste0(as.character(witch_region_names$longname), " (",as.character(witch_region_names$n),")"), warn_missing = F)

#load specialized functions
source('functions/map_functions.R')
source('functions/add_historical_values.R')
source('functions/export_variables.R')
source('functions/diagnostics.R')
source('functions/impact_plots.R')
source('functions/energy_plots.R')
source('functions/emission_plots.R')
source('functions/climate_plots.R')
source('functions/policy_cost.R')



