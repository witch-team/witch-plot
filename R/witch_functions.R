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

# gdxtools
require_gdxtools <- function(){ 
  if(!is.element("gdxtools", .packages(all.available = TRUE))){
  require_package("devtools")
  install_github('lolow/gdxtools')
}
if(packageVersion("gdxtools")<numeric_version("0.4.0")){
  stop("You need to install a newer version of gdxtools (>=0.4.0). Please run remove.packages('gdxtools'), restart R and rerun this script.")
}
suppressPackageStartupMessages(library(gdxtools, quietly = TRUE))
}
#Install and load packages
require_package <- function(package){
  if(!is.element(package, .packages(all.available = TRUE))){
    try(install.packages(package, repos="http://cran.rstudio.com"), silent = TRUE)
  }
  suppressPackageStartupMessages(library(package,character.only=T, quietly = TRUE))  
}

pkgs <- c('data.table', 'stringr', 'docopt', 'countrycode', 'taRifx', 'ggplot2', 'ggpubr', 'scales', 'RColorBrewer', 'dplyr', 'openxlsx', 'gsubfn', 'tidyr', 'rlang', 'shiny', 'shinythemes', 'rworldmap','sf', 'rnaturalearth', 'plotly', 'purrr', 'reldist')
res <- lapply(pkgs, require_package)
require_gdxtools()
library(dplyr, warn.conflicts = FALSE)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

#In case creating data Excel files creates a problem with old zip!!!
#Sys.setenv(R_ZIPCMD= "C:/apps/Rtools/bin/zip")   

## Local Options ##
deploy_online <- FALSE #if not deployed onlinse save graphs
figure_format="png"
historical = TRUE  #add historical data where available
theme_set(theme_bw())
show_numbers_2100 = FALSE
ssp_grid = FALSE
legend_position="bottom"    # "none", "bottom", or "right"
write_plotdata_csv = F #if true, saves data of plot as csv file
varlist_combine_old_new_j <- c("Q_EN", "K_EN", "I_EN", "Q_IN")  #variables for which to combine old and new j technologies
if(!exists("year0")) {year0 = 2005; tstep = 5;}
if(!exists("yearmin")) yearmin = 1980
if(!exists("yearmax")) yearmax = 2100
## End of Local Options ##



#load basic functions
source('R/auxiliary_functions.R')
source('R/witch_load_and_plot.R')
source('R/add_historical_values.R')

filelist = gsub(".gdx","",list.files(path=fullpathdir[1], full.names = FALSE, pattern="*.gdx", recursive = FALSE))
if(restrict_files[1]!="") {
  for(i in 1:length(restrict_files)){
    .filelist_res = filelist[apply(outer(filelist, restrict_files[i], str_detect), 1, all)]
    if(i==1) .filelist_res_all <- .filelist_res else .filelist_res_all <- c(.filelist_res_all, .filelist_res)
  }
  filelist <- unique(.filelist_res_all)
}
if(exclude_files[1]!="") filelist = filelist[!str_detect(filelist, paste(exclude_files, collapse = '|'))]
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


#get variable description of all variables from the 1st file
mygdx <- gdx(file.path(fullpathdir[1],paste0(filelist[1],".gdx")))
all_var_descriptions <- rbind(data.frame(name=mygdx$variables$name, description=mygdx$variables$text), data.frame(name=mygdx$parameters$name, description=mygdx$parameters$text))

#Palettes for WITCH regions and regional aggregation
if(!exists("region_id")){
get_witch_simple("conf")
if(!(exists("conf"))) stop("No conf set found. Please specify region_i = x manually!")
if(length(unique(subset(conf, V1=="regions")$V2))>1) print("Be careful: not all results files were run with the same regional aggregation!")
region_id <- subset(conf, file==scenlist[1] & pathdir==basename(fullpathdir[1]) & V1=="regions")$V2
}
n <- suppressWarnings(batch_extract("n", files = file.path(fullpathdir,paste0(filelist,".gdx"))))
if(is.null(n$n)) witch_regions <- "World" else witch_regions <- unique(n$n$V1)
display_regions <- witch_regions

region_palette_specific <- setNames(rainbow(length(witch_regions)), witch_regions) #just in case have a fall back colour
region_palette_witch <- c(usa="darkblue",Usa="darkblue",oldeuro="blue", neweuro="cornflowerblue",kosau="darkgreen",Kosau="darkgreen",cajaz="chartreuse4",Cajaz="chartreuse4",te="gold2",Te="gold2",mena="darkgoldenrod4",Mena="darkgoldenrod4",ssa="goldenrod",Ssa="goldenrod",sasia="darkorange2","South Asia"="darkorange2",china="deeppink3",PRC="deeppink3",easia="orangered",ESEAP="orangered",laca="#fbb714",Laca="#fbb714",india="#fbf003",India="#fbf003",europe="blue",Europe="blue",indonesia="lightsalmon3",Indonesia="lightsalmon3",Rest_of_World="grey48",chinaw="darkorange",chinac="darkorange2",chinae="darkorange4",italy="green",mexico="slateblue2",brazil="tomato4",canada="blueviolet",jpnkor="darkseagreen",oceania="forestgreen",southafrica="indianred3",seasia="orangered",World="black", "Global Pool"="black")
#add ed57 region colors for RICE50+
region_palette_ed57 <- c("arg" =  "#000000","aus" =  "#48d1cc","aut" =  "#ae8000","bel" =  "#800000","bgr" =  "#003366","blt" =  "#bf4040","bra" =  "#ffd633","can" =  "#6600cc","chl" =  "#ffece6","chn" =  "#ff531a","cor" =  "#adebad","cro" =  "#808080","dnk" =  "#ff9933","egy" =  "#0044cc","esp" =  "#ffd6cc","fin" =  "#00cccc","fra" =  "#cc0000","gbr" =  "#ffdddd","golf57"  =  "#33d6ff","grc" =  "#00ffcc","hun" =  "#9999ff","idn" =  "#996633","irl" =  "#ff4dff","ita" =  "#ffff00","jpn" =  "#006600","meme"=  "#b32d00","mex" =  "#ccff33","mys" =  "#145252","nde" =  "#00d900","nld" =  "#c309bd","noan"=  "#ffff99","noap"=  "#ecf2f9","nor" =  "#ff3399","oeu" =  "#ffb3ff","osea"=  "#008fb3","pol" =  "#d6f5d6","prt" =  "#003300","rcam"=  "#4d1919","rcz" =  "#00ffff","rfa" =  "#deb887","ris" =  "#000080","rjan57"  =  "#bf00ff","rom" =  "#ff00ff","rsaf"=  "#ff8000","rsam"=  "#0000ff","rsas"=  "#ccd6dd","rsl" =  "#00ff00","rus" =  "#66757f","slo" =  "#ff3091","sui" =  "#61a62f","swe" =  "#cb1942","tha" =  "#efff14","tur" =  "#4b0082","ukr" =  "#c198ff","usa" =  "#ffcc00","vnm" =  "#3377ff","zaf" =  "#b3ccff")
region_palette <- replace(region_palette_specific, names(region_palette_witch), region_palette_witch)
region_palette <- replace(region_palette, names(region_palette_ed57), region_palette_ed57)
print(paste("Numbers of regions considered:", length(witch_regions)))

witch_name_short <- function(witch_name){
  witch_name  <- gsub("indonesia", "IDN", witch_name)
  witch_name_shortened <- substr(toupper(witch_name), 1, 3)
  witch_name_shortened <- gsub("MEN", "MEA", witch_name_shortened)
  witch_name_shortened <- gsub("SOU", "ZAF", witch_name_shortened)
  witch_name_shortened <- gsub("CHI", "CHN", witch_name_shortened)
  witch_name_shortened <- gsub("TE", "TEC", witch_name_shortened)
  return(witch_name_shortened)
}
region_palette_specific_short <- region_palette; names(region_palette_specific_short) <- witch_name_short(names(region_palette_specific))

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
source('R/map_functions.R')
source('R/export_variables.R')
source('R/diagnostics.R')
source('R/impact_plots.R')
source('R/energy_plots.R')
source('R/emission_plots.R')
source('R/climate_plots.R')
source('R/policy_cost.R')
source('R/inequality_plots.R')

if(!dir.exists(file.path(witch_folder, paste0("data_", region_id)))) print("No data_* directory for historical data found.")


