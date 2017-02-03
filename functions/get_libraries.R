
# Check R version (>=3.2.0)
if((as.numeric(version$major)<3) | (as.numeric(version$major)==3)&(as.numeric(version$minor)<2) ){
  stop('Please use R with a version > 3.2.0 to run the script')
}

# default loading (from R CRAN)
require_package <- function(package){
  if(!suppressMessages(suppressWarnings(require(package, character.only = TRUE, quietly = TRUE)))) {
    try(install.packages(package, repos="http://cran.r-project.org"), silent = TRUE)
    suppressPackageStartupMessages(library(package,character.only=T, quietly = TRUE))
  } 
}

require_gdxtools <- function(){
  # gdxtools
  if(!suppressMessages(suppressWarnings(require(gdxtools, quietly = TRUE)))){ 
    require_package("devtools")
    require_package("Rcpp")
    install_github('lolow/gdxtools')
    suppressPackageStartupMessages(library(gdxtools, quietly = TRUE))
  }
  
  if(packageVersion("gdxtools")<numeric_version("0.4.0")){
    stop("You need to install a newer version of gdxtools (>=0.4.0). Please run remove.packages('gdxtools', restart R and rerun this script.")
  }
}
