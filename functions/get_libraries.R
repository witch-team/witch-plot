
# Check R version (>=3.2.0)
if((as.numeric(version$major)<3) | (as.numeric(version$major)==3)&(as.numeric(version$minor)<2) ){
  stop('Please use R with a version > 3.2.0 to run the script')
}

# default loading (from R CRAN)
require_package <- function(package){
  if(!is.element(package, .packages(all.available = TRUE))){
    try(install.packages(package, repos="http://cran.rstudio.com"), silent = TRUE)
  }
  suppressPackageStartupMessages(library(package,character.only=T, quietly = TRUE))  
}

require_gdxtools <- function(){
  # gdxtools
  if(!is.element("gdxtools", .packages(all.available = TRUE))){
    require_package("devtools")
    install_github('lolow/gdxtools')
  }
  if(packageVersion("gdxtools")<numeric_version("0.4.0")){
    stop("You need to install a newer version of gdxtools (>=0.4.0). Please run remove.packages('gdxtools'), restart R and rerun this script.")
  }
  suppressPackageStartupMessages(library(gdxtools, quietly = TRUE))
}
