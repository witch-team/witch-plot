#First install these four packaged if not yet available
#require(tidyverse)
#require(arrow)
#require(reticulate)
#require(plotly)

#Function to get IIASAdb variable
get_iiasadb <- function(database="ar6-public", varlist="Emissions|CO2", varname=NULL, region="World", show_variables = F){
  require(reticulate)
  pyam <- import("pyam", convert = FALSE)
  #if private databases provide credentials
  if(file.exists("iiasa_credentials.yml")){
    iiasa_credentials <- yaml::read_yaml("iiasa_credentials.yml") # two lines with username: and password:
    pyam$iiasa$set_config(iiasa_credentials$username, iiasa_credentials$password)
  }
  #show variables in case
  if(show_variables) print(paste(variable=py_to_r(pyam$iiasa$Connection(database)$variables())))
  assign("variables", as.data.frame(py_to_r(pyam$iiasa$Connection(database)$variables())), envir = .GlobalEnv)
  ar6_data <- pyam$read_iiasa(database, model='*', scenario="*", variable=varlist, region=region, meta=1)
  #as_pandas concatenates data and meta into a pandas DF (meta_cols = TRUE adds all meta data)
  iiasadb_df <- ar6_data$as_pandas(meta_cols = c("Ssp_family", "Policy_category", "Policy_category_name", "Category", "IMP_marker"))
  #pandas to R data frame
  iiasadb_df <- py_to_r(iiasadb_df)
  #all categories are lists, convert to simple vectors
  Policy_category <- data.frame(Policy_category=unlist(iiasadb_df$Policy_category))
  Policy_category_name <- data.frame(Policy_category_name=unlist(iiasadb_df$Policy_category_name))
  Category <- data.frame(Category=unlist(iiasadb_df$Category))
  iiasadb_df <- iiasadb_df %>% select(-c("Policy_category", "Policy_category_name", "Category"))
  iiasadb_df <- cbind(iiasadb_df, Policy_category, Policy_category_name, Category)
  iiasadb_df <- iiasadb_df %>% dplyr::filter(Category!="failed-vetting" & Category!="NaN" & Category!="no-climate-assessment")
  if(!is.null(varname)) iiasadb_df <- iiasadb_df %>% mutate(variable = plyr::mapvalues(variable, from = varlist, to = varname))
  return(iiasadb_df)
}