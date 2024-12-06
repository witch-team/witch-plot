#First install these packages if not yet available (for standalone mode)
#require(tidyverse)
#require(reticulate)
#require(yaml)

#Function to get IIASAdb variable
get_iiasadb <- function(database="ar6-public", varlist="Emissions|CO2", varname=NULL, modlist="*", scenlist="*", region="World", show_variables = F, add_metadata = T){
  require(reticulate)
  pyam <- import("pyam", convert = FALSE)
  #if private databases provide credentials
  if(file.exists("iiasa_credentials.yml")){
    require(yaml)
    iiasa_credentials <- yaml::read_yaml("iiasa_credentials.yml") # two lines with username: and password:
    pyam$iiasa$set_config(iiasa_credentials$username, iiasa_credentials$password)
  }
  #show variables in case
  if(show_variables) print(py_to_r(pyam$iiasa$Connection(database)$variables()))
  assign("iiasadb_variables_available", as.data.frame(py_to_r(pyam$iiasa$Connection(database)$variables())), envir = .GlobalEnv)
  print("Available_Connections:"); print(py_to_r(pyam$iiasa$Connection(database)$valid_connections))
  iiasadb_data <- pyam$read_iiasa(database, model=modlist, scenario=scenlist, variable=varlist, region=region, meta=1)
  #If AR6, also add meta categories and other meta data
  if(database == "ar6_public" & add_metadata){
     #as_pandas concatenates data and meta into a pandas DF (meta_cols = TRUE adds all meta data)
    iiasadb_df <- iiasadb_data$as_pandas(meta_cols = c("Ssp_family", "Policy_category", "Policy_category_name", "Category", "IMP_marker"))
    #pandas to R data frame
    iiasadb_df <- py_to_r(iiasadb_df)
    #all categories are lists, convert to simple vectors
    Policy_category <- data.frame(Policy_category=unlist(iiasadb_df$Policy_category))
    Policy_category_name <- data.frame(Policy_category_name=unlist(iiasadb_df$Policy_category_name))
    Category <- data.frame(Category=unlist(iiasadb_df$Category))
    iiasadb_df <- iiasadb_df %>% select(-c("Policy_category", "Policy_category_name", "Category"))
    iiasadb_df <- cbind(iiasadb_df, Policy_category, Policy_category_name, Category)
    iiasadb_df <- iiasadb_df %>% dplyr::filter(Category!="failed-vetting" & Category!="NaN" & Category!="no-climate-assessment")
  } 
  else
  {
    iiasadb_df <- py_to_r(iiasadb_data$as_pandas())
  }
  if(!is.null(varname)) iiasadb_df <- iiasadb_df %>% mutate(variable = dplyr::recode(variable, !!!setNames(varname, varlist)))
  return(iiasadb_df)
}
