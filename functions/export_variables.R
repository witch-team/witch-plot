#epoort data routines to create datasets based on historical (and possibly WITCH) or projected data, always only for ONE scenario!

#function to store a set of variables in long format, or optional wide with regions as columns
write_witch_data_csv <- function(vars, years = "all", wide_region=FALSE){		
  for(.var in vars){
    data <- get_witch_simple(.var)
    data$pathdir <- NULL		
    data$t <- ttoyear(data$t); setnames(data, "t", "year")
    data$variable <- .var
    set_dep <- setdiff(colnames(data), c("file", "n", "year", "variable", "value"))
    if(length(set_dep)>0){
      for(.set in setdep) data[[.set]] <- tolower(data[[.set]]) #ensure lower case of all set elements   
      setnames(data, set_dep, paste0("V", seq(1:length(set_dep))))
      }
    if(.var==vars[1]){allvars <- data}else{allvars <- rbind(allvars, data, fill=T)}
  }
  allvars <- as.data.table(allvars)
  setcolorder(allvars, c("file", "variable", "n", "year", setdiff(colnames(allvars), c("file", "n", "year", "variable", "value")), "value"))
  if(years[1]!="all"){allvars <- subset(allvars, year %in% years)}
  if(length(scenlist)==1){allvars$file <- NULL}
  if(wide_region){allvars <- dcast(allvars, formula = as.formula(paste(paste(setdiff(colnames(allvars), c("n", "value")), collapse = " + "), "~ n")))}
  write.csv(allvars, file=paste0(graphdir, "witch_dataset_long", ".csv"), row.names = FALSE)		
}		


write_witch_historical_iso3_dataset <- function(maxsetdep=3){
  #SPECIAL historical dataset
  data_historical_values_special <- gdx(file.path(witch_folder,'input','build',"data_historical_values_special.gdx"))
  data_historical_values_special_allvars <- batch_extract(data_historical_values_special$parameters$name, files = file.path(witch_folder,'input','build',"data_historical_values_special.gdx"))
  data_historical_values_special_allvars <- lapply(data_historical_values_special_allvars, FUN=function(data) data[-c(ncol(data))]) #remove gdx filename
  same_length_and_sets <- function(data){
    data <- as.data.table(data)
    setdep <- setdiff(colnames(data), c("iso3", "year", "value"))
    if(length(setdep)>0){
      for(.set in setdep) data[[.set]] <- tolower(data[[.set]]) #ensure lower case of all set elements      
      setnames(data, setdep, paste0("V",seq(1,length(setdep))))
      
    } 
    for(s in seq(1, maxsetdep)) if(is.null(data[[paste0("V",s)]])) data[[paste0("V",s)]] <- NA
    return(data)
  }
  data_historical_values_special_allvars <- lapply(data_historical_values_special_allvars, same_length_and_sets)
  for (.var in names(data_historical_values_special_allvars)){
    if("iso3" %in% colnames(data_historical_values_special_allvars[[.var]])){
    .df <- data_historical_values_special_allvars[[.var]]
    .df$variable <- .var
    setcolorder(.df, c("variable", "iso3", "year", paste0("V",seq(1,maxsetdep)), "value"))
    if(.var==names(data_historical_values_special_allvars)[1]){allvars <- .df}else{allvars <- rbind(allvars, .df)}
    }
  }
  allvars_special <- allvars
  #Standard historical dataset
  data_historical_values <- gdx(file.path(witch_folder,'input','build',"data_historical_values.gdx"))
  data_historical_values_allvars <- batch_extract(data_historical_values$parameters$name, files = file.path(witch_folder,'input','build',"data_historical_values.gdx"))
  data_historical_values_allvars <- lapply(data_historical_values_allvars, FUN=function(data) data[-c(ncol(data))]) #remove gdx filename
  data_historical_values_allvars <- lapply(data_historical_values_allvars, same_length_and_sets)
  for (.var in names(data_historical_values_allvars)){
    if("iso3" %in% colnames(data_historical_values_allvars[[.var]])){
    print(paste("Processing",.var))
    .df <- data_historical_values_allvars[[.var]]
    .df$variable <- .var
    setcolorder(.df, c("variable", "iso3", "year", paste0("V",seq(1,maxsetdep)), "value"))
    if(.var==names(data_historical_values_allvars)[1]){allvars <- .df}else{allvars <- rbind(allvars, .df)}
    }
  }
  allvars <- rbind(allvars, allvars_special)
  #write dataset
  write.csv(allvars, file=paste0(graphdir, "witch_historical_iso3_dataset", ".csv"), row.names = FALSE)	
}



