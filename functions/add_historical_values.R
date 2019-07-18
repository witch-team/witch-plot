
add_historical_values <- function(variable, varname=deparse(substitute(variable)), scenplot=scenlist, check_calibration=FALSE, verbose=T){
  
  #have to decide what to do with years with both model and historical data
  display_years = "model"#historical" #"model" #"historical"

  valid_suffix <- "_valid"
  if(varname=="Q_EMI"){valid_suffix <- "_valid_primap"} #for CO2IND emission
  if(varname=="quantiles"){valid_suffix <- "_valid_swiid"} #for quantiles
  if(varname=="K_EN"){valid_suffix <- c("_valid_platts_tot", "_valid_irena", "_valid_iaea", "_valid_gcpt")} #for quantiles, set it to 
  
  #treat special varnames
  if(str_detect(varname, "MAGICC")) varname <- gsub("MAGICC", "", varname)
  if(str_detect(varname, "HECTOR")) varname <- gsub("HECTOR", "", varname)
  
  #check which GDX file to use (all files that start with data_historical*.gdx)
  if(!dir.exists(file.path(witch_folder, paste0("data_", region_id)))){stop("Please check your witch/data_* directory!")}
  gdxhistlist <- list.files(path=file.path(witch_folder, paste0("data_", region_id)), full.names = TRUE, pattern="^data_historical", recursive = FALSE)
  
  for(.gdxname in gdxhistlist){
    .gdx <- gdx(.gdxname)
    if(length(grep(paste(paste0("^", tolower(varname), valid_suffix), collapse = '|'), .gdx$parameters$name, value = TRUE))!=0){break} #to find the hist file with the valid data (only one!)
  }
  
  if(length(grep(paste(paste0("^", tolower(varname), valid_suffix), collapse = '|'), .gdx$parameters$name, value = TRUE))!=0){
    if(verbose) print(paste0("Historical values added for '", varname, "'."))
    item <- grep(paste(paste0("^", tolower(varname), valid_suffix), collapse = '|'), .gdx$parameters$name, value = TRUE) #use grep with ^ to have them start by varname
    for(.item in item){.hist_single <- as.data.table(.gdx[.item]); .hist_single$file <- gsub(paste0(tolower(varname), "_"), "", .item); if(.item==item[1]){.hist <- .hist_single}else{.hist <- rbind(.hist,.hist_single)} } 
    #get set dependency based on /build/ folder
    .gdxiso3 <- gdx(file.path(witch_folder, "input", "build", basename(.gdxname))); 
    colnames(.hist) <- c(colnames(.gdxiso3[item[1]]), "file")	
    #in built global data have set "global", but in input folder it gets converted to iso3, so:
    colnames(.hist) <- gsub("global", "iso3", colnames(.hist)) #add "World" if no country level data but global
    if(!("iso3" %in% colnames(.hist))){.hist$n = "World"}else{colnames(.hist) <- gsub("iso3", "n", colnames(.hist))}
    setnames(.hist, "year", "t")
    #adjust time unit to model
    .hist$t <- yeartot(.hist$t)
    t_historical<-unique(.hist$t)
    #adjust scenario names
    .hist$n  <- mapvalues(.hist$n , from=witch_regions, to=display_regions, warn_missing = F)
    
    #if check_calibration, add validation as data points!
    if(check_calibration){
      .gdx_validation <- gdx(file.path(witch_folder, paste0("data_", region_id), "data_validation.gdx"))
      for(.item in item){.hist_validation_single <- as.data.table(.gdx_validation[.item]); .hist_validation_single$file <- gsub(paste0(tolower(varname), "_"), "", .item); if(.item==item[1]){.hist_validation <- .hist_validation_single}else{.hist_validation <- rbind(.hist_validation,.hist_validation_single)} }
      #.hist_validation <- as.data.table(.gdx_validation[item])
      if(!("n" %in% colnames(.hist_validation))){.hist_validation$n = "World"}
      colnames(.hist_validation) <- colnames(.hist)
      #for the historical set, use "historical"
      .hist$file <- gsub("valid", "historical", .hist$file)
      .hist <- rbind(.hist,.hist_validation)
    }
    else{
      #if not check_calibration and historical files are added to the scenarios, compute the mean in case multiple historical sources for one sub-item (e.g., elhydro) and drop the file column
      .hist$file <- NULL
      .hist <- .hist %>% group_by_at(setdiff(names(.hist), "value")) %>% summarize(value=mean(value)) %>% as.data.table()
    }


    #special case where categories do not match exactly
    if("q_fen_valid_weo" %in% item)
    {
      setnames(.hist, "sec", "z")
      setnames(.hist, "fuel", "fr")
    }

    if("q_in_valid_weo" %in% item) #add fuel column
    {
      .hist$fuel <- "oil"
      .hist[jfed=="elgastr"]$fuel <- "gas"
      .hist[jfed=="elpc"]$fuel <- "coal"
      .hist[jfed=="elpb"]$fuel <- "wbio"
      .hist[jfed=="elnuclear"]$fuel <- "uranium"
    }
    
       
    #merge with variable
    if(check_calibration){
      #just multiply by the pathdir so it appears for each pathdir
      .hist_temp <- .hist
      for(pd in basename(fullpathdir))
      {
        .hist_temp$pathdir <- pd
        if(pd==basename(fullpathdir[1])){.hist=.hist_temp}else{.hist <-rbind(.hist,.hist_temp)}
      }
    }else{
    #first multiply by scenplot, add missing columns here add historical data to results
    .hist_temp <- .hist
    
      for(scen in scenplot)
      {
        .hist_temp$file <- scen
        if(scen==scenplot[1]){.hist=.hist_temp}else{.hist <-rbind(.hist,.hist_temp)}
      }
      .hist_temp <- .hist
      for(pd in basename(fullpathdir))
      {
        .hist_temp$pathdir <- pd
        if(pd==basename(fullpathdir)[1]){.hist=.hist_temp}else{.hist <-rbind(.hist,.hist_temp)}
      }
      if(display_years=="model"){
        #display model data for overlapping years, delete historical data
        .hist <- subset(.hist, !(t %in% t_model))
      }else{
        #or display historical data years, delete model data for 2005 and 2010
        #variable <- subset(variable, !(t %in% unique(.hist$t)))
        #variable <- subset(variable, !(t %in% unique(.hist$t)))
        variable <- subset(variable, !(t %in% t_historical))
      }
    }


    
 
    merged_variable <- rbind(variable, .hist)
    merged_variable$t <- as.numeric(merged_variable$t)
    #assign("varname", merged_variable, envir = .GlobalEnv)
    return(merged_variable)
    }
  else
    {
    return(as.data.table(variable))
    }
}
