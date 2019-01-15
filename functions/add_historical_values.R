
add_historical_values <- function(variable, varname=deparse(substitute(variable)), scenplot=scenlist, check_calibration=FALSE, verbose=T){
  
  #have to decide what to do with years with both model and historical data
  display_years = "historical"#historical" #"model" #"historical"

  valid_suffix <- "_valid"
  if(varname=="Q_EMI"){valid_suffix <- "_valid_primap"} #for CO2IND emissions, set it to 
  if(varname=="quantiles"){valid_suffix <- "_valid_swiid"} #for quantiles, set it to 
  if(varname=="K_EN"){valid_suffix <- "_valid_platts_tot"} #for quantiles, set it to 
  
  
  #check which GDX file to use (all files that start with data_historical*.gdx)
  gdxhistlist <- list.files(path=paste0(witch_folder, "data_", region_id), full.names = TRUE, pattern="^data_historical", recursive = FALSE)
  
  for(.gdxname in gdxhistlist){
    .gdx <- gdx(.gdxname)
    if(!is.na(pmatch(paste0(tolower(varname), valid_suffix) ,.gdx$parameters$name))){break}
  }
  
  if(!is.na(pmatch(paste0(tolower(varname), valid_suffix) ,.gdx$parameters$name))){
    if(verbose) print(paste0("Historical values added for '", varname, "'."))
    item <- .gdx$parameters$name[pmatch(paste0(tolower(varname), valid_suffix) ,.gdx$parameters$name)]
    .hist <- as.data.table(.gdx[item]) 
    #get set dependency based on WITCH variable
    #colnames(.hist) <- setdiff(colnames(variable), c("file", "pathdir"))
    #better: get it from /built/!!!
    .gdxiso3 <- gdx(paste0(witch_folder, "input/build/", basename(.gdxname))); 
    colnames(.hist) <- colnames(.gdxiso3[item])	
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
      .gdx_validation <- gdx(paste0(witch_folder, "data_", region_id, "/data_validation.gdx"))
      .hist_validation <- as.data.table(.gdx_validation[item])
      colnames(.hist_validation) <- colnames(.hist)
      .hist_validation$file <- "validation"
      #for the historical set, uswe "historical"
      .hist$file <- "historical"
      .hist <- rbind(.hist,.hist_validation)
    }


    #special case where categories do not match exactly
    if(item=="q_en_valid_weo")
    {
      .ren <- .hist[j=="elsolwind"]  #take half solar half wind as proxy!!
      .pv <- .ren; .pv$value = .pv$value*0.5; .pv$j <- "elpv"
      .wind <- .ren; .wind$value = .wind$value*0.5; .wind$j <- "elwind"
      .csp <- .ren; .csp$value = .csp$value*0; .csp$j <- "elcsp"
      #now replace in original data
      .hist <- .hist[j!="elsolwind"]
      .hist <- rbind(.hist, .pv, .csp, .wind)
    }
    
    if(item=="q_fen_valid_weo")
    {
      setnames(.hist, "sec", "z")
      setnames(.hist, "fuel", "fr")
    }
    
    if(item=="q_in_valid_weo") #add fuel column
    {
      .hist$fuel <- "oil"
      .hist[jfed=="elgastr"]$fuel <- "gas"
      .hist[jfed=="elpc"]$fuel <- "coal"
      .hist[jfed=="elpb"]$fuel <- "wbio"
      .hist[jfed=="elnuclear"]$fuel <- "uranium"
    }
    
    if(item=="k_en_valid_platts_tot")
    {
      setnames(.hist, "j", "jreal")
    }
 
       
    #merge with variable
    if(check_calibration){
      #just multiply by the pathdir so it appears for each pathdir
      .hist_temp <- .hist
      for(pd in basename(pathdir))
      {
        .hist_temp$pathdir <- pd
        if(pd==basename(pathdir)[1]){.hist=.hist_temp}else{.hist <-rbind(.hist,.hist_temp)}
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
      for(pd in basename(pathdir))
      {
        .hist_temp$pathdir <- pd
        if(pd==basename(pathdir)[1]){.hist=.hist_temp}else{.hist <-rbind(.hist,.hist_temp)}
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
 
    #assign("varname", merged_variable, envir = .GlobalEnv)
    return(merged_variable)
    }
  else
    {
    return(as.data.table(variable))
    }
}
