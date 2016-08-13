
add_historical_values <- function(variable, varname=deparse(substitute(variable))){
  .gdx <- gdx(paste0(witch_folder, "data_", region_id, "/data_historical_values.gdx"))
  valid_suffix <- "_valid"  #for CO2IND emissions, set it to 
  if(varname=="Q_EMI"){valid_suffix <- "_valid_oecd"}
  if(!is.na(pmatch(paste0(tolower(varname), valid_suffix) ,.gdx$parameters$name))){
    print(paste0("Historical values added for '", varname, "'."))
    item <- .gdx$parameters$name[pmatch(paste0(tolower(varname), valid_suffix) ,.gdx$parameters$name)]
    .hist <- as.data.table(.gdx[item]) 
    
    #recover set dependency from iso3 data
    .gdxiso3 <- gdx(paste0(witch_folder, "input/build/data_historical_values.gdx"))
    colnames(.hist) <-colnames(.gdxiso3[item])
    setnames(.hist, "iso3", "n")
    
    #adjust time unit to model
    .hist$year <- yeartot(.hist$year)
    setnames(.hist, "year", "t")
    t_historical<-unique(.hist$t)

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
      .hist$j <- mapvalues(.hist$j, from=c("elnuclear", "elpc", "elpb", "elgastr", "elhydro", "eloil"), to=c("elnuclear_old", "elpc_old", "elpb_old", "elgastr_old", "elhydro_old", "eloil_old"))
      
    }
    if(item=="q_in_valid_weo") #add f column
    {
      .hist$f <- "oil"
      .hist[jfed=="elgastr"]$f <- "gas"
      .hist[jfed=="elpc"]$f <- "coal"
      .hist[jfed=="elpb"]$f <- "wbio"
      .hist[jfed=="elnuclear"]$f <- "uranium"
      .hist$jfed <- mapvalues(.hist$jfed, from=c("elnuclear", "elpc", "elpb", "elgastr", "eloil"), to=c("elnuclear_old", "elpc_old", "elpb_old", "elgastr_old", "eloil_old"))
    }
    
    
    
    #merge with variable
    #first multiply by scenlist, add missing columns
    .hist_temp <- .hist
    for(scen in scenlist)
    {
      .hist_temp$file <- scen
      if(scen==scenlist[1]){.hist=.hist_temp}else{.hist <-rbind(.hist,.hist_temp)}
    }
    .hist_temp <- .hist
    for(pd in basename(pathdir))
    {
      .hist_temp$pathdir <- pd
      if(pd==basename(pathdir)[1]){.hist=.hist_temp}else{.hist <-rbind(.hist,.hist_temp)}
    }
    
    #if(varname=="Q_IN"){print(.hist[jfed=="elpc_old" & n=="china" & file=="BAU" & t>=0 & t<=2])}

    #have to decide what to do with years with both model and historical data
    display_years = "model"
    if(display_years=="model"){
      #display model data for overlapping years, delete historical data
      .hist <- subset(.hist, !(t %in% t_model))
    }else{
      #or display historical data years, delete model data for 2005 and 2010
      #variable <- subset(variable, !(t %in% unique(.hist$t)))
      #variable <- subset(variable, !(t %in% unique(.hist$t)))
      variable <- subset(variable, !(t %in% t_historical))
    }
    
    #if(varname=="Q_IN"){print(.hist[jfed=="elpc_old" & n=="china" & file=="BAU" & t>=0 & t<=2])}
    
    merged_variable <- rbind(variable, .hist)
    
    #if(varname=="Q_IN"){print(merged_variable[jfed=="elpc_old" & n=="china" & file=="BAU" & t>=0 & t<=2])}
    
    #assign("varname", merged_variable, envir = .GlobalEnv)
    return(merged_variable)
    }
  else
    {
    return(variable)
    }
}


  