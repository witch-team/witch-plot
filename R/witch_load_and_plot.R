# Load GDX of all scenarios and basic pre-processing 
get_witch_simple <- function(variable_name, variable_name_save=variable_name, scenplot=scenlist, check_calibration=FALSE, results="assign", force_reload=F, field = "l", postprocesssuffix=NULL){
  if(!is.data.frame(variable_name) | (variable_name %in% c("t", "n", "p", "I")) | force_reload){
    if(exists("allfilesdata", envir = .GlobalEnv)){rm(allfilesdata, envir = .GlobalEnv)}
    variable_name_save=as.character(gsub("_", " ", variable_name_save))
    for (current_pathdir in fullpathdir){
      for (file in filelist){
        if(file.exists(file.path(current_pathdir, paste0(file,".gdx")))){
          mygdx <- gdx(file.path(current_pathdir, paste0(file,".gdx")))
          if(!is.null(postprocesssuffix)) mygdx <- gdx(file.path(current_pathdir, paste0(paste0(file, "_", postprocesssuffix),".gdx")))
          if(is.element(variable_name, all_items(mygdx)$variables) | is.element(variable_name, all_items(mygdx)$parameters) | is.element(variable_name, all_items(mygdx)$sets) | is.element(variable_name, all_items(mygdx)$variables) | is.element(variable_name, all_items(mygdx)$equations))
          {
            tempdata <- data.table(mygdx[variable_name, field = field])
            if(!("n" %in% names(tempdata))) tempdata$n <- "World"
            tempdata$file <- as.character(file)
            if(length(fullpathdir)>=1){tempdata$pathdir <- basename(current_pathdir)}
            if(!exists("allfilesdata")){allfilesdata<-tempdata}else{allfilesdata <-rbind(allfilesdata,tempdata)}
            remove(tempdata)
          }
        }
      }
    }
    if(exists("allfilesdata")){
      allfilesdata$file <- mapvalues(allfilesdata$file , from=filelist, to=scenlist, warn_missing = FALSE)
      if(str_detect(variable_name, "eq")) {
        colnames(allfilesdata) <- gsub("V1", "t", colnames(allfilesdata)) 
        colnames(allfilesdata) <- gsub("V2", "n", colnames(allfilesdata))
      }
      allfilesdata <- subset(allfilesdata, file %in% scenplot)
      if(("t" %in% colnames(allfilesdata)) & !(variable_name=="t")){
        #check if stochastic and if so convert "branch" to "file" element
        allfilesdata <- convert_stochastic_gdx(allfilesdata)            
        allfilesdata$t <- as.numeric(allfilesdata$t)
      }
      if(("n" %in% colnames(allfilesdata)) & !(is.element(variable_name, all_items(mygdx)$sets))){allfilesdata$n  <- mapvalues(allfilesdata$n , from=witch_regions, to=display_regions, warn_missing = F)}else{allfilesdata$n <- "World"}
      if(str_detect(variable_name, "MAGICC|HECTOR")) {allfilesdata <- suppressWarnings(allfilesdata[,-c("magicc_n", "hector_n")])}
      
      #combine _old, _new, _late to one unit in case present
      combine_old_new_j = TRUE
      if(combine_old_new_j & (variable_name %in% varlist_combine_old_new_j)){
        j_set <- str_subset(names(allfilesdata), "^j")  
        if(length(j_set)>0){
          #if Q_EN, REMOVE old, new etc. to avoid double counting
          if(variable_name=="Q_EN") allfilesdata <- allfilesdata %>% filter(!str_detect(get(j_set), paste(c("_old", "_new", "_late"), collapse = "|")))   
          allfilesdata <- allfilesdata %>% mutate(!!j_set := gsub(paste(c("_old", "_new", "_late"), collapse = "|"), "", get(j_set))) %>% group_by_at(setdiff(names(allfilesdata), "value")) %>% summarize(value = sum(value), .groups = "drop") %>% as.data.frame()
        }}
      
      #try adding historical values
      if(historical & !(is.element(variable_name, all_items(mygdx)$sets))){allfilesdata <- add_historical_values(allfilesdata, varname=variable_name, scenplot=scenplot, check_calibration=check_calibration, verbose=F)}
      # also save as data.table
      allfilesdata <- as.data.table(allfilesdata)
      #in case separate file to more meaningful columns
      if(exists("file_separate")) allfilesdata <- filetosep(allfilesdata, type = file_separate[1], sep = file_separate[2], names = file_separate[-c(1,2)])
      if(("t" %in% names(allfilesdata)) & (!any(str_detect(allfilesdata$t, "_")))) allfilesdata$t <- as.numeric(allfilesdata$t)
      if(results=="assign") assign(variable_name,allfilesdata,envir = .GlobalEnv)
      if(results=="return") return(allfilesdata)
    }else{print(str_glue("Element {variable_name} not found in any GDX file."))}
  }else{
    if(results=="return") return(get(variable_name))
  }
}




#Regional or global line plots of already loaded data
witch_regional_line_plot <- function(data, varname="value", regions="World", scenplot=scenlist, ylab=varname, ylim0=FALSE, conv_factor=1, nagg="sum", rm.NA = T){
  line_size = 1.5;
  data <- subset(data, file %in% c(scenplot, "historical") & ttoyear(t) <= yearmax & ttoyear(t) >= yearmin)
  if(rm.NA) data <- subset(data, !is.na(get(varname)))
  if(regions[1]=="World"){
    if(nagg=="sum"){data <- data %>% group_by(pathdir, file, t) %>% summarise_at(., .vars=vars(-n), funs(sum)) %>% mutate(n="World")}else
    if(nagg=="mean"){data <- data %>% group_by(pathdir, file, t) %>% summarise_at(., .vars=vars(-n), funs(mean)) %>% mutate(n="World")}
    }else{data <- data %>% filter(n %in% regions)}
  data <- data %>% mutate(plot_value=!!parse_quo(varname, env = .GlobalEnv)*conv_factor)
  p <- ggplot() + xlab("") +ylab(ylab)
  if(ylim0) p <- p + ylim(0, NA)
  if(regions[1]=="World" | length(regions)==1){
    p <- p + geom_line(data = data %>% filter(file!="historical"), aes(ttoyear(t),plot_value,colour=file), stat="identity", size=line_size) + geom_line(data = data %>% filter(file=="historical"), aes(ttoyear(t),plot_value,colour=file), stat="identity", size=0.5)
  }else{
    p <- p + geom_line(data = data %>% filter(file!="historical"), aes(ttoyear(t),plot_value,colour=n, linetype=file), stat="identity", size=line_size) + scale_colour_manual(values = region_palette) + geom_line(data = data %>% filter(file=="historical"), aes(ttoyear(t),plot_value,colour=n, linetype=file), stat="identity", size=0.5)
  }
  if(length(fullpathdir)!=1){p <- p + facet_grid(pathdir ~ .)}
  return(p)
}





#get all the WITCH variables
get_witch_variable <- function(variable_name, variable_name_save=variable_name, additional_set="na", additional_set_id="na", convert=1, unit="", aggregation="regional", plot=TRUE, bar="", bar_x="time", bar_y="value", bar_setvalues="", bar_colors="", regions=witch_regions, scenplot=scenlist){
  #aggregation=none: no graph is created no aggregation performed, just loads the element
  #some default values, maybe not even needed to customize
  #removepattern="results_"
  #ssp_grid = FALSE
  #DEBUG:
  #variable_name="Q_OUT"; variable_name_save=variable_name; additional_set="f"; additional_set_id="oil"; convert=1; unit=""; aggregation="regional"; cumulative=FALSE; plot=TRUE; bar=""; bar_x="time"; bar_y="value"; bar_setvalues=""; bar_colors=""; regions=witch_regions; scenplot=scenlist; variable_field="l"; current_pathdir = fullpathdir[1]; file <- filelist[1];
  line_size = 1.5;
  show_legend_title = F
  if(additional_set_id=="all"){plot=FALSE}
  line_colour = "file"; line_type="pathdir"; #defaults for colour and linetype aesthetics
  variable_name_save=as.character(gsub("_", " ", variable_name_save))
  
  #CALL MAIN READING FUNCTION
  allfilesdata <- get_witch_simple(variable_name, results = "return")
  
  if(exists("allfilesdata")){
  
    #clean data, edit sets, convert, replace NAs
    allfilesdata <- subset(allfilesdata, file %in% scenplot)
    #unit conversion
    unit_conversion <- unit_conversion(variable_name, unit, convert)
  
    #get time frame needed
    if(("t" %in% colnames(allfilesdata)) & !(variable_name=="t")) allfilesdata <- subset(allfilesdata, allfilesdata$t <= yeartot(yearmax) & allfilesdata$t >= yeartot(yearmin))
    
    #get reporting items to same structure
    if(any(colnames(allfilesdata)=="nrep"))
    {
      setnames(allfilesdata, "nrep", "n")
      allfilesdata <- allfilesdata[allfilesdata$n %in% regions]
    }
    
    if (additional_set != "na" & additional_set_id != "all" & additional_set_id != "sum") 
    {
      allfilesdata <- subset(allfilesdata, tolower(get(additional_set))==as.character(additional_set_id))
      #finally remove column containing the only remaining set since values are in "values" 
      allfilesdata <- subset(allfilesdata, select=-get(additional_set))
    }
    if (additional_set != "na" & additional_set_id == "sum")   #sum over the set if "sum" is choosen
    {
      if(length(fullpathdir)>=1){allfilesdata <- aggregate(value~n+t+file+pathdir, data=allfilesdata, sum)}
      else{allfilesdata <- aggregate(value~n+t+file, data=allfilesdata, sum, na.rm=TRUE)}
      allfilesdata <- as.data.frame(allfilesdata)
      allfilesdata <- as.data.table(allfilesdata)
    }    
    allfilesdata[is.na(allfilesdata)] <- 0
    allfilesdata$value = allfilesdata$value * unit_conversion$convert 
    #assign("test",allfilesdata,envir = .GlobalEnv)
    
    
    allfilesdata$n <- as.factor(allfilesdata$n)
    #if(additional_set!="na"){allfilesdata[[additional_set]] <- as.factor(allfilesdata[[additional_set]])}
    #print(str(allfilesdata)); assign("test",allfilesdata,envir = .GlobalEnv)
    

    
    
    
    #Plot for each variable
    if (aggregation == "global_sum")
    {
      allfilesdata$n <- NULL
      if(length(fullpathdir)>=1){allfilesdata <- aggregate(value~t+file+pathdir, data=allfilesdata, sum)}
      else{allfilesdata <- aggregate(value~t+file, data=allfilesdata, sum)}
      #print(str(allfilesdata)); assign("test",allfilesdata,envir = .GlobalEnv)
      allfilesdata <- as.data.table(allfilesdata)
      #if(ssp_grid){allfilesdata$ssp <- str_extract(allfilesdata$file, "ssp[1-5]")}
      #try for RCP:
      if(ssp_grid){allfilesdata <- ssptriple(allfilesdata); line_colour = "rcp"; line_type="spa"}
      p <- ggplot(data=subset(allfilesdata),aes(ttoyear(t),value, colour=get(line_colour), linetype=get(line_type))) + geom_line(stat="identity", size=line_size) + xlab("year") +ylab(unit_conversion$unit)
      if(show_legend_title){p <- p + labs(linetype=line_type, colour=line_colour)}else{p <- p + theme(legend.title=element_blank())} 
      if(show_numbers_2100){p <- p + geom_text(data=subset(allfilesdata, t==20), aes(x=2100, y=value, label=round(value, 2)))}
      if(ssp_grid){p <- p + facet_grid(. ~ ssp)}
      if(length(fullpathdir)!=1){p <- p + facet_grid(pathdir ~ .)}
      if(length(fullpathdir)!=1 & ssp_grid){p <- p + facet_grid(pathdir ~ ssp)}
      if(length(fullpathdir)==1){p <- p + guides(linetype=FALSE)}
      if(plot){saveplot(variable_name_save, plotdata=subset(allfilesdata))}
    } 
    if (aggregation == "global_mean")
    {
      allfilesdata$n <- NULL      
  allfilesdata <- allfilesdata %>% group_by_at(c("pathdir", file_group_columns, "t")) %>% summarize(value=mean(value), .groups = "drop")
      if(ssp_grid){allfilesdata <- ssptriple(allfilesdata); line_colour = "rcp"; line_type="spa"}
      p <- ggplot(data=subset(allfilesdata),aes(ttoyear(t),value, colour=get(line_colour), linetype=get(line_type))) + geom_line(stat="identity", size=line_size) + xlab("year") +ylab(unit_conversion$unit) + labs(linetype=line_type, colour=line_colour)
      if(show_numbers_2100){p <- p + geom_text(data=subset(allfilesdata, t==20), aes(x=2100, y=value, label=round(value, 2)))}
      if(ssp_grid){p <- p + facet_grid(. ~ ssp)}
      if(length(fullpathdir)!=1){p <- p + facet_grid(pathdir ~ .)}
      if(length(fullpathdir)!=1 & ssp_grid){p <- p + facet_grid(pathdir ~ ssp)}   
      if(length(fullpathdir)==1){p <- p + guides(linetype=FALSE)}
      if(plot){saveplot(variable_name_save, plotdata=subset(allfilesdata))}
    } 
    if (aggregation == "regional") 
    {
      if(ssp_grid){allfilesdata$ssp <- str_extract(allfilesdata$file, "ssp[1-5]")}
      # print(str(allfilesdata))
      # assign("test", allfilesdata)
      p <- ggplot(subset(allfilesdata, n %in% regions),aes(ttoyear(t),value,colour=n, linetype=file)) + geom_line(stat="identity", size=line_size) + xlab("year") +ylab(unit_conversion$unit) + scale_colour_manual(values = region_palette)
      if(ssp_grid){p <- p + facet_grid(. ~ ssp)}
      if(length(fullpathdir)!=1){p <- p + facet_grid(pathdir ~ .)}
      if(length(fullpathdir)!=1 & ssp_grid){p <- p + facet_grid(pathdir ~ ssp)}   
      if(plot){saveplot(variable_name_save, plotdata=subset(allfilesdata, n %in% regions))}
    }
    if (aggregation == "all") 
    {
      allfilesdata <- allfilesdata
    }
    if (bar != "")  #area or bar, share or values
    {
      allfilesdata <- subset(allfilesdata, t%%2==0) #now also take only 10 year steps
      
      if(bar=="set"){
        if(bar_setvalues[1] != ""){allfilesdata <- subset(allfilesdata, get(additional_set) %in% bar_setvalues)}
        if(additional_set!="na"){allfilesdata[[additional_set]] <- as.factor(allfilesdata[[additional_set]])}
        if(length(fullpathdir)>=1){allfilesdata <- allfilesdata[, lapply(.SD, sum), by=c("t", "file", additional_set, "pathdir")]}else{allfilesdata <- allfilesdata[, lapply(.SD, sum), by=c("t", "file", additional_set)]}
        allfilesdata$n <- NULL
        if(additional_set!="na"){allfilesdata[[additional_set]] <- as.factor(allfilesdata[[additional_set]])}
        if(bar_setvalues[1] != ""){allfilesdata[[additional_set]] <- reorder.factor(allfilesdata[[additional_set]], new.order=bar_setvalues)}   #to keep order from setlist in function call
        if(bar_y=="share"){if(length(fullpathdir)!=1){allfilesdata <- plyr::ddply(allfilesdata, c("t", file_group_columns, "pathdir"), transform, value=value/(sum(value))*100)}else{allfilesdata <- plyr::ddply(allfilesdata, c("t", "file"), transform, value=value/(sum(value))*100)}}
        if(str_detect(bar_x, "time")){
          if(!is.na(destring(bar_x))){allfilesdata <- subset(allfilesdata, t==yeartot(destring(bar_x)))}
          p <- ggplot(data=subset(allfilesdata),aes(ttoyear(t),value, fill=get(additional_set))) + geom_bar(stat="identity") + xlab("year") + facet_grid( ~ file) + guides(fill=guide_legend(title=NULL)) 
          if(bar_colors[1]!=""){p <- p + scale_fill_manual(values=bar_colors)}  
        }else{
          p <- ggplot(data=subset(allfilesdata, t==yeartot(destring(bar_x))),aes(file,value, fill=get(additional_set))) + geom_bar(stat="identity") + xlab("scenario") + guides(fill=guide_legend(title=NULL)) 
          if(bar_colors[1]!=""){p <- p  + scale_fill_manual(values=bar_colors)}            
        }
      }
      if(bar=="region"){
        allfilesdata[["n"]] <- reorder.factor(allfilesdata[["n"]], new.order=regions)   #to keep order from setlist in function call
        if(bar_y=="share"){if(length(fullpathdir)!=1){allfilesdata <- plyr::ddply(allfilesdata, c("t", file_group_columns, "pathdir"), transform, value=value/(sum(value))*100)}else{allfilesdata <- plyr::ddply(allfilesdata, c("t", "file"), transform, value=value/(sum(value))*100)}}
        if(str_detect(bar_x, "time")){
          if(!is.na(destring(bar_x))){allfilesdata <- subset(allfilesdata, t==yeartot(destring(bar_x)))}
          p <- ggplot(data=subset(allfilesdata),aes(ttoyear(t),value, fill=n)) + geom_bar(stat="identity") + xlab("year") + facet_grid( ~ file) + guides(fill=guide_legend(title=NULL)) + scale_fill_manual(values=region_palette)
        }else{
          p <- ggplot(data=subset(allfilesdata, t==yeartot(destring(bar_x))),aes(file,value, fill=n)) + geom_bar(stat="identity") + xlab("scenario") + guides(fill=guide_legend(title=NULL)) + scale_fill_manual(values=region_palette)          
        }
      }
      if(length(fullpathdir)!=1){p <- p + facet_grid(pathdir ~ file)}
      legend_position_old = legend_position; assign("legend_position", "bottom", envir = .GlobalEnv);  
	  if(plot){saveplot(variable_name_save, plotdata=subset(allfilesdata))}; assign("legend_position", legend_position_old, envir = .GlobalEnv) 
    } 
    #save the variable under the WITCH name in the global environment
    if(additional_set_id!="na") variable_name <- paste0(variable_name, "_", additional_set_id)
    assign(variable_name,allfilesdata,envir = .GlobalEnv)
  }
  #END OF THE STANDARD GRAPHS AND DATA READ LOOP    
}




getvar_witchhist <- function(varname, unit_conversion=1, hist_varname=varname, additional_sets=NA, ylab=varname){
  #additional _sets: e.g., c("iq"="y", "e="co2")
  tempvar <- get_witch_simple(varname, results = "return")
  n_model <- unique(tempvar$n)
  if(!is.na(additional_sets)){
    for(s in 1:length(additional_sets)) tempvar[[names(additional_sets[s])]] <- additional_sets[s]
  }
  tempvar$value <- tempvar$value * unit_conversion;
  tempvar <- add_historical_values(tempvar, varname = hist_varname, check_calibration = T)
  tempvar <- tempvar %>% filter(n %in% n_model)
  print(ggplot(tempvar) + geom_line(aes(ttoyear(t), value, color=file, linetype=n)) + xlab("") + ylab(ylab))
  assign(varname, tempvar, envir = .GlobalEnv)
}


#Function to create a snapshot and setup the data for gdxompaR witch-online to be self-containted and deployed e.g. through shinyapps.io
create_witch_online <- function(list_of_variables=c("Q", "Q_EN", "Q_FUEL", "Q_OUT", "Q_EMI", "K", "K_EN", "I_EN", "I", "I_RD", "MCOST_INV", "COST_EMI", "MCOST_EMI", "CPRICE", "MCOST_FUEL", "TEMP", "TRF", "OMEGA", "Q_IN", "ykali", "tpes", "carbonprice", "emi_cap", "l"), deploy = F) {
  #preload all variables (execult eht followig lines separately before deploying)
  aux_vars <- c("ghg", "csi", "allerr", "allinfoiter", "all_optimal", "all_feasible", "price_iter")
  lapply(c(aux_vars, list_of_variables), get_witch_simple)
  if(file.exists("gdxcompaR//witch-online//allvariables.Rdata")) file.remove("gdxcompaR//witch-online//allvariables.Rdata")
  assign("deploy_online", TRUE, envir = .GlobalEnv)
  save.image(file="gdxcompaR//witch-online//allvariables.Rdata")
  #deploy app
  if(deploy){
    library(rsconnect)
    deployApp(appDir = "gdxcompaR/witch-online/")
  }
}

