# Load GDX of all scenarios and basic pre-processing 
get_witch <- function(variable_name, variable_name_save=variable_name, scenplot=scenlist, check_calibration=FALSE, results="assign", force_reload=T, field = "l", postprocesssuffix=NULL, skip_restrict_regions=F){
  if(!exists(variable_name) | (variable_name %in% c("t", "n", "p", "I")) | force_reload){
    if(exists("allfilesdata", envir = .GlobalEnv)){rm(allfilesdata, envir = .GlobalEnv)}
    variable_name_save=as.character(gsub("_", " ", variable_name_save))
    for (current_pathdir in fullpathdir){
      for (file in filelist){
        if(file.exists(file.path(current_pathdir, paste0(file,".gdx")))){
          mygdx <- gdx(file.path(current_pathdir, paste0(file,".gdx")))
          if(!is.null(postprocesssuffix)) mygdx <- gdx(file.path(current_pathdir, postprocesssuffix, paste0(paste0(file, "_", postprocesssuffix),".gdx")))
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
      allfilesdata$file <- mapvalues(allfilesdata$file , from=names(scenlist), to=scenlist, warn_missing = FALSE)
      allfilesdata$file <- factor(allfilesdata$file, levels = scenlist) # to snsure ordering in the order of scenarios in scenlist
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
      if(("n" %in% colnames(allfilesdata)) & !(is.element(variable_name, all_items(mygdx)$sets))){allfilesdata$n  <- mapvalues(allfilesdata$n , from=witch_regions, to=display_regions, warn_missing = F)}else{if(!(variable_name %in% c("eu", "oecd", "eu27", "eu28", "europe"))) allfilesdata$n <- "World"}
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
      #in case nice_region_names exist map region names for those with a nice name
      if(exists("nice_region_names") & !unique(allfilesdata$n)[1]=="World") allfilesdata$n <- mapvalues(allfilesdata$n , from=names(nice_region_names), to=nice_region_names, warn_missing = FALSE)
      #in case restrict_regions exists keep only these regions
      if(exists("restrict_regions") & !skip_restrict_regions & !unique(allfilesdata$n)[1]=="World") allfilesdata <- subset(allfilesdata, n %in% restrict_regions)
      #in case separate file to more meaningful columns
      if(exists("file_separate")){
        allfilesdata <- filetosep(allfilesdata, type = file_separate[1], sep = file_separate[2], names = file_separate[-c(1,2)])
        for(sep in unname(file_separate[3:length(file_separate)])) allfilesdata[[sep]] <- gsub(sep, "", allfilesdata[[sep]])
      }
      
      if(("t" %in% names(allfilesdata)) & (!any(str_detect(allfilesdata$t, "_")))) allfilesdata$t <- as.numeric(allfilesdata$t)
      if(results=="assign") assign(variable_name,allfilesdata,envir = .GlobalEnv)
      if(results=="return") return(allfilesdata)
    }else{print(str_glue("Element {variable_name} was not found in any GDX file."))}
  }else{
    if(results=="return") return(get(variable_name))
  }
}