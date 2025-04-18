# Load GDX of all scenarios and basic pre-processing 
get_witch <- function(variable_name, 
                      scenplot = scenlist, 
                      check_calibration = TRUE, 
                      field = "l", 
                      postprocesssuffix = NULL, 
                      skip_restrict_regions = FALSE){
  for (current_pathdir in fullpathdir){
    for (file in filelist){
      if(file.exists(file.path(current_pathdir, paste0(file,".gdx")))){
        mygdx <- gdx(file.path(current_pathdir, paste0(file,".gdx")))
        if(!is.null(postprocesssuffix)) {
          mygdx <- gdx(file.path(current_pathdir, postprocesssuffix, 
                                 paste0(paste0(file, "_", postprocesssuffix),
                                        ".gdx")))}
        if(is.element(variable_name, all_items(mygdx)$variables) | 
           is.element(variable_name, all_items(mygdx)$parameters) | 
           is.element(variable_name, all_items(mygdx)$sets) | 
           is.element(variable_name, all_items(mygdx)$variables) | 
           is.element(variable_name, all_items(mygdx)$equations))
        {
          tempdata <- data.table(mygdx[variable_name, field = field])
          if(is.element(variable_name, all_items(mygdx)$equations)) names(tempdata)[1:2] <- c("t", "n")
          if(variable_name %in% c("E", "EIND", "MIU", "ABATEDEMI", "ABATECOST") & !("ghg" %in% names(tempdata))) tempdata$ghg <- "co2" #just for RICE as definition had changed in 2.5.0
          if(!("n" %in% names(tempdata))) tempdata$n <- "World"
          tempdata$file <- as.character(file)
          if(length(fullpathdir)>=1){
            tempdata$pathdir <- basename(current_pathdir)
          }
          if(!exists("allfilesdata")) {
            allfilesdata <- tempdata
          } else {
            allfilesdata <- rbind(allfilesdata,tempdata)
          }
          remove(tempdata)
        }
      }
    }
  }
  if(exists("allfilesdata")){
    allfilesdata <- allfilesdata %>% mutate(file = dplyr::recode(file, !!!scenlist))
    allfilesdata$file <- factor(allfilesdata$file, levels = scenlist) # SLOW (450) to ensure ordering in the order of scenarios in scenlist
    if(str_detect(variable_name, "eq")) {
      colnames(allfilesdata) <- gsub("V1", "t", colnames(allfilesdata)) 
      colnames(allfilesdata) <- gsub("V2", "n", colnames(allfilesdata))
    }
    allfilesdata <- subset(allfilesdata, file %in% scenplot)
    if(("t" %in% colnames(allfilesdata)) & !(variable_name=="t")){
      #check if stochastic and if so convert "branch" to "file" element
      if(length(stochastic_files)>0){
        #first add branches BEFORE bifurcation for each branch
        allfilesdata_pre_bifurk <- allfilesdata %>% filter(!str_detect(t, "_")) %>% left_join(stochastic_files, by = "file") %>% filter(!is.na(num_branches)) %>% group_by(t, n, file, pathdir) %>%
          slice(rep(1:n(), num_branches)) %>% mutate(t=paste0(t,"_",row_number())) %>% select(-num_branches) %>% ungroup()
        #remove the string after "_" from t and addto file  after a parenthesis
        allfilesdata <- bind_rows(allfilesdata_pre_bifurk, allfilesdata %>% filter(!(file %in% stochastic_files$file & !str_detect(t, "_")))) %>% mutate(
          file = ifelse(str_detect(t, "_"), paste0(file, "(b", sub(".*_(\\w+)$", "\\1", t), ")"), as.character(file)),
          t = sub("_(\\w+)$", "", t))
        }
      #since t is character in gams convert to numeric fastest way
      allfilesdata <- allfilesdata %>% mutate(t=as.numeric(as.character(t)))
      }
    if(exists("display_regions"))  allfilesdata <- allfilesdata %>% mutate(n = dplyr::recode(n, !!!setNames(witch_regions, display_regions))) #map n to display regions
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
    if(historical & !(is.element(variable_name, all_items(mygdx)$sets))) {
      allfilesdata <- add_historical_values(allfilesdata, 
                                            varname = variable_name, 
                                            scenplot = scenplot, 
                                            check_calibration = check_calibration, 
                                            verbose = FALSE)}
    # also save as data.table
    allfilesdata <- as.data.table(allfilesdata)
    #in case nice_region_names exist map region names for those with a nice name
    if(exists("nice_region_names") & !unique(allfilesdata$n)[1]=="World") allfilesdata <- allfilesdata %>% mutate(n = dplyr::recode(n, !!!nice_region_names))
    #in case restrict_regions exists keep only these regions
    if(exists("restrict_regions") & !skip_restrict_regions & !unique(allfilesdata$n)[1]=="World") allfilesdata <- subset(allfilesdata, n %in% restrict_regions)
    #in case separate file to more meaningful columns
    if(exists("file_separate")){
      allfilesdata <- filetosep(as.data.frame(allfilesdata), type = file_separate[1], sep = file_separate[2], names = file_separate[-c(1,2)])
      for(sep in unname(file_separate[3:length(file_separate)])) allfilesdata[[sep]] <- gsub(sep, "", allfilesdata[[sep]])
    }
    
    return(allfilesdata)
  }else{print(str_glue("Element {variable_name} was not found in any GDX file."));return(data.frame())}
}

#use memoise for get_witch function
get_witch <- memoise(get_witch)
