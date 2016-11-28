#get all the WITCH variables
get_witch_variable <- function(variable_name, variable_name_save=variable_name, additional_set="na", additional_set_id="na", convert=1, unit="", aggregation="regional", cumulative=FALSE, plot=TRUE, bar="", bar_x="time", bar_y="value", bar_setvalues="", bar_colors="", regions=witch_regions, scenplot=scenlist, variable_field="l"){
  #aggregation=none: no graph is created no aggregation performed, just loads the element
  #some default values, maybe not even needed to customize
  #removepattern="results_"
  #ssp_grid = FALSE
  line_size = 1.5;
  
  show_legend_title = F
  
  if(additional_set_id=="all"){plot=FALSE}
  line_colour = "file"; line_type="pathdir"; #defaults for colour and linetype aesthetics
  
  variable_name_save=as.character(gsub("_", " ", variable_name_save))
  for (current_pathdir in pathdir){
    for (file in filelist){
      if(file.exists(paste(current_pathdir, file,".gdx",sep=""))){
        #read data from GDX file
        mygdx <- gdx(paste(current_pathdir, file,".gdx",sep=""))
        if(is.element(variable_name, all_items(mygdx)$variables) | is.element(variable_name, all_items(mygdx)$parameters) | is.element(variable_name, all_items(mygdx)$sets) | is.element(variable_name, all_items(mygdx)$variables) | is.element(variable_name, all_items(mygdx)$equations))
        {
          tempdata <- data.table(mygdx[variable_name, field=variable_field])
          tempdata$file <- as.character(file)
          tempdata$t <- as.numeric(tempdata$t)
          tempdata$pathdir <- basename(current_pathdir)
          if(!exists("allfilesdata")){allfilesdata=tempdata}else{allfilesdata <-rbind(allfilesdata,tempdata)}
          remove(tempdata)
        }
      }
    } # close file loop
    
  } #close pathdir loop
  #if it is containted in at least one file, process data and plot
  
  if(exists("allfilesdata")){
    allfilesdata$file  <- mapvalues(allfilesdata$file , from=filelist, to=scenlist)
    allfilesdata$n  <- mapvalues(allfilesdata$n , from=witch_regions, to=display_regions)
    
    
    #TRY: adding historical values
    #assign("test",allfilesdata,envir = .GlobalEnv)
    if(historical){allfilesdata <- add_historical_values(allfilesdata, varname=variable_name, scenplot=scenplot)}
    
    #clean data, edit sets, convert, replace NAs
    #print(scenplot)
    #print(str(allfilesdata))
    allfilesdata <- subset(allfilesdata, file %in% scenplot)
    #print(str(allfilesdata))
    #if $ value or USD value, apply USD deflator
    if(str_detect(unit, "$") | str_detect(unit, "USD")){allfilesdata$value <- allfilesdata$value * usd_deflator}
  
    #get time frame needed        
    allfilesdata <- subset(allfilesdata, allfilesdata$t <= yeartot(yearmax) & allfilesdata$t >= yeartot(yearmin))
    
    #get reporting items to same structure
    if(any(colnames(allfilesdata)=="nrep"))
    {
      setnames(allfilesdata, "nrep", "n")
      allfilesdata <- allfilesdata[allfilesdata$n %in% regions]
    }
    
    if (additional_set != "na" & additional_set_id != "all" & additional_set_id != "sum") 
    {
      allfilesdata <- subset(allfilesdata, get(additional_set)==as.character(additional_set_id))
      #finally remove column containing the only remaining set since values are in "values" 
      allfilesdata <- subset(allfilesdata, select=-get(additional_set))
    }
    if (additional_set != "na" & additional_set_id == "sum")   #sum over the set if "sum" is choosen
    {
      if(length(pathdir)>=1){allfilesdata <- aggregate(value~n+t+file+pathdir, data=allfilesdata, sum)}
      else{allfilesdata <- aggregate(value~n+t+file, data=allfilesdata, sum, na.rm=TRUE)}
      allfilesdata <- as.data.frame(allfilesdata)
      allfilesdata <- as.data.table(allfilesdata)
    }    
    allfilesdata[is.na(allfilesdata)] <- 0
    allfilesdata$value = allfilesdata$value * as.numeric(convert)
    #assign("test",allfilesdata,envir = .GlobalEnv)
    
    
    allfilesdata$n <- as.factor(allfilesdata$n)
    #if(additional_set!="na"){allfilesdata[[additional_set]] <- as.factor(allfilesdata[[additional_set]])}
    #print(str(allfilesdata)); assign("test",allfilesdata,envir = .GlobalEnv)
    
    #Plot for each variable
    if (aggregation == "global_sum")
    {
      if(length(pathdir)>=1){allfilesdata <- aggregate(value~t+file+pathdir, data=allfilesdata, sum)}
      else{allfilesdata <- aggregate(value~t+file, data=allfilesdata, sum)}
      allfilesdata$n <- NULL
      #print(str(allfilesdata)); assign("test",allfilesdata,envir = .GlobalEnv)
      allfilesdata <- as.data.table(allfilesdata)
      if(cumulative)
        {
        allfilesdata <- subset(allfilesdata, round(t) == t ) #to only keep every five years to get right cumulative, if historical data
        allfilesdata <- allfilesdata[,list(value,t,value_cumulative=cumsum(value)*5-value*5),list(file, pathdir)]
        allfilesdata$value <- allfilesdata$value_cumulative 
        allfilesdata$value_cumulative <- NULL
        }
      #if(ssp_grid){allfilesdata$ssp <- str_extract(allfilesdata$file, "ssp[1-5]")}
      #try for RCP:
      if(ssp_grid){allfilesdata <- ssptriple(allfilesdata); line_colour = "rcp"; line_type="spa"}
      p <- ggplot(data=subset(allfilesdata),aes(ttoyear(t),value, colour=get(line_colour), linetype=get(line_type))) + geom_line(stat="identity", size=line_size) + xlab("year") +ylab(unit)
      if(show_legend_title){p <- p + labs(linetype=line_type, colour=line_colour)}else{p <- p + theme(legend.title=element_blank())} 
      if(show_numbers_2100){p <- p + geom_text(data=subset(allfilesdata, t==20), aes(x=2100, y=value, label=round(value, 2)))}
      if(ssp_grid){p <- p + facet_grid(. ~ ssp)}
      if(length(pathdir)!=1){p <- p + facet_grid(pathdir ~ .)}
      if(length(pathdir)!=1 & ssp_grid){p <- p + facet_grid(pathdir ~ ssp)}
      if(length(pathdir)==1){p <- p + guides(linetype=FALSE)}
      if(line2005){p <- p + geom_vline(size=0.5,aes(xintercept=2005), linetype="solid", color="grey")}
      if(plot){saveplot(variable_name_save, plotdata=subset(allfilesdata))}
    } 
    if (aggregation == "global_mean")
    {
      if(length(pathdir)>=1){allfilesdata <- allfilesdata[, lapply(.SD, mean), by=c("t", "file", "pathdir")]}
      else{allfilesdata <- allfilesdata[, lapply(.SD, mean), by=c("t", "file")]}
      allfilesdata$n <- NULL
      if(ssp_grid){allfilesdata <- ssptriple(allfilesdata); line_colour = "rcp"; line_type="spa"}
      p <- ggplot(data=subset(allfilesdata),aes(ttoyear(t),value, colour=get(line_colour), linetype=get(line_type))) + geom_line(stat="identity", size=line_size) + xlab("year") +ylab(unit) + labs(linetype=line_type, colour=line_colour)
      if(show_numbers_2100){p <- p + geom_text(data=subset(allfilesdata, t==20), aes(x=2100, y=value, label=round(value, 2)))}
      if(ssp_grid){p <- p + facet_grid(. ~ ssp)}
      if(length(pathdir)!=1){p <- p + facet_grid(pathdir ~ .)}
      if(length(pathdir)!=1 & ssp_grid){p <- p + facet_grid(pathdir ~ ssp)}   
      if(length(pathdir)==1){p <- p + guides(linetype=FALSE)}
      if(line2005){p <- p + geom_vline(size=0.5,aes(xintercept=2005), linetype="solid", color="grey")}
      if(plot){saveplot(variable_name_save, plotdata=subset(allfilesdata))}
    } 
    if (aggregation == "regional") 
    {
      if(ssp_grid){allfilesdata$ssp <- str_extract(allfilesdata$file, "ssp[1-5]")}
      # print(str(allfilesdata))
      # assign("test", allfilesdata)
      p <- ggplot(subset(allfilesdata, n %in% regions),aes(ttoyear(t),value,colour=n, linetype=file)) + geom_line(stat="identity", size=line_size) + xlab("year") +ylab(unit) + scale_colour_manual(values = region_palette)
      if(ssp_grid){p <- p + facet_grid(. ~ ssp)}
      if(length(pathdir)!=1){p <- p + facet_grid(pathdir ~ .)}
      if(length(pathdir)!=1 & ssp_grid){p <- p + facet_grid(pathdir ~ ssp)}   
      if(line2005){p <- p + geom_vline(size=0.5,aes(xintercept=2005), linetype="solid", color="grey")}
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
        if(length(pathdir)>=1){allfilesdata <- allfilesdata[, lapply(.SD, sum), by=c("t", "file", additional_set, "pathdir")]}else{allfilesdata <- allfilesdata[, lapply(.SD, sum), by=c("t", "file", additional_set)]}
        allfilesdata$n <- NULL
        if(additional_set!="na"){allfilesdata[[additional_set]] <- as.factor(allfilesdata[[additional_set]])}
        if(bar_setvalues[1] != ""){allfilesdata[[additional_set]] <- reorder.factor(allfilesdata[[additional_set]], new.order=bar_setvalues)}   #to keep order from setlist in function call
        if(bar_y=="share"){if(length(pathdir)!=1){allfilesdata <- ddply(allfilesdata, c("t", "file", "pathdir"), transform, value=value/(sum(value))*100)}else{allfilesdata <- ddply(allfilesdata, c("t", "file"), transform, value=value/(sum(value))*100)}}
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
        if(bar_y=="share"){if(length(pathdir)!=1){allfilesdata <- ddply(allfilesdata, c("t", "file", "pathdir"), transform, value=value/(sum(value))*100)}else{allfilesdata <- ddply(allfilesdata, c("t", "file"), transform, value=value/(sum(value))*100)}}
        if(str_detect(bar_x, "time")){
          if(!is.na(destring(bar_x))){allfilesdata <- subset(allfilesdata, t==yeartot(destring(bar_x)))}
          p <- ggplot(data=subset(allfilesdata),aes(ttoyear(t),value, fill=n)) + geom_bar(stat="identity") + xlab("year") + facet_grid( ~ file) + guides(fill=guide_legend(title=NULL)) + scale_fill_manual(values=region_palette)
        }else{
          p <- ggplot(data=subset(allfilesdata, t==yeartot(destring(bar_x))),aes(file,value, fill=n)) + geom_bar(stat="identity") + xlab("scenario") + guides(fill=guide_legend(title=NULL)) + scale_fill_manual(values=region_palette)          
        }
      }
      if(length(pathdir)!=1){p <- p + facet_grid(pathdir ~ file)}
      legend_position_old = legend_position; assign("legend_position", "bottom", envir = .GlobalEnv);  
	  if(plot){saveplot(variable_name_save, plotdata=subset(allfilesdata))}; assign("legend_position", legend_position_old, envir = .GlobalEnv) 
    } 
    #save the variable under the WITCH name in the global environment
    assign(variable_name,allfilesdata,envir = .GlobalEnv)
  }
  #END OF THE STANDARD GRAPHS AND DATA READ LOOP    
}

























# only load GDX and process basically

get_witch_simple <- function(variable_name, variable_name_save=variable_name, scenplot=scenlist){
  variable_name_save=as.character(gsub("_", " ", variable_name_save))
  for (current_pathdir in pathdir){
    for (file in filelist){
      if(file.exists(paste(current_pathdir, file,".gdx",sep=""))){
        mygdx <- gdx(paste(current_pathdir, file,".gdx",sep=""))
        if(is.element(variable_name, all_items(mygdx)$variables) | is.element(variable_name, all_items(mygdx)$parameters) | is.element(variable_name, all_items(mygdx)$sets) | is.element(variable_name, all_items(mygdx)$variables) | is.element(variable_name, all_items(mygdx)$equations))
        {
          tempdata <- data.table(mygdx[variable_name])
          tempdata$file <- as.character(file)
          if("t" %in% colnames(tempdata)){tempdata$t <- as.numeric(tempdata$t)}
          if(length(pathdir)>=1){tempdata$pathdir <- tempdata$pathdir <- basename(current_pathdir)}
          if(!exists("allfilesdata")){allfilesdata=tempdata}else{allfilesdata <-rbind(allfilesdata,tempdata)}
          remove(tempdata)
        }
      }
    }
  }
  
  if(exists("allfilesdata")){
    allfilesdata$file  <- mapvalues(allfilesdata$file , from=filelist, to=scenlist)
    if(!(is.element(variable_name, all_items(mygdx)$sets))){allfilesdata$n  <- mapvalues(allfilesdata$n , from=witch_regions, to=display_regions)}
    allfilesdata <- subset(allfilesdata, file %in% scenplot)
    #change column of numeric values to "value"
    #print(colnames(allfilesdata))
    #for(col in 1:length(allfilesdata)){if(is.numeric(allfilesdata[[col]])){setnames(allfilesdata, colnames(allfilesdata)[col], "value")}}
    
    #try adding historical values
    if(historical & !(is.element(variable_name, all_items(mygdx)$sets))){allfilesdata <- add_historical_values(allfilesdata, varname=variable_name, scenplot=scenplot)}
    assign(variable_name,allfilesdata,envir = .GlobalEnv)
  }
}

