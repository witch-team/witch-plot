#Auxiliary Functions




ttoyear <- function(t){year=(as.numeric(t) * 5 + 2000); return(year);}
yeartot <- function(year){t=(as.numeric(as.character(year)) - 2000) / 5; return(t);}



saveplot <- function(plotname, width=7, height=5, text_size=10, plotdata=NULL, suffix="", transparent=FALSE, add_title=TRUE){
  if(figure_format!="png"){transparent = FALSE}
  if("t" %in% colnames(plotdata)){plotdata$t <- ttoyear(plotdata$t)}
  if(!exists("legend_position")){legend_position = "bottom"}
  if(legend_position=="bottom"){legend_direction="horizontal"}else{legend_direction="vertical"}
  if(transparent){transparent_background <- theme(legend.background = element_blank(), panel.background = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA))}else{transparent_background = NULL}
  print(last_plot() + labs(title=if(add_title){plotname}else{""}) + theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size=text_size), legend.position=legend_position, legend.direction = legend_direction, legend.key = element_rect(colour = NA), legend.title=element_blank()) + transparent_background); 
  ggsave(filename=paste0(graphdir,as.character(gsub(" ", "_", plotname)),suffix,".",figure_format), plot = last_plot() + labs(title=if(add_title){plotname}else{""}) + theme(text = element_text(size=text_size), legend.position=legend_position, legend.direction = legend_direction, legend.key = element_rect(colour = NA), legend.title=element_blank()), width=width, height=height, bg = "transparent")
  if(!is.null(plotdata) & export_plotdata){write.xlsx(subset(plotdata, select=-pathdir), file = paste0(graphdir,as.character(gsub(" ", "_", plotname, suffix)),".xlsx"))}
}



ssptriple <- function(df) #Function converts a single "file" columns to three with SSP, RCP, SPA
{
  scenario <- df$file
  triple <- as.data.frame(matrix(0, ncol = 0, nrow = length(scenario)))
  triple$ssp=substr(scenario, 1, 4)
  triple$rcp=substr(scenario, 6, 9)
  triple$spa=substr(scenario, 11, 14)  
  triple$spa <- str_replace(triple$spa, "spa[1-5]", "spaX")
  #special cases for BAU
  if(length(triple[str_detect(triple$rcp, "bau"),1])>0){triple[str_detect(triple$rcp, "bau"),]$rcp <- "bau"}
  if(length(triple[str_detect(triple$rcp, "bau"),1])>0){triple[str_detect(triple$rcp, "bau"),]$spa <- "spa0"}
  df_new <- cbind(df, triple)
  df_new$file <- NULL
  return(df_new)
}

readkey <- function()
{
  cat ("Press [enter] to continue")
  line <- readline()
}




convert_stochastic_gdx <- function(allfilesdata){
  for(.file in unique(allfilesdata$file)){
    tempstochdata <- subset(allfilesdata, file==.file)
    if('10_1' %in% tempstochdata$t){
      tempstochdata_before_resolution <- subset(tempstochdata, !grepl("_", t))
      tempstochdata <- subset(tempstochdata, grepl("_", t))
      tempstochdata$file <- paste0(.file, "(b",str_sub(tempstochdata$t, -1),")")
      branches <- unique(str_sub(tempstochdata$t, -1))
      tempstochdata$t <- str_sub(tempstochdata$t, 1,2)
      for(.branch in branches){
        tempstochdata_before_resolution$file <- paste0(.file, "(b",.branch,")")
        tempstochdata <-rbind(tempstochdata,tempstochdata_before_resolution)}
    }
    if(.file==unique(allfilesdata$file)[1]){allfilesdata_stoch_converted=tempstochdata}else{allfilesdata_stoch_converted <-rbind(allfilesdata_stoch_converted,tempstochdata)}
  }
  return(allfilesdata_stoch_converted)  
}








