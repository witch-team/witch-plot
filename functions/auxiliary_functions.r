#Auxiliary Functions




ttoyear <- function(t){year=(as.numeric(t) * 5 + 2000); return(year);}
yeartot <- function(year){t=(as.numeric(as.character(year)) - 2000) / 5; return(t);}



saveplot <- function(plotname, width=7, height=5, text_size=10, plotdata=NULL, suffix="", transparent=FALSE, add_title=TRUE){
  if(figure_format!="png"){transparent = FALSE}
  if(figure_format=="pdf"){plot_device=cairo_pdf}else{plot_device=figure_format}
  #device=cairo_pdf makes PDFs work with greek symbols etc.
  if("t" %in% colnames(plotdata)){plotdata$t <- ttoyear(plotdata$t)}
  if(!exists("legend_position")){legend_position = "bottom"}
  if(legend_position=="bottom"){legend_direction="horizontal"}else{legend_direction="vertical"}
  if(transparent){transparent_background <- theme(legend.background = element_blank(), panel.background = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA))}else{transparent_background = NULL}
  print(last_plot() + if(add_title){labs(title=plotname)}else{labs(title="")} + theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size=text_size), legend.position=legend_position, legend.direction = legend_direction, legend.key = element_rect(colour = NA), legend.title=element_blank()) + transparent_background); 
  ggsave(filename=paste0(graphdir,as.character(gsub(" ", "_", plotname)),suffix,".",figure_format), plot = last_plot() + if(add_title){labs(title=plotname)}else{labs(title="")} + theme(text = element_text(size=text_size), legend.position=legend_position, legend.direction = legend_direction, legend.key = element_rect(colour = NA), legend.title=element_blank()), width=width, height=height, bg = "transparent", device = plot_device)
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




unit_conversion <- function(variable_name, unit="", convert=1){
  #if unit is not "", keep its unit and convert using convert factor
  if(unit!=""){
    unit_plot <- unit; unit_conversion <- convert
  }else{
  #automatic unit and conversion factor
  mygdx <- gdx(paste(pathdir[1], filelist[1],".gdx",sep=""))
  if(variable_name %in% mygdx$variables$name){variable_description <- mygdx$variables$text[match(variable_name, mygdx$variables$name)]}
  if(variable_name %in% mygdx$parameters$name){variable_description <- mygdx$parameters$text[match(variable_name, mygdx$parameters$name)]}
  unit_witch <- gsub(".*\\[(.*).*", "\\1", sub(" *\\].*", "", variable_description))
  if(is.na(unit_witch) | unit_witch==""){unit_witch="na"}
  unit_conversion_table <-"witch_unit plot_unit conversion_factor
  TWh       EJ                 0.0036
  T$        'billion USD'      1e3
  T$/TWh    $/GJ               277777.777778
  GtCe      GtCO2              3.67
  TW        GW                 1e3
  T$/GTon   $/tCO2             272.727272727
  T$/GTonC  $/tCO2             272.727272727
  T$/GtCeq  $/tCO2             272.727272727
  GTonC     GtCO2              3.67
  GtonC     GtCO2              3.67
  GtCe      GtCO2              3.67
  na        na                 1   
  'deg C above preindustrial levels'    °C    1
  "
  unit_conversion_table <- read.table(textConnection(unit_conversion_table), sep="", head=T, dec=".")
  unit_plot = unit_witch;unit_conversion=1 #by default, keep original
  if(!is.na(match(unit_witch, unit_conversion_table$witch_unit))){
    unit_plot <- as.character(unit_conversion_table$plot_unit[match(unit_witch, unit_conversion_table$witch_unit)])
    unit_conversion <- unit_conversion_table$conversion_factor[match(unit_witch, unit_conversion_table$witch_unit)]}
  }
  
  #Finally, for specific variables apply custom unit and conversion
  unit_conversion_user_specific <-"varname plot_unit conversion_factor
  tpes       EJ                 0.0036
  tpes_kali  EJ                 0.0036
  ei_kali    MJ/$               1     
  "
  unit_conversion_user_specific <- read.table(textConnection(unit_conversion_user_specific), sep="", head=T, dec=".")
  if(variable_name %in% unit_conversion_user_specific$varname){
    unit_plot <- unit_conversion_user_specific$plot_unit[unit_conversion_user_specific$varname==variable_name]
    unit_conversion <- unit_conversion_user_specific$conversion_factor[unit_conversion_user_specific$varname==variable_name]
  }
  
  
  #dollar deflator conversion if other base year than 2005
  usd_deflator = 1 #by default, all values in 2005 USD
  #usd_deflator = 108.686/91.987  #2014 USD
  #usd_deflator = 1.10774   #2010 USD
  if(str_detect(unit_plot, "$") | str_detect(unit_plot, "USD")){unit_conversion <- unit_conversion * usd_deflator}
  
  return(list(unit=unit_plot, convert=unit_conversion))
}



# default meta param (taken from scaling.R)
default_meta_param <- function(){
  "parameter,value
  I,sum
  K,sum
  Q,sum
  BAU_Q,sum
  COST_EN,mean
  COST_PES,mean
  I_EN,sum
  K_EN,sum
  MCOST_INV,mean
  MCOST_PES,mean
  Q_EN,sum
  Q_IN,sum
  Q_PES,sum
  SHARE_EL,mean
  COST_EMI,mean
  CUM_EMI,sum
  Q_EMI,sum
  BAU_Q_EMI,sum
  I_RD,sum
  K_RD,sum
  K_RD_F,sum
  SPILL,mean
  ABAT,sum
  Q_WBIO,sum
  Q_REDD,sum
  MCOST_EMI,mean
  I_EN_WINDOFF,sum
  I_EN_WINDON,sum
  K_EN_WINDOFF,sum
  K_EN_WINDON,sum
  Q_EN_WINDOFF,sum
  Q_EN_WINDON,sum
  I_EN_PV,sum
  I_EN_CSP,sum
  K_EN_PV,sum
  K_EN_CSP,sum
  Q_EN_PV,sum
  Q_EN_CSP,sum
  Q_EL_FLEX,sum
  K_EN_GRID,sum
  I_EN_GRID,sum
  ADDOILCAP,sum
  COST_OIL,mean
  CUM_OIL,sum
  I_OIL,sum
  I_OUT,sum
  OILCAP,sum
  OILPROD,sum
  Q_EMI_OUT,sum
  Q_OUT,sum
  RF,max
  TEMP,max
  TRF,max
  W_EMI,max
  WCUM_EMI,max
  OMEGA,max
  QEL_EDV,sum
  QEL_FR_EDV,sum
  emi_cap,sum
  ken_policy,sum
  ren_share,mean
  temp_valid_hadcrut4,mean
  ctax,mean
  CPRICE,mean
  FPRICE,mean
  " -> defmap
  dm <- fread(defmap)
  dm[,type:="nagg"]
  dm = rbind(dm,data.table(parameter=dm$parameter, type="nweight", value="gdp"))
  setcolorder(dm,c("parameter", "type", "value"))
  return(dm)
}



#function to save variable in CSV
writewitchcsv <- function(varname){		
  data <- get_witch_simple(varname)
  data$pathdir <- NULL;data$file <- NULL		
  data$t <- ttoyear(data$t)		
  data <- subset(data, t >= 1980)
  data <- dcast(data, formula = as.formula(paste(paste(setdiff(colnames(data), c("n", "value")), collapse = " + "), "~ n")))		
  write.csv(data, file=paste0(graphdir, varname, ".csv"), row.names = FALSE)		
}		





