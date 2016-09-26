# Add data based on GLOBIOM based on the carbon and biomass prices found in WITCH

get_globiom_variables <- function(regions = witch_regions, varplot="ForestCover", varname="Forest Cover", varunit="%", scenplot=scenlist){
  get_witch_simple("lu_wbp_class")
  get_witch_simple("lu_cbp_class")
  #get globiom SSP2 data
  globiom_data <- read.table(unz("datasets/globiom_data_witch_ssp2.csv.zip", "globiom_data_witch_ssp2.csv"), header=T, quote="\"", sep=",", check.names = FALSE)
  globiom_data$region <- tolower(globiom_data$region)
  globiom_data <- subset(globiom_data, region!="World" & region!="world")
  
  #list of GLOBIOM variables:
  print(unique(globiom_data$variable))
  #print(unique(globiom_data$unit))
  #Land use: TotalLnd = CrpLnd + PltFor + GrsLnd + OldFor_G4M + NewFor_G4M + OtherLnd
  globiom_data$"1990" <- NULL
  globiom_data[is.na(globiom_data)] <- 0  # it seems NA in many cases measn zeros (needed to get sums or shares right)
  
  globiom_variable_witch <- lu_wbp_class
  setnames(globiom_variable_witch, "value", "lu_wbp_class")
  globiom_variable_witch <- cbind(globiom_variable_witch, lu_cbp_class$value)
  setnames(globiom_variable_witch, "V2", "lu_cbp_class")
  
  globiom_data$lu_cbp_class <- as.numeric(globiom_data$ghg_price)
  globiom_data$lu_wbp_class <- as.numeric(globiom_data$bio_price)
  globiom_data <- subset(globiom_data, select=-c(ssp,scen,unit,bio_price, ghg_price))
  
  #adjust years
  globiom_variable_witch[t==1]$t <- 0  #2000 instead of 2005 for first year
  globiom_variable_witch <- subset(globiom_variable_witch, t <= 20 & t %% 2 == 0) # ten year time steps until 2100
  
  globiom_data_long <- melt(globiom_data, id.vars = c("region", "lu_cbp_class", "lu_wbp_class", "variable"), variable.name = "t")
  globiom_data_long$t <- yeartot(globiom_data_long$t)
  setnames(globiom_data_long, "region", "n")
  
  globiom_all_variables_witch <- merge(globiom_variable_witch, globiom_data_long, by = c("t", "n", "lu_wbp_class", "lu_cbp_class"))
  globiom_all_variables_witch$lu_wbp_class <- NULL; globiom_all_variables_witch$lu_cbp_class <- NULL
  
  globiom_all_variables_witch_wide <- dcast(globiom_all_variables_witch, pathdir + file + n + t ~ variable, value.var = "value")
  
  #Forest coverage
  globiom_all_variables_witch_wide$ForestCover = 100*(globiom_all_variables_witch_wide$PltFor+globiom_all_variables_witch_wide$OldFor_G4M+globiom_all_variables_witch_wide$NewFor_G4M)/globiom_all_variables_witch_wide$TotalLnd
  
  #add Indonesia: 2014: 50.2% forest cover, easia in globiom 36%
  indo <- subset(globiom_all_variables_witch_wide, n=="easia")
  indo$n <- "indonesia"
  indo$ForestCover <- indo$ForestCover* (50.2/36)
  globiom_all_variables_witch_wide <- rbind(globiom_all_variables_witch_wide, indo)
  #indo: 1.9Mio km2, SEA: 4.5mio km2
  globiom_all_variables_witch_wide <- as.data.table(globiom_all_variables_witch_wide)
  globiom_all_variables_witch_wide[n=="easia"]$ForestCover <- globiom_all_variables_witch_wide[n=="easia"]$ForestCover * (36*4.5-50*1.9)/(2.6*36)
  
  globiom_all_variables_witch <- melt(globiom_all_variables_witch_wide, id.vars = c("pathdir", "file", "n", "t"))
  
  ggplot(subset(globiom_all_variables_witch, n %in% regions & variable==varplot & file %in% scenplot)) + geom_line(aes(ttoyear(t), value, colour=n, linetype=file)) + xlab("") + ylab(paste0(varname, " [", varunit, "]")) 
  saveplot(varname, plotdata = subset(globiom_all_variables_witch, n %in% regions & variable==varplot & file %in% scenplot))  
  
}
