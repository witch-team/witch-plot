#Plots for WITCH runs with climate damages or climate_datas

climate_plot <- function(scenplot=scenlist, regions = "World"){
  emi_sum <- "ghg" #or "co2" for only CO2 or ghg for all gases
  
  get_witch_simple("Q_EMI")
  get_witch_simple("ghg") # to get GHGs for non-co2 sets
  if(emi_sum=="ghg") ghg_used <- unique(ghg$e) else if(emi_sum=="co2") ghg_used = c("co2")
  Q_EMI <- Q_EMI %>% filter(e %in% ghg_used) %>% group_by(pathdir, file, n, t) %>% summarize(emiall = sum(value)) %>% filter(t %in% t_model)
  #get also BAU values
  get_witch_simple("BAU_Q_EMI")
  BAU_Q_EMI <- BAU_Q_EMI %>% filter(e %in% ghg_used) %>% group_by(pathdir, file, n, t) %>% summarize(emiall = sum(value)) %>% filter(t %in% t_model)
  climate_data <- Q_EMI %>% rename(emi=emiall)
  climate_data <- merge(climate_data, BAU_Q_EMI, by = c("pathdir", "file", "n", "t")); setnames(climate_data, "emiall", "emi_bau")
  
  get_witch_simple("TRF")
  climate_data <- merge(climate_data, TRF, by = c("pathdir", "file", "n", "t")); setnames(climate_data, "value", "trf")
  #add external climate modules in case
  get_witch_simple("MAGICCTRF")
  if(exists("MAGICCTEMP")) {climate_data <- merge(climate_data, MAGICCTRF, by = c("pathdir", "file", "n", "t"), all.x = T); setnames(climate_data, "value", "trf_magicc6")}
  
  get_witch_simple("TEMP")
  climate_data <- merge(climate_data, TEMP %>% filter(m=="atm") %>% select(-m), by = c("pathdir", "file", "n", "t")); setnames(climate_data, "value", "temp")
  #add external climate modules in case
  get_witch_simple("MAGICCTEMP")
  if(exists("MAGICCTEMP")) {climate_data <- merge(climate_data, MAGICCTEMP %>% filter(m=="atm") %>% select(-m), by = c("pathdir", "file", "n", "t"), all.x = T); setnames(climate_data, "value", "temp_magicc6")}
  
  #PLOTS:
  #GHG Emissions
  emi_plot <- witch_regional_line_plot(climate_data, varname = "emi", regions = regions, scenplot = scenplot_nopulse, ylab = "GHG Emissions [GtCO2eq]", conv_factor=44/12)
 
  #Radiative Forcing
  trf_plot <- witch_regional_line_plot(climate_data, varname = "trf", scenplot = scenplot, regions = "World", ylab = "Total Radiative Forcing [W/m2] (WITCH)", conv_factor=1, nagg="mean")
  trf_plot_magicc <- witch_regional_line_plot(climate_data, varname = "trf_magicc6", scenplot = scenplot, regions = "World", ylab = "Total Radiative Forcing [W/m2] (MAGICC6)", conv_factor=1, nagg="mean")

  #Temperature
  temp_plot <- witch_regional_line_plot(climate_data, varname = "temp", scenplot = scenplot, regions = "World", ylab = "Temperature increase [deg C] (WITCH)", conv_factor=1, nagg="mean")
  temp_plot_magicc <- witch_regional_line_plot(climate_data, varname = "temp_magicc6", scenplot = scenplot, regions = "World", ylab = "Temperature increase [deg C] (MAGICC6)", conv_factor=1, nagg="mean")
  
  
  print(ggarrange(emi_plot, NULL, trf_plot, trf_plot_magicc, temp_plot, temp_plot_magicc, ncol = 2, nrow=3, common.legend = T, legend = "bottom"))
  
}

