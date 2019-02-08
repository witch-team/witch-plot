#Plots for WITCH runs with climate damages or climate_datas

climate_plot <- function(scenplot=scenlist, regions = "World"){
  get_witch_simple("Q_EMI", check_calibration = T)
  get_witch_simple("ghg") # to get GHGs for non-co2 sets
  ghgs <- unique(ghg$e)
  Q_EMI <- Q_EMI %>% filter(e %in% ghgs) %>% group_by(pathdir, file, n, t) %>% summarize(emiall = sum(value), emico2=sum(value[e=="co2"]))
  #get also BAU values
  get_witch_simple("BAU_Q_EMI", check_calibration = T)
  BAU_Q_EMI <- BAU_Q_EMI %>% filter(e %in% ghgs) %>% group_by(pathdir, file, n, t) %>% summarize(emi_bau = sum(value), co2_bau=sum(value[e=="co2"]))
  climate_data <- Q_EMI %>% rename(emi=emiall, co2=emico2)
  climate_data <- merge(climate_data, BAU_Q_EMI, by = c("pathdir", "file", "n", "t"), all = T)
  
  get_witch_simple("TRF", check_calibration = T)
  climate_data <- merge(climate_data, TRF, by = c("pathdir", "file", "n", "t"), all = T); setnames(climate_data, "value", "trf")
  #add external climate modules in case
  get_witch_simple("MAGICCTRF", check_calibration = T)
  if(exists("MAGICCTRF")) {climate_data <- merge(climate_data, MAGICCTRF, by = c("pathdir", "file", "n", "t"), all = T); setnames(climate_data, "value", "trf_magicc6")}
  
  get_witch_simple("TEMP", check_calibration = T)
  climate_data <- merge(climate_data, TEMP %>% filter(m=="atm") %>% select(-m), by = c("pathdir", "file", "n", "t"), all = T); setnames(climate_data, "value", "temp")
  #add external climate modules in case
  get_witch_simple("MAGICCTEMP", check_calibration = T)
  if(exists("MAGICCTEMP")) {climate_data <- merge(climate_data, MAGICCTEMP %>% filter(m=="atm") %>% select(-m), by = c("pathdir", "file", "n", "t"), all = T); setnames(climate_data, "value", "temp_magicc6")}
  
  #PLOTS:
  #CO2 emissions
  co2_plot <- witch_regional_line_plot(climate_data, varname = "co2", regions = regions, scenplot = scenplot, ylab = "GHG Emissions [GtCO2eq]", conv_factor=44/12)
  #GHG Emissions
  emi_plot <- witch_regional_line_plot(climate_data, varname = "emi", regions = regions, scenplot = scenplot, ylab = "GHG Emissions [GtCO2eq]", conv_factor=44/12)
 
  #Radiative Forcing
  trf_plot <- witch_regional_line_plot(climate_data, varname = "trf", scenplot = scenplot, regions = "World", ylab = "Total Radiative Forcing [W/m2] (WITCH)", conv_factor=1, nagg="mean")
  trf_plot_magicc <- witch_regional_line_plot(climate_data, varname = "trf_magicc6", scenplot = scenplot, regions = "World", ylab = "Total Radiative Forcing [W/m2] (MAGICC6)", conv_factor=1, nagg="mean")

  #Temperature
  temp_plot <- witch_regional_line_plot(climate_data, varname = "temp", scenplot = scenplot, regions = "World", ylab = "Temp. increase [degC] (WITCH)", conv_factor=1, nagg="mean")
  temp_plot_magicc <- witch_regional_line_plot(climate_data, varname = "temp_magicc6", scenplot = scenplot, regions = "World", ylab = "Temp. increase [degC] (MAGICC6)", conv_factor=1, nagg="mean")
  
  
  ymax_emi = max(ggplot_build(emi_plot)$layout$panel_scales_y[[1]]$range$range[2], ggplot_build(co2_plot)$layout$panel_scales_y[[1]]$range$range[2])
  ymax_trf = max(ggplot_build(trf_plot)$layout$panel_scales_y[[1]]$range$range[2], ggplot_build(trf_plot_magicc)$layout$panel_scales_y[[1]]$range$range[2])
  ymax_temp = max(ggplot_build(temp_plot)$layout$panel_scales_y[[1]]$range$range[2], ggplot_build(temp_plot_magicc)$layout$panel_scales_y[[1]]$range$range[2])
  
  print(ggarrange(co2_plot + ylim(0,ymax_emi), emi_plot + ylim(0,ymax_emi), trf_plot + ylim(0,ymax_trf), trf_plot_magicc + ylim(0,ymax_trf), temp_plot + ylim(0,ymax_temp), temp_plot_magicc + ylim(0,ymax_temp), ncol = 2, nrow=3, common.legend = T, legend = "bottom"))
  
}

