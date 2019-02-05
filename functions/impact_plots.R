#Plots for WITCH runs with climate damages or impacts

SCC_plot <- function(scenplot=scenlist, regions = witch_regions, normalization_region = "World"){
  #Impacts and Damages computation
  get_witch_simple("OMEGA")
  get_witch_simple("Q")
  Q <- Q %>% filter(iq == "y") %>% select(-iq) %>% filter(t %in% t_model)
  get_witch_simple("l")
  get_witch_simple("Q_EMI")
  get_witch_simple("ghg") # to get GHGs for non-co2 sets
  ghg_used <- unique(ghg$e) #ghg_used = c("co2")
  Q_EMI <- Q_EMI %>% filter(e %in% ghg_used) %>% group_by(pathdir, file, n, t) %>% summarize(emiall = sum(value)) %>% filter(t %in% t_model)
  #get also BAU values
  get_witch_simple("BAU_Q")
  BAU_Q <- BAU_Q %>% filter(iq == "y") %>% select(-iq) %>% filter(t %in% t_model)
  get_witch_simple("BAU_Q_EMI")
  BAU_Q_EMI <- BAU_Q_EMI %>% filter(e %in% ghg_used) %>% group_by(pathdir, file, n, t) %>% summarize(emiall = sum(value)) %>% filter(t %in% t_model)
  impact <- Q %>% rename(gdp=value)
  impact <- merge(impact, BAU_Q, by = c("pathdir", "file", "n", "t")); setnames(impact, "value", "gdp_bau")
  impact <- merge(impact, Q_EMI, by = c("pathdir", "file", "n", "t")); setnames(impact, "emiall", "emi")
  impact <- merge(impact, BAU_Q_EMI, by = c("pathdir", "file", "n", "t")); setnames(impact, "emiall", "emi_bau")
  impact <- merge(impact, l, by = c("pathdir", "file", "n", "t")); setnames(impact, "value", "pop")
  
  get_witch_simple("TEMP")
  impact <- merge(impact, TEMP %>% filter(m=="atm") %>% select(-m), by = c("pathdir", "file", "n", "t")); setnames(impact, "value", "temp")
  #add external climate modules in case
  get_witch_simple("MAGICCTEMP")
  if(exists("MAGICCTEMP")) {impact <- merge(impact, MAGICCTEMP %>% filter(m=="atm") %>% select(-m), by = c("pathdir", "file", "n", "t")); setnames(impact, "value", "temp_magicc6")}
  
  
  scenplot_nopulse <- setdiff(scenplot, str_subset(scenlist, "emission_pulse"))
  #PLOTS:
  #COs Emissions
  emi_plot <- witch_regional_line_plot(impact, varname = "emi", regions = regions, scenplot = scenplot_nopulse, ylab = "Emissions [GtCO2]", conv_factor=44/12)
  #Plot of relative GDP loss
  gdp_loss_plot <- witch_regional_line_plot(impact, varname = "-(gdp/gdp_bau-1)", regions = regions, scenplot = scenplot_nopulse, ylab = "% GDP loss", conv_factor=100)
  #GDP loss per ton of CO2eq emissions
  gdp_loss_per_ton_plot <- witch_regional_line_plot(impact, varname = "((gdp_bau-gdp)/emi)", regions = regions, scenplot = scenplot_nopulse, ylab = "GDP loss in $ per tCo2eq", conv_factor=(1e3/(44/12)))
 #GDP
  gdp_plot <- witch_regional_line_plot(impact, varname = "gdp", regions = regions, scenplot = scenplot_nopulse, ylab = "GDP [billion USD]", conv_factor=1e3)
  #Temperature
  temp_plot <- witch_regional_line_plot(impact, varname = "temp", scenplot = scenplot_nopulse, regions = "World", ylab = "Temperature increase [deg C]", conv_factor=1, nagg="mean")
  
  temp_plot <- ggplot(data = impact %>% filter(file %in% scenplot & ttoyear(t) <= yearmax & ttoyear(t) >= yearmin) %>% group_by(pathdir, file, t) %>% summarise_at(., .vars=vars(str_subset(names(impact), "temp")), funs(mean))) + geom_line(aes(ttoyear(t),temp,colour=file), stat="identity", size=1.5, linetype = "solid") + xlab("") + ylab("Temperature [deg C]")
  if(exists("MAGICCTEMP")){temp_plot <- temp_plot + geom_line(aes(ttoyear(t),temp_magicc6,colour=file), stat="identity", size=1.5, linetype = "dashed") + ylab("Temp., MAGICC dashed")}
  
  
  
  
  ########### COMPUTE THE SOCIAL COST OF CARBON ####################
  t0 = 3 #time also of the pulse!!
  tmax = 30

  #SCC from marginals
  get_witch_simple("m_eqq_emi_tree")
  get_witch_simple("m_eqq_y")
  scc <- m_eqq_emi_tree %>% filter(e=="co2") %>% rename(m_emi=value)
  scc <- merge(scc, m_eqq_y, by = c("pathdir", "file", "n", "t")); setnames(scc, "value", "m_eqq_y")
  #SCC from T$/GtC to $/tCO2 
  scc <- scc %>% filter(t>= t0 & t <= tmax) %>% mutate(SCC_tn = 1e3 * (-m_emi / m_eqq_y) / (44/12)) %>% group_by(pathdir, file, n) %>% mutate(SCC_t0 = 1e3 * (-m_emi / m_eqq_y[t==t0]) / (44/12))
  if(normalization_region=="World"){
    scc <- scc %>% group_by(pathdir, file, t) %>% mutate(m_eqq_normalization = mean(m_eqq_y))
  }else{
    scc <- scc %>% group_by(pathdir, file, t) %>% mutate(m_eqq_normalization = m_eqq_y[n==normalization_region])
  }
  
  scc_value_marginals <- scc %>% filter(t==t0) %>% group_by(pathdir, file) %>% mutate(SCC_contrib_norm=1e3 * (-m_emi / m_eqq_normalization) / (44/12)) %>% summarize(SCC=sum(SCC_contrib_norm)) %>% as.data.frame()
  #print(scc_value_marginals)
  SCC_bar_chart <- ggplot(subset(scc_value_marginals, file %in% scenplot_nopulse)) + geom_bar(aes(file, SCC, fill=file), position = "dodge", stat="identity") + ylab("SCC [$/tCO2eq]") + xlab("") + geom_text(aes(file, SCC*0.9, label=paste0(round(SCC,1), "$"))) + guides(fill=FALSE) + ylim(NA, max(scc_value_marginals$SCC))
  #ggplot(subset(scc, n %in% regions & file %in% scenplot),aes(ttoyear(t),SCC_tn,colour=n, linetype=file)) + geom_line(stat="identity", size=line_size) + xlab("year") +ylab("$/tCO2eq") + scale_colour_manual(values = region_palette)
  
  #compute SCC based on emission pulse method
  SCC_bar_chart_pulse <- NULL
  if(any(str_detect(scenplot, "_emission_pulse"))){
    gamma = 0.0
    eta = 1.5
    srtp = 1.5 #%
    pulse_size_theoretical = 1e6 #in tCO2eq
    
    pulsescen = str_subset(scenplot, "_emission_pulse")
    pulsescen_no = gsub("_emission_pulse" , "", pulsescen)
    impact_pulse <- impact %>% filter(file %in% pulsescen)
    impact_pulse_no <- impact %>% filter(file %in% pulsescen_no)
    names(impact_pulse)[5:ncol(impact_pulse)] <- paste0(names(impact_pulse)[5:ncol(impact_pulse)], "_pulse"); impact_pulse$file <- gsub("_emission_pulse" , "", impact_pulse$file)
    scc_pulse <- merge(impact_pulse_no, impact_pulse, by = c("pathdir", "file", "n", "t"))
    #check pulse
    print(scc_pulse %>% filter(t==t0) %>%group_by(file) %>% summarize(pulse_actual=(sum(emi_pulse)-sum(emi))*1e9*44/12) %>% as.data.frame())
    scc_pulse <- scc_pulse %>% group_by(file) %>% mutate(pulse_actual=(sum(emi_pulse[t==t0])-sum(emi[t==t0]))*1e9*44/12)
    
    #using theoretical or actual pulse size
    scc_pulse$pulse_used <- scc_pulse$pulse_actual 
    #scc_pulse$pulse_used <- pulse_size_theoretical
    
    #compute SCC
    scc_pulse <- scc_pulse %>% mutate(damage_nt=gdp-gdp_pulse) %>%
      group_by(pathdir, file, t) %>% mutate(ede_t=(sum(pop*(gdp*1e6/pop)^(1-gamma))/(sum(pop)))^(1/(1-gamma)), gdppc_t=(sum(pop*(gdp*1e6/pop))/(sum(pop)))) %>% ungroup() %>%
      group_by(pathdir, file, n) %>% mutate(scc_contribution=(ede_t/ede_t[t==t0])^(gamma-eta)*((gdp*1e6/pop)^(-gamma))*(1+srtp/100)^(-(t-t0)*5)*damage_nt) %>% ungroup()
      if(normalization_region=="World"){
        scc_pulse <- scc_pulse %>% group_by(pathdir, file, n) %>% mutate(normalization=gdppc_t[t==t0]^(-gamma)) %>% ungroup()
      }else{
        scc_pulse <- scc_pulse %>% group_by(pathdir, file) %>% mutate(normalization=gdp[t==t0 & n==normalization_region]^(-gamma)) %>% ungroup()
      }
    scc_pulse <- scc_pulse %>% mutate(scc_contribution_normalized=scc_contribution/normalization)
    #now computing SCC given damage in T$, emissions in tCO2eq
    scc_value_pulse <- scc_pulse %>% group_by(pathdir, file) %>% filter(t>= t0 & t <= tmax) %>% summarize(SCC=sum(scc_contribution_normalized*1e12/pulse_used)) %>% as.data.frame()
    scc_value_pulse$pathdir <- "Emission_pulse"
    print(scc_value_pulse)
    SCC_bar_chart_pulse <- ggplot(scc_value_pulse) + geom_bar(aes(file, SCC, fill=file), position = "dodge", stat="identity") + ylab("SCC [$/tCO2eq] (PULSE)") + xlab("")  + geom_text(aes(file, SCC*0.9, label=paste0(round(SCC,1), "$"))) + guides(fill=FALSE) + ylim(NA, max(c(scc_value_marginals$SCC, scc_value_pulse$SCC)))
    SCC_bar_chart <- ggplot(subset(scc_value_marginals, file %in% scenplot_nopulse)) + geom_bar(aes(file, SCC, fill=file), position = "dodge", stat="identity") + ylab("SCC [$/tCO2eq]") + xlab("") + geom_text(aes(file, SCC*0.9, label=paste0(round(SCC,1), "$"))) + guides(fill=FALSE) + ylim(NA, max(c(scc_value_marginals$SCC, scc_value_pulse$SCC)))
  }
  
  print(ggarrange(gdp_plot, gdp_loss_plot, emi_plot, temp_plot, SCC_bar_chart, SCC_bar_chart_pulse, ncol = 2, nrow=3, common.legend = T))
  
}

