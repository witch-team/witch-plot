#Special Plots

# Intensity Plot for EI/CI
# Policy Costs

Intensity_Plot <- function(years=c(2050, 2100), regions="World", year0=2010, scenplot=scenlist, animate_plot=FALSE){
  if(animate_plot) {regions="World"; years = seq(yearmin, yearmax); year0 = 2005; require(gganimate)}
  get_witch_simple("tpes"); tpes_IP <- tpes %>% mutate(value=value*0.0036) %>% rename(PES=value)
  get_witch_simple("Q_EMI"); Q_EMI_IP <- Q_EMI %>% mutate(value=value*3.667) %>% filter(e=="co2") %>% select(-e) %>% rename(CO2=value)
  get_witch_simple("Q"); Q_IP <- Q %>% mutate(value=value*1e3) %>% filter(iq=="y") %>% select(-iq) %>% rename(GDP=value)
  Intensity <- merge(tpes_IP, Q_EMI_IP, by=c("t", file_group_columns, "pathdir", "n"))
  Intensity <- merge(Intensity, Q_IP, by=c("t", file_group_columns, "pathdir", "n"))
  Intensity_World <- Intensity; Intensity_World$n <- NULL
  Intensity_World <- as.data.table(Intensity_World)[, lapply(.SD, sum), by=c("t", file_group_columns, "pathdir")]
  Intensity_World$n <- "World"
  Intensity <- rbind(Intensity, Intensity_World)
  Intensity <- subset(Intensity, n %in% regions)
  Intensity$CI=Intensity$CO2/Intensity$PES *1e3 #gCO2/MJ (from GTCO2eq/EJ)
  Intensity$EI=Intensity$PES/Intensity$GDP *1e3 #MJ/$ (from EJ/billion $)
  Intensity_t <- subset(Intensity, t %in% yeartot(c(years, year0)))
  Intensity_t <- Intensity_t %>% group_by_at(c("pathdir", file_group_columns, "n")) %>% mutate(CI_change=(((CI/CI[t==yeartot(year0)])**(1/(ttoyear(t)-year0)))-1)*100, EI_change=(((EI/EI[t==yeartot(year0)])**(1/(ttoyear(t)-year0)))-1)*100) %>% as.data.frame()
  Intensity_t <- subset(Intensity_t, file %in% scenplot)
  if(regions[1]=="World"){
    p_imp <- ggplot() + geom_point(data=subset(Intensity_t, ttoyear(t)!=year0+1e3), mapping=aes(x=CI_change, y=EI_change, color=file, shape=as.character(ttoyear(t))), size=6) + geom_hline(size=1,aes(yintercept=-1.1), linetype="dashed") + geom_vline(size=1,aes(xintercept=-0.3), linetype="dashed") + xlab(paste0("Carbon Intensity Change")) + ylab(paste0("Energy Intensity Change")) + guides(color=guide_legend(title=NULL), shape=guide_legend(title=NULL)) + theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal")
    p_ciei <- ggplot() + geom_point(data=subset(Intensity_t), mapping=aes(x=CI, y=EI, color=file, shape=as.character(ttoyear(t))), size=6) + xlab(paste0("Carbon Intensity [gCO2/MJ]")) + ylab(paste0("Energy Intensity [MJ/$]")) + guides(color=guide_legend(title=NULL), shape=guide_legend(title=NULL)) + theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal")
    if(animate_plot) p_ciei <- ggplot() + geom_point(data=Intensity_t %>% select(CI,EI,file,t) %>% mutate(year=ttoyear(as.numeric(t))) %>% select(-t), mapping=aes(x=CI, y=EI, color=file), size=6) + xlab(paste0("Carbon Intensity [gCO2/MJ]")) + ylab(paste0("Energy Intensity [MJ/$]")) + guides(color=guide_legend(title=NULL), shape=guide_legend(title=NULL)) + theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal") + labs(title = 'Year: {frame_time}') + transition_time(year) + ease_aes('linear')
    }else{
    Intensity_t <- subset(Intensity_t, t==yeartot(years[1])) #for regional results only first year!
    p_imp <- ggplot() + geom_point(data=Intensity_t, mapping=aes(x=CI_change, y=EI_change, colour=n, shape=file), size=6) + geom_hline(size=1,aes(yintercept=-1.1), linetype="dashed") + geom_vline(size=1,aes(xintercept=-0.3), linetype="dashed") + xlab(paste0("Carbon Intensity Change")) + ylab(paste0("Energy Intensity Change")) + guides(color=guide_legend(title=NULL, nrow = 2)) + theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal")
    p_ciei <- ggplot() + geom_point(data=subset(Intensity_t), mapping=aes(x=CI, y=EI, color=n, shape=file), size=6) + xlab(paste0("Carbon Intensity [gCO2eq/MJ]")) + ylab(paste0("Energy Intensity [MJ/$]")) + guides(color=guide_legend(title=NULL), shape=guide_legend(title=NULL)) + theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal")
    }
  if(animate_plot) print(animate(p_ciei, nframes = 20, duration = 10, rewind = FALSE))
  if(!animate_plot) {
    ggarrange(p_ciei, p_imp, common.legend = T, align="h", legend = "bottom")
    saveplot("CI_EI_plot", add_title = F, width = 10, height = 5)
  }
  assign("CI_EI_Improvement", Intensity_t, envir = .GlobalEnv)
}  
  




#Sectoral Emissions
Sectoral_Emissions <- function(regions=witch_regions, scenplot=scenlist){
get_witch_simple("Q_EMI"); Q_EMI_FFI <- Q_EMI %>% mutate(value=value*3.667) %>% filter(e=="co2ffi") %>% select(-e)
Q_EMI_FFI$sector="Fossil Fuels and Industrial"#FFI
get_witch_simple("Q_EMI"); Q_EMI_LU <- Q_EMI %>% mutate(value=value*3.667) %>% filter(e=="co2lu") %>% select(-e)
Q_EMI_LU$sector="Land Use"#LU
Q_EMI_SECTORS = rbind(Q_EMI_FFI, Q_EMI_LU)
#Stacked Regions Plot
ggplot(subset(Q_EMI_SECTORS, file %in% scenplot),aes(ttoyear(t),value, fill=n)) + geom_area(stat="identity") + facet_grid(sector ~ file, scales = "free") + ylab("GtCO2") + xlab("") + guides(fill=guide_legend(title=NULL, nrow = 2)) + theme(legend.position="bottom") + scale_fill_manual(values = region_palette)
saveplot("Sectoral CO2 Emissions Regions", plotdata=subset(Q_EMI_SECTORS, file %in% scenplot), add_title=F)
ggplot(subset(Q_EMI_SECTORS, t<=10 & n %in% regions & sector=="Fossil Fuels and Industrial" & file %in% scenplot)) + geom_line(stat="identity", size=1.2, aes(ttoyear(t),value, color=file)) + facet_wrap( ~ n, scales = "free", switch=NULL, ncol=length(regions)) + ylab("GtCO2") + xlab("") + guides(color=guide_legend(title=NULL, nrow = 2)) + theme(legend.position="bottom")
saveplot("Sectoral CO2 Emissions FFI", plotdata=subset(Q_EMI_SECTORS, t<=10 & n %in% regions & sector=="Fossil Fuels and Industrial" & file %in% scenplot))
ggplot(subset(Q_EMI_SECTORS, t<=10 & n %in% regions & sector=="Land Use" & file %in% scenplot)) + geom_line(stat="identity", size=1.2, aes(ttoyear(t),value, color=file)) + facet_wrap( ~ n, scales = "free", switch=NULL, ncol=length(regions)) + ylab("GtCO2") + xlab("") + guides(color=guide_legend(title=NULL, nrow = 2)) + theme(legend.position="bottom")
saveplot("Sectoral CO2 Emissions LU", plotdata=subset(Q_EMI_SECTORS, t<=10 & n %in% regions & sector=="Land Use" & file %in% scenplot))
}


#Emission reduction by source
Mitigation_Sources <- function(regions=witch_regions, scenario_stringency_order){
  get_witch_simple("Q_EMI")
  Q_EMI <- as.data.frame(Q_EMI); Q_EMI_orig <- Q_EMI
  Q_EMI <- subset(Q_EMI, select=-pathdir)
  Q_EMI <- reshape(Q_EMI, timevar = "e",idvar = c("t", "n", "file"),direction = "wide")
  emi_sources= c("CO2FFI", "CCS", "CO2LU", "NON-CO2")
  Q_EMI$CO2FFI <- Q_EMI$value.co2ffi + Q_EMI$value.ccs
  Q_EMI$CCS <- -Q_EMI$value.ccs
  Q_EMI$CO2LU <- Q_EMI$value.co2lu
  #Non-CO2 based on set
  get_witch_simple("ghg") # to get GHGs for non-co2 sets
  Q_EMI$"NON-CO2" <- rowSums(Q_EMI[colnames(Q_EMI) %in% paste0("value.",unique(ghg$e))]) - Q_EMI$value.co2
  Q_EMI <- subset(Q_EMI, select=c("t", "n", "file", emi_sources))
  Q_EMI <- subset(Q_EMI, file %in% scenario_stringency_order)
  ALL_EMI <- Q_EMI
  ALL_EMI$GHG <- ALL_EMI$CO2FFI + ALL_EMI$CCS + ALL_EMI$CO2LU + ALL_EMI$"NON-CO2"

  for(.num in length(scenario_stringency_order):2){
    .cur_file <- (Q_EMI$file==scenario_stringency_order[.num])
    .last_file <- (Q_EMI$file==scenario_stringency_order[.num-1])
    Q_EMI[.cur_file, c(4:7)] <- Q_EMI[.cur_file, c(4:7)] - Q_EMI[.last_file, c(4:7)]
  }
  
  MITIGATION_SOURCES <- melt(Q_EMI,id=c("t","n", "file"), variable.name = "source")
  MITIGATION_SOURCES$value <- MITIGATION_SOURCES$value * (-1) * 1e3 * (44/12)
  MITIGATION_SOURCES <- subset(MITIGATION_SOURCES, file!=scenario_stringency_order[1])
  MITIGATION_SOURCES <- MITIGATION_SOURCES[order(match(MITIGATION_SOURCES$file,scenario_stringency_order)),]
  MITIGATION_SOURCES <- MITIGATION_SOURCES[order(match(MITIGATION_SOURCES$file,scenario_stringency_order),match(MITIGATION_SOURCES$source,emi_sources)) ,]
  MITIGATION_SOURCES$pathdir <- basename(fullpathdir[1]) #to avoid issues when saving data as EXCEL
  #to set minimal negative vales to zero
  MITIGATION_SOURCES$value <- pmax(MITIGATION_SOURCES$value, 0)
  #Stacked Regions Plot
  MITIGATION_SOURCES <- subset(MITIGATION_SOURCES, t%%2==0)
  ggplot(subset(MITIGATION_SOURCES, ttoyear(t)<=yearmax & n %in% regions),aes(ttoyear(t),value, fill=interaction(file, source), group=interaction(file, source))) + geom_bar(stat="identity", position = "stack") + ylab("MtCO2") + xlab("") + guides(fill=guide_legend(title=NULL, nrow = 2)) + theme(legend.position="bottom") + facet_wrap( ~ n, scales = "free") + scale_fill_manual(values=c("#0000FF", "#000066", "#FFFF00", "#666600","#00FF00", "#006600", "#FF0000", "#660000"))
  saveplot("Emission reduction by source", plotdata=subset(MITIGATION_SOURCES, ttoyear(t)<=yearmax & n %in% regions))
  assign("Emissions", ALL_EMI, envir = .GlobalEnv)  
  assign("MITIGATION_SOURCES", MITIGATION_SOURCES, envir = .GlobalEnv)
}

Global_Emissions_Stacked <- function(regions=witch_regions, scenario, plotname="CO2 Emissions Carbon Budget"){
  #add GLOBAL carbon budget for some regions and RoW based on one scenario
  #for now only CO2 with hist!
  get_witch_simple("Q_EMI"); ALL_EMI <- Q_EMI %>% mutate(value=value*3.667) %>% filter(e=="co2ffi") %>% select(-e) %>% filter(file==scenario)
  ALL_REST_WOLD <- ALL_EMI %>% filter(!(n %in% regions)) %>% select(-n) %>% group_by(t, file, pathdir) %>% summarize(value=sum(value)) %>% mutate(n="Rest_of_World") %>% as.data.frame()
  #ALL_REST_WOLD <- subset(ALL_EMI, !(n %in% regions))[, lapply(.SD, sum), by=c("t", file_group_columns, "pathdir")]
  #ALL_REST_WOLD$n <- "Rest_of_World"
  ALL_EMI <- rbind(subset(ALL_EMI, (n %in% regions)), ALL_REST_WOLD)
  regions <- c(regions, "Rest_of_World")
  setnames(ALL_EMI, "value", "GHG")
  ggplot(subset(ALL_EMI, n %in% regions & ttoyear(t)<=yearmax & ttoyear(t) >= 1990),aes(ttoyear(t),GHG,fill=n)) + geom_area(stat="identity") + xlab("year") +ylab("GtCO2") + scale_fill_manual(values = region_palette) + scale_x_continuous(breaks=seq(1990,yearmax,10))
  saveplot(plotname, plotdata=subset(ALL_EMI, n %in% regions & ttoyear(t)<=yearmax & file==scenario))
}



#LMDI decomposition
Mitigation_Decomposition <- function(regions=witch_regions, scenario_stringency_order, scen_short="", t_plot=c(2,4,6,8,10), plotname="Mitigation Decomposition"){
  if("ida" %in% rownames(installed.packages()) == FALSE) {install.packages("ida", repos = c(getOption("repos"), "http://userpage.fu-berlin.de/~kweinert/R"), dependencies = c("Depends", "Suggests"))}
  library("ida")
  get_witch_simple("l"); l_decomp <- l
  get_witch_simple("Q"); Q_decomp <- Q %>% mutate(value=value*1e3) %>% filter(iq=="y") %>% select(-iq)
  get_witch_simple("tpes"); tpes_decomp <- tpes %>% mutate(value=value*0.0036)
  Sectoral_Emissions(regions=regions, scenplot = scenario_stringency_order)
  Mitigation_Sources(regions=regions, scenario_stringency_order = scenario_stringency_order)
  setnames(l, "value", "Population")
  setnames(Q, "value", "GDP")
  setnames(tpes, "value", "Energy")
  l <- subset(l, file %in% scenario_stringency_order & n %in% regions & t %in% t_plot)
  Q <- subset(Q, file %in% scenario_stringency_order & n %in% regions & t %in% t_plot)
  tpes <- subset(tpes, file %in% scenario_stringency_order & n %in% regions & t %in% t_plot)
  Emissions <- subset(Emissions, file %in% scenario_stringency_order & n %in% regions & t %in% t_plot)
  kaya_data <- cbind(l, Q$GDP, tpes$Energy, Emissions$CO2FFI)
  setnames(kaya_data, c("V2", "V3", "V4"), c("GDP", "Energy", "Emissions"))
  kaya_data$GDP_PC <- kaya_data$GDP/kaya_data$Population
  kaya_data$Emissions <- kaya_data$Emissions*44/12  #GtCO2
  
  kaya_data$EI <- kaya_data$Energy/kaya_data$GDP
  kaya_data$CI <- kaya_data$Emissions/kaya_data$Energy
  kaya_data_allvars <- kaya_data
  
  kaya_data$Emissions <- NULL; kaya_data$Energy <- NULL; kaya_data$GDP <- NULL; 
  
  kaya_rearranged <- melt(kaya_data, id.vars = c("pathdir", "file", "n", "t"))
  
  kaya_rearranged <- dcast(kaya_rearranged, formula = pathdir + n + t + variable ~ file)
  
  #apply LMDI for each region and time step
  #ida(test, effect = "variable", from = "REF", to = "INDC_TRADE", method = "lmdi1")$result[,2]
    lmdi_apply <- function(x){
    return(ida(x, effect = "variable", from = bau, to = mitscen, method = "lmdi1")$result[,2])
  }
  
  for(s in seq(2,length(scenario_stringency_order))){
    bau = scenario_stringency_order[s-1]
    mitscen = scenario_stringency_order[s]
    .lmdi <- plyr::ddply(kaya_rearranged, c("pathdir", "n", "t"), .fun=lmdi_apply)
    colnames(.lmdi) <- c("pathdir", "n", "t", "POP", "GDP", "EN_EFF", "EN_MIX")
    .lmdi$file <- mitscen
    .lmdi <- melt(.lmdi, id.vars = c("pathdir", "n", "t", "file"), variable.name = "source")
    .lmdi$value <- -1 * 1e3 * .lmdi$value #since we look in abatement as positive values and in MtCO2
    if(s==2){full_lmdi <- .lmdi}else{full_lmdi <- rbind(full_lmdi, .lmdi)}
  }
  
  #combine with other sectors from WITCH
  MIT_DECOMP <- rbind(MITIGATION_SOURCES, full_lmdi)
  MIT_DECOMP <- subset(MIT_DECOMP, source!="POP" & source!="CO2FFI")
  #to set minimal negative vales to zero
  MIT_DECOMP$value <- pmax(MIT_DECOMP$value, 0)
  #order scenarios and sources
  MIT_DECOMP <- MIT_DECOMP[order(match(MIT_DECOMP$file,scenario_stringency_order)),]
  emi_sources= c("GDP", "EN_EFF", "EN_MIX", "CCS", "CO2LU", "NON-CO2")
  MIT_DECOMP <- MIT_DECOMP[order(match(MIT_DECOMP$file,scenario_stringency_order),match(MIT_DECOMP$source,emi_sources)) ,]
  #short scenario names
  if(scen_short[1] != ""){MIT_DECOMP$file <- mapvalues(MIT_DECOMP$file, from=scenario_stringency_order, to=scen_short)}
  
  ggplot(subset(MIT_DECOMP, ttoyear(t)<=yearmax & n %in% regions),aes(ttoyear(t),value, fill=interaction(file, source), group=interaction(file, source))) + geom_bar(stat="identity", position = "stack") + ylab("MtCO2") + xlab("") + guides(fill=guide_legend(title=NULL, nrow = 2)) + theme(legend.position="bottom") + facet_wrap( ~ n, scales = "free") + scale_fill_manual(values=c("#FFFF00", "#666600","#00FF00", "#006600", "#FF0000", "#660000", "#333333", "#000000", "#0000FF", "#000066", "#FF8106", "#B44010"))
  saveplot(plotname, plotdata=subset(MIT_DECOMP, ttoyear(t)<=yearmax & n %in% regions))
}


Plot_Global_Emissions <- function(bauscen="ssp2_bau", scenplot=scenlist){
  get_witch_simple("Q_EMI", scenplot = scenplot)
  Q_EMI <- as.data.frame(Q_EMI); Q_EMI_orig <- Q_EMI
  Q_EMI <- reshape(Q_EMI, timevar = "e",idvar = c("t", "n", "file", "pathdir"),direction = "wide")
  emi_sources= c("CO2FFI", "CCS", "CO2LU", "NON-CO2")
  Q_EMI$CO2FFI <- Q_EMI$value.co2ffi + Q_EMI$value.ccs
  Q_EMI$CCS <- -Q_EMI$value.ccs
  Q_EMI$CO2LU <- Q_EMI$value.co2lu
  #Non-CO2 based on set
  get_witch_simple("ghg") # to get GHGs for non-co2 sets
  Q_EMI$"NON-CO2"  <- rowSums(Q_EMI[colnames(Q_EMI) %in% paste0("value.",unique(ghg$e))]) - Q_EMI$value.co2
  Q_EMI <- subset(Q_EMI, select=c("t", "n", "file", "pathdir", emi_sources))
  ALL_EMI <- Q_EMI
  ALL_EMI$GHG <- ALL_EMI$CO2FFI + ALL_EMI$CCS + ALL_EMI$CO2LU + ALL_EMI$"NON-CO2"
  p <- ggplot(data=subset(aggregate(GHG~t+file+pathdir, data=ALL_EMI, sum), ttoyear(t) <= yearmax),aes(ttoyear(t),GHG*44/12, colour=file)) + geom_line(stat="identity") + xlab("") +ylab("GtCO2")
  saveplot("Global GHG Emissions", plotdata = subset(aggregate(GHG~t+file+pathdir, data=ALL_EMI, sum)))
  
  assign("Global_Emissions_Data", subset(aggregate(GHG~t+file+pathdir, data=ALL_EMI, sum)), envir = .GlobalEnv)  
 
  #add also abatement (requires valid bauscen!!!)
  if(bauscen %in% scenlist){ 
  Abatement <- dcast(Global_Emissions_Data, pathdir + t ~ file, value.var="GHG")
  Emissions_BAU <- Abatement[bauscen]
  scen_stoch_plot <- colnames(Abatement)[3:length(colnames(Abatement))]
  for(abat_scen in scen_stoch_plot){Abatement[abat_scen] <- -(Abatement[abat_scen]-Emissions_BAU)}
  Abatement <- melt(Abatement, id.vars = c("pathdir", "t"), variable.name = "file")
  Abatement$file <- as.character(Abatement$file)
  Abatement <- subset(Abatement, file!=bauscen)
  Abatement$value <- Abatement$value*44/12
  ggplot(data=subset(Abatement, ttoyear(t) <= yearmax),aes(ttoyear(t),value, colour=file)) + geom_line(stat="identity") + xlab("") +ylab("GtCO2")
  saveplot("Global Abatement", plotdata = Abatement)
  assign("Global_Abatement_Data", Abatement, envir = .GlobalEnv)
  }
}






