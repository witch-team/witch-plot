#Special Plots

# Intensity Plot for EI/CI
# Policy Costs

Intensity_Plot <- function(year=2050, region="WORLD", year0=2010, scenplot=scenlist){
  get_witch_variable("tpes", "tpes", "na", "na", 0.0036, "EJ", "regional", plot=FALSE)
  setnames(tpes, "value", "PES")
  get_witch_variable("Q_EMI", "Q_EMI", "e", "co2ind", 0.0036, "EJ", "regional", plot=FALSE)
  setnames(Q_EMI, "value", "CO2")
  get_witch_variable("Q", "Q", "iq", "y", 1e3, "bln. USD", "regional", plot=FALSE)
  setnames(Q, "value", "GDP")
  Intensity <- merge(tpes, Q_EMI, by=c("t", "file", "pathdir", "n"))
  Intensity <- merge(Intensity, Q, by=c("t", "file", "pathdir", "n"))
  
  Intensity_World <- Intensity[, lapply(.SD, sum), by=c("t", "file", "pathdir")]
  Intensity_World$n <- "WORLD"
  
  Intensity <- rbind(Intensity, Intensity_World)
  
  Intensity <- subset(Intensity, n %in% region)
  
  Intensity$CI=Intensity$CO2/Intensity$PES
  Intensity$EI=Intensity$PES/Intensity$GDP
  
  Intensity_2010 <- subset(Intensity, t==yeartot(year0))
  Intensity_t <- subset(Intensity, t==yeartot(year))
  
  Intensity_t$CI_change <- (((Intensity_t$CI/Intensity_2010$CI)**(1/(5*(year-year0))))-1)*100
  Intensity_t$EI_change <- (((Intensity_t$EI/Intensity_2010$EI)**(1/(5*(year-year0))))-1)*100
  
  Intensity_t <- subset(Intensity_t, file %in% scenplot)
  
  if(region[1]=="global"){
    ggplot() + geom_point(data=Intensity_t, mapping=aes(x=CI_change, y=EI_change, shape=file), size=6) + geom_hline(size=1,aes(yintercept=-1.1), linetype="dashed") + geom_vline(size=1,aes(xintercept=-0.3), linetype="dashed") + xlab(paste0("Carbon Intensity Change, ", year0,"-",year)) + ylab(paste0("Energy Intensity Change, ", year0,"-",year)) + guides(color=guide_legend(title=NULL, nrow = 2)) + theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal") + ylim(-1, +1) + xlim(-1, +1)
  }else{
    ggplot() + geom_point(data=Intensity_t, mapping=aes(x=CI_change, y=EI_change, colour=n, shape=file), size=6) + geom_hline(size=1,aes(yintercept=-1.1), linetype="dashed") + geom_vline(size=1,aes(xintercept=-0.3), linetype="dashed") + xlab(paste0("Carbon Intensity Change, ", year0,"-",year)) + ylab(paste0("Energy Intensity Change, ", year0,"-",year)) + guides(color=guide_legend(title=NULL, nrow = 2)) + theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal") + ylim(-2, 0) + xlim(-0.5, +0.2)
  }
  saveplot("CI_EI_Improvement", plotdata=Intensity_t, add_title = F)
}




#Sectoral Emissions
Sectoral_Emissions <- function(regions=witch_regions, scenplot=scenlist){
get_witch_variable("Q_EMI", "CO2_FFI", "e", "co2ind", 3.67, "GtCO2", "regional", plot = F)
Q_EMI_FFI <- Q_EMI
Q_EMI_FFI$sector="Fossil Fuels and Industrial"#FFI
get_witch_variable("Q_EMI", "CO2_LU", "e", "co2lu", 3.67, "GtCO2", "regional", plot = F)
Q_EMI_LU <- Q_EMI
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
  Q_EMI$CO2FFI <- Q_EMI$value.co2ind + Q_EMI$value.ccs
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
  MITIGATION_SOURCES$pathdir <- basename(pathdir[1]) #to avoid issues when saving data as EXCEL
  #to set minimal negative vales to zero
  MITIGATION_SOURCES$value <- pmax(MITIGATION_SOURCES$value, 0)
  #Stacked Regions Plot
  MITIGATION_SOURCES <- subset(MITIGATION_SOURCES, t%%2==0)
  ggplot(subset(MITIGATION_SOURCES, ttoyear(t)<=yearmax & n %in% regions),aes(ttoyear(t),value, fill=interaction(file, source), group=interaction(file, source))) + geom_bar(stat="identity", position = "stack") + ylab("MtCO2") + xlab("") + guides(fill=guide_legend(title=NULL, nrow = 2)) + theme(legend.position="bottom") + facet_wrap( ~ n, scales = "free") + scale_fill_manual(values=c("#0000FF", "#000066", "#FFFF00", "#666600","#00FF00", "#006600", "#FF0000", "#660000"))
  saveplot("Emission reduction by source", plotdata=subset(MITIGATION_SOURCES, ttoyear(t)<=yearmax & n %in% regions))
  assign("Emissions", ALL_EMI, envir = .GlobalEnv)  
  assign("MITIGATION_SOURCES", MITIGATION_SOURCES, envir = .GlobalEnv)
}

Carbon_Budget <- function(regions=witch_regions, scenario, plotname="CO2 Emissions Carbon Budget"){
  #add GLOBAL carbon budget for some regions and RoW based on one scenario
  #for now only CO2 with hist!
  get_witch_variable("Q_EMI", "CO2_Emissions", "e", "co2ind", 44/12, "GtCO2", "regional", scenplot = scenario, plot = FALSE)
  ALL_EMI <- Q_EMI
  ALL_REST_WOLD <- subset(ALL_EMI, !(n %in% regions))[, lapply(.SD, sum), by=c("t", "file", "pathdir")]
  ALL_REST_WOLD$n <- "Rest_of_World"
  ALL_EMI <- rbind(subset(ALL_EMI, (n %in% regions)), ALL_REST_WOLD)
  regions <- c(regions, "Rest_of_World")
  setnames(ALL_EMI, "value", "GHG")
  ggplot(subset(ALL_EMI, n %in% regions & ttoyear(t)<=yearmax & file==scenario),aes(ttoyear(t),GHG,fill=n)) + geom_area(stat="identity") + xlab("year") +ylab("GtCO2") + scale_fill_manual(values = region_palette) + scale_x_continuous(breaks=seq(1990,yearmax,10))
  #legend_position = "right"
  saveplot(plotname, plotdata=subset(ALL_EMI, n %in% regions & ttoyear(t)<=yearmax & file==scenario))
  
}


Investment_Plot <- function(regions=witch_regions, scenplot=scenlist){
  get_witch_simple("I_EN", scenplot = scenplot); I_EN_orig <- I_EN
  get_witch_simple("I_RD", scenplot = scenplot); I_RD_orig <- I_RD
  #I_RD <- subset(I_RD, rd=="en"); I_RD$rd <- NULL;
  I_EN <- aggregate(value~n+t+file+pathdir, data=I_EN, sum)
  #I_EN$type = "Energy Supply"
  #I_RD$type = "Energy Efficiency"
  #Investment_Energy <- rbind(I_EN, I_RD)

  #I_RD plot
  I_RD$rd  <- mapvalues(I_RD$rd , from=unique(I_RD$rd), to=c("Energy Efficiency", "Advanced Biofuels", "Batteries"))
  I_RD <- subset(I_RD, rd!="Batteries")
  ggplot(subset(I_RD, ttoyear(t)<=yearmax & n %in% regions)) + geom_line(stat="identity", size=1.2, aes(ttoyear(t),value*1e3, linetype=rd, color=file)) + facet_wrap( ~ n, scales = "free", switch=NULL, ncol=length(regions)) + ylab("Billion USD") + xlab("") + guides(color=guide_legend(title=NULL, nrow = 1)) + theme(legend.position="bottom") + guides(linetype=guide_legend(title=NULL)) 
  saveplot("Investment in RnD", plotdata=subset(I_RD, ttoyear(t)<=yearmax & n %in% regions))
  
  #Investment in Energy Supply
  ggplot(subset(I_EN, ttoyear(t)<=yearmax & n %in% regions)) + geom_line(stat="identity", size=1.2, aes(ttoyear(t),value*1e3, color=file)) + facet_wrap( ~ n, scales = "free", switch=NULL, ncol=length(regions)) + ylab("Billion USD") + xlab("") + guides(color=guide_legend(title=NULL, nrow = 1)) + theme(legend.position="bottom") + guides(linetype=guide_legend(title=NULL)) 
  saveplot("Investment in Energy Supply", plotdata=subset(I_EN, ttoyear(t)<=yearmax & n %in% regions))
  
  #now get global energy investment picture
  #I_EN <- NULL
  #get_witch_simple("I_EN", scenplot = scenlist)
  I_EN <- I_EN_orig
  get_witch_simple("I_EN_GRID", scenplot = scenplot)
  I_EN_Renewables <- aggregate(value~n+t+file+pathdir, data=subset(I_EN, jinv %in% c("elpv", "elcsp", "elwindon", "elwindoff", "elhydro_new")), sum);I_EN_Renewables$category <- "Renewables"
  I_EN_FossilFuels <- aggregate(value~n+t+file+pathdir, data=subset(I_EN, jinv %in% c("elpc_new", "eloil_new", "elgastr_new", "elpb_new")), sum);I_EN_FossilFuels$category <- "Fossil Fuels"
  I_EN_Nuclear <- aggregate(value~n+t+file+pathdir, data=subset(I_EN, jinv %in% c("elnuclear_new")), sum);I_EN_Nuclear$category <- "Nuclear"
  I_EN_CCS <- aggregate(value~n+t+file+pathdir, data=subset(I_EN, jinv %in% c("elcigcc", "elgasccs", "elbigcc", "nelcoalccs")), sum);I_EN_CCS$category <- "Fossils with CCS"
  I_EN_GRID$jinv <- "grid"; I_EN <- rbind(I_EN, I_EN_GRID)
  I_EN_TDS <- aggregate(value~n+t+file+pathdir, data=subset(I_EN, jinv %in% c("elstorage", "grid")), sum);I_EN_TDS$category <- "Grid&Storage"
  I_EN_categorized <- rbind(I_EN_Renewables, I_EN_CCS, I_EN_FossilFuels, I_EN_Nuclear, I_EN_TDS)
  I_TRANSPORT_trad <- aggregate(value~n+t+file+pathdir, data=subset(I_EN, jinv %in% c("trad_cars", "hybrid", "trad_stfr", "hbd_stfr")), sum);I_TRANSPORT_trad$category <- "ICE/Hybrid"
  I_TRANSPORT_lowcarbon <- aggregate(value~n+t+file+pathdir, data=subset(I_EN, jinv %in% c("edv", "edv_stfr", "plg_hybrid", "plg_hbd_stfr")), sum);I_TRANSPORT_lowcarbon$category <- "Electric Vehicles"
  I_TRANSPORT <- rbind(I_TRANSPORT_trad, I_TRANSPORT_lowcarbon); 
  #I_TRANSPORT$sector <- "Road Transport"
  #I_TRANSPORT$sector <- "Road Transport"
  #for now add low carbon transport to RnD
  I_RD <- I_RD_orig
  I_RD$rd  <- mapvalues(I_RD$rd , from=unique(I_RD$rd), to=c("Energy Efficiency", "Advanced Biofuels", "Batteries"))
  #I_RD <- subset(I_RD, rd!="Batteries")
  
  #I_RD <- rbind(I_RD, I_TRANSPORT_lowcarbon)
  #year factor, to be checked!!
  #I_RD$value <- I_RD$value * 5;  #since it seems verrrry low!!!
  
  
  get_witch_simple("I_OUT", scenplot = scenplot)
  I_OUT <- subset(I_OUT, f=="oil");setnames(I_OUT, "f", "category")
  I_OUT$category <- "Oil Extraction"
  setnames(I_RD, "rd", "category")
  I_OUT$sector <- "Fuel supply"; I_EN_categorized$sector <- "Power supply"; I_RD$sector <- "Energy RnD"
  Investment_Energy <- rbind(I_EN_categorized, I_RD, I_OUT)
  
  
  Investment_Energy <- subset(Investment_Energy, t>=3 & t<=10)
  
  Investment_Energy_global <- aggregate(value~sector+category+file+pathdir, data=subset(Investment_Energy, n %in% regions), sum)
  Investment_Energy_global$value <-   Investment_Energy_global$value*5 

  ggplot(subset(Investment_Energy_global),aes(file,value, fill=category)) + geom_bar(stat="identity", position = "stack") + ylab("Trillion USD (2015-2050)") + xlab("") + guides(fill=guide_legend(title=NULL, nrow = 2)) + theme(legend.position="bottom") + facet_wrap( ~ sector, scales = "fixed")  + scale_x_discrete(limits=scenplot) + scale_fill_brewer(palette="Spectral")
  #+ scale_fill_manual(values=c("#0000FF", "#000066", "#FFFF00", "#666600","#00FF00", "#006600", "#FF0000", "#660000"))
  saveplot("Investments Developing Asia, 2015-2050", plotdata=Investment_Energy_global)
}



#LMDI decomposition
Mitigation_Decomposition <- function(regions=witch_regions, scenario_stringency_order, scen_short="", t_plot=c(2,4,6,8,10), plotname="Mitigation Decomposition"){
  if("ida" %in% rownames(installed.packages()) == FALSE) {install.packages("ida", repos = c(getOption("repos"), "http://userpage.fu-berlin.de/~kweinert/R"), dependencies = c("Depends", "Suggests"))}
  library("ida")
  get_witch_variable("l", "Population", "na", "na", 1, "Mio.", "regional", plot=F)
  get_witch_variable("Q", "GDP", "iq", "y", 1e3, "billion USD", "regional", plot=F)
  get_witch_variable("tpes", "Energy", "na", "na", 0.0036, "EJ", "regional", plot=F)
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
    .lmdi <- ddply(kaya_rearranged, c("pathdir", "n", "t"), .fun=lmdi_apply)
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


Plot_Global_Emissions <- function(show_ar5=TRUE, ar5_budget=2000, bauscen="ssp2_bau", scenplot=scenlist){
  get_witch_simple("Q_EMI", scenplot = scenplot)
  Q_EMI <- as.data.frame(Q_EMI); Q_EMI_orig <- Q_EMI
  Q_EMI <- reshape(Q_EMI, timevar = "e",idvar = c("t", "n", "file", "pathdir"),direction = "wide")
  emi_sources= c("CO2FFI", "CCS", "CO2LU", "NON-CO2")
  Q_EMI$CO2FFI <- Q_EMI$value.co2ind + Q_EMI$value.ccs
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
 
  #now add AR5
  if(show_ar5){
  load("datasets/ar5emission.RData")
  p <- ggplot(kemi[cbudget.co2<ar5_budget]) + geom_line(aes(x=YEAR,y=KGHG,group=paste(SCENARIO,MODEL)),alpha=0.1,size=1,color="lightgrey") 
  p <- p + geom_line(stat="identity", data=subset(aggregate(GHG~t+file+pathdir, data=ALL_EMI, sum), ttoyear(t) <= yearmax),aes(ttoyear(t),GHG*44/12, colour=file)) + xlab("") + ylab("GtCO2")
  saveplot("Global GHG Emissions (with AR5)", plotdata = subset(aggregate(GHG~t+file+pathdir, data=ALL_EMI, sum)))
  }
  
  assign("Global_Emissions_Data", subset(aggregate(GHG~t+file+pathdir, data=ALL_EMI, sum)), envir = .GlobalEnv)  
 
  #add also abatement
  Abatement <- dcast(Global_Emissions_Data, pathdir + t ~ file, value.var="GHG")
  Emissions_BAU <- Abatement[bauscen]
  for(abat_scen in scenplot){Abatement[abat_scen] <- -(Abatement[abat_scen]-Emissions_BAU)}
  Abatement <- melt(Abatement, id.vars = c("pathdir", "t"), variable.name = "file")
  Abatement$file <- as.character(Abatement$file)
  Abatement <- subset(Abatement, file!=bauscen)
  Abatement$value <- Abatement$value*44/12
  ggplot(data=subset(Abatement, ttoyear(t) <= yearmax),aes(ttoyear(t),value, colour=file)) + geom_line(stat="identity") + xlab("") +ylab("GtCO2")
  saveplot("Global Abatement", plotdata = Abatement)
  assign("Global_Abatement_Data", Abatement, envir = .GlobalEnv)  
}


Energy_Prices <- function(unit="GJ", scenplot=scenlist){
  #unit conversion factor
  witch2iiasa = (1000/0.0036)  #T$/TWH to $/GJ
  twh2ej = 0.0036
  gj2boe = 5.86152
  for (variable_name in c("FPRICE","CPRICE")){   
    for (file in filelist) {
      #read data from GDX file
      mygdx <- gdx(paste(pathdir, file,".gdx",sep=""))
      tempdata <- mygdx[variable_name]
      if(nrow(tempdata)!=0){tempdata$file <- as.character(gsub("results_","",file))}
      if(file==filelist[1]){allfilesdata=tempdata}else{allfilesdata <-rbind(allfilesdata,tempdata)}
      #create dataframe based on variable_name
      remove(tempdata)
    }
    assign(variable_name, allfilesdata)
  }
  # energy price charts  (gas prices are EU, Japan, USA. Coal is Europe, Oil is WTI)
  historical_energy_prices_table <-"year coal	oil	gas_eu	gas_jpn	gas
  1987	1.046822742	3.138895876	2.018957346	0	          1.575829384
  1988	1.335785953	2.613507823	1.800947867	0	          1.59399684
  1989	1.40735786	3.220329804	1.504739336	0	          1.60821485
  1990	1.454180602	4.009483584	1.928909953	0	          1.609794629
  1991	1.431438127	3.524921882	2.8507109		0           1.409162717
  1992	1.288628763	3.366475728	2.241706161	3.548183254	1.679304897
  1993	1.126421405	3.01962689	2.462875197	3.409952607	2.010268562
  1994	1.243478261	2.815976285	2.191943128	3.113744076	1.819905213
  1995	1.488294314	3.015145663	2.555292259	3.371248025	1.632701422
  1996	1.379598662	3.626440229	2.606635071	3.699052133	2.591627172
  1997	1.301672241	3.372532013	2.530805687	3.564770932	2.352369668
  1998	1.070234114	2.354408871	2.127962085	2.610584518	1.978120063
  1999	0.962876254	3.161057283	1.713270142	3.168246445	2.14849921
  2000	1.20367893	4.969981391	3.274881517	4.962085308	4.083728278
  2001	1.305492668	4.244156203	3.671406003	4.425750395	3.749605055
  2002	1.058515565	4.28188216	2.52685624	4.195892575	3.180094787
  2003	1.458123231	5.084863191	3.304897314	4.721169036	5.205670774
  2004	2.410644456	6.790129427	3.559241706	5.583728278	5.587551786
  2005	2.024723437	9.262012504	5.606635071	6.665876777	8.450874717
  2006	2.14408284	10.80553429	7.78436019	7.629541864	6.36923516
  2007	2.969404425	11.81723622	7.718009479	7.97235387	6.617962736
  2008	4.938918189	16.37682658	12.45260664	10.98657188	8.395452522
  2009	2.36317284	10.13464309	8.393364929	7.100315956	3.744352291
  2010	3.093632622	13.00318203	7.793838863	8.89178515	4.156638644
  2011	4.064297659	15.55416306	10.04423381	14.74249605	3.790121852
  2012	3.093632622	15.40533773	11.35624013	17.19905213	2.6085703
  2013	0           0        		10.60584518	16.42101106	3.534386211"
  historical_energy_prices <- read.table(textConnection(historical_energy_prices_table), sep="", head=T, dec=".")
  historical_energy_prices <- melt(historical_energy_prices,id.vars="year")
  historical_witch <- historical_energy_prices; historical_witch$value = historical_witch$value / witch2iiasa; setnames(historical_witch, "variable", "fuel")
  historical_witch <- subset(historical_witch, fuel %in% unique(FPRICE$fuel))
  
  FPRICE$year=as.numeric(FPRICE$t) * 5 + 2000; FPRICE$t <- NULL
  FPRICE$fuel <- as.factor(FPRICE$fuel); FPRICE$file <- as.factor(FPRICE$file)
  #ggplot(subset(FPRICE, fuel!="uranium"&year<=yearmax), aes(year, witch2iiasa*value, group=interaction(fuel, file), colour=fuel, linetype=file)) + geom_line(size = 1.0) + labs(x="", y="World Energy Prices ($/GJ)", colour="Fuel", linetype="scenario")
  #saveplot("World Energy Prices Timeseries")
  FPRICE <- subset(FPRICE, file %in% scenplot)
  FPRICE <- subset(FPRICE, year>2013); 
  historical_witch$year <- as.numeric(historical_witch$year) 
  historical_witch <- subset(historical_witch, year<2013); 
  FPRICE$fuel <- as.character(FPRICE$fuel);historical_witch$fuel <- as.character(historical_witch$fuel)
  #add it for each scenario
  .historical_witch_temp <- historical_witch
  for(scen in unique(FPRICE$file))
  {
    .historical_witch_temp$file <- scen
    if(scen==unique(FPRICE$file)[1]){historical_witch=.historical_witch_temp}else{historical_witch <-rbind(historical_witch,.historical_witch_temp)}
  }
  FPRICE$file <- as.character(FPRICE$file)
  prices_merged <- merge(subset(FPRICE, fuel %in% c("oil", "gas", "coal")), historical_witch, by = c("year", "fuel", "file"), all=TRUE)
  prices_merged[is.na(prices_merged)] <- 0
  prices_merged$value <- prices_merged$value.x + prices_merged$value.y   #to keep both series
  #prices_merged$value.x <- NUL; prices_merged$value.y <- NULL
  if(unit=="GJ"){p <- ggplot(prices_merged, aes(year, witch2iiasa*value, group=interaction(fuel, file), colour=fuel, linetype=file)) + geom_line(size = 1.0) + labs(x="", y="World Energy Prices ($/GJ)", colour="Fuel", linetype="scenario")}
  else{p <- ggplot(prices_merged, aes(year, gj2boe*witch2iiasa*value, group=interaction(fuel, file), colour=fuel, linetype=file)) + geom_line(size = 1.0) + labs(x="", y="World Energy Prices ($/boe)", colour="Fuel", linetype="scenario")}
  legend_position = "right"
  saveplot("World Energy Prices", plotdata = prices_merged)
  Energy_Price_Data <- prices_merged
  Energy_Price_Data$value.x <- NULL
  Energy_Price_Data$value.y <- NULL
  Energy_Price_Data$year <- yeartot(Energy_Price_Data$year); setnames(Energy_Price_Data, "year", "t")
  setnames(Energy_Price_Data, "value", "energy_price"); Energy_Price_Data$energy_price <- Energy_Price_Data$energy_price
  assign("Energy_Price_Data", Energy_Price_Data, envir = .GlobalEnv)  
}




Energy_Trade <- function(fuel="oil", scenplot=scenlist){
  get_witch_variable("Q_OUT", "Extraction", "f", f, 1, "TWh", "regional", plot = F)
  get_witch_variable("Q_FUEL", "Consumption", "fuel", fuel, 1, "TWh", "regional", plot = F)
  NET_EXPORT <- Q_OUT
  setnames(NET_EXPORT, "value", "Extraction")
  NET_EXPORT <- merge(NET_EXPORT, Q_FUEL, by = c("t", "n", "file", "pathdir"))
  setnames(NET_EXPORT, "value", "Consumption")
  Energy_Prices(scenplot=scenlist)
  NET_EXPORT <- merge(NET_EXPORT, subset(Energy_Price_Data, fuel==fuel), by = c("t", "file"), all.x = TRUE)
  #volume in EJ, prices in $/GJ, value in billion USD
  NET_EXPORT$Net_Export_Volume <- (NET_EXPORT$Extraction - NET_EXPORT$Consumption) * 0.0036
  NET_EXPORT$Net_Export_Value <- ((NET_EXPORT$Extraction - NET_EXPORT$Consumption) * NET_EXPORT$energy_price) * 1e3
  NET_EXPORT$pathdir <- NULL
  NET_EXPORT$f <- NULL
  NET_EXPORT <- NET_EXPORT[!is.na(energy_price)]
  NET_EXPORT <- melt(NET_EXPORT, id.vars = c("t", "n", "file"))
  
  ggplot(subset(NET_EXPORT, file %in% scenplot & variable %in% c("Net_Export_Volume", "Net_Export_Value")),aes(ttoyear(t),value, fill=n)) + geom_area(stat="identity") + facet_grid(variable ~ file, scales = "free") + ylab("billion USD / EJ") + xlab("") + guides(fill=guide_legend(title=NULL, nrow = 2)) + theme(legend.position="bottom") + scale_fill_manual(values = region_palette)
  saveplot(paste("Energy Trade:", fuel), plotdata = NET_EXPORT)
}



