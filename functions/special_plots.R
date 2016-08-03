#Special Plots

# Intensity Plot for EI/CI
# Policy Costs

Intensity_Plot <- function(year=2050, region="WORLD", year0=2010){
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
  
  if(region[1]=="global"){
    ggplot() + geom_point(data=Intensity_t, mapping=aes(x=CI_change, y=EI_change, shape=file), size=6) + geom_hline(size=1,aes(yintercept=-1.1), linetype="dashed") + geom_vline(size=1,aes(xintercept=-0.3), linetype="dashed") + xlab(paste0("Carbon Intensity Change, ", year0,"-",year)) + ylab(paste0("Energy Intensity Change, ", year0,"-",year)) + guides(color=guide_legend(title=NULL, nrow = 2)) + theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal") + ylim(-1, +1) + xlim(-1, +1)
  }else{
    ggplot() + geom_point(data=Intensity_t, mapping=aes(x=CI_change, y=EI_change, colour=n, shape=file), size=6) + geom_hline(size=1,aes(yintercept=-1.1), linetype="dashed") + geom_vline(size=1,aes(xintercept=-0.3), linetype="dashed") + xlab(paste0("Carbon Intensity Change, ", year0,"-",year)) + ylab(paste0("Energy Intensity Change, ", year0,"-",year)) + guides(color=guide_legend(title=NULL, nrow = 2)) + theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal") + ylim(-2, 0) + xlim(-0.5, +0.2)
  }
  saveplot("CI_EI_Improvement", plotdata=Intensity_t)
}




#Sectoral Emissions
Sectoral_Emissions <- function(regions=witch_regions){
get_witch_variable("Q_EMI", "CO2_FFI", "e", "co2ind", 3.67, "GtCO2", "regional", plot = F)
Q_EMI_FFI <- Q_EMI
Q_EMI_FFI$sector="FFI"
get_witch_variable("Q_EMI", "CO2_LU", "e", "co2lu", 3.67, "GtCO2", "regional", plot = F)
Q_EMI_LU <- Q_EMI
Q_EMI_LU$sector="LU"
Q_EMI_SECTORS = rbind(Q_EMI_FFI, Q_EMI_LU)
#Stacked Regions Plot
ggplot(subset(Q_EMI_SECTORS),aes(ttoyear(t),value, fill=n)) + geom_area(stat="identity") + facet_grid(sector ~ file, scales = "free") + ylab("GtCO2") + xlab("") + guides(fill=guide_legend(title=NULL, nrow = 1)) + theme(legend.position="bottom") + scale_fill_manual(values = region_palette)
saveplot("Sectoral CO2 Emissions RegionsStacked", plotdata=subset(Q_EMI_SECTORS))
ggplot(subset(Q_EMI_SECTORS, t<=10 & n %in% regions & sector=="FFI")) + geom_line(stat="identity", size=1.2, aes(ttoyear(t),value, color=file)) + facet_wrap( ~ n, scales = "free", switch=NULL, ncol=length(regions_plotgrid)) + ylab("GtCO2") + xlab("") + guides(color=guide_legend(title=NULL, nrow = 1)) + theme(legend.position="bottom")
saveplot("Sectoral CO2 Emissions FFI", plotdata=subset(Q_EMI_SECTORS, t<=10 & n %in% regions & sector=="FFI"))
ggplot(subset(Q_EMI_SECTORS, t<=10 & n %in% regions & sector=="LU")) + geom_line(stat="identity", size=1.2, aes(ttoyear(t),value, color=file)) + facet_wrap( ~ n, scales = "free", switch=NULL, ncol=length(regions_plotgrid)) + ylab("GtCO2") + xlab("") + guides(color=guide_legend(title=NULL, nrow = 1)) + theme(legend.position="bottom")
saveplot("Sectoral CO2 Emissions LU", plotdata=subset(Q_EMI_SECTORS, t<=10 & n %in% regions & sector=="LU"))
}


#Emission reduction by source
Emission_reduction <- function(regions=witch_regions, scenario_stringency_order){
  get_witch_simple("Q_EMI")
  Q_EMI <- as.data.frame(Q_EMI)
  Q_EMI <- subset(Q_EMI, select=-pathdir)
  Q_EMI <- reshape(Q_EMI, timevar = "e",idvar = c("t", "n", "file"),direction = "wide")
  emi_sources= c("CO2FFI", "CCS", "CO2LU", "NON-CO2")
  Q_EMI$CO2FFI <- Q_EMI$value.co2ind + Q_EMI$value.ccs
  Q_EMI$CCS <- -Q_EMI$value.ccs
  Q_EMI$CO2LU <- Q_EMI$value.co2lu
  Q_EMI$"NON-CO2" <- Q_EMI$value.ch4 + Q_EMI$value.n2o + Q_EMI$value.slf + Q_EMI$value.llf
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
  MITIGATION_SOURCES$pathdir <- pathdir[1] #to avoid issues when saving data as EXCEL
  #to set minimal negtaive vales to zero
  #MITIGATION_SOURCES$value <- max(0, MITIGATION_SOURCES$value)
  #Stacked Regions Plot
  ggplot(subset(MITIGATION_SOURCES, ttoyear(t)<=yearmax & n %in% regions),aes(ttoyear(t),value, fill=interaction(file, source), group=interaction(file, source))) + geom_bar(stat="identity", position = "stack") + ylab("MtCO2") + xlab("") + guides(fill=guide_legend(title=NULL, nrow = 2)) + theme(legend.position="bottom") + facet_wrap( ~ n, scales = "free") + scale_fill_manual(values=c("#0000FF", "#000066", "#FFFF00", "#666600","#00FF00", "#006600", "#FF0000", "#660000"))
  saveplot("Emission reduction by source", plotdata=subset(MITIGATION_SOURCES, ttoyear(t)<=yearmax & n %in% regions))
  
  #add GLOBAL carbon budget by region of the most stringent scenario
  #for now only CO2 with hist!
  get_witch_variable("Q_EMI", "CO2_Emissions", "e", "co2ind", 44/12, "GtCO2", "regional", scenplot = scenario_stringency_order[length(scenario_stringency_order)])
  ALL_EMI <- Q_EMI
  ALL_REST_WOLD <- subset(ALL_EMI, !(n %in% regions))[, lapply(.SD, sum), by=c("t", "file", "pathdir")]
  ALL_REST_WOLD$n <- "Rest_of_World"
  ALL_EMI <- rbind(subset(ALL_EMI, (n %in% regions)), ALL_REST_WOLD)
  regions <- c(regions, "Rest_of_World")
  setnames(ALL_EMI, "value", "GHG")
  ggplot(subset(ALL_EMI, n %in% regions & ttoyear(t)<=yearmax & file==scenario_stringency_order[length(scenario_stringency_order)]),aes(ttoyear(t),GHG,fill=n)) + geom_area(stat="identity") + xlab("year") +ylab("GtCO2") + scale_fill_manual(values = region_palette) + scale_x_continuous(breaks=seq(1990,2050,10))
  #legend_position = "right"
  saveplot("CO2 FFI Emissions Asia and RoW 2C", plotdata=subset(ALL_EMI, n %in% regions & ttoyear(t)<=yearmax & file==scenario_stringency_order[length(scenario_stringency_order)]))
}



Investment_Plot <- function(regions=witch_regions, scenplot=scenlist){
  get_witch_simple("I_EN", scenplot = scenplot)
  get_witch_simple("I_RD", scenplot = scenplot)
  #I_RD <- subset(I_RD, rd=="en"); I_RD$rd <- NULL;
  I_EN <- aggregate(value~n+t+file+pathdir, data=I_EN, sum)
  #I_EN$type = "Energy Supply"
  #I_RD$type = "Energy Efficiency"
  #Investment_Energy <- rbind(I_EN, I_RD)

  #I_RD plot
  I_RD$rd  <- mapvalues(I_RD$rd , from=unique(I_RD$rd), to=c("Energy Efficiency", "Advanced Biofuels", "Batteries"))
  I_RD <- subset(I_RD, rd!="Batteries")
  ggplot(subset(I_RD, ttoyear(t)<=yearmax & n %in% regions)) + geom_line(stat="identity", size=1.2, aes(ttoyear(t),value*1e3, linetype=rd, color=file)) + facet_wrap( ~ n, scales = "free", switch=NULL, ncol=length(regions_plotgrid)) + ylab("Billion USD") + xlab("") + guides(color=guide_legend(title=NULL, nrow = 1)) + theme(legend.position="bottom") + guides(linetype=guide_legend(title=NULL)) 
  saveplot("Investment in RnD", plotdata=subset(I_RD, ttoyear(t)<=yearmax & n %in% regions))
  
  #Investment in Energy Supply
  ggplot(subset(I_EN, ttoyear(t)<=yearmax & n %in% regions)) + geom_line(stat="identity", size=1.2, aes(ttoyear(t),value*1e3, color=file)) + facet_wrap( ~ n, scales = "free", switch=NULL, ncol=length(regions_plotgrid)) + ylab("Billion USD") + xlab("") + guides(color=guide_legend(title=NULL, nrow = 1)) + theme(legend.position="bottom") + guides(linetype=guide_legend(title=NULL)) 
  saveplot("Investment in Energy Supply", plotdata=subset(I_EN, ttoyear(t)<=yearmax & n %in% regions))
  
  #now get global energy investment picture
  get_witch_simple("I_EN", scenplot = scenplot)
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
  get_witch_simple("I_RD", scenplot = scenplot)
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
  
  #ggplot(subset(Investment_Energy_global),aes(ttoyear(t),value*1e3,color=type)) + geom_line(stat="identity") + facet_grid(. ~ file, scales = "free") + ylab("Billion USD") + xlab("") + guides(fill=guide_legend(title=NULL, nrow = 1)) + theme(legend.position="bottom") + scale_fill_manual(values = region_palette)
  ggplot(subset(Investment_Energy_global),aes(file,value*5, fill=category)) + geom_bar(stat="identity", position = "stack") + ylab("Trillion USD (2015-2050)") + xlab("") + guides(fill=guide_legend(title=NULL, nrow = 2)) + theme(legend.position="bottom") + facet_wrap( ~ sector, scales = "fixed")  + scale_x_discrete(limits=scenplot) + scale_fill_brewer(palette="Spectral")
  #+ scale_fill_manual(values=c("#0000FF", "#000066", "#FFFF00", "#666600","#00FF00", "#006600", "#FF0000", "#660000"))
  saveplot("Investments Developing Asia, 2015-2050", plotdata=Investment_Energy_global)
}





get_globiom_variables <- function(regions = witch_regions, varplot="ForestCover", varname="Forest Cover", varunit="%"){
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
  
  globiom_all_variables_witch <- melt(globiom_all_variables_witch_wide, id.vars = c("pathdir", "file", "n", "t"))
  
 ggplot(subset(globiom_all_variables_witch, n %in% regions & variable==varplot)) + geom_line(aes(ttoyear(t), value, colour=n, linetype=file)) + xlab("") + ylab(paste0(varname, " [", varunit, "]")) 
 saveplot(varname, plotdata = subset(globiom_all_variables_witch, n %in% regions & variable==varplot))  
  
}


