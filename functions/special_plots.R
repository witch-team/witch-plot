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
    ggplot() + geom_point(data=Intensity_t, mapping=aes(x=CI_change, y=EI_change, shape=file), size=6) + geom_hline(size=1,aes(yintercept=-1.1), linetype="dashed") + geom_vline(size=1,aes(xintercept=-0.3), linetype="dashed") + xlab(paste0("Carbon Intensity Change, ", year0,"-",year)) + ylab(paste0("Energy Intensity Change", year0,"-",year)) + guides(color=guide_legend(title=NULL, nrow = 1)) + theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal") + ylim(-1, +1) + xlim(-1, +1)
  }else{
    ggplot() + geom_point(data=Intensity_t, mapping=aes(x=CI_change, y=EI_change, colour=n, shape=file), size=6) + geom_hline(size=1,aes(yintercept=-1.1), linetype="dashed") + geom_vline(size=1,aes(xintercept=-0.3), linetype="dashed") + xlab(paste0("Carbon Intensity Change, ", year0,"-",year)) + ylab(paste0("Energy Intensity Change", year0,"-",year)) + guides(color=guide_legend(title=NULL, nrow = 1)) + theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal") + ylim(-2, 0) + xlim(-0.5, +0.2)
  }
  saveplot("CI EI Intensity Improvement")
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
saveplot("Sectoral CO2 Emissions RegionsStacked")
ggplot(subset(Q_EMI_SECTORS, t<=10 & n %in% regions & sector=="FFI")) + geom_line(stat="identity", size=1.2, aes(ttoyear(t),value, color=file)) + facet_wrap( ~ n, scales = "free", switch=NULL, ncol=length(regions_plotgrid)) + ylab("GtCO2") + xlab("") + guides(color=guide_legend(title=NULL, nrow = 1)) + theme(legend.position="bottom")
saveplot("Sectoral CO2 Emissions FFI")
ggplot(subset(Q_EMI_SECTORS, t<=10 & n %in% regions & sector=="LU")) + geom_line(stat="identity", size=1.2, aes(ttoyear(t),value, color=file)) + facet_wrap( ~ n, scales = "free", switch=NULL, ncol=length(regions_plotgrid)) + ylab("GtCO2") + xlab("") + guides(color=guide_legend(title=NULL, nrow = 1)) + theme(legend.position="bottom")
saveplot("Sectoral CO2 Emissions LU")
}



