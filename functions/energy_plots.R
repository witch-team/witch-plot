

Primary_Energy_Mix <- function(PES_y="value", regions="World", years=seq(2005, 2100, 5), plot_type="area", scenplot=scenlist, plot_name="Primary Energy Mix", plot_total_tpes=FALSE){
  if(length(pathdir)!=1){print("PES mix REGIONAL only for one directory at a time!")}else{
    ssp_grid_old=ssp_grid; assign("ssp_grid", FALSE, envir = .GlobalEnv) 
    get_witch_variable("Q_FUEL", "Q_FUEL", "fuel", "all", 0.0036, "EJ", "regional", plot=FALSE)
    get_witch_variable("Q_EN", "Q_EN", "jreal", "all", 0.0036, "EJ", "regional", plot=FALSE)
    assign("ssp_grid", ssp_grid_old, envir = .GlobalEnv) 
    #aggregate sub-categories
    setnames(Q_FUEL,"fuel", "j")
    TPES <- rbind(Q_FUEL, Q_EN)
    TPES <- subset(TPES, j %in% c("oil", "coal", "gas", "uranium", "trbiofuel", "wbio", "advbio", "trbiomass") | j %in% c("elpv", "elcsp", "elhydro", "elback", "elwindon", "elwindoff"))
    TPES$category[TPES$j %in% c("oil")] = "Oil"
    TPES$category[TPES$j %in% c("gas")] = "Natural Gas"
    TPES$category[TPES$j %in% c("coal")] = "Coal"
    TPES$category[TPES$j %in% c("uranium", "elback")] = "Nuclear"
    TPES$category[TPES$j %in% c("trbiofuel", "wbio", "advbio", "trbiomass")] = "Biomass"
    TPES$category[TPES$j %in% c("elpv", "elcsp")] = "Solar"
    TPES$category[TPES$j %in% c("elhydro")] = "Hydro"
    TPES$category[TPES$j %in% c("elwindon", "elwindoff")] = "Wind"
    #order categories for plots
    PES_Categories <- c("Oil", "Coal", "Natural Gas", "Nuclear", "Biomass", "Hydro", "Wind", "Solar")
    TPES <- TPES[order(match(TPES$category,PES_Categories)),]
    TPES$j <- NULL
    TPES <- TPES[, lapply(.SD, sum), by=c("t", "file", "pathdir", "n", "category")]
    if(regions[1]=="World"){
      TPES$n <- NULL; TPES <- TPES[, lapply(.SD, sum), by=c("t", "file", "pathdir", "category")]; TPES$n <- "World"
    }else{
      TPES <- subset(TPES, n %in% regions)
    }
    assign("PES_MIX",TPES,envir = .GlobalEnv)
    if(PES_y=="share"){TPES <- ddply(TPES, c("t", "file", "n", "pathdir"), transform, value=value/(sum(value))*100)}
    p <- ggplot(data=subset(TPES, ttoyear(t) %in% years & file %in% scenplot))
    if(plot_type=="area"){
      p <- p + geom_area(aes(ttoyear(t),value, fill=category), stat="identity") + scale_fill_manual(values=c("green", "black", "blue", "chocolate2", "red", "brown", "yellow", "gold1"))
    }else if(plot_type=="bar"){
      p <- p + geom_bar(aes(ttoyear(t),value, fill=category), stat="identity") + scale_fill_manual(values=c("green", "black", "blue", "chocolate2", "red", "brown", "yellow", "gold1")) #+ scale_x_continuous(breaks=years)
    }else if(plot_type=="line"){
      p <- p + geom_line(aes(ttoyear(t),value, color=category), stat="identity", size=2) + scale_color_manual(values=c("green", "black", "blue", "chocolate2", "red", "brown", "yellow", "gold1"))
    }
    p <- p + xlab("") + guides(fill=guide_legend(title=NULL, nrow = 1)) + theme(legend.position="bottom")   
    p <- p + facet_grid(n ~ file, scales="free")
    if(PES_y=="share"){p <- p + ylab("%")}else{p <- p + ylab("EJ")}
    legend_position_old = legend_position; assign("legend_position", "bottom", envir = .GlobalEnv)
    saveplot(plot_name,plotdata=subset(TPES, ttoyear(t) %in% years & file %in% scenplot))
    assign("legend_position", legend_position_old, envir = .GlobalEnv) 
  }
  if(plot_total_tpes){
  get_witch_variable("tpes", "Primary_Energy", "na", "na", 0.0036, "EJ", "regional", plot = FALSE)
  ggplot(subset(tpes, ttoyear(t)<=yearmax & n %in% regions & file %in% scenplot)) + geom_line(stat="identity", size=1.2, aes(ttoyear(t),value, color=file)) + facet_wrap( ~ n, scales = "free", switch=NULL, ncol=length(regions)) + ylab("EJ") + xlab("") + guides(color=guide_legend(title=NULL, nrow = 1)) + theme(legend.position="bottom")
  saveplot("Primary Energy Regional", plotdata=subset(tpes, ttoyear(t)<=yearmax & n %in% regions & file %in% scenplot))
  }
}





Electricity_Mix <- function(Electricity_y="value", regions="World", years=seq(2005, 2100, 5), plot_type="area", plot_name="Electricity Mix", scenplot=scenlist){
  if(length(pathdir)!=1){print("Electricity mix only for one directory at a time!")}else{
    ssp_grid_old=ssp_grid; assign("ssp_grid", FALSE, envir = .GlobalEnv) 
    get_witch_simple("Q_IN"); Q_IN$value <- Q_IN$value * 0.0036 
    get_witch_simple("csi")
    setnames(csi, "value", "csi")
    JFED <- merge(Q_IN, csi, by = c("t", "n", "file", "pathdir", "fuel", "jfed"), all=TRUE)
    #take efficiency for EL into account
    #add csi for historical (seems to be 1!)
    JFED$csi[is.na(JFED$csi) & JFED$jfed=="elpc"] <- 0.45
    JFED$csi[is.na(JFED$csi) & JFED$jfed=="eloil"] <- 0.3529
    JFED$csi[is.na(JFED$csi) & JFED$jfed=="elgastr"] <- 0.4554
    JFED$csi[is.na(JFED$csi) & JFED$jfed=="elpb"] <- 1
    JFED$csi[is.na(JFED$csi)] <- 1
    JFED$value <- JFED$value * JFED$csi
    JFED$csi <- NULL
    JFED$fuel <- NULL
    setnames(JFED, "jfed", "j")
    get_witch_variable("Q_EN", "Q_EN", "jreal", "all", 0.0036, "EJ", "regional", plot=FALSE)
    Q_EN <- subset(Q_EN, j %in% c("elpv", "elcsp", "elnuclear", "elnuclear", "elwind", "elhydro", "elhydro"))
    ELEC <- rbind(Q_EN, JFED)
    ELEC[is.na(ELEC)] <- 0 #get rid of NAs to avoid sums not being correct, mainly from historical data!
    #aggregate sub-categories
    ELEC$category[ELEC$j %in% c("elnuclear")] = "Nuclear"
    ELEC$category[ELEC$j %in% c("elpv", "elcsp")] = "Solar"
    ELEC$category[ELEC$j %in% c("elhydro")] = "Hydro"
    ELEC$category[ELEC$j %in% c("elwind")] = "Wind"
    ELEC$category[ELEC$j %in% c("elpb")] = "Biomass w/o CCS"
    ELEC$category[ELEC$j %in% c("elbigcc")] = "Biomass w/ CCS"
    ELEC$category[ELEC$j %in% c("elpc")] = "Coal w/o CCS"
    ELEC$category[ELEC$j %in% c("elcigcc", "elpc_ccs", "elpc_oxy")] = "Coal w/ CCS"
    ELEC$category[ELEC$j %in% c("elgastr")] = "Gas w/o CCS"
    ELEC$category[ELEC$j %in% c("elgasccs")] = "Gas w/ CCS"
    ELEC$category[ELEC$j %in% c("eloil")] = "Oil" 
    #remove other categories, important!!!
    ELEC <- subset(ELEC, !is.na(ELEC$category))
    #order categories for plots
    Electricity_Categories <- c("Coal w/o CCS", "Coal w/ CCS", "Gas w/o CCS", "Gas w/ CCS", "Oil", "Nuclear", "Biomass w/o CCS", "Biomass w/ CCS", "Hydro", "Wind", "Solar")
    ELEC <- ELEC[order(match(ELEC$category,Electricity_Categories)),]
    ELEC$j <- NULL
    ELEC <- ELEC[, lapply(.SD, sum), by=c("t", "file", "pathdir", "n", "category")]
    if(regions[1]=="World"){
      ELEC$n <- NULL; ELEC <- ELEC[, lapply(.SD, sum), by=c("t", "file", "pathdir", "category")]; ELEC$n <- "World"
    }else{
      ELEC <- subset(ELEC, n %in% regions)
    }
    assign("ELEC_MIX",ELEC,envir = .GlobalEnv)
    if(Electricity_y=="share"){ELEC <- ddply(ELEC, c("t", "file", "n", "pathdir"), transform, value=value/(sum(value))*100)}
    p <- ggplot(data=subset(ELEC, ttoyear(t) %in% years  & file %in% scenplot))
    p <- p + xlab("") + guides(fill=guide_legend(title=NULL, nrow = 2)) + theme(legend.position="bottom")
    if(plot_type=="area"){
      p <- p + geom_area(aes(ttoyear(t),value, fill=category), stat="identity") + scale_fill_manual(values=c("Solar"="yellow", "Hydro"="blue", "Nuclear"="cyan", "Wind"="orange", "Coal w/ CCS"="dimgrey", "Coal w/o CCS"="black", "Gas w/ CCS"="brown2", "Gas w/o CCS"="brown", "Oil"="darkorchid4", "Biomass w/ CCS"="green",  "Biomass w/o CCS"="darkgreen"))
    }else if(plot_type=="bar"){
      p <- p + geom_bar(aes(ttoyear(t),value, fill=category), stat="identity") +scale_fill_manual(values=c("Solar"="yellow", "Hydro"="blue", "Nuclear"="cyan", "Wind"="orange", "Coal w/ CCS"="dimgrey", "Coal w/o CCS"="black", "Gas w/ CCS"="brown2", "Gas w/o CCS"="brown", "Oil"="darkorchid4", "Biomass w/ CCS"="green",  "Biomass w/o CCS"="darkgreen")) #+ scale_x_continuous(breaks=years)
    }else if(plot_type=="line"){
      p <- p + geom_line(aes(ttoyear(t),value, color=category), stat="identity", size=2) + scale_color_manual(values=c("Solar"="yellow", "Hydro"="blue", "Nuclear"="cyan", "Wind"="orange", "Coal w/ CCS"="dimgrey", "Coal w/o CCS"="black", "Gas w/ CCS"="brown2", "Gas w/o CCS"="brown", "Oil"="darkorchid4", "Biomass w/ CCS"="green",  "Biomass w/o CCS"="darkgreen"))
    }
    p <- p + facet_grid(n ~ file, scales="free")
    if(Electricity_y=="share"){p <- p + ylab("%")}else{p <- p + ylab("EJ")}
    legend_position_old = legend_position; assign("legend_position", "bottom", envir = .GlobalEnv)
    saveplot(plot_name, plotdata=subset(ELEC, ttoyear(t) %in% years  & file %in% scenplot))
    assign("legend_position", legend_position_old, envir = .GlobalEnv) 
  }
}





Energy_Trade <- function(fuelplot="oil", scenplot=scenlist, add_value=F){
  get_witch_variable("Q_OUT", "Extraction", "f", fuelplot, 1, "TWh", "regional", plot = T)
  get_witch_variable("Q_FUEL", "Consumption", "fuel", fuelplot, 1, "TWh", "regional", plot = T)
  NET_EXPORT <- Q_OUT
  setnames(NET_EXPORT, "value", "Extraction")
  NET_EXPORT <- merge(NET_EXPORT, Q_FUEL, by = c("t", "n", "file", "pathdir"))
  setnames(NET_EXPORT, "value", "Consumption")
  Energy_Prices(scenplot=scenlist)
  #get common time horizon
  t_common <- intersect(unique(Q_OUT$t),unique(Q_FUEL$t)) #, Energy_Price_Data$t
  NET_EXPORT <- subset(NET_EXPORT, t %in% t_common)
  Energy_Price_Data <- subset(Energy_Price_Data, t %in% t_common & fuel==fuelplot);Energy_Price_Data$fuel <- NULL
  NET_EXPORT <- merge(NET_EXPORT, Energy_Price_Data, by = c("t", "file"), all.x = TRUE)
  #volume in EJ, prices in $/GJ, value in billion USD
  NET_EXPORT$Net_Export_Volume <- (NET_EXPORT$Extraction - NET_EXPORT$Consumption) * 0.0036
  NET_EXPORT$Net_Export_Value <- ((NET_EXPORT$Extraction - NET_EXPORT$Consumption) * NET_EXPORT$energy_price) * 1e3
  NET_EXPORT$pathdir <- NULL
  #NET_EXPORT$fuel <- NULL
  NET_EXPORT <- NET_EXPORT[!is.na(energy_price)]
  NET_EXPORT <- melt(NET_EXPORT, id.vars = c("t", "n", "file"))
  ggplot(subset(NET_EXPORT, file %in% scenplot & variable %in% c("Net_Export_Volume")),aes(ttoyear(t),value, fill=n)) + geom_area(stat="identity") + facet_grid(. ~ file, scales = "free") + ylab("EJ") + xlab("") + guides(fill=guide_legend(title=NULL, nrow = 2)) + theme(legend.position="bottom") + scale_fill_manual(values = region_palette)
  if(add_value){
    ggplot(subset(NET_EXPORT, file %in% scenplot & variable %in% c("Net_Export_Volume", "Net_Export_Value")),aes(ttoyear(t),value, fill=n)) + geom_area(stat="identity") + facet_grid(variable ~ file, scales = "free") + ylab("billion USD / EJ") + xlab("") + guides(fill=guide_legend(title=NULL, nrow = 2)) + theme(legend.position="bottom") + scale_fill_manual(values = region_palette)
  }
  saveplot(paste("Energy Trade:", fuelplot), plotdata = NET_EXPORT)
}









Investment_Plot <- function(regions=witch_regions, scenplot=scenlist){
  if(regions[1]=="World") regions <- witch_regions
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
  plot_rd <- ggplot(subset(I_RD, ttoyear(t)<=yearmax & n %in% regions)) + geom_line(stat="identity", size=1.2, aes(ttoyear(t),value*1e3, linetype=rd, color=file)) + facet_wrap( ~ n, scales = "free", switch=NULL, ncol=length(regions)) + ylab("Billion USD") + xlab("") + guides(color=guide_legend(title=NULL, nrow = 1)) + theme(legend.position="bottom") + guides(linetype=guide_legend(title=NULL)) 
  #saveplot("Investment in RnD", plotdata=subset(I_RD, ttoyear(t)<=yearmax & n %in% regions))
  #Investment in Energy Supply
  plot_supply <- ggplot(subset(I_EN, ttoyear(t)<=yearmax & n %in% regions)) + geom_line(stat="identity", size=1.2, aes(ttoyear(t),value*1e3, color=file)) + facet_wrap( ~ n, scales = "free", switch=NULL, ncol=length(regions)) + ylab("Billion USD") + xlab("") + guides(color=guide_legend(title=NULL, nrow = 1)) + theme(legend.position="bottom") + guides(linetype=guide_legend(title=NULL)) 
  #saveplot("Investment in Energy Supply", plotdata=subset(I_EN, ttoyear(t)<=yearmax & n %in% regions))
  #now get global energy investment picture
  #I_EN <- NULL
  #get_witch_simple("I_EN", scenplot = scenlist)
  I_EN <- I_EN_orig
  get_witch_simple("I_EN_GRID", scenplot = scenplot)
  I_EN_Renewables <- aggregate(value~n+t+file+pathdir, data=subset(I_EN, jinv %in% c("elpv", "elcsp", "elwindon", "elwindoff", "elhydro")), sum);I_EN_Renewables$category <- "Renewables"
  I_EN_FossilFuels <- aggregate(value~n+t+file+pathdir, data=subset(I_EN, jinv %in% c("elpc", "eloil", "elgastr", "elpb")), sum);I_EN_FossilFuels$category <- "Fossil Fuels"
  I_EN_Nuclear <- aggregate(value~n+t+file+pathdir, data=subset(I_EN, jinv %in% c("elnuclear")), sum);I_EN_Nuclear$category <- "Nuclear"
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
  get_witch_simple("I_OUT", scenplot = scenplot)
  I_OUT <- subset(I_OUT, f=="oil");setnames(I_OUT, "f", "category")
  I_OUT$category <- "Oil Extraction"
  setnames(I_RD, "rd", "category")
  I_OUT$sector <- "Fuel supply"; I_EN_categorized$sector <- "Power supply"; I_RD$sector <- "Energy RnD"
  Investment_Energy <- rbind(I_EN_categorized, I_RD, I_OUT)
  Investment_Energy <- subset(Investment_Energy, t>=3 & t<=10)
  Investment_Energy_global <- aggregate(value~sector+category+file+pathdir, data=subset(Investment_Energy, n %in% regions), sum)  
  Investment_Energy_global$value <-   Investment_Energy_global$value*5 
  
  ggplot(subset(Investment_Energy_global),aes(file,value, fill=category)) + geom_bar(stat="identity", position = "stack") + ylab("Trillion USD annually, (2015-2050)") + xlab("") + guides(fill=guide_legend(title=NULL, nrow = 2)) + theme(legend.position="bottom") + facet_wrap( ~ sector, scales = "fixed")  + scale_x_discrete(limits=scenplot) + scale_fill_brewer(palette="Spectral")
  #+ scale_fill_manual(values=c("#0000FF", "#000066", "#FFFF00", "#666600","#00FF00", "#006600", "#FF0000", "#660000"))
  saveplot("Investment Plot Global", plotdata=Investment_Energy_global)
}




