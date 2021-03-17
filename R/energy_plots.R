

Primary_Energy_Mix <- function(PES_y="value", regions="World", years=seq(yearmin, yearmax), plot_type="area", scenplot=scenlist, plot_name="Primary Energy Mix", plot_total_tpes=FALSE){
  if(length(fullpathdir)!=1){print("PES mix REGIONAL only for one directory at a time!")}else{
    ssp_grid_old=ssp_grid; assign("ssp_grid", FALSE, envir = .GlobalEnv) 
    get_witch_simple("Q_FUEL"); Q_FUEL_pes <- Q_FUEL %>% mutate(value=value*0.0036)
    get_witch_simple("Q_EN"); Q_EN_pes <- Q_EN %>% mutate(value=value*0.0036)
    assign("ssp_grid", ssp_grid_old, envir = .GlobalEnv) 
    #aggregate sub-categories
    setnames(Q_FUEL_pes,"fuel", "j")
    TPES <- rbind(Q_FUEL_pes, Q_EN_pes)
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
    TPES <- as.data.table(TPES)[, lapply(.SD, sum), by=c("t", file_group_columns, "pathdir", "n", "category")]
    if(regions[1]=="World"){
      TPES$n <- NULL; TPES <- TPES[, lapply(.SD, sum), by=c("t", file_group_columns, "pathdir", "category")]; TPES$n <- "World"
    }else{
      TPES <- subset(TPES, n %in% regions)
    }
    assign("PES_MIX",TPES,envir = .GlobalEnv)
    if(PES_y=="share"){TPES <- plyr::ddply(TPES, c("t", "file", "n", "pathdir"), transform, value=value/(sum(value))*100)}
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
  get_witch_simple("tpes"); tpes_global <- tpes %>% mutate(value=value*0.0036)
  ggplot(subset(tpes_global, ttoyear(t)<=yearmax & n %in% regions & file %in% scenplot)) + geom_line(stat="identity", size=1.2, aes(ttoyear(t),value, color=file)) + facet_wrap( ~ n, scales = "free", switch=NULL, ncol=length(regions)) + ylab("EJ") + xlab("") + guides(color=guide_legend(title=NULL, nrow = 1)) + theme(legend.position="bottom")
  saveplot("Primary Energy Regional", plotdata=subset(tpes, ttoyear(t)<=yearmax & n %in% regions & file %in% scenplot))
  }
}





Electricity_Mix <- function(Electricity_y="value", regions="World", years=seq(yearmin, yearmax), plot_type="area", plot_name="Electricity Mix", scenplot=scenlist){
  if(length(fullpathdir)!=1){print("Electricity mix only for one directory at a time!")}else{
    ssp_grid_old=ssp_grid; assign("ssp_grid", FALSE, envir = .GlobalEnv) 
    get_witch_simple("Q_IN"); Q_IN_el <- Q_IN %>% mutate(value=value * 0.0036)
    get_witch_simple("csi"); csi_el <- csi %>% rename(csi=value)
    JFED <- merge(Q_IN_el, csi_el, by = c("t", "n", file_group_columns, "pathdir", "fuel", "jfed"), all=TRUE)
    JFED <- JFED %>% filter(jfed %in% c("eloil", "elpb", "elpc", "elgastr", "elbigcc", "elcigcc", "elgasccs", "elpc_ccs", "elpc_oxy"))
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
    get_witch_simple("Q_EN"); Q_EN_pes <- Q_EN %>% mutate(value=value*0.0036)
    Q_EN_pes <- subset(Q_EN_pes, j %in% c("elpv", "elcsp", "elnuclear", "elwind", "elhydro"))
    ELEC <- rbind(Q_EN_pes, JFED)
    ELEC[is.na(ELEC)] <- 0 #get rid of NAs to avoid sums not being correct, mainly from historical data!
    #aggregate sub-categories1
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
    ELEC <- as.data.table(ELEC)[, lapply(.SD, sum), by=c("t", file_group_columns, "pathdir", "n", "category")]
    if(regions[1]=="World"){
      ELEC$n <- NULL; ELEC <- ELEC[, lapply(.SD, sum), by=c("t", file_group_columns, "pathdir", "category")]; ELEC$n <- "World"
    }else{
      ELEC <- subset(ELEC, n %in% regions)
    }
    assign("ELEC_MIX",ELEC,envir = .GlobalEnv)
    if(Electricity_y=="share"){ELEC <- plyr::ddply(ELEC, c("t", file_group_columns, "n", "pathdir"), transform, value=value/(sum(value))*100)}
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
  get_witch_simple("Q_FUEL"); Q_FUEL_trade <- Q_FUEL %>% filter(fuel==fuelplot) %>% select(-fuel)
  get_witch_simple("Q_OUT"); Q_OUT_trade <- Q_OUT %>% filter(f==fuelplot) %>% select(-f)
  NET_EXPORT <- Q_OUT_trade
  setnames(NET_EXPORT, "value", "Extraction")
  NET_EXPORT <- merge(NET_EXPORT, Q_FUEL_trade, by = c("t", "n", file_group_columns, "pathdir"))
  setnames(NET_EXPORT, "value", "Consumption")
  get_witch_simple("FPRICE"); FPRICE_trade <- FPRICE %>% filter(fuel==fuelplot) %>% select(-fuel, -n) %>% rename(energy_price=value)
  NET_EXPORT <- merge(NET_EXPORT, FPRICE_trade, by = c("t", file_group_columns, "pathdir"), all.x = TRUE)
  #volume in EJ, prices in $/GJ, value in billion USD
  NET_EXPORT$Net_Export_Volume <- (NET_EXPORT$Extraction - NET_EXPORT$Consumption) * 0.0036
  NET_EXPORT$Net_Export_Value <- ((NET_EXPORT$Extraction - NET_EXPORT$Consumption) * NET_EXPORT$energy_price) * 1e3
  NET_EXPORT <- NET_EXPORT %>% filter(!is.na(energy_price)) %>% select(-pathdir)
  NET_EXPORT <- melt(NET_EXPORT, id.vars = c("t", "n", file_group_columns))
  ggplot(subset(NET_EXPORT, file %in% scenplot & variable %in% c("Net_Export_Volume")),aes(ttoyear(t),value, fill=n)) + geom_area(stat="identity") + facet_grid(. ~ file, scales = "free") + ylab("EJ") + xlab("") + guides(fill=guide_legend(title=NULL, nrow = 2)) + theme(legend.position="bottom") + scale_fill_manual(values = region_palette)
  if(add_value){
    ggplot(subset(NET_EXPORT, file %in% scenplot & variable %in% c("Net_Export_Volume", "Net_Export_Value")),aes(ttoyear(t),value, fill=n)) + geom_area(stat="identity") + facet_grid(variable ~ file, scales = "free") + ylab("billion USD / EJ") + xlab("") + guides(fill=guide_legend(title=NULL, nrow = 2)) + theme(legend.position="bottom") + scale_fill_manual(values = region_palette)
  }
  saveplot(paste("Energy Trade:", fuelplot), plotdata = NET_EXPORT)
}









Investment_Plot <- function(regions=witch_regions, scenplot=scenlist){
  if(regions[1]=="World") regions <- witch_regions
  get_witch_simple("I_EN"); I_EN_inv <- I_EN
  I_RD_inv = get_witch_simple("I_RD", results = "return")
  I_EN_sum <- I_EN %>% group_by_at(c("pathdir", file_group_columns, "n", "t")) %>% summarize(value=sum(value))
  I_RD_inv$rd  <- mapvalues(I_RD_inv$rd , from=c("en","nelback","battery"), to=c("Energy Efficiency", "Advanced Biofuels", "Batteries"))
  I_RD_inv <- I_RD_inv %>% rename(category="rd") %>% mutate(sector="Energy RnD")
  get_witch_simple("I_EN_GRID")
  I_EN_Renewables <- I_EN %>% group_by_at(c("pathdir", file_group_columns, "n", "t")) %>% filter(jinv %in% c("elpv", "elcsp", "elwindon", "elwindoff", "elhydro")) %>% summarize(value=sum(value)) %>% mutate(category="Renewables")
  I_EN_FossilFuels <- I_EN %>% group_by_at(c("pathdir", file_group_columns, "n", "t")) %>% filter(jinv %in% c("elpc", "eloil", "elgastr", "elpb")) %>% summarize(value=sum(value)) %>% mutate(category="Fossil Fuels")
  I_EN_Nuclear <- I_EN %>% group_by_at(c("pathdir", file_group_columns, "n", "t")) %>% filter(jinv %in% c("elnuclear")) %>% summarize(value=sum(value)) %>% mutate(category="Nuclear")
  I_EN_CCS <- I_EN %>% group_by_at(c("pathdir", file_group_columns, "n", "t")) %>% filter(jinv %in% c("elcigcc", "elgasccs", "elbigcc", "nelcoalccs")) %>% summarize(value=sum(value)) %>% mutate(category="Fossils with CCS")
 I_EN_GRID$jinv <- "grid"; I_EN <- rbind(I_EN, I_EN_GRID)
 I_EN_TDS <- I_EN %>% group_by_at(c("pathdir", file_group_columns, "n", "t")) %>% filter(jinv %in% c("elstorage", "grid")) %>% summarize(value=sum(value)) %>% mutate(category="Grid&Storage")
  I_EN_categorized <- rbind(I_EN_Renewables, I_EN_CCS, I_EN_FossilFuels, I_EN_Nuclear, I_EN_TDS)
  I_EN_categorized$sector <- "Power supply"
  I_TRANSPORT_trad <- I_EN %>% group_by_at(c("pathdir", file_group_columns, "n", "t")) %>% filter(jinv %in% c("trad_cars", "hybrid", "trad_stfr", "hbd_stfr")) %>% summarize(value=sum(value)) %>% mutate(category="ICE/Hybrid")
  I_TRANSPORT_lowcarbon <- I_EN %>% group_by_at(c("pathdir", file_group_columns, "n", "t")) %>% filter(jinv %in% c("edv", "edv_stfr", "plg_hybrid", "plg_hbd_stfr")) %>% summarize(value=sum(value)) %>% mutate(category="Electric Vehicles")
  I_TRANSPORT <- rbind(I_TRANSPORT_trad, I_TRANSPORT_lowcarbon); 
  get_witch_simple("I"); I_inv <- I %>% rename(category=g) %>% mutate(sector="Final Good") %>% filter(category=="fg")
  get_witch_simple("I_OUT"); I_OUT_inv <- I_OUT %>% rename(category=f) %>% filter(category=="oil") %>% mutate(category="Oil Extraction") %>% mutate(sector="Fuel supply")
  Investment_Energy <- rbind(as.data.frame(I_EN_categorized), I_RD_inv, I_OUT_inv, I_inv)
  Investment_Energy <- subset(Investment_Energy, t>=3 & t<=10)
  Investment_Energy_global <- Investment_Energy %>% group_by_at(c("category", "sector", "pathdir", file_group_columns, "t")) %>% filter(n %in% regions) %>% summarize(value=sum(value)*5)
  ggplot(subset(Investment_Energy_global, (file %in% scenplot) & category !="fg"),aes(file,value / (2050-2015) * 1e3, fill=category)) + geom_bar(stat="identity", position = "stack") + ylab("Billion USD annually, (2015-2050)") + xlab("") + guides(fill=guide_legend(title=NULL)) + theme(legend.position="bottom") + facet_wrap( ~ sector, scales = "free")  + scale_x_discrete(limits=scenplot) + scale_fill_brewer(palette="Spectral")
  #+ scale_fill_manual(values=c("#0000FF", "#000066", "#FFFF00", "#666600","#00FF00", "#006600", "#FF0000", "#660000"))
  saveplot("Investment Plot Global", plotdata=Investment_Energy_global)
}




