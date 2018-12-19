

Primary_Energy_Mix <- function(PES_y="value", regions="World", years=seq(2005, 2100, 5), plot_type="area", scenplot=scenlist, plot_name="Primary Energy Mix", plot_total_tpes=FALSE){
  if(length(pathdir)!=1){print("PES mix REGIONAL only for one directory at a time!")}else{
    ssp_grid_old=ssp_grid; assign("ssp_grid", FALSE, envir = .GlobalEnv) 
    get_witch_variable("Q_FUEL", "Q_FUEL", "fuel", "all", 0.0036, "EJ", "regional", plot=FALSE)
    get_witch_variable("Q_EN", "Q_EN", "jreal", "all", 0.0036, "EJ", "regional", plot=FALSE)
    assign("ssp_grid", ssp_grid_old, envir = .GlobalEnv) 
    #aggregate sub-categories
    setnames(Q_FUEL,"fuel", "j")
    TPES <- rbind(Q_FUEL, Q_EN)
    TPES <- subset(TPES, j %in% c("oil", "coal", "gas", "uranium", "trbiofuel", "wbio", "advbio", "trbiomass") | j %in% c("elpv", "elcsp", "elhydro_new", "elhydro_old", "elback", "elwindon", "elwindoff"))
    TPES$category[TPES$j %in% c("oil")] = "Oil"
    TPES$category[TPES$j %in% c("gas")] = "Natural Gas"
    TPES$category[TPES$j %in% c("coal")] = "Coal"
    TPES$category[TPES$j %in% c("uranium", "elback")] = "Nuclear"
    TPES$category[TPES$j %in% c("trbiofuel", "wbio", "advbio", "trbiomass")] = "Biomass"
    TPES$category[TPES$j %in% c("elpv", "elcsp")] = "Solar"
    TPES$category[TPES$j %in% c("elhydro_new", "elhydro_old")] = "Hydro"
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
    JFED$csi[is.na(JFED$csi) & JFED$jfed=="elpc_old"] <- 0.45
    JFED$csi[is.na(JFED$csi) & JFED$jfed=="eloil_old"] <- 0.3529
    JFED$csi[is.na(JFED$csi) & JFED$jfed=="elgastr_old"] <- 0.4554
    JFED$csi[is.na(JFED$csi) & JFED$jfed=="elpb_old"] <- 1
    JFED$csi[is.na(JFED$csi)] <- 1
    JFED$value <- JFED$value * JFED$csi
    JFED$csi <- NULL
    JFED$fuel <- NULL
    setnames(JFED, "jfed", "j")
    get_witch_variable("Q_EN", "Q_EN", "jreal", "all", 0.0036, "EJ", "regional", plot=FALSE)
    Q_EN <- subset(Q_EN, j %in% c("elpv", "elcsp", "elnuclear_old", "elnuclear_new", "elwind", "elhydro_new", "elhydro_old"))
    ELEC <- rbind(Q_EN, JFED)
    ELEC[is.na(ELEC)] <- 0 #get rid of NAs to avoid sums not being correct, mainly from historical data!
    #aggregate sub-categories
    ELEC$category[ELEC$j %in% c("elnuclear_old", "elnuclear_new")] = "Nuclear"
    ELEC$category[ELEC$j %in% c("elpv", "elcsp")] = "Solar"
    ELEC$category[ELEC$j %in% c("elhydro_new", "elhydro_old")] = "Hydro"
    ELEC$category[ELEC$j %in% c("elwind")] = "Wind"
    ELEC$category[ELEC$j %in% c("elpb_new", "elpb_old")] = "Biomass w/o CCS"
    ELEC$category[ELEC$j %in% c("elbigcc")] = "Biomass w/ CCS"
    ELEC$category[ELEC$j %in% c("elpc_new", "elpc_old", "elpc_vint")] = "Coal w/o CCS"
    ELEC$category[ELEC$j %in% c("elcigcc", "elpc_ccs", "elpc_oxy")] = "Coal w/ CCS"
    ELEC$category[ELEC$j %in% c("elgastr_new", "elgastr_old")] = "Gas w/o CCS"
    ELEC$category[ELEC$j %in% c("elgasccs")] = "Gas w/ CCS"
    ELEC$category[ELEC$j %in% c("eloil_new", "eloil_old")] = "Oil" 
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

