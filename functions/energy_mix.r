


Primary_Energy_Mix <- function(PES_y="value", restrict_files_pes="."){
  #filelist_old <- filelist
  #assign("filelist", filelist[str_detect(filelist, restrict_files_pes)], envir = .GlobalEnv)
  if(length(pathdir)==10){print("PES mix only for one directory at a time!")}else{
    #tpes(t,n) = sum(f$(sum(jfed,csi(f,jfed,t,n))), Q_PES(f,t,n))+sum(jreal$(not xiny(jreal,jfed)), Q_EN(jreal,t,n));
    ssp_grid_old=ssp_grid; assign("ssp_grid", FALSE, envir = .GlobalEnv) 
    get_witch_variable("Q_PES", "Q_PES", "f", "all", 0.0036, "EJ", "regional", plot=FALSE)
    get_witch_variable("Q_EN", "Q_EN", "jreal", "all", 0.0036, "EJ", "regional", plot=FALSE)
    assign("ssp_grid", ssp_grid_old, envir = .GlobalEnv) 
    #aggregate sub-categories
    setnames(Q_PES,"f", "j")
    TPES <- rbind(Q_PES, Q_EN)
    TPES <- subset(TPES, j %in% c("oil", "coal", "gas", "uranium", "trbiofuel", "wbio", "advbio", "trbiomass") | j %in% c("elpv", "elcsp", "elhydro_new", "elhydro_old", "elback", "nelcoalabat", "elwindon", "elwindoff"))
    TPES$category[TPES$j %in% c("oil")] = "Oil"
    TPES$category[TPES$j %in% c("gas")] = "Natural Gas"
    TPES$category[TPES$j %in% c("coal", "nelcoalabat")] = "Coal"
    TPES$category[TPES$j %in% c("uranium", "elback")] = "Nuclear"
    TPES$category[TPES$j %in% c("trbiofuel", "wbio", "advbio", "trbiomass")] = "Biomass"
    TPES$category[TPES$j %in% c("elpv", "elcsp")] = "Solar"
    TPES$category[TPES$j %in% c("elhydro_new", "elhydro_old")] = "Hydro"
    TPES$category[TPES$j %in% c("elwindon", "elwindoff")] = "Wind"
    
    #order categories for plots
    PES_Categories <- c("Oil", "Coal", "Natural Gas", "Nuclear", "Biomass", "Hydro", "Wind", "Solar")
    TPES <- TPES[order(match(TPES$category,PES_Categories)),]
    
    TPES$j <- NULL
    #get global picture for now
    TPES <- TPES[, lapply(.SD, sum), by=c("t", "file", "pathdir", "category")]
    TPES$n <- NULL
    #Plot PES over time
    assign("PES_MIX",TPES,envir = .GlobalEnv)
    if(PES_y=="share"){TPES <- ddply(TPES, c("t", "file", "pathdir"), transform, value=value/(sum(value))*100)}
    p <- ggplot(data=subset(TPES, t<=yeartot(yearmax) & t>=yeartot(yearmin) & str_detect(file, restrict_files_pes)),aes(ttoyear(t),value, fill=category)) + geom_area(stat="identity") + ylab("EJ") + xlab("") + guides(fill=guide_legend(title=NULL, nrow = 1)) + theme(legend.position="bottom") +  scale_fill_manual(values=c("green", "black", "blue", "chocolate2", "red", "brown", "yellow", "gold1")) 
    if(length(pathdir)!=1){p <- p + facet_grid(pathdir ~ file)}else{p <- p + facet_grid(. ~ file)}
    legend_position_old = legend_position; assign("legend_position", "bottom", envir = .GlobalEnv)
    saveplot("Primary Energy Mix")
    assign("legend_position", legend_position_old, envir = .GlobalEnv) 
    #now get also normal graph of total PES
    TPES <- PES_MIX  
    TPES$category <- as.factor(TPES$category)
    if(length(pathdir)>=1){TPES <- TPES[, lapply(.SD, sum), by=c("t", "file", "pathdir")]}
    else{TPES <- TPES[, lapply(.SD, sum), by=c("t", "file")]}
    if(ssp_grid){TPES <- ssptriple(TPES); line_colour = "rcp"; line_type="spa"}else{line_colour = "file"; line_type="pathdir"}
    p <- ggplot(data=subset(TPES),aes(ttoyear(t),value, colour=get(line_colour), linetype=get(line_type))) + geom_line(stat="identity", size=1.5) + xlab("year") +ylab("EJ") + labs(linetype=line_type, colour=line_colour)
    if(show_numbers_2100){p + geom_text(data=subset(TPES, t==20), aes(x=2100, y=value, label=format(value, digits=2)),  size=3)}
    if(ssp_grid){p <- p + facet_grid(. ~ ssp)}
    if(length(pathdir)!=1){p <- p + facet_grid(pathdir ~ .)}
    if(length(pathdir)!=1 & ssp_grid){p <- p + facet_grid(pathdir ~ ssp)}
    saveplot("PES Total")
  }
  #assign("filelist", filelist_old, envir = .GlobalEnv)
}





Primary_Energy_Mix_Regional <- function(PES_y="value", restrict_files_pes=".", regions=witch_regions, years=seq(2005, 2100, 5), plot_type="area", plot_name="Primary Energy Mix Regional"){
  #filelist_old <- filelist
  #assign("filelist", filelist[str_detect(filelist, restrict_files_pes)], envir = .GlobalEnv)
  if(length(pathdir)!=1){print("PES mix REGIONAL only for one directory at a time!")}else{
    #tpes(t,n) = sum(f$(sum(jfed,csi(f,jfed,t,n))), Q_PES(f,t,n))+sum(jreal$(not xiny(jreal,jfed)), Q_EN(jreal,t,n));
    ssp_grid_old=ssp_grid; assign("ssp_grid", FALSE, envir = .GlobalEnv) 
    get_witch_variable("Q_PES", "Q_PES", "f", "all", 0.0036, "EJ", "regional", plot=FALSE)
    get_witch_variable("Q_EN", "Q_EN", "jreal", "all", 0.0036, "EJ", "regional", plot=FALSE)
    assign("ssp_grid", ssp_grid_old, envir = .GlobalEnv) 
    #aggregate sub-categories
    setnames(Q_PES,"f", "j")
    TPES <- rbind(Q_PES, Q_EN)
    TPES <- subset(TPES, n %in% regions)
    TPES <- subset(TPES, j %in% c("oil", "coal", "gas", "uranium", "trbiofuel", "wbio", "advbio", "trbiomass") | j %in% c("elpv", "elcsp", "elhydro_new", "elhydro_old", "elback", "nelcoalabat", "elwindon", "elwindoff"))
    TPES$category[TPES$j %in% c("oil")] = "Oil"
    TPES$category[TPES$j %in% c("gas")] = "Natural Gas"
    TPES$category[TPES$j %in% c("coal", "nelcoalabat")] = "Coal"
    TPES$category[TPES$j %in% c("uranium", "elback")] = "Nuclear"
    TPES$category[TPES$j %in% c("trbiofuel", "wbio", "advbio", "trbiomass")] = "Biomass"
    TPES$category[TPES$j %in% c("elpv", "elcsp")] = "Solar"
    TPES$category[TPES$j %in% c("elhydro_new", "elhydro_old")] = "Hydro"
    TPES$category[TPES$j %in% c("elwindon", "elwindoff")] = "Wind"
    
    #order categories for plots
    PES_Categories <- c("Oil", "Coal", "Natural Gas", "Nuclear", "Biomass", "Hydro", "Wind", "Solar")
    TPES <- TPES[order(match(TPES$category,PES_Categories)),]
    
    TPES$j <- NULL
    #get global picture for now
    TPES <- TPES[, lapply(.SD, sum), by=c("t", "file", "pathdir", "n", "category")]
    #TPES$n <- NULL
    #Plot PES over time
    assign("PES_MIX",TPES,envir = .GlobalEnv)
    if(PES_y=="share"){TPES <- ddply(TPES, c("t", "file", "n", "pathdir"), transform, value=value/(sum(value))*100)}
    p <- ggplot(data=subset(TPES, ttoyear(t) %in% years & str_detect(file, restrict_files_pes)),aes(ttoyear(t),value, fill=category))
    if(plot_type=="area"){p <- p + geom_area(stat="identity")}else{p <- p + geom_bar(stat="identity") + scale_x_continuous(breaks=years)}
    p <- p + ylab("EJ") + xlab("") + guides(fill=guide_legend(title=NULL, nrow = 1)) + theme(legend.position="bottom") +  scale_fill_manual(values=c("green", "black", "blue", "chocolate2", "red", "brown", "yellow", "gold1"))     
    p <- p + facet_grid(n ~ file, scales="free")
    
    legend_position_old = legend_position; assign("legend_position", "bottom", envir = .GlobalEnv)
    saveplot(plot_name)
    assign("legend_position", legend_position_old, envir = .GlobalEnv) 
  }
  #assign("filelist", filelist_old, envir = .GlobalEnv)
}






Electricity_Mix <- function(Electricity_y="value", restrict_files_elec="."){
  #filelist_old <- filelist
  #assign("filelist", filelist[str_detect(filelist, restrict_files_pes)], envir = .GlobalEnv)
  if(length(pathdir)==10){print("PES mix only for one directory at a time!")}else{
    ssp_grid_old=ssp_grid; assign("ssp_grid", FALSE, envir = .GlobalEnv) 
    get_witch_simple("Q_IN"); Q_IN$value <- Q_IN$value * 0.0036 
    get_witch_simple("csi")
    setnames(csi, "value", "csi")
    JFED <- merge(Q_IN, csi, by = c("t", "n", "file", "pathdir", "f", "jfed"), all=TRUE)
    #take efficiency for EL into account
    #add csi for historical (seems to be 1!)
    JFED$csi[is.na(JFED$csi) & JFED$jfed=="elpc_old"] <- 0.45
    JFED$csi[is.na(JFED$csi) & JFED$jfed=="eloil_old"] <- 0.3529
    JFED$csi[is.na(JFED$csi) & JFED$jfed=="elgastr_old"] <- 0.4554
    JFED$csi[is.na(JFED$csi) & JFED$jfed=="elpb_old"] <- 1
    JFED$csi[is.na(JFED$csi)] <- 1
    
    JFED$value <- JFED$value * JFED$csi
    JFED$csi <- NULL
    JFED$f <- NULL
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
    ELEC$category[ELEC$j %in% c("elpc_new", "elpc_old")] = "Coal w/o CCS"
    ELEC$category[ELEC$j %in% c("elcigcc")] = "Coal w/ CCS"
    ELEC$category[ELEC$j %in% c("elgastr_new", "elgastr_old")] = "Gas w/o CCS"
    ELEC$category[ELEC$j %in% c("elgasccs")] = "Gas w/ CCS"
    ELEC$category[ELEC$j %in% c("eloil_new", "eloil_old")] = "Oil" 
    
    #remove other categories, important!!!
    ELEC <- subset(ELEC, !is.na(ELEC$category))
    #order categories for plots
    Electricity_Categories <- c("Coal w/o CCS", "Coal w/ CCS", "Gas w/o CCS", "Gas w/ CCS", "Oil", "Nuclear", "Biomass w/o CCS", "Biomass w/ CCS", "Hydro", "Wind", "Solar")
    ELEC <- ELEC[order(match(ELEC$category,Electricity_Categories)),]
    
    ELEC$j <- NULL
    #get global picture for now
    ELEC <- ELEC[, lapply(.SD, sum), by=c("t", "file", "pathdir", "category")]
    ELEC$n <- NULL
    #Plot PES over time
    assign("ELEC_MIX",ELEC,envir = .GlobalEnv)
    if(Electricity_y=="share"){ELEC <- ddply(ELEC, c("t", "file", "pathdir"), transform, value=value/(sum(value))*100)}
    p <- ggplot(data=subset(ELEC, t<=yeartot(yearmax) & str_detect(file, restrict_files_elec)),aes(ttoyear(t),value, fill=category)) + geom_area(stat="identity") + ylab("EJ") + xlab("") + guides(fill=guide_legend(title=NULL, nrow = 2)) + theme(legend.position="bottom") +  scale_fill_manual(values=c("Solar"="yellow", "Hydro"="blue", "Nuclear"="red", "Wind"="orange", "Coal w/ CCS"="dimgrey", "Coal w/o CCS"="black", "Gas w/ CCS"="brown", "Gas w/o CCS"="brown2", "Oil"="darkorchid4", "Biomass w/ CCS"="green",  "Biomass w/o CCS"="darkgreen"))
    if(length(pathdir)!=1){p <- p + facet_grid(pathdir ~ file)}else{p <- p + facet_grid(. ~ file)}
    legend_position_old = legend_position; assign("legend_position", "bottom", envir = .GlobalEnv)
    saveplot("Electricity Mix")
    assign("legend_position", legend_position_old, envir = .GlobalEnv) 
  }
  assign("ssp_grid", ssp_grid_old, envir = .GlobalEnv) 
  #assign("filelist", filelist_old, envir = .GlobalEnv)
}


Electricity_Mix_Regional <- function(Electricity_y="value", restrict_files_elec=".", regions=witch_regions, years=seq(2005, 2100, 5), plot_type="area", plot_name="Electricity Mix Regional"){
  #filelist_old <- filelist
  #assign("filelist", filelist[str_detect(filelist, restrict_files_pes)], envir = .GlobalEnv)
  if(length(pathdir)!=1){print("Electricity mix only for one directory at a time!")}else{
    ssp_grid_old=ssp_grid; assign("ssp_grid", FALSE, envir = .GlobalEnv) 
    get_witch_simple("Q_IN"); Q_IN$value <- Q_IN$value * 0.0036 
    get_witch_simple("csi")
    setnames(csi, "value", "csi")
    JFED <- merge(Q_IN, csi, by = c("t", "n", "file", "pathdir", "f", "jfed"), all=TRUE)
    #take efficiency for EL into account
    #add csi for historical (seems to be 1!)
    JFED$csi[is.na(JFED$csi) & JFED$jfed=="elpc_old"] <- 0.45
    JFED$csi[is.na(JFED$csi) & JFED$jfed=="eloil_old"] <- 0.3529
    JFED$csi[is.na(JFED$csi) & JFED$jfed=="elgastr_old"] <- 0.4554
    JFED$csi[is.na(JFED$csi) & JFED$jfed=="elpb_old"] <- 1
    JFED$csi[is.na(JFED$csi)] <- 1
    
    JFED$value <- JFED$value * JFED$csi
    JFED$csi <- NULL
    JFED$f <- NULL
    setnames(JFED, "jfed", "j")
    
    get_witch_variable("Q_EN", "Q_EN", "jreal", "all", 0.0036, "EJ", "regional", plot=FALSE)
    Q_EN <- subset(Q_EN, j %in% c("elpv", "elcsp", "elnuclear_old", "elnuclear_new", "elwind", "elhydro_new", "elhydro_old"))
    
    ELEC <- rbind(Q_EN, JFED)
    
    ELEC[is.na(ELEC)] <- 0 #get rid of NAs to avoid sums not being correct, mainly from historical data!
    
    ELEC <- subset(ELEC, n %in% regions)
    #aggregate sub-categories
    ELEC$category[ELEC$j %in% c("elnuclear_old", "elnuclear_new")] = "Nuclear"
    ELEC$category[ELEC$j %in% c("elpv", "elcsp")] = "Solar"
    ELEC$category[ELEC$j %in% c("elhydro_new", "elhydro_old")] = "Hydro"
    ELEC$category[ELEC$j %in% c("elwind")] = "Wind"
    ELEC$category[ELEC$j %in% c("elpb_new", "elpb_old")] = "Biomass w/o CCS"
    ELEC$category[ELEC$j %in% c("elbigcc")] = "Biomass w/ CCS"
    ELEC$category[ELEC$j %in% c("elpc_new", "elpc_old")] = "Coal w/o CCS"
    ELEC$category[ELEC$j %in% c("elcigcc")] = "Coal w/ CCS"
    ELEC$category[ELEC$j %in% c("elgastr_new", "elgastr_old")] = "Gas w/o CCS"
    ELEC$category[ELEC$j %in% c("elgasccs")] = "Gas w/ CCS"
    ELEC$category[ELEC$j %in% c("eloil_new", "eloil_old")] = "Oil" 
    
    #remove other categories, important!!!
    ELEC <- subset(ELEC, !is.na(ELEC$category))
    #order categories for plots
    Electricity_Categories <- c("Coal w/o CCS", "Coal w/ CCS", "Gas w/o CCS", "Gas w/ CCS", "Oil", "Nuclear", "Biomass w/o CCS", "Biomass w/ CCS", "Hydro", "Wind", "Solar")
    ELEC <- ELEC[order(match(ELEC$category,Electricity_Categories)),]
    
    ELEC$j <- NULL
    #get global picture for now
    ELEC <- ELEC[, lapply(.SD, sum), by=c("t", "file", "pathdir", "n", "category")]
    #ELEC$n <- NULL
    #Plot PES over time
    assign("ELEC_MIX",ELEC,envir = .GlobalEnv)
    if(Electricity_y=="share"){ELEC <- ddply(ELEC, c("t", "file", "n", "pathdir"), transform, value=value/(sum(value))*100)}
    p <- ggplot(data=subset(ELEC, ttoyear(t) %in% years & str_detect(file, restrict_files_elec)),aes(ttoyear(t),value, fill=category))
    p <- p + ylab("EJ") + xlab("") + guides(fill=guide_legend(title=NULL, nrow = 2)) + theme(legend.position="bottom") +  scale_fill_manual(values=c("Solar"="yellow", "Hydro"="blue", "Nuclear"="cyan", "Wind"="orange", "Coal w/ CCS"="dimgrey", "Coal w/o CCS"="black", "Gas w/ CCS"="brown2", "Gas w/o CCS"="brown", "Oil"="darkorchid4", "Biomass w/ CCS"="green",  "Biomass w/o CCS"="darkgreen"))
    if(plot_type=="area"){p <- p + geom_area(stat="identity")}else{p <- p + geom_bar(stat="identity") + scale_x_continuous(breaks=years)}
    p <- p + facet_grid(n ~ file, scales="free")
    legend_position_old = legend_position; assign("legend_position", "bottom", envir = .GlobalEnv)
    saveplot(plot_name)
    assign("legend_position", legend_position_old, envir = .GlobalEnv) 
  }
  #assign("filelist", filelist_old, envir = .GlobalEnv)
}

