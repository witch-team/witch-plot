# Compute Policy Costs and Carbon Prices


Policy_Cost <- function(discount_rate=5, tmin=3, tmax=20, bauscen="ssp2_bau", regions="WORLD", show_numbers=TRUE, scenplot=scenlist, measure="GDP", suffix=""){
  if(!(bauscen %in% scenlist)){stop("For policy costs define an existing BAU scenario")}
  get_witch_simple("Q")
  Q$value <- Q$value * usd_deflator    #Apply deflator
  Q <- subset(Q, (t %in% t_model))
  if(measure=="GDP"){GDP <- subset(Q, iq=="y")}
  if(measure=="Consumption"){GDP <- subset(Q, iq=="cc")}
  GDP$iq <- NULL
  bau <- subset(GDP, file==bauscen); setnames(bau, "value", "bau"); bau$file <- NULL
  GDP <- merge(GDP, bau, by=c("n", "t", "pathdir"), all = TRUE)
  GDP$"GDP Loss" <- -(GDP$"value" -GDP$"bau")
  GDP$GDP_Loss_discounted = (GDP$"GDP Loss")*(1+discount_rate/100)^(-(5*(GDP$t-3)))
  GDP$GDP_MER_discounted = (GDP$"value")*(1+discount_rate/100)^(-(5*(GDP$t-3)))
  GDP <- subset(GDP, file %in% scenplot)
  #need factors for sum!!
  GDP$n <- as.factor(GDP$n)
  GDP$t <- as.factor(GDP$t)
  GDP$pathdir <- as.factor(GDP$pathdir)
  GDP$file <- as.factor(GDP$file)
  GDP_WORLD <- GDP; GDP_WORLD$n <- NULL
  GDP_WORLD <- GDP_WORLD[, lapply(.SD, sum), by=c("t", "file", "pathdir")]
  GDP_WORLD$n <- "WORLD"
  GDP <- rbind(GDP, GDP_WORLD)
  GDP$t <- as.numeric(GDP$t)
  #PC over time plot (NOT discounted!)
  PC_annual_relative <- subset(GDP, t<=tmax&t>=tmin); PC_annual_relative$rel_cost <- PC_annual_relative$"GDP Loss"/PC_annual_relative$"bau";
  p <- ggplot(subset(PC_annual_relative, n %in% regions & file!=bauscen)) + geom_line(aes(ttoyear(t), rel_cost*100, color=file), show.legend = TRUE) +ylab(paste("% of", measure)) + xlab("") + theme(legend.position="bottom",legend.direction="horizontal") + guides(fill=guide_legend(title=NULL, nrow = 1))
  if(length(pathdir) > 1){p <- p + facet_grid(. ~ pathdir)}
  if(regions[1] != "WORLD"){p <- p + facet_grid(. ~ n)}
  saveplot(paste0("Policy Cost Yearly (", measure, ")", suffix), plotdata=subset(PC_annual_relative, n %in% regions & file!=bauscen))
  #now compute also discounted NPV value
  Policy_Cost <- subset(GDP, t<=tmax&t>=tmin)[, lapply(.SD, sum), by=c("n", "file", "pathdir") , .SDcols = c("GDP_Loss_discounted", "GDP_MER_discounted")]
  Policy_Cost$PC = 100*Policy_Cost$GDP_Loss_discounted / Policy_Cost$GDP_MER_discounted
  #set negative Policy costs to zero
  #Policy_Cost$PC <- pmax(Policy_Cost$PC, 0)
  assign("POLCOST", Policy_Cost, envir = .GlobalEnv) 
  p <- ggplot(subset(Policy_Cost, n %in% regions & file!=bauscen)) + geom_bar(position=position_dodge(), stat="identity",aes(file, PC, fill=file), show.legend = TRUE) +ylab(paste("% of", measure, "(NPV)")) + xlab("") + theme(legend.position="bottom",legend.direction="horizontal") + guides(fill=guide_legend(title=NULL, nrow = 1))
  if(length(pathdir) > 1){p <- p + facet_grid(. ~ pathdir)}
  if(regions[1] != "WORLD"){p <- p + facet_grid(. ~ n)}
  if(show_numbers){p <- p + geom_text(data=subset(Policy_Cost, n %in% regions & file!=bauscen), aes(x=file, y=PC+0.1, label=paste0(round(PC, 1),"%")))}
  p <- p  + theme(axis.ticks = element_blank(), axis.text.x = element_blank())
  saveplot(paste0("Policy Cost (", measure, ")", suffix), plotdata=subset(Policy_Cost, n %in% regions & file!=bauscen))
}






Policy_Cost_Decomposition <- function(discount_rate=5, tmin=3, tmax=20, bauscen="ssp2_bau", coopbauscen=F, regions="WORLD", show_numbers=TRUE, scenplot=scenlist, measure="GDP", suffix=""){
get_witch_simple("dam_rep")
dam_rep <- dcast(dam_rep, formula = t + n + file + pathdir ~ V3)
get_witch_simple("Q")
if(measure=="GDP"){Q <- subset(Q, iq=="y")}
if(measure=="Consumption"){Q <- subset(Q, iq=="cc")}
Q$iq <- NULL
Q <- subset(Q, t %in% seq(1,30))
#add emission reduction and PES costs to get full gross GDP
get_witch_simple("COST_PES")
get_witch_simple("COST_EMI")
get_witch_simple("SRM_COST"); setnames(SRM_COST, "value", "SRM_COST")
COST_EMI$e <- NULL;
COST_EMI <- COST_EMI[, lapply(.SD, sum), by=c("t", "n", "file", "pathdir")]; setnames(COST_EMI, "value", "COST_EMI")
COST_PES$f <- NULL;
COST_PES <- COST_PES[, lapply(.SD, sum), by=c("t", "n", "file", "pathdir")]; setnames(COST_PES, "value", "COST_PES")
Q <- merge(Q, COST_EMI, by = c("t", "n", "file", "pathdir"))
Q <- merge(Q, COST_PES, by = c("t", "n", "file", "pathdir"))
Q <- merge(Q, SRM_COST, by = c("t", "n", "file", "pathdir"), all = T)
Q$COST <- Q$COST_EMI + Q$COST_PES + Q$SRM_COST
Q$COST_EMI <- NULL; Q$COST_PES <- NULL 
#finished adding COSTs

bau <- subset(Q, file==bauscen); bau$SRM_COST <- NULL; setnames(bau, "value", "bau"); setnames(bau, "COST", "COSTbau"); bau$file <- NULL
Q <- merge(Q, bau, by=c("n", "t", "pathdir"), all = TRUE)
#routine to add BAU COOP for cooperative runs
if(coopbauscen!=FALSE){
coopbaugdxfile <- gdx(coopbauscen)
coopbau_Q <- data.table(coopbaugdxfile["Q"])
if(measure=="GDP"){coopbau_Q <- subset(coopbau_Q, iq=="y")}
if(measure=="Consumption"){coopbau_Q <- subset(coopbau_Q, iq=="cc")}
setnames(coopbau_Q, "value", "coopbau"); coopbau_Q$iq <- NULL
coopbau_COST_EMI <- data.table(coopbaugdxfile["COST_EMI"])
coopbau_COST_PES <- data.table(coopbaugdxfile["COST_PES"])
coopbau_COST_EMI$e <- NULL;
coopbau_COST_EMI <- coopbau_COST_EMI[, lapply(.SD, sum), by=c("t", "n")];  setnames(coopbau_COST_EMI, "value", "COST_EMI")
coopbau_COST_PES$f <- NULL;
coopbau_COST_PES <- coopbau_COST_PES[, lapply(.SD, sum), by=c("t", "n")]; setnames(coopbau_COST_PES, "value", "COST_PES")
coopbau_Q <- merge(coopbau_Q, coopbau_COST_EMI, by = c("t", "n"))
coopbau_Q <- merge(coopbau_Q, coopbau_COST_PES, by = c("t", "n"))
coopbau_Q$COSTcoopbau <- coopbau_Q$COST_EMI + coopbau_Q$COST_PES
coopbau_Q$COST_EMI <- NULL; coopbau_Q$COST_PES <- NULL 
coopbau_Q$t <- as.numeric(coopbau_Q$t);
Q <- merge(Q, coopbau_Q, by=c("n", "t"), all = TRUE)
Q[str_detect(file, "coop")]$bau <- Q[str_detect(file, "coop")]$coopbau
Q[str_detect(file, "coop")]$COSTbau <- Q[str_detect(file, "coop")]$COSTcoopbau
}



#end routine to get coop bau

setnames(Q, "value", "Consumption (net)")   #MEANING : Q.l value
DAM_DECOMP <- merge(dam_rep, Q, by = c("t", "n", "file", "pathdir"))
DAM_DECOMP[is.na(DAM_DECOMP)] <- 0
#compute decomposed impacts
#### THIS ONE: WITHOUT COSt!!!
#DAM_DECOMP$"Consumption (gross)" <- DAM_DECOMP$"Consumption (net)" * (1 + DAM_DECOMP$standard_gross + DAM_DECOMP$gradient_damage + DAM_DECOMP$geoeng)
#DAM_DECOMP$"Mitigation costs" <- DAM_DECOMP$"bau" - DAM_DECOMP$"Consumption (gross)"
#DAM_DECOMP$"Standard Climate impacts" <- (DAM_DECOMP$"Consumption (gross)" - DAM_DECOMP$"Consumption (net)") * DAM_DECOMP$standard_gross / (DAM_DECOMP$standard_gross + DAM_DECOMP$gradient_damage + DAM_DECOMP$geoeng)
#DAM_DECOMP$"Gradient Climate impacts" <- (DAM_DECOMP$"Consumption (gross)" - DAM_DECOMP$"Consumption (net)") * DAM_DECOMP$gradient_damage / (DAM_DECOMP$standard_gross + DAM_DECOMP$gradient_damage + DAM_DECOMP$geoeng)
#DAM_DECOMP$"Geoengineering impacts" <- (DAM_DECOMP$"Consumption (gross)" - DAM_DECOMP$"Consumption (net)") * DAM_DECOMP$geoeng / (DAM_DECOMP$standard_gross + DAM_DECOMP$gradient_damage + DAM_DECOMP$geoeng)
#DAM_DECOMP <- subset(DAM_DECOMP, file %in% scenplot)

DAM_DECOMP$"Consumption (gross)" <- (DAM_DECOMP$"Consumption (net)" + DAM_DECOMP$COST) * (1 + DAM_DECOMP$standard_gross + DAM_DECOMP$gradient_damage + DAM_DECOMP$geoeng) - DAM_DECOMP$COST
DAM_DECOMP$"Mitigation costs" <- (DAM_DECOMP$"bau") - (DAM_DECOMP$"Consumption (gross)") - DAM_DECOMP$SRM_COST
#DAM_DECOMP$"Mitigation costs" <- DAM_DECOMP$"Mitigation costs" - (DAM_DECOMP$COST - DAM_DECOMP$COSTbau)
DAM_DECOMP$"Standard Climate impacts" <- (DAM_DECOMP$"Consumption (gross)" - DAM_DECOMP$"Consumption (net)") * DAM_DECOMP$standard_gross / (DAM_DECOMP$standard_gross + DAM_DECOMP$gradient_damage + DAM_DECOMP$geoeng)
DAM_DECOMP$"Gradient Climate impacts" <- (DAM_DECOMP$"Consumption (gross)" - DAM_DECOMP$"Consumption (net)") * DAM_DECOMP$gradient_damage / (DAM_DECOMP$standard_gross + DAM_DECOMP$gradient_damage + DAM_DECOMP$geoeng)
DAM_DECOMP$"Geoengineering impacts" <- (DAM_DECOMP$"Consumption (gross)" - DAM_DECOMP$"Consumption (net)") * DAM_DECOMP$geoeng / (DAM_DECOMP$standard_gross + DAM_DECOMP$gradient_damage + DAM_DECOMP$geoeng)
DAM_DECOMP$"Geoengineering costs" <- DAM_DECOMP$SRM_COST

DAM_DECOMP <- subset(DAM_DECOMP, file %in% scenplot)
#add world values
DAM_DECOMP$n <- as.factor(DAM_DECOMP$n)
DAM_DECOMP$t <- as.factor(DAM_DECOMP$t)
DAM_DECOMP$pathdir <- as.factor(DAM_DECOMP$pathdir)
DAM_DECOMP$file <- as.factor(DAM_DECOMP$file)
DAM_DECOMP <- as.data.table(DAM_DECOMP)
DAM_DECOMP_WORLD <- DAM_DECOMP; DAM_DECOMP_WORLD$n <- NULL
DAM_DECOMP_WORLD <- DAM_DECOMP_WORLD[, lapply(.SD, sum), by=c("t", "file", "pathdir")]
DAM_DECOMP_WORLD$n <- "WORLD"
DAM_DECOMP <- rbind(DAM_DECOMP, DAM_DECOMP_WORLD)
assign("DAM_DECOMP", DAM_DECOMP, envir = .GlobalEnv)

#now aggregate to NPV discounted values (PC)
DAM_DECOMP_NPV <- DAM_DECOMP
DAM_DECOMP_NPV$t <- as.numeric(DAM_DECOMP_NPV$t)
DAM_DECOMP_NPV$"Mitigation costs" <- (1+discount_rate/100)^(-(5*(DAM_DECOMP_NPV$t-3))) * DAM_DECOMP_NPV$"Mitigation costs"
DAM_DECOMP_NPV$"Standard Climate impacts" <- (1+discount_rate/100)^(-(5*(DAM_DECOMP_NPV$t-3))) * DAM_DECOMP_NPV$"Standard Climate impacts"
DAM_DECOMP_NPV$"Gradient Climate impacts" <- (1+discount_rate/100)^(-(5*(DAM_DECOMP_NPV$t-3))) * DAM_DECOMP_NPV$"Gradient Climate impacts" 
DAM_DECOMP_NPV$"Geoengineering impacts" <- (1+discount_rate/100)^(-(5*(DAM_DECOMP_NPV$t-3))) * DAM_DECOMP_NPV$"Geoengineering impacts"
DAM_DECOMP_NPV$"Geoengineering costs" <- (1+discount_rate/100)^(-(5*(DAM_DECOMP_NPV$t-3))) * DAM_DECOMP_NPV$"Geoengineering costs"
DAM_DECOMP_NPV$"bau"  <- (1+discount_rate/100)^(-(5*(DAM_DECOMP_NPV$t-3))) * DAM_DECOMP_NPV$"bau" 

DAM_DECOMP_NPV <- subset(DAM_DECOMP_NPV, t<=tmax&t>=tmin)
DAM_DECOMP_NPV$t <- NULL
DAM_DECOMP_NPV <- as.data.table(DAM_DECOMP_NPV)[, lapply(.SD, sum), by=c("n", "file", "pathdir"), .SDcols = c("Mitigation costs", "Standard Climate impacts" , "Gradient Climate impacts", "Geoengineering impacts", "Geoengineering costs", "bau")]
DAM_DECOMP_NPV$"Mitigation costs" = 100*DAM_DECOMP_NPV$"Mitigation costs"/DAM_DECOMP_NPV$bau
DAM_DECOMP_NPV$"Standard Climate impacts" = 100*DAM_DECOMP_NPV$"Standard Climate impacts"/DAM_DECOMP_NPV$bau
DAM_DECOMP_NPV$"Geoengineering impacts" = 100*DAM_DECOMP_NPV$"Geoengineering impacts"/DAM_DECOMP_NPV$bau
DAM_DECOMP_NPV$"Geoengineering costs" = 100*DAM_DECOMP_NPV$"Geoengineering costs"/DAM_DECOMP_NPV$bau
DAM_DECOMP_NPV$"bau" <- NULL
DAM_DECOMP_NPV <- melt(DAM_DECOMP_NPV, id.vars = c("n", "file", "pathdir"))
#add totals for labels
DAM_DECOMP_NPV <- ddply(DAM_DECOMP_NPV, .(file, n), transform, total = cumsum(value))
#Plot
#dodged just to see negative values
print(ggplot(subset(DAM_DECOMP_NPV, n %in% regions & file!=bauscen)) + geom_bar(position=position_dodge(), stat="identity",aes(file, value, fill=variable), show.legend = TRUE) +ylab(paste("% of", measure, "(NPV)")) + xlab("") + theme(legend.position="bottom",legend.direction="horizontal") + guides(fill=guide_legend(title=NULL, nrow = 1)))
p <- ggplot(subset(DAM_DECOMP_NPV, n %in% regions & file!=bauscen)) + geom_bar(position=position_stack(), stat="identity",aes(file, value, fill=variable), show.legend = TRUE) +ylab(paste("% of", measure, "(NPV)")) + xlab("") + theme(legend.position="bottom",legend.direction="horizontal") + guides(fill=guide_legend(title=NULL, nrow = 1))
if(length(pathdir) > 1){p <- p + facet_grid(. ~ pathdir)}
if(regions[1] != "WORLD"){p <- p + facet_grid(. ~ n)}
if(show_numbers){p <- p + geom_text(data=subset(DAM_DECOMP_NPV, n %in% regions & file!=bauscen & variable==tail(unique(DAM_DECOMP_NPV$variable), n=1)), aes(x=file, y=total+0.1, label=paste0(round(total, 1),"%")))}
#p <- p  + theme(axis.ticks = element_blank(), axis.text.x = element_blank())
saveplot(paste0(measure, " loss decomposition", suffix), plotdata=DAM_DECOMP_NPV)
}











#Plots of Carbon Prices (TO BE FIXED)
Carbon_Price <- function(scenplot=scenlist){
  get_witch_simple("carbonprice")
  carbonprice <- subset(carbonprice, file %in% scenplot)
  carbonprice$value <- carbonprice$value * usd_deflator    #Apply deflator
  p <- ggplot(subset(carbonprice, t==20 & n=="usa")) + geom_bar(position=position_dodge(), stat="identity",aes(file, value*1e3/(44/12), fill=file), show.legend = TRUE) +ylab("$/tCO2") + theme(legend.position="bottom",legend.direction="horizontal")+ guides(fill=guide_legend(title=NULL, nrow = 1))
  if(length(pathdir) > 1){p <- p + facet_grid(. ~ pathdir)}
  saveplot("Global Carbon Price 2100")
}


Social_Cost_of_Carbon <- function(regions=witch_regions, scenplot=scenlist){
  get_witch_simple("m_eqq_emi_tree")
  get_witch_simple("m_eqq_y")
  m_eqq_emi_tree <- subset(m_eqq_emi_tree, e=="co2")
  m_eqq_emi_tree$e <- NULL
  SCC <- m_eqq_emi_tree
  SCC$SCC <- (-1) * (m_eqq_emi_tree$value / m_eqq_y$value) * 1000 / (44/12)
  SCC$value <- NULL; #SCC$pathdir <- NULL
  
  p <- ggplot(subset(SCC, n %in% regions & ttoyear(t) <= yearmax & ttoyear(t)>=2015 & file %in% scenplot),aes(ttoyear(t),SCC,colour=file)) + geom_line(stat="identity", size=1.2) + xlab("year") +ylab("$/tCO2")
  if(length(regions)>1){p <- p + facet_grid(. ~ n, scales="free")}
  if(length(pathdir)!=1){p <- p + facet_grid(pathdir ~ .)}
  saveplot("Social Cost of Carbon", plotdata=subset(SCC, n %in% regions & ttoyear(t) <= yearmax))
}


Energy_Prices <- function(scenplot=scenlist){
  #unit conversion factor
  witch2iiasa = (1000/0.0036)
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
  historical_witch <- historical_energy_prices; historical_witch$value = historical_witch$value / witch2iiasa; setnames(historical_witch, "variable", "f")
  historical_witch <- subset(historical_witch, f %in% unique(FPRICE$f))
  
  FPRICE$year=as.numeric(FPRICE$t) * 5 + 2000; FPRICE$t <- NULL
  FPRICE$f <- as.factor(FPRICE$f); FPRICE$file <- as.factor(FPRICE$file)
  #ggplot(subset(FPRICE, f!="uranium"&year<=yearmax), aes(year, witch2iiasa*value, group=interaction(f, file), colour=f, linetype=file)) + geom_line(size = 1.0) + labs(x="", y="World Energy Prices ($/GJ)", colour="Fuel", linetype="scenario")
  #saveplot("World Energy Prices Timeseries")
  
  FPRICE <- subset(FPRICE, file %in% scenplot)
  
  FPRICE <- subset(FPRICE, year>2013); 
  historical_witch$year <- as.numeric(historical_witch$year) 
  historical_witch <- subset(historical_witch, year<2013); 
  FPRICE$f <- as.character(FPRICE$f);historical_witch$f <- as.character(historical_witch$f)
  #add it for each scenario
  .historical_witch_temp <- historical_witch
  for(scen in unique(FPRICE$file))
  {
    .historical_witch_temp$file <- scen
    if(scen==unique(FPRICE$file)[1]){historical_witch=.historical_witch_temp}else{historical_witch <-rbind(historical_witch,.historical_witch_temp)}
  }
  FPRICE$file <- as.character(FPRICE$file)
  prices_merged <- merge(subset(FPRICE, f!="uranium"), historical_witch, by = c("year", "f", "file"), all=TRUE)
  prices_merged[is.na(prices_merged)] <- 0
  prices_merged$value <- prices_merged$value.x + prices_merged$value.y
  p <- ggplot(prices_merged, aes(year, witch2iiasa*value, group=interaction(f, file), colour=f, linetype=file)) + geom_line(size = 1.0) + labs(x="", y="World Energy Prices ($/GJ)", colour="Fuel", linetype="scenario")
  legend_position = "right"
  saveplot("World Energy Prices", plotdata = prices_merged)
}

