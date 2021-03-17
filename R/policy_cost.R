# Compute Policy Costs and Carbon Prices


Policy_Cost <- function(discount_rate=5, tmin=3, tmax=20, bauscen="ssp2_bau", regions="World", show_numbers=TRUE, scenplot=scenlist, measure="GDP", suffix=""){
  if(!(bauscen %in% scenlist)){stop("For policy costs define an existing BAU scenario")}
  get_witch_simple("Q")
  #Q$value <- Q$value * usd_deflator    #Apply deflator
  Q <- subset(Q, (t %in% seq(1,100)))
  if(measure=="GDP"){GDP <- subset(Q, tolower(iq)=="y")}
  if(measure=="Consumption"){GDP <- subset(Q, iq=="cc")}
  GDP$iq <- NULL
  bau <- GDP %>% filter(file==bauscen) %>% rename(bau=value) %>% select(t,n,bau)
  GDP <- merge(GDP, bau, by=c("n", "t"), all = TRUE)
  GDP$"GDP Loss" <- -(GDP$"value" -GDP$"bau")
  GDP$GDP_Loss_discounted = (GDP$"GDP Loss")*(1+discount_rate/100)^(-(5*(GDP$t-3)))
  GDP$GDP_MER_discounted = (GDP$"value")*(1+discount_rate/100)^(-(5*(GDP$t-3)))
  GDP <- subset(GDP, file %in% scenplot)
  GDP_WORLD <- GDP %>% select(-n) %>% group_by_at(c("t", file_group_columns, "pathdir")) %>% summarize_all(.funs = sum, na.rm=T) %>% mutate(n="World") %>% as.data.frame() 
  GDP <- rbind(GDP, GDP_WORLD)
  #PC over time plot (NOT discounted!)
  PC_annual_relative <- subset(GDP, t<=tmax&t>=tmin); PC_annual_relative$rel_cost <- PC_annual_relative$"GDP Loss"/PC_annual_relative$"bau";
  p <- ggplot(subset(PC_annual_relative, n %in% regions & file!=bauscen)) + geom_line(aes(ttoyear(t), rel_cost*100, color=file), show.legend = TRUE) +ylab(paste("% of", measure)) + xlab("") + theme(legend.position="bottom",legend.direction="horizontal") + guides(fill=guide_legend(title=NULL, nrow = 1))
  if(length(fullpathdir) > 1){p <- p + facet_grid(. ~ pathdir)}
  if(regions[1] != "World" & (length(regions)>1)){p <- p + facet_grid(. ~ n)}
  #now compute also discounted NPV value
  Policy_Cost <- GDP %>% filter(t<=tmax & t>=tmin) %>% group_by_at(c("n", file_group_columns, "pathdir")) %>% summarize(GDP_Loss_discounted=sum(GDP_Loss_discounted), GDP_MER_discounted=sum(GDP_MER_discounted))
  Policy_Cost$PC = 100*Policy_Cost$GDP_Loss_discounted / Policy_Cost$GDP_MER_discounted
  #set negative Policy costs to zero
  #Policy_Cost$PC <- pmax(Policy_Cost$PC, 0)
  assign("POLCOST", Policy_Cost, envir = .GlobalEnv) 
  p <- ggplot(subset(Policy_Cost, n %in% regions & file!=bauscen)) + geom_bar(position=position_dodge(), stat="identity",aes(file, PC, fill=file), show.legend = TRUE) +ylab(paste("% of", measure, "(NPV)")) + xlab("") + theme(legend.position="bottom",legend.direction="horizontal") + guides(fill=guide_legend(title=NULL, nrow = 1))
  if(length(fullpathdir) > 1){p <- p + facet_grid(. ~ pathdir)}
  if(regions[1] != "World"){p <- p + facet_grid(. ~ n)}
  if(show_numbers){p <- p + geom_text(data=subset(Policy_Cost, n %in% regions & file!=bauscen), aes(x=file, y=PC+0.1, label=paste0(round(PC, 1),"%")))}
  p <- p  + theme(axis.ticks = element_blank(), axis.text.x = element_blank())
  saveplot(paste0("Policy Cost (", measure, ")", suffix), plotdata=subset(Policy_Cost, n %in% regions & file!=bauscen))
}






Policy_Cost_Decomposition <- function(discount_rate=5, tmin=3, tmax=20, bauscen="ssp2_bau", coopbauscen=F, regions="World", show_numbers=TRUE, scenplot=scenlist, measure="GDP", suffix=""){
get_witch_simple("dam_rep")
dam_rep <- dcast(dam_rep, formula = t + n + file + pathdir ~ V3)
get_witch_simple("Q")
if(measure=="GDP"){Q <- subset(Q, iq=="y")}
if(measure=="Consumption"){Q <- subset(Q, iq=="cc")}
Q$iq <- NULL
Q <- subset(Q, t %in% seq(1,30))
#add emission reduction and PES costs to get full gross GDP
#get_witch_simple("COST_FUEL")
get_witch_simple("COST_PES"); COST_FUEL <- COST_PES; setnames(COST_FUEL, "f", "fuel") #deprecated!!
get_witch_simple("COST_EMI")
get_witch_simple("SRM_COST"); setnames(SRM_COST, "value", "SRM_COST")
COST_EMI$e <- NULL;
COST_EMI <- COST_EMI[, lapply(.SD, sum), by=c("t", "n", "file", "pathdir")]; setnames(COST_EMI, "value", "COST_EMI")
COST_FUEL$fuel <- NULL;
COST_FUEL <- COST_FUEL[, lapply(.SD, sum), by=c("t", "n", "file", "pathdir")]; setnames(COST_FUEL, "value", "COST_FUEL")
Q <- merge(Q, COST_EMI, by = c("t", "n", "file", "pathdir"))
Q <- merge(Q, COST_FUEL, by = c("t", "n", "file", "pathdir"))
Q <- merge(Q, SRM_COST, by = c("t", "n", "file", "pathdir"), all = T)
Q$COST <- Q$COST_EMI + Q$COST_FUEL + Q$SRM_COST
Q$COST_EMI <- NULL; Q$COST_FUEL <- NULL 
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
coopbau_COST_FUEL <- data.table(coopbaugdxfile["COST_FUEL"])
coopbau_COST_EMI$e <- NULL;
coopbau_COST_EMI <- coopbau_COST_EMI[, lapply(.SD, sum), by=c("t", "n")];  setnames(coopbau_COST_EMI, "value", "COST_EMI")
coopbau_COST_FUEL$fuel <- NULL;
coopbau_COST_FUEL <- coopbau_COST_FUEL[, lapply(.SD, sum), by=c("t", "n")]; setnames(coopbau_COST_FUEL, "value", "COST_FUEL")
coopbau_Q <- merge(coopbau_Q, coopbau_COST_EMI, by = c("t", "n"))
coopbau_Q <- merge(coopbau_Q, coopbau_COST_FUEL, by = c("t", "n"))
coopbau_Q$COSTcoopbau <- coopbau_Q$COST_EMI + coopbau_Q$COST_FUEL
coopbau_Q$COST_EMI <- NULL; coopbau_Q$COST_FUEL <- NULL 
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
DAM_DECOMP$"Residual costs" <- (DAM_DECOMP$"bau") - (DAM_DECOMP$"Consumption (gross)") - DAM_DECOMP$SRM_COST
#DAM_DECOMP$"Mitigation costs" <- DAM_DECOMP$"Mitigation costs" - (DAM_DECOMP$COST - DAM_DECOMP$COSTbau)
DAM_DECOMP$"Standard Climate impacts" <- (DAM_DECOMP$"Consumption (gross)" - DAM_DECOMP$"Consumption (net)") * DAM_DECOMP$standard_gross / (DAM_DECOMP$standard_gross + DAM_DECOMP$gradient_damage + DAM_DECOMP$geoeng)
DAM_DECOMP$"Gradient Climate impacts" <- (DAM_DECOMP$"Consumption (gross)" - DAM_DECOMP$"Consumption (net)") * DAM_DECOMP$gradient_damage / (DAM_DECOMP$standard_gross + DAM_DECOMP$gradient_damage + DAM_DECOMP$geoeng)
DAM_DECOMP$"SRM impacts" <- (DAM_DECOMP$"Consumption (gross)" - DAM_DECOMP$"Consumption (net)") * DAM_DECOMP$geoeng / (DAM_DECOMP$standard_gross + DAM_DECOMP$gradient_damage + DAM_DECOMP$geoeng)
DAM_DECOMP$"SRM costs" <- DAM_DECOMP$SRM_COST

DAM_DECOMP <- subset(DAM_DECOMP, file %in% scenplot)
#add world values
DAM_DECOMP$n <- as.factor(DAM_DECOMP$n)
DAM_DECOMP$t <- as.factor(DAM_DECOMP$t)
DAM_DECOMP$pathdir <- as.factor(DAM_DECOMP$pathdir)
DAM_DECOMP$file <- as.factor(DAM_DECOMP$file)
DAM_DECOMP <- as.data.table(DAM_DECOMP)
DAM_DECOMP_WORLD <- DAM_DECOMP; DAM_DECOMP_WORLD$n <- NULL
DAM_DECOMP_WORLD <- DAM_DECOMP_WORLD[, lapply(.SD, sum), by=c("t", file_group_columns, "pathdir")]
DAM_DECOMP_WORLD$n <- "World"
DAM_DECOMP <- rbind(DAM_DECOMP, DAM_DECOMP_WORLD)
assign("DAM_DECOMP", DAM_DECOMP, envir = .GlobalEnv)

#now aggregate to NPV discounted values (PC)
DAM_DECOMP_NPV <- DAM_DECOMP
DAM_DECOMP_NPV$t <- as.numeric(DAM_DECOMP_NPV$t)
DAM_DECOMP_NPV$"Residual costs" <- (1+discount_rate/100)^(-(5*(DAM_DECOMP_NPV$t-3))) * DAM_DECOMP_NPV$"Residual costs"
DAM_DECOMP_NPV$"Standard Climate impacts" <- (1+discount_rate/100)^(-(5*(DAM_DECOMP_NPV$t-3))) * DAM_DECOMP_NPV$"Standard Climate impacts"
DAM_DECOMP_NPV$"Gradient Climate impacts" <- (1+discount_rate/100)^(-(5*(DAM_DECOMP_NPV$t-3))) * DAM_DECOMP_NPV$"Gradient Climate impacts" 
DAM_DECOMP_NPV$"SRM impacts" <- (1+discount_rate/100)^(-(5*(DAM_DECOMP_NPV$t-3))) * DAM_DECOMP_NPV$"SRM impacts"
DAM_DECOMP_NPV$"SRM costs" <- (1+discount_rate/100)^(-(5*(DAM_DECOMP_NPV$t-3))) * DAM_DECOMP_NPV$"SRM costs"
DAM_DECOMP_NPV$"bau"  <- (1+discount_rate/100)^(-(5*(DAM_DECOMP_NPV$t-3))) * DAM_DECOMP_NPV$"bau" 

DAM_DECOMP_NPV <- subset(DAM_DECOMP_NPV, t<=tmax&t>=tmin)
DAM_DECOMP_NPV$t <- NULL
DAM_DECOMP_NPV <- as.data.table(DAM_DECOMP_NPV)[, lapply(.SD, sum), by=c("n", "file", "pathdir"), .SDcols = c("Residual costs", "Standard Climate impacts" , "Gradient Climate impacts", "SRM impacts", "SRM costs", "bau")]
DAM_DECOMP_NPV$"Residual costs" = 100*DAM_DECOMP_NPV$"Residual costs"/DAM_DECOMP_NPV$bau
DAM_DECOMP_NPV$"Standard Climate impacts" = 100*DAM_DECOMP_NPV$"Standard Climate impacts"/DAM_DECOMP_NPV$bau
DAM_DECOMP_NPV$"SRM impacts" = 100*DAM_DECOMP_NPV$"SRM impacts"/DAM_DECOMP_NPV$bau
DAM_DECOMP_NPV$"SRM costs" = 100*DAM_DECOMP_NPV$"SRM costs"/DAM_DECOMP_NPV$bau
DAM_DECOMP_NPV$"bau" <- NULL
DAM_DECOMP_NPV <- melt(DAM_DECOMP_NPV, id.vars = c("n", "file", "pathdir"))
#add totals for labels
DAM_DECOMP_NPV <- plyr::ddply(DAM_DECOMP_NPV, .(file, n), transform, total = cumsum(value))
assign("DAM_DECOMP_NPV", DAM_DECOMP_NPV, envir = .GlobalEnv)
#Plot
#dodged just to see negative values
print(ggplot(subset(DAM_DECOMP_NPV, n %in% regions & file!=bauscen)) + geom_bar(position=position_dodge(), stat="identity",aes(file, value, fill=variable), show.legend = TRUE) +ylab(paste("% of", measure, "(NPV)")) + xlab("") + theme(legend.position="bottom",legend.direction="horizontal") + guides(fill=guide_legend(title=NULL, nrow = 1)))
p <- ggplot(subset(DAM_DECOMP_NPV, n %in% regions & file!=bauscen)) + geom_bar(position=position_stack(), stat="identity",aes(file, value, fill=variable), show.legend = TRUE) +ylab(paste("% of", measure, "(NPV)")) + xlab("") + theme(legend.position="bottom",legend.direction="horizontal") + guides(fill=guide_legend(title=NULL, nrow = 1))
if(length(fullpathdir) > 1){p <- p + facet_grid(. ~ pathdir)}
if(regions[1] != "World"){p <- p + facet_grid(. ~ n)}
if(show_numbers){p <- p + geom_text(data=subset(DAM_DECOMP_NPV, n %in% regions & file!=bauscen & variable==tail(unique(DAM_DECOMP_NPV$variable), n=1)), aes(x=file, y=total+0.1, label=paste0(round(total, 1),"%")))}
#p <- p  + theme(axis.ticks = element_blank(), axis.text.x = element_blank())
saveplot(paste0(measure, " loss decomposition", suffix), plotdata=DAM_DECOMP_NPV)
}











#Plots of Carbon Prices (TO BE FIXED)
Carbon_Price <- function(scenplot=scenlist){
  get_witch_simple("carbonprice")
  carbonprice <- subset(carbonprice, file %in% scenplot)
  #carbonprice$value <- carbonprice$value * usd_deflator    #Apply deflator
  p <- ggplot(subset(carbonprice, t==20 & n=="usa")) + geom_bar(position=position_dodge(), stat="identity",aes(file, value*1e3/(44/12), fill=file), show.legend = TRUE) +ylab("$/tCO2") + theme(legend.position="bottom",legend.direction="horizontal")+ guides(fill=guide_legend(title=NULL, nrow = 1))
  if(length(fullpathdir) > 1){p <- p + facet_grid(. ~ pathdir)}
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
  if(length(fullpathdir)!=1){p <- p + facet_grid(pathdir ~ .)}
  saveplot("Social Cost of Carbon", plotdata=subset(SCC, n %in% regions & ttoyear(t) <= yearmax))
}
