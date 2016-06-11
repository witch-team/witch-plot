# Compute Policy Costs and Carbon Prices


Policy_Cost <- function(discount_rate=5, tmin=3, tmax=20, bauscen="ssp2_bau", regions="WORLD", show_numbers=TRUE, scenplot=scenlist){
  get_witch_simple("Q")
  Q$value <- Q$value * usd_deflator    #Apply deflator
  Q <- subset(Q, (t %in% t_model))
  GDP <- subset(Q, iq=="y"); GDP$iq <- NULL
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
  if(regions[1]=="WORLD"){
    GDP <- GDP[, lapply(.SD, sum), by=c("t", "file", "pathdir")]
    GDP$n <- "WORLD"
    }
  GDP$t <- as.numeric(GDP$t)
  Policy_Cost <- subset(GDP, t<=tmax&t>=tmin)[, lapply(.SD, sum), by=c("n", "file", "pathdir") , .SDcols = c("GDP_Loss_discounted", "GDP_MER_discounted")]
  Policy_Cost$PC = 100*Policy_Cost$GDP_Loss_discounted / Policy_Cost$GDP_MER_discounted
  #set negative Policy costs to zero
  #Policy_Cost$PC <- pmax(Policy_Cost$PC, 0)
  assign("POLCOST", Policy_Cost, envir = .GlobalEnv) 
  p <- ggplot(subset(Policy_Cost, n %in% regions & file!=bauscen)) + geom_bar(position=position_dodge(), stat="identity",aes(file, PC, fill=file), show.legend = TRUE) +ylab("% of GDP (NPV)") + xlab("") + theme(legend.position="bottom",legend.direction="horizontal") + guides(fill=guide_legend(title=NULL, nrow = 1))
  if(length(pathdir) > 1){p <- p + facet_grid(. ~ pathdir)}
  if(regions[1] != "WORLD"){p <- p + facet_grid(. ~ n)}
  if(show_numbers){p <- p + geom_text(data=subset(Policy_Cost, n %in% regions & file!=bauscen), aes(x=file, y=PC+0.1, label=paste0(round(PC, 1),"%")))}
  p <- p  + theme(axis.ticks = element_blank(), axis.text.x = element_blank())
  saveplot("Policy Cost", plotdata=subset(Policy_Cost, n %in% regions & file!=bauscen))
  
  #PC over time plot
  PC_annual_relative <- subset(GDP, t<=tmax&t>=tmin); PC_annual_relative$rel_cost <- PC_annual_relative$"GDP Loss"/PC_annual_relative$"bau";
  p <- ggplot(subset(PC_annual_relative, n %in% regions & file!=bauscen)) + geom_line(aes(ttoyear(t), rel_cost*100, color=file), show.legend = TRUE) +ylab("% of GDP (NPV)") + xlab("") + theme(legend.position="bottom",legend.direction="horizontal") + guides(fill=guide_legend(title=NULL, nrow = 1))
  if(length(pathdir) > 1){p <- p + facet_grid(. ~ pathdir)}
  if(regions[1] != "WORLD"){p <- p + facet_grid(. ~ n)}
  saveplot("Policy Cost Yearly", plotdata=subset(PC_annual_relative, n %in% regions & file!=bauscen))
  
  
  
  
  
  #Add Carbon Price in 2100
  get_witch_simple("carbonprice")
  carbonprice <- subset(carbonprice, file %in% scenplot)
  
  carbonprice$value <- carbonprice$value * usd_deflator    #Apply deflator
  p <- ggplot(subset(carbonprice, t==20 & n=="usa")) + geom_bar(position=position_dodge(), stat="identity",aes(file, value*1e3/(44/12), fill=file), show.legend = TRUE) +ylab("$/tCO2") + theme(legend.position="bottom",legend.direction="horizontal")+ guides(fill=guide_legend(title=NULL, nrow = 1))
  if(length(pathdir) > 1){p <- p + facet_grid(. ~ pathdir)}
  saveplot("Global Carbon Price 2100", plotdata=subset(carbonprice, t==20 & n=="usa"))
  

}
