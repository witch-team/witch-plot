# Inequality plots

plot_inequality <- function(varname, plot_type = "quantiles", value_share="value", quantile_set = "dist", scenplot = scenlist, regions = witch_regions, years = seq(yearmin, yearmax)){
  # dist_type="value|share"
  # plot_type = "quantiles|gini|lorenz"
  require(reldist)
  require(ineq)
  require(gglorenz)
  # plot_type = "quantiles|gini|lorenz"
  ineq_data <- get_witch_simple(varname, results = "return")
  ineq_data <- ineq_data %>% filter(file %in% scenplot & ttoyear(t) %in% years & n %in% regions)
  if(quantile_set %in% names(ineq_data)){
  if(value_share == "share") ineq_data <- ineq_data %>% group_by_at(setdiff(names(ineq_data), c("value", quantile_set))) %>% mutate(value=value/sum(value))
  ineq_data_indices <- ineq_data %>% group_by_at(setdiff(names(ineq_data), c("value", quantile_set))) %>% summarize(gini=gini(value))
  if(plot_type=="quantiles"){
    #facetted plot of quantiles over files and regions
    p <- ggplot(ineq_data) + geom_area(aes(ttoyear(t), value, fill=get(quantile_set))) + facet_grid(file ~ n) + xlab("")+ ylab(varname) + scale_fill_brewer(palette = "YlGnBu" , name = "Quantile")
  }
  else if(plot_type=="gini"){
    #Gini plot based on Quantiles
    p <- ggplot(ineq_data_indices) + geom_line(aes(ttoyear(t), gini, color=file)) + facet_grid(. ~ n) + xlab("")+ ylab("Gini") + ylim(0,1) 
  }
  else if(plot_type=="lorenz_curve"){
    #Lorenz curve
    #plot(Lc(lcdat_cpc[lcdat_cpc$file=="BAU",]$value, lcdat_pop[lcdat_pop$file=="BAU",]$value),col="black",lwd=2)
    p <- ggplot(ineq_data %>% filter(t==1)) + stat_lorenz(aes(value, color=file)) + geom_abline(color = "grey") + xlab("") + ylab("") + facet_grid(. ~ n)
  }
  p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank())
  saveplot(paste("Distribution of", varname, "as", plot_type))
  }else{print("No distributional information in this variable.")}
}