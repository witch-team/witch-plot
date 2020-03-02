# Inequality plots

plot_inequality <- function(varname, plot_type = "quantiles", q_vector = NA, value_share="value", quantile_set = "dist", per_capita_var = 1, scenplot = scenlist, regions = witch_regions, years = seq(yearmin, yearmax), years_lorenz = range(yearmin, yearmax)){
  # dist_type="value|share"
  # plot_type = "quantiles|lorenz_curve|gini|distribution"
  res <- lapply(c('reldist', 'ineq', 'gglorenz', 'GB2', 'GB2group', 'VGAM'), require_package)
  ineq_data <- get_witch_simple(varname, results = "return")
  ineq_data <- ineq_data %>% filter(file %in% scenplot & ttoyear(t) %in% years & n %in% regions)
  if(quantile_set %in% names(ineq_data)){
  q_vector <- rep(1/nrow(unique(ineq_data[quantile_set])),nrow(unique(ineq_data[quantile_set]))) 
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
    p <- ggplot(ineq_data %>% filter(ttoyear(t) %in% years_lorenz) %>% mutate(year=ttoyear(t))) + stat_lorenz(aes(value, color=file)) + geom_abline(color = "grey") + xlab("") + ylab("") + facet_grid(n  ~ year)
  }
  else if(plot_type=="distribution"){
    total_data <- get_witch_simple(total_var, results = "return")
    total_data <- total_data %>% filter(t %in% unique(ineq_data$t))
    population_data <- get_witch_simple(population_var, results = "return")
    population_data <- population_data %>% filter(t %in% unique(ineq_data$t))
    #combine data
    
    #1) generate sample based on quantiles
    value_pc = 40000
    q_test <- c(0.05,0.10, 0.12, 0.15, 0.58)
    q_values <- data.frame(x = q_test/q_vector * value_pc, p = q_test)

    #Singh-Maddala Distribution
    fitdist <- fitgroup.sm(y = q_test, x = q_vector, pc.inc = value_pc, gini.e = gini_test, rescale = 1000)
    fitdistparam <- unlist((fitdist$nls.estimation))[1,]
    #Plot distribution CDF
    ggplot(q_values, aes(x)) + stat_ecdf(geom = "step") + stat_function(fun = psinmad, args = list(scale = 1000*fitdistparam["b"], shape1.a = fitdistparam["a"], shape3.q = fitdistparam["q"]), colour = "red") + xlab("") + ylab("") + xlim(0,NA)
    #Plot Lorenz curve
    Lc_manual <- Lc(x = q_test, n = q_vector)
    Lc_manual <- data.frame(p=Lc_manual$p, Lc=Lc_manual$L)
    LCsingh <- function(p,a,q){pbeta(q = (1 - (1 - p)^(1/q)), shape1 = (1 + 1/a), shape2 = (q-1/a))} #Source: http://www.vcharite.univ-mrs.fr/PP/lubrano/cours/Lecture-4.pdf
    #Gini
    GINIsingh <- function(a,q){return(1-((gamma(q)*gamma(2*q-1/a))/(gamma(q-1/a)*gamma(2*q))))}
    GINIsingh(fitdistparam["a"],fitdistparam["q"])
    ggplot(data.frame(x=seq(0,1,1000))) +  stat_function(fun = LCsingh, args = list(a=fitdistparam["a"], q=fitdistparam["q"]), color = "red") + geom_line(data = Lc_manual, aes(p,Lc)) + xlab("") + ylab("") + geom_abline(color = "grey")
  }  
  #common for all plots
  p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank())
  saveplot(paste("Distribution of", varname, "as", plot_type))
  }else{print("No distributional information in this variable.")}
}