# Inequality plots

plot_inequality <- function(varname, plot_type = "quantiles", q_shares = NULL, value_share="value", quantile_set = "dist", per_capita_var = 1000, scenplot = scenlist, regions = witch_regions, years = seq(yearmin, yearmax), years_lorenz = NULL){
  # dist_type="value|share"
  # plot_type = "quantiles|lorenz_curve|gini|distribution"
  res <- lapply(c('reldist', 'ineq', 'gglorenz', 'GB2', 'GB2group', 'VGAM'), require_package)
  ineq_data <- get_witch_simple(varname, results = "return")
  ineq_data <- ineq_data %>% filter(file %in% scenplot & ttoyear(t) %in% years & n %in% regions)
  if(quantile_set %in% names(ineq_data)){
  if(is.null(q_shares)) q_shares <- data.frame(dist=unique(ineq_data[quantile_set]), share=rep(1/nrow(unique(ineq_data[quantile_set])),nrow(unique(ineq_data[quantile_set])))); setnames(q_shares, "dist", quantile_set)
  if(is.null(years_lorenz)) years_lorenz <- range(unique(ttoyear(ineq_data$t)))
  if(value_share == "share") ineq_data <- ineq_data %>% group_by_at(setdiff(names(ineq_data), c("value", quantile_set))) %>% mutate(value=value/sum(value))
  ineq_data_indices <- ineq_data %>% group_by_at(setdiff(names(ineq_data), c("value", quantile_set))) %>% summarize(gini=gini(value))
  if(plot_type=="quantiles"){
    #facetted plot of quantiles over files and regions
    p <- ggplot(ineq_data) + geom_area(aes_string("ttoyear(t)", "value", fill=quantile_set)) + facet_grid(file ~ n) + xlab("")+ ylab(varname) + scale_fill_brewer(palette = "YlGnBu" , name = "Quantile")
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
    if(is.numeric(per_capita_var[1])) ineq_data$per_capita <- per_capita_var else ineq_data <- ineq_data %>% full_join(get_witch_simple(per_capita_var, results = "return") %>% filter(t %in% unique(ineq_data$t)) %>% rename(per_capita=value))
    #Create complete dataframe
    ineq_data_plot <- ineq_data %>% full_join(ineq_data_indices) %>% full_join(q_shares) %>% mutate(value_cst_dist=value/share * per_capita) %>% filter(ttoyear(t) %in% years_lorenz)
    
    ### FUNCTIONS ###
    #Plot Lorenz curve
    Lc_manual <- function(x, n, return_param = "df"){
      .Lc <- Lc(x = x, n = n)
      .Lc <- data.frame(p=.Lc$p, Lc=.Lc$L)
      if(return_param=="df") return(.Lc) else return(.Lc[return_param])
    }
    #Singh-Maddala Distribution
    fit_parametric_dist <- function(y, x, pc.inc, gini.e, return_param){
      fitdist <- fitgroup.sm(y = y, x = x, pc.inc = pc.inc, gini.e = gini.e, rescale = 1000)
      fitdistparam <- unlist((fitdist$nls.estimation))[1,]
      return(fitdistparam[return_param])
    }
    LCsingh <- function(p,a,q){pbeta(q = (1 - (1 - p)^(1/q)), shape1 = (1 + 1/a), shape2 = (q-1/a))} #Source: http://www.vcharite.univ-mrs.fr/PP/lubrano/cours/Lecture-4.pdf
    #Gini
    GINIsingh <- function(a,q){return(1-((gamma(q)*gamma(2*q-1/a))/(gamma(q-1/a)*gamma(2*q))))}
    ### END FUNCTIONS ###
    
    #compute fitted distribution parameters in dataframe
    ineq_data_plot_agg <- ineq_data_plot %>% group_by_at(c("t", file_group_columns, "pathdir")) %>% summarize(gini=mean(gini), per_capita=mean(per_capita), a = fit_parametric_dist(y=value, x=share, pc.inc=per_capita, gini.e=gini, return_param = "a"), q = fit_parametric_dist(y=value, x=share, pc.inc=per_capita, gini.e=gini, return_param = "q"), b = fit_parametric_dist(y=value, x=share, pc.inc=per_capita, gini.e=gini, return_param = "b")) %>% as.data.frame()
    
    ineq_data_plot_everything <- ineq_data_plot %>% full_join(ineq_data_plot_agg) %>% mutate(year=ttoyear(t)) %>% select(-t, -gini,-per_capita) %>% as.data.frame()
    
    #for now only one figure of first year and n in dataset (multiple not yet working as requires different stat-functions per facets)
    ineq_data_plot_everything <- ineq_data_plot_everything %>% filter(n %in% unique(ineq_data_plot$n)[1] & year %in% unique(ineq_data_plot_everything$year)[1] & file %in% unique(ineq_data_plot$file)[1])
    
    #Plot one distribution CDF
    p_cdf <- ggplot(ineq_data_plot_everything, aes(value_cst_dist)) + stat_ecdf(geom = "step") + stat_function(fun = psinmad, args = list(scale = 1000*mean(ineq_data_plot_everything$b), shape1.a = mean(ineq_data_plot_everything$a), shape3.q = mean(ineq_data_plot_everything$q)), colour = "red") + xlab("") + ylab("") + xlim(0,NA) #+ facet_grid(n  ~ year)
    
    #Plot one Lorenz curve
    p_lorenz <- ggplot(data.frame(x=seq(0,1,1000))) + geom_line(data = Lc_manual(ineq_data_plot_everything$value, ineq_data_plot_everything$share), aes(p,Lc)) + xlab("") + ylab("") + geom_abline(color = "grey") + stat_function(fun = LCsingh, args = list(a=mean(ineq_data_plot_everything$a), q=mean(ineq_data_plot_everything$q)), color = "red") + geom_text(aes(0.4, 0.8), label = paste("Gini (q):",round(gini(ineq_data_plot_everything$value, ineq_data_plot_everything$share),3), "(dist):", round(GINIsingh(mean(ineq_data_plot_everything$a), mean(ineq_data_plot_everything$q)),3))) #+ facet_grid(n  ~ year)
    p <- ggarrange(p_cdf, p_lorenz, ncol = 1)
    #saveplot("Distribution and Lorenz curve with fitted distribution")
  }  
  #common for all plots
  p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank())
  saveplot(paste("Distribution of", varname, "as", plot_type))
  }else{print("No distributional information in this variable.")}
}
