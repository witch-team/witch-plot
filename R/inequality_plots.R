# Inequality plots

plot_inequality <- function(variable = variable, plot_type = "quantiles", q_shares = NULL, value_share="value", quantile_set = "dist", per_capita_var = 1000, scenplot = scenlist, regions = witch_regions[1], years = seq(yearmin, yearmax), years_lorenz = NULL, q_plot = NULL, q_fit = NULL, verbose = T){
  # variable can either be a variable name or dataframe.
  # value_share="value|share" if share, already shares (sum to 1, not 100!!), if value compute shares
  # plot_type = "quantiles|lorenz_curve|gini|distribution"
  # q_shares: dataframe with column quantile_set and share (in increasing order!)
  # q_plot, q_fit: chose which quantiles use to plot and which to fit the distribution (optional), by default all are used and plotted
  res <- lapply(c('reldist', 'ineq', 'gglorenz', 'GB2', 'GB2group', 'VGAM'), require_package)
  if(is.character(variable))ineq_data <- get_witch_simple(variable, results = "return") else{ineq_data <- variable; variable <- deparse(substitute(variable))}
  ineq_data <- ineq_data %>% filter(file %in% scenplot & ttoyear(t) %in% years & n %in% regions)
  if(quantile_set %in% names(ineq_data)){
  setnames(ineq_data, quantile_set, "dist")
  ineq_data <- subset(ineq_data, !is.na("dist")) #removes scenarios without distirbutive information
  if(is.null(q_shares)) q_shares <- data.frame(dist=unique(ineq_data[["dist"]]), share=rep(1/length(unique(ineq_data[["dist"]])),length(unique(ineq_data[["dist"]]))))
  ineq_data$dist <- factor(ineq_data$dist, levels = rev(q_shares$dist))
  if(is.null(years_lorenz)) years_lorenz <- range(unique(ttoyear(ineq_data$t)))
  if(value_share == "value") ineq_data <- ineq_data %>% group_by_at(setdiff(names(ineq_data), c("value", "dist"))) %>% mutate(value=value/sum(value))
  if(!is.null(q_plot)) ineq_data <- ineq_data %>% filter(dist %in% q_plot)
  ineq_data_indices <- ineq_data %>% group_by_at(setdiff(names(ineq_data), c("value", "dist"))) %>% summarize(gini=gini(value))
  
  if(plot_type=="quantiles"){
    #facetted plot of quantiles over files and regions
    ggplot(ineq_data)  + geom_bar(aes(ttoyear(t), value, fill=dist), stat = "identity", position = "stack") + facet_grid(file ~ n) + xlab("")+ ylab(variable) #+ scale_fill_manual(values = quantile_colors, name = "Quantiles")
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
    if(is.numeric(per_capita_var[1])) ineq_data$per_capita <- per_capita_var else ineq_data <- ineq_data %>% full_join(get_witch_simple(per_capita_var, results = "return") %>% filter(t %in% unique(ineq_data$t)) %>% rename(per_capita=value)) %>% filter(!is.na(dist))
    #Create complete dataframe
    ineq_data$dist <- as.character(ineq_data$dist); q_shares$dist <- as.character(q_shares$dist)
    ineq_data_plot <- ineq_data %>% full_join(ineq_data_indices) %>% full_join(q_shares) %>% mutate(value_cst_dist=value/share * per_capita) %>% filter(ttoyear(t) %in% years_lorenz)
    
    ### FUNCTIONS ###
    #Plot Lorenz curve
    Lc_manual <- function(x, n, return_param = "df"){
      .Lc <- Lc(x = x, n = n)
      .Lc <- data.frame(p=.Lc$p, Lc=.Lc$L)
      if(return_param=="df") return(.Lc) else return(.Lc[return_param])
    }
    #Singh-Maddala Distribution (rescale is just for fit, not per capita value!)
    rescale_fit <- 1000
    fit_parametric_dist <- function(y, x, pc.inc, gini.e, return_param){
      fitdist <- fitgroup.sm(y = y, x = x, pc.inc = pc.inc, gini.e = gini.e, rescale = rescale_fit)
      fitdistparam <- unlist((fitdist$ewmd.estimation))[1,]
      return(fitdistparam[return_param])
    }
    LCsingh <- function(p,a,q){pbeta(q = (1 - (1 - p)^(1/q)), shape1 = (1 + 1/a), shape2 = (q-1/a))}   
    #Source: http://www.vcharite.univ-mrs.fr/PP/lubrano/cours/Lecture-4.pdf
    #Gini
    GINIsingh <- function(a,q){return(1-((gamma(q)*gamma(2*q-1/a))/(gamma(q-1/a)*gamma(2*q))))}
    ### END FUNCTIONS ###
    
    #compute fitted distribution parameters in dataframe
    ineq_data_plot_agg <- ineq_data_plot 
    if(!is.null(q_fit)) ineq_data_plot_agg <- ineq_data_plot_agg %>% filter(dist %in% q_fit)
    ineq_data_plot_agg <- ineq_data_plot_agg %>% group_by_at(c("t", file_group_columns, "pathdir")) %>% summarize(gini=mean(gini), per_capita=mean(per_capita), a = fit_parametric_dist(y=value, x=share, pc.inc=per_capita, gini.e=gini, return_param = "a"), q = fit_parametric_dist(y=value, x=share, pc.inc=per_capita, gini.e=gini, return_param = "q"), b = fit_parametric_dist(y=value, x=share, pc.inc=per_capita, gini.e=gini, return_param = "b")) %>% as.data.frame()
    ineq_data_plot_everything <- (ineq_data_plot %>% full_join(ineq_data_plot_agg)) %>% mutate(year=ttoyear(t)) %>% select(-gini,-per_capita) %>% as.data.frame()
    
    if(verbose){
      print("Estimated Maddala-Sing Distribution Parameters")
      print(ineq_data_plot_agg %>% mutate(P99=qsinmad(0.99, shape1.a = a, shape3.q = q , scale = rescale_fit*b)))
    }
    
    #for now only one figure of first year and n in dataset (multiple not yet working as requires different stat-functions per facets)
    ineq_data_plot_everything <- ineq_data_plot_everything %>% filter(n %in% unique(ineq_data_plot$n)[1] & year %in% unique(ineq_data_plot_everything$year)[1] & file %in% unique(ineq_data_plot$file)[1])
    
    #Plot one distribution CDF
    p_cdf <- ggplot(ineq_data_plot_everything, aes(value_cst_dist)) + stat_ecdf(geom = "step") + stat_function(fun = psinmad, args = list(scale = mean(ineq_data_plot_everything$b), shape1.a = mean(ineq_data_plot_everything$a), shape3.q = mean(ineq_data_plot_everything$q)), colour = "red") + xlab("") + ylab("") + xlim(0,NA) #+ facet_grid(n  ~ year)
    
    #Plot one Lorenz curve
    p_lorenz <- ggplot(data.frame(x=seq(0,1,0.001))) + geom_line(data = Lc_manual(ineq_data_plot_everything$value, ineq_data_plot_everything$share), aes(p,Lc)) + xlab("") + ylab("") + geom_abline(color = "grey") + stat_function(fun = LCsingh, args = list(a=mean(ineq_data_plot_everything$a), q=mean(ineq_data_plot_everything$q)), color = "red") + geom_text(aes(0.4, 0.8), label = paste("Gini (q):",round(gini(ineq_data_plot_everything$value, ineq_data_plot_everything$share),3), "(dist):", round(GINIsingh(mean(ineq_data_plot_everything$a), mean(ineq_data_plot_everything$q)),3))) #+ facet_grid(n  ~ year)
    p <- ggarrange(p_cdf, p_lorenz, ncol = 1)
    #saveplot("Distribution and Lorenz curve with fitted distribution")
  }  
  #common for all plots
  p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank())
  saveplot(paste("Distribution of", variable, "as", plot_type))
  return(ineq_data_indices) #return inequality indices
  }else{print("No distributional information in this variable.")}
}
