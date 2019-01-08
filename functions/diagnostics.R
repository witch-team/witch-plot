#little diagnostics dash board based on ggplot

diagnostics_plots <- function(scenplot=scenlist){
  get_witch_simple("allerr", scenplot = scenplot)
  get_witch_simple("allinfoiter", scenplot = scenplot)
  get_witch_simple("all_optimal", scenplot = scenplot); all_optimal$optimal <- 1; all_optimal$n <- NULL;
  get_witch_simple("all_feasible", scenplot = scenplot); all_feasible$feasible <- 1; all_feasible$n <- NULL;
  get_witch_simple("price_iter", scenplot = scenplot)
  
  iterations <- allinfoiter %>% filter(iterrep=="itertime") %>% select(-n) %>% group_by(pathdir, file, run, siter) %>% mutate(time=value) %>% select(-value, -iterrep)
  iterations <- merge(iterations, all_optimal, by = c("pathdir", "file", "run", "siter"), all = T)
  iterations <- merge(iterations, all_feasible, by = c("pathdir", "file", "run", "siter"), all = T)
  
  runs <- iterations %>% group_by(pathdir, file, run) %>% summarize(total_time=sum(time)/60, n_iter=length(siter), Optimal=sum(optimal, na.rm = T), n_iter_feas=sum(feasible, na.rm = T)) %>% mutate(Infeasible=n_iter-n_iter_feas, Nonoptimal=n_iter-Optimal-Infeasible)
  runs_long <- melt(runs, id.vars = c("pathdir", "file", "run"))
  
  plot_time <- ggplot(runs) + geom_bar(aes(pathdir, total_time), stat = "identity") + facet_grid(. ~ file) + ylab("Total duration (minutes)") + xlab("")
  
  plot_iterations <- ggplot(runs_long %>% filter(variable %in% c("Optimal", "Nonoptimal", "Infeasible"))) + geom_bar(aes(pathdir, value, fill=variable), stat = "identity", position = "stack") + facet_grid(. ~ file) + ylab("Iterations") + scale_fill_manual(values = c("Optimal"="darkgreen", "Nonoptimal"="yellow", "Infeasible"="red")) + xlab("") + theme(legend.position="none")
  
  allerr$siter <- as.numeric(gsub("i", "", allerr$siter))
  plot_convergence <- ggplot(allerr) + geom_line(aes(siter, pmax(value,0.001), color=V3)) + facet_grid(. ~ file) + ylab("Convergence") + xlab("Iteration") +  scale_y_log10(breaks = c(0.005, 0.1, 0.5, 1, 100), labels = c(0.005, 0.1, 0.5, 1, 100)) + geom_hline(yintercept = c(0.005), color="grey") + theme(legend.position = c(0.1,0.5), legend.title = element_blank(), legend.background = element_blank(), legend.key = element_blank())
  
  price_iter$siter <- as.numeric(gsub("i", "", price_iter$siter))
  price_iter <- price_iter %>% group_by(run, siter, V3, file, pathdir, n) %>% mutate(value = value / mean(value[1])) %>% as.data.frame() #convert in starting all from 1
  plot_prices <- ggplot(price_iter) + geom_line(data=subset(price_iter, V3=="nip"), aes(ttoyear(t), value, alpha=siter, group=siter), color="red") + facet_grid(. ~ file) + ylab("Prices") + xlab("") +  scale_y_log10() + geom_line(data=subset(price_iter, V3=="oil"), aes(ttoyear(t), value, alpha=siter, group=siter), color="black") + facet_grid(. ~ file) + ylab("Prices") + xlab("") + theme(legend.position="none")

  ggarrange(plot_time, plot_iterations + theme(strip.background = element_blank(), strip.text = element_blank()) , plot_convergence + theme(strip.background = element_blank(), strip.text = element_blank()) , plot_prices + theme(strip.background = element_blank(), strip.text = element_blank()) , nrow = 4)
  
  }

