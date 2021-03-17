#little diagnostics dash board based on ggplot

diagnostics_plots <- function(scenplot=scenlist){
  get_witch_simple("allerr", scenplot = scenplot)
  get_witch_simple("allinfoiter", scenplot = scenplot)
  get_witch_simple("all_optimal", scenplot = scenplot); all_optimal$optimal <- 1; all_optimal$n <- NULL;
  get_witch_simple("all_feasible", scenplot = scenplot); all_feasible$feasible <- 1; all_feasible$n <- NULL;
  get_witch_simple("price_iter", scenplot = scenplot)
  
  iterations <- allinfoiter %>% filter(iterrep=="itertime") %>% select(-n) %>% group_by(pathdir, file, run, siter) %>% mutate(time=value) %>% select(-value, -iterrep)
  iterations <- merge(iterations, all_optimal, by = c("pathdir", file_group_columns, "run", "siter"), all = T)
  iterations <- merge(iterations, all_feasible, by = c("pathdir", file_group_columns, "run", "siter"), all = T)
  
  runs <- iterations %>% group_by_at(c("pathdir", file_group_columns, "run")) %>% summarize(total_time=sum(time), n_iter=length(siter), Optimal=sum(optimal, na.rm = T), n_iter_feas=sum(feasible, na.rm = T)) %>% mutate(Infeasible=n_iter-n_iter_feas, Nonoptimal=n_iter-Optimal-Infeasible) %>% as.data.table()
  runs_long <- suppressWarnings(melt(runs, id.vars = c("pathdir", file_group_columns, "run")))
  
  #run time
  time_dhms <- function(t){
    paste0(ifelse(t>86400, paste0(t %/% (60*60*24),"d"), ""),
    ifelse(t>3600,paste0(formatC(t %/% (60*60) %% 24, width = 2, format = "d", flag = "0"),":"),""),
    paste0(formatC(t %/% 60 %% 60, width = 2, format = "d", flag = "0"),":",formatC(t %% 60, width = 2, format = "d", flag = "0"))
  )}
  plot_time <- ggplot(runs) + geom_bar(aes(run, total_time/60), stat = "identity") + facet_grid(. ~ file) + ylab("Total duration (minutes)") + xlab("") + geom_text(aes(x=run, y=total_time/60+10, label=time_dhms(total_time)))
  
  iterations <- iterations %>% mutate(ONI=ifelse(!is.na(optimal), "Optimal", ifelse(is.na(feasible), "Infeasible", "Nonoptimal")), one=1) %>% mutate(siter=as.numeric(gsub("i","", siter))) %>% arrange(pathdir, file, run, siter)
  
  plot_iterations <- ggplot(iterations) + geom_tile(aes(run, one, fill=ONI, group=one), stat = "identity", position = "stack") + facet_grid(. ~ file) + ylab("Iterations") + scale_fill_manual(values = c("Optimal"="darkgreen", "Nonoptimal"="yellow", "Infeasible"="red")) + xlab("") + theme(legend.position="none")
  
  
  allerr$siter <- as.numeric(gsub("i", "", allerr$siter))
  plot_convergence <- ggplot(allerr) + geom_line(aes(siter, pmax(value,0.001), color=V3, linetype=run)) + facet_grid(. ~ file) + ylab("Convergence") + xlab("Iteration") +  scale_y_log10(breaks = c(0.005, 0.1, 0.5, 1, 100), labels = c(0.005, 0.1, 0.5, 1, 100)) + geom_hline(yintercept = c(0.005), color="grey") + theme(legend.position = c(0.1,0.5), legend.title = element_blank(), legend.background = element_blank(), legend.key = element_blank())
  
  price_iter$siter <- as.numeric(gsub("i", "", price_iter$siter))
  price_iter <- price_iter %>% group_by(run, siter, V3, file, pathdir, n) %>% mutate(value = value / mean(value[1])) %>% as.data.frame() #convert in starting all from 1
  plot_prices <- ggplot(price_iter) + geom_line(data=subset(price_iter, V3=="nip"), aes(ttoyear(t), value, alpha=siter, group=interaction(siter,run), linetype=run), color="red") + facet_grid(. ~ file) + ylab("Prices") + xlab("") +  scale_y_log10() + geom_line(data=subset(price_iter, V3=="oil"), aes(ttoyear(t), value, alpha=siter, group=interaction(siter,run), linetype=run), color="black") + facet_grid(. ~ file) + ylab("Prices") + xlab("") + theme(legend.position="none")

  suppressWarnings(ggarrange(plot_time, plot_iterations + theme(strip.background = element_blank(), strip.text = element_blank()) , plot_convergence + theme(strip.background = element_blank(), strip.text = element_blank()) , plot_prices + theme(strip.background = element_blank(), strip.text = element_blank()) , nrow = 4))
  
  }

