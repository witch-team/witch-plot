#Specific functions for the RICE50+ model


plot_macc_fit <- function(yearcheck = 2040){
#check MACC curves
get_witch("ax_co2")
get_witch("bx_co2")
get_witch("emi_bau_co2")
get_witch("mx")

check_macc <- ax_co2 %>% select(t,n,value) %>% rename(ax_co2=value) %>% full_join(bx_co2 %>% select(t,n,value) %>% rename(bx_co2=value))  %>% full_join(emi_bau_co2 %>% select(t,n,value) %>% rename(emi_bau_co2=value))  %>% full_join(mx %>% select(t,n,value) %>% rename(mx=value)) 

plot_check_macc <- check_macc %>% filter(ttoyear(t) == yearcheck)
xmin<-0
xmax<-1.2
step<-0.05
xx<-seq(xmin,xmax,by=step)
miudf<-data.frame(matrix("", ncol = length(plot_check_macc$n), nrow = length(xx)))
names(miudf) <- plot_check_macc$n
miudf$xx<-xx
for(i in 1:length(plot_check_macc$n)){
  miudf[,i] <- plot_check_macc$ax_co2[i]*(miudf$xx^2)/2 + plot_check_macc$bx_co2[i]*(miudf$xx^5)/5
  miudf[,i] <- plot_check_macc$mx[i] * (plot_check_macc$ax_co2[i]*(miudf$xx^2)/2 + plot_check_macc$bx_co2[i]*(miudf$xx^5)/5)
  miudf[,i] <- plot_check_macc$mx[i] * (plot_check_macc$ax_co2[i]*(miudf$xx^2)/2 + plot_check_macc$bx_co2[i]*(miudf$xx^5)/5) * plot_check_macc$emi_bau_co2[i]
  miudf[,i] <- plot_check_macc$mx[i]*(plot_check_macc$ax_co2[i]*(miudf$xx^1) + plot_check_macc$bx_co2[i]*(miudf$xx^4))
  miudf[,i] <- (plot_check_macc$ax_co2[i]*(miudf$xx^1) + plot_check_macc$bx_co2[i]*(miudf$xx^4))
}
ggplot(miudf %>% pivot_longer(cols = !xx, names_to = "n"), aes(xx,value)) + geom_line(aes(colour = n))
plotly::ggplotly()
#add enerdata points
enerdata <- fread(file = file.path(witch_folder, "input", "data", "enerdata-enerdata_macc_full.csv"))
enerdata <- enerdata %>% filter(sector=="Total_CO2" & scenario=="Ener-Blue") %>% mutate(t=yeartot(Year), mju=abatement_perc, n=Code) %>% select(t,n,cost,mju)
ggplot(enerdata %>% filter(ttoyear(t)==yearcheck), aes(mju,cost)) + geom_point(aes(colour = n))

#plot enerdata and model together
ggplot(miudf %>% pivot_longer(cols = !xx, names_to = "n"), aes(xx,value))  + xlim(0,0.6) + ylim(0,1000) + geom_line(aes(colour = n)) + geom_point(data=enerdata %>% filter(ttoyear(t)==yearcheck), aes(mju,cost, colour = n)
) + xlab("mju") + ylab("Carbon price $/tCO2") + guides(color=FALSE)
saveplot("RICE50+ MACC Curves Fit")
}
