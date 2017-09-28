gdxcompaR_static <- function(variable, additional_set="na", additional_set_id="na", convert=1, unit="", regions=witch_regions, plot=TRUE){
  
  
line2005 = TRUE

#general
get_witch_simple(variable, check_calibration=TRUE)
allfilesdata <- get(variable)

#choose subsets
if(additional_set!="na"){
  allfilesdata <- subset(allfilesdata, tolower(get(additional_set))==additional_set_id)
}

p <- ggplot(subset(allfilesdata, n %in% regions & file!="calibration"),aes(ttoyear(t),value,colour=n, linetype=file)) + geom_line(stat="identity", size=1.5) + xlab("year") + ylab(unit) + scale_colour_manual(values = region_palette)
p <- p + geom_line(data=subset(allfilesdata, n %in% regions & file=="calibration"),aes(ttoyear(t),value,colour=n), stat="identity", size=0.5)

if(length(pathdir)!=1){p <- p + facet_grid(pathdir ~ .)}
if(line2005){p <- p + geom_vline(size=0.5,aes(xintercept=2005), linetype="solid", color="grey")}
if(plot){saveplot(variable)}
}