#epoort data routines to create datasets based on historical (and possibly WITCH) or projected data, always only for ONE scenario!

#function to store a set of variables in long format, or optional wide with regions as columns
write_witch_data_csv <- function(vars, years = "all", wide_region=FALSE){		
  for(.var in vars){
    data <- get_witch_simple(.var)
    data$pathdir <- NULL		
    data$t <- ttoyear(data$t); setnames(data, "t", "year")
    data$variable <- .var
    set_dep <- setdiff(colnames(data), c("file", "n", "year", "variable", "value"))
    if(length(set_dep)>0){setnames(data, set_dep, paste0("V", seq(1:length(set_dep))))}
    if(.var==vars[1]){allvars <- data}else{allvars <- rbind(allvars, data, fill=T)}
  }
  allvars <- as.data.table(allvars)
  setcolorder(allvars, c("file", "variable", "n", "year", setdiff(colnames(allvars), c("file", "n", "year", "variable", "value")), "value"))
  if(years!="all"){allvars <- subset(allvars, year %in% years)}
  if(length(scenlist)==1){allvars$file <- NULL}
  if(wide_region){allvars <- dcast(allvars, formula = as.formula(paste(paste(setdiff(colnames(allvars), c("n", "value")), collapse = " + "), "~ n")))}
  write.csv(allvars, file=paste0(graphdir, "witch_dataset_long", ".csv"), row.names = FALSE)		
}		
