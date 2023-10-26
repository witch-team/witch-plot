rm(list = ls())
witch_folder = "../witch-ecemf" #Where you're WITCH code is located
main_folder <- witch_folder # by default, the witch source folder
subdir = c("EIEE-MIP") #can be multiple directories

reg_id <- c("witch20", "global") #choose the aggregations that has the historical data at the aggregation closes to the iamc data

#set or an iamc_filename OR iamc_databasename
iamc_filename <- "EIEE_MIP.csv"  #IIASADB snapshot file to read in main_folder/subdir/
#iamc_databasename <- "ar6_public" #IIASADB database name to read


source('R/witch_functions.R')


#mapping of variables to historical and validation statistics and unit conversion from WITCH historical to model
map_var_hist <- fread("varname_model, set_model, element_model, var_witch, set_witch, element_witch, conv
Primary Energy, na, na, TPES, , , 0.0036 
Emissions|CO2, na, na, Q_EMI, e, co2, 1e3*(44/12)
Emissions|CO2|Energy, na, na, Q_EMI, e, co2ffi, 1e3*(44/12)
Emissions|CH4, na, na, Q_EMI, e, ch4,  1e3*(44/12)/28
Population, na, na, l, , , 1
GDP|MER, na, na, Q, iq, y, 1e3
")
#compute numerical conversion factor
map_var_hist <- map_var_hist %>% rowwise() %>% mutate(conv=eval(parse(text = conv))) %>% as.data.table()



if(exists("iamc_databasename")){
  iiasadb_snapshot <- get_iiasadb(database = iamc_databasename, varlist = "Emissions|CO2", region="World", modlist = "*", scenlist = "*", add_metadata = F)
  #convert to IAMC standard format
  names(iiasadb_snapshot) <- toupper(names(iiasadb_snapshot))
  iiasadb_snapshot <- iiasadb_snapshot %>% select(MODEL, SCENARIO, REGION, VARIABLE, UNIT, YEAR, VALUE) %>% dplyr::rename(value=VALUE) %>% filter(!is.na(value))
}else{
#IIASADB from a xlsx/csv/zipped csv file in the subfolder specified above
# IIASADB snapshot file to read
if(str_detect(iamc_filename, ".xlsx$")){iiasadb_snapshot <- read.xlsx(file.path(main_folder, subdir, iamc_filename), sheet = 1);names(iiasadb_snapshot) <- toupper(names(iiasadb_snapshot))}
#from zipped CSV files (old iiasadb snapshots)
if(str_detect(iamc_filename, ".csv.zip$")){iiasadb_snapshot <- fread(cmd=paste0('unzip -cq "', file.path(file.path(main_folder, subdir, iamc_filename)),'" ', gsub(".zip","",basename(file.path(main_folder, subdir, iamc_filename)))), header=T, quote="\"", sep=",", check.names = FALSE);names(iiasadb_snapshot) <- toupper(names(iiasadb_snapshot))}
#from zipped CSV files (old iiasadb snapshots)
if(str_detect(iamc_filename, ".csv$")){iiasadb_snapshot <- fread(file.path(main_folder, subdir, iamc_filename), header=T, quote="\"", sep=",", check.names = FALSE);names(iiasadb_snapshot) <- toupper(names(iiasadb_snapshot))}
#convert to iiasadb long format
iiasadb_snapshot <- iiasadb_snapshot %>% pivot_longer(cols = -c(MODEL, SCENARIO, REGION, VARIABLE, UNIT), names_to = "YEAR") %>% mutate(YEAR=as.integer(YEAR)) %>% as.data.frame()
}
#to avoid casing issues, for now always use upper case for regions
iiasadb_snapshot <- iiasadb_snapshot %>% mutate(REGION=toupper(REGION))
if(!exists("iiasadb_snapshot")) stop("Please check you specified a correct iiasadb file or connection.")

#use only a subset of the data
iiasadb_snapshot <- iiasadb_snapshot %>% filter(REGION %in% c("WORLD", "EU27", "EUROPE", "ITALY"))
#store temporarily also in teh shiny folder for eventual online publication
save(iiasadb_snapshot, file = "gdxcompaR/iiasadb/iiasadb_snapshot.Rdata")
#launch gdxcompaR
runApp(appDir = "gdxcompaR/iiasadb")



