#SSP database
iiasadb_file <-  "V:\\WITCH\\IIASADB snapshots\\SSP_IAM_World_5Regs_2017-01-23.csv.zip"
#CD-LINKS database
#iiasadb_file <-  "V:\\WITCH\\IIASADB snapshots\\cdlinks_compare_20190531-122548.csv.zip"
#iiasadb_file <-  "C:\\Users\\Emmerling\\Documents\\Dropbox (CMCC)\\EIEE\\Happiness\\iiasadb\\cdlinks_compare_20180615-153808.csv.zip"
#Effort Sharing
#iiasadb_file <-  "V:\\WITCH\\IIASADB snapshots\\cdlinks_effort_sharing_compare_20190604-191132.csv.zip"
# Define server 
shinyServer(function(input, output, session) {
    # IIASADB snapshot file to read
    if(!exists("iiasadb_snapshot"))iiasadb_snapshot <- fread(cmd=paste0('unzip -cq "', file.path(iiasadb_file),'" ', gsub(".zip","",basename(file.path(iiasadb_file)))), header=T, quote="\"", sep=",", check.names = FALSE)

    #some global flags
    verbose = FALSE
    save_plot = FALSE
    
    #get list of variables
    regions <- unique(iiasadb_snapshot$REGION)
    models <- unique(iiasadb_snapshot$MODEL)
    variables <- unique(iiasadb_snapshot$VARIABLE)
    variables <- sort(variables)

    scenarios <- unique(iiasadb_snapshot$SCENARIO)
    scenarios_list <- strsplit(scenarios, "[-|_]")
    scen1 = unlist(unique(purrr::map(scenarios_list, 1)))
    scen2 = unlist(unique(purrr::map(scenarios_list, 2)))
    scen3 = unlist(unique(purrr::map(scenarios_list, 3)))
    scen4 = unlist(unique(purrr::map(scenarios_list, 4)))
    

    #Get historical data
    region_id <- c("r5", "limits10", "cdlinksg20")
    gdxhistnames <- list.files(path=file.path(witch_folder, paste0("data_", region_id)), full.names = TRUE, pattern="^data_historical", recursive = FALSE)
    gdxhistnames <- gdxhistnames[file.exists(gdxhistnames)]
    iamc_hist_match <- "iamc_name, hist_param_name, setid, setelement, conversion
    Primary Energy, tpes_valid_weo, na, na, 0.0036 
    Emissions|CO2, q_emi_valid_primap, e, co2, 3667
    Final Energy, q_fen_valid_weo, sec, , 0.0036
    Population, l_valid_weo, na, na, 1
    GDP|MER, ykali_valid_weo, na, na, 1e3
    "
    iamc_hist_match <- fread(iamc_hist_match)
    #function to get historical data in IIASADB format
    get_historical_iiasadb <- function(variable) {
      if(variable %in% iamc_hist_match$iamc_name){
        item <- iamc_hist_match[iamc_name==variable]$hist_param_name 
        for(.file in gdxhistnames){
          gdxhist <- gdx(.file)
          .hist <- gdxhist[item]
          #get set dependency based on /build/ folder
          .gdxiso3 <- gdx(file.path(witch_folder, "input", "build", basename(.file))); 
          colnames(.hist) <- c(colnames(.gdxiso3[item[1]]))	
          #in built global data have set "global", but in input folder it gets converted to iso3, so:
          colnames(.hist) <- gsub("global", "iso3", colnames(.hist)) #add "World" if no country level data but global
          if(!("iso3" %in% colnames(.hist))){.hist$n = "World"}else{colnames(.hist) <- gsub("iso3", "n", colnames(.hist))}
          #subsetting and set selection
          if(iamc_hist_match[iamc_name==variable]$setid!="na" & iamc_hist_match[iamc_name==variable]$setid!="all") {
            .hist <- .hist %>% filter(get(iamc_hist_match[iamc_name==variable]$setid)==iamc_hist_match[iamc_name==variable]$setelement)
            .hist <- .hist %>% group_by(n, year) %>% dplyr::summarize(value=sum(value)) %>% ungroup() #always take the sum over all other set elements
          }
          if(iamc_hist_match[iamc_name==variable]$setid=="all") .hist <- .hist %>% group_by(n, year) %>% dplyr::summarize(value=sum(value)) %>% ungroup()
          if(.file==gdxhistnames[1]){
            #add global values
            .hist_World <- .hist %>% group_by(year) %>% summarize(value=sum(value)) %>% mutate(n="World")
            .hist <- rbind(.hist, .hist_World)
            .hist_all <- .hist
          }else{
              .hist_all <- rbind(.hist_all, .hist)
          }
        }
        .hist <- .hist_all
        #Unit conversion
        .hist <- .hist %>% mutate(value=value*iamc_hist_match[iamc_name==variable]$conversion)
        #adjusting region names
        .hist <- .hist %>% mutate(REGION = ifelse(n=="World", "World", toupper(n))) %>% select(-n)
        if(any(str_detect(regions, "R5.2"))) .hist <- .hist %>% mutate(REGION = gsub("R5", "R5.2", REGION))
        .hist <- .hist %>% filter(REGION %in% regions)
        #creating same data format as iiasadb
        .hist <- .hist %>% mutate(VARIABLE=variable, UNIT="historical", SCENARIO="historical", MODEL="historical", year=as.integer(year)) %>% rename(YEAR=year)
        return(.hist)
      }else{return(data.frame())}
    }

    
    #Scenario selector
    output$select_scenarios_1 <- renderUI({
      selectInput("scenarios_selected_1", "Select scenarios", scen1, size=length(scen1), selectize = F, multiple = T, selected = scen1)
    })  
    
    output$select_scenarios_2 <- renderUI({
      selectInput("scenarios_selected_2", "Select scenarios", scen2, size=length(scen2), selectize = F, multiple = T, selected = scen2)
    })  
    
    output$select_scenarios_3 <- renderUI({
      selectInput("scenarios_selected_3", "Select scenarios", scen3, size=length(scen3), selectize = F, multiple = T, selected = scen3)
    })  
    
    output$select_scenarios_4 <- renderUI({
      selectInput("scenarios_selected_4", "Select scenarios", scen4, size=length(scen4), selectize = F, multiple = T, selected = scen4)
    })  
    
    #Variable selector
    output$select_variable <- renderUI({
    selectInput("variable_selected", "Select variable", variables, size=1, selectize = F, multiple = F, selected = variables[1])
    })  
    variable_selected_reactive <- reactive({input$variable_selected})
  

    #MODEL selector
    output$select_models <- renderUI({
      selectInput("models_selected", "Select models", models, size=length(models), selectize = F, multiple = T, selected = models)
    })  
    
    #REGION selector
    output$select_regions <- renderUI({
      regions_for_selector <- regions
    selectInput("regions_selected", "Select regions", regions_for_selector, size=length(regions_for_selector), selectize = F, multiple = T, selected = "World")
    })
  
    #Additional selector for specific Panels
    
    

    # MAIN CODE FOR PLOT GENERATION  
    output$iiasadb_compaRplot <- renderPlot({
      ylim_zero <- input$ylim_zero
      button_writecsv <- input$button_writecsv
      button_saveplot <- input$button_saveplot
      variable <- input$variable_selected
      if(is.null(variable)) variable <- variables[1]
      #get data
      allfilesdata <- subset(iiasadb_snapshot, VARIABLE==variable)
      #convert to witchplotR format
      allfilesdata <- as.data.frame(allfilesdata)[,colSums(is.na(allfilesdata))<nrow(allfilesdata)] #drop years without observations
      allfilesdata <- as.data.table(allfilesdata)
      allfilesdata <- data.table::melt(allfilesdata, id.vars = c("VARIABLE", "UNIT", "REGION", "SCENARIO", "MODEL"), variable.name = "YEAR")
      allfilesdata$YEAR <- as.integer(as.character(allfilesdata$YEAR))
      unitplot <- unique(allfilesdata$UNIT)[1]
      #add historical data
      allfilesdata <- rbind(allfilesdata, get_historical_iiasadb(variable))

      #get input from sliders/buttons
      yearmin = input$yearmin
      yearmax = input$yearmax
      regions <- input$regions_selected
      models_selected <- input$models_selected
      #get all possible scenarios
      scen1 <- input$scenarios_selected_1
      scen2 <- input$scenarios_selected_2
      scen3 <- input$scenarios_selected_3
      scen4 <- input$scenarios_selected_4
      scenarios_selected <- scenarios
      scenarios_selected <- str_subset(scenarios_selected, paste(scen1, collapse = "|"))
      scenarios_selected <- str_subset(scenarios_selected, paste(scen2, collapse = "|"))
      scenarios_selected <- str_subset(scenarios_selected, paste(scen3, collapse = "|"))
      scenarios_selected <- str_subset(scenarios_selected, paste(scen4, collapse = "|"))
      
      #select scenarios
      allfilesdata <- subset(allfilesdata, SCENARIO %in% c(scenarios_selected, "historical"))
      allfilesdata <- subset(allfilesdata, MODEL %in% c(models_selected, "historical"))
      
      #time frame
      allfilesdata <- subset(allfilesdata, YEAR>=yearmin & YEAR<=yearmax)
      #clean data
      allfilesdata <- subset(allfilesdata, !is.na(value))
     
       if(is.null(regions)) regions <- "World"
      
      if(regions[1]=="World" | length(regions)==1){#if only World is displayed or only one region, show files with colors
        p <- ggplot(subset(allfilesdata, REGION %in% regions & SCENARIO!="historical"),aes(YEAR,value,colour=MODEL, linetype=SCENARIO)) + geom_line(stat="identity", size=1.5) + xlab("year") + ylab(unitplot) + xlim(yearmin,yearmax)
        p <- p + geom_line(data=subset(allfilesdata, REGION %in% regions & SCENARIO=="historical"), aes(YEAR,value), stat="identity", size=1.0, colour = "black")
        if(ylim_zero) p <- p + ylim(0, NA)
        #legends:
        p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL))
       }else{
        p <- ggplot(subset(allfilesdata, REGION %in% regions & SCENARIO!="historical"),aes(YEAR,value,colour=interaction(REGION, MODEL), linetype=SCENARIO)) + geom_line(stat="identity", size=1.5) + xlab("year") + ylab(unitplot) + xlim(yearmin,yearmax) + facet_grid(. ~ REGION)
        p <- p + geom_line(data=subset(allfilesdata, REGION %in% regions & SCENARIO=="historical"), aes(YEAR,value,colour=REGION), stat="identity", size=1.0)
        if(ylim_zero) p <- p + ylim(0, NA)
        #legends:
        p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL, nrow = 2), linetype=guide_legend(title=NULL))
      }
      print(p + labs(title=variable))
      if(save_plot) saveplot(variable)
      })
    
    
    
 
    
    
    
    
    # MAIN CODE FOR PLOT GENERATION  
    output$iiasadb_compaRly <- renderPlotly({
      ylim_zero <- input$ylim_zero
      button_writecsv <- input$button_writecsv
      button_saveplot <- input$button_saveplot
      variable <- input$variable_selected
      if(is.null(variable)) variable <- variables[1]
      #get data
      allfilesdata <- subset(iiasadb_snapshot, VARIABLE==variable)
      #convert to witchplotR format
      allfilesdata <- as.data.frame(allfilesdata)[,colSums(is.na(allfilesdata))<nrow(allfilesdata)] #drop years without observations
      allfilesdata <- as.data.table(allfilesdata)
      allfilesdata <- data.table::melt(allfilesdata, id.vars = c("VARIABLE", "UNIT", "REGION", "SCENARIO", "MODEL"), variable.name = "YEAR")
      allfilesdata$YEAR <- as.integer(as.character(allfilesdata$YEAR))
      unitplot <- unique(allfilesdata$UNIT)[1]
      #add historical data
      allfilesdata <- rbind(allfilesdata, get_historical_iiasadb(variable))
      
      #get input from sliders/buttons
      yearmin = input$yearmin
      yearmax = input$yearmax
      regions <- input$regions_selected
      models_selected <- input$models_selected
      #get all possible scenarios
      scen1 <- input$scenarios_selected_1
      scen2 <- input$scenarios_selected_2
      scen3 <- input$scenarios_selected_3
      scen4 <- input$scenarios_selected_4
      scenarios_selected <- scenarios
      scenarios_selected <- str_subset(scenarios_selected, paste(scen1, collapse = "|"))
      scenarios_selected <- str_subset(scenarios_selected, paste(scen2, collapse = "|"))
      scenarios_selected <- str_subset(scenarios_selected, paste(scen3, collapse = "|"))
      scenarios_selected <- str_subset(scenarios_selected, paste(scen4, collapse = "|"))
      
      #select scenarios
      allfilesdata <- subset(allfilesdata, SCENARIO %in% c(scenarios_selected, "historical"))
      allfilesdata <- subset(allfilesdata, MODEL %in% c(models_selected, "historical"))
      
      #time frame
      allfilesdata <- subset(allfilesdata, YEAR>=yearmin & YEAR<=yearmax)
      #clean data
      allfilesdata <- subset(allfilesdata, !is.na(value))
      
      if(is.null(regions)) regions <- "World"
      
      if(regions[1]=="World" | length(regions)==1){#if only World is displayed or only one region, show files with colors
        p <- ggplot(subset(allfilesdata, REGION %in% regions & SCENARIO!="historical"),aes(YEAR,value,colour=MODEL, linetype=SCENARIO)) + geom_line(stat="identity", size=1.5) + xlab("year") + ylab(unitplot) + xlim(yearmin,yearmax)
        p <- p + geom_line(data=subset(allfilesdata, REGION %in% regions & SCENARIO=="historical"), aes(YEAR,value), stat="identity", size=1.0, colour = "black")
        if(ylim_zero) p <- p + ylim(0, NA)
        #legends:
        p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL))
      }else{
        p <- ggplot(subset(allfilesdata, REGION %in% regions & SCENARIO!="historical"),aes(YEAR,value,colour=interaction(REGION, MODEL), linetype=SCENARIO)) + geom_line(stat="identity", size=1.5) + xlab("year") + ylab(unitplot) + xlim(yearmin,yearmax) + facet_grid(. ~ REGION)
        p <- p + geom_line(data=subset(allfilesdata, REGION %in% regions & SCENARIO=="historical"), aes(YEAR,value,colour=REGION), stat="identity", size=1.0)
        if(ylim_zero) p <- p + ylim(0, NA)
        #legends:
        p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL, nrow = 2), linetype=guide_legend(title=NULL))
      }
      p_dyn <- p + theme(legend.position = "none") + labs(title=variable)
      print(p_dyn)
      suppressWarnings(ggplotly()) #to be done: fix error "argument 1 is not a vector", shoudl be done by plotly package
    })
    
    
    


    
    
})