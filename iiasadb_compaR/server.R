#SSP database
iiasadb_file <-  "C:\\Users\\Emmerling\\Documents\\Dropbox (CMCC)\\EIEE\\Happiness\\iiasadb\\SSP_IAM_World_5Regs_2017-01-23.csv.zip"
#CD-LINKS database
#iiasadb_file <-  "C:\\Users\\Emmerling\\Documents\\Dropbox (CMCC)\\EIEE\\Happiness\\iiasadb\\cdlinks_compare_20180615-153808.csv.zip"

# Define server 
shinyServer(function(input, output, session) {
    # IIASADB snapshot file to read
    iiasadb_snapshot <- fread(cmd=paste0('unzip -cq "', file.path(iiasadb_file),'"'), header=T, quote="\"", sep=",", check.names = FALSE)

    #some global flags
    verbose = FALSE
    save_plot = FALSE
    
    #get list of variables
    regions <- unique(iiasadb_snapshot$REGION)
    models <- unique(iiasadb_snapshot$MODEL)
    
    variables <- unique(iiasadb_snapshot$VARIABLE)
    variables_list <- strsplit(variables, "\\|")
    var1 = unlist(unique(map(variables_list, 1)))
    var2 = unlist(unique(map(variables_list, 2)))
    var3 = unlist(unique(map(variables_list, 3)))
    var4 = unlist(unique(map(variables_list, 4)))
    var5 = unlist(unique(map(variables_list, 5)))
  
      
    scenarios <- unique(iiasadb_snapshot$SCENARIO)
    scenarios_list <- strsplit(scenarios, "[-|_]")
    scen1 = unlist(unique(map(scenarios_list, 1)))
    scen2 = unlist(unique(map(scenarios_list, 2)))
    scen3 = unlist(unique(map(scenarios_list, 3)))
    scen4 = unlist(unique(map(scenarios_list, 4)))
    

    
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
      variable <- input$variable_selected
      if(is.null(variable)) variable <- variables[1]
   
      #get data
      #remove years with zero obs
      allfilesdata <- subset(iiasadb_snapshot, VARIABLE==variable)
      #convert to witchplotR format
      allfilesdata <- as.data.frame(allfilesdata)[,colSums(is.na(allfilesdata))<nrow(allfilesdata)] #drop years without observations
      allfilesdata <- melt(allfilesdata, id.vars = c("VARIABLE", "UNIT", "REGION", "SCENARIO", "MODEL"), variable.name = "YEAR")
      allfilesdata$YEAR <- as.integer(as.character(allfilesdata$YEAR))
      unitplot <- unique(allfilesdata$UNIT)[1]
      
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
      allfilesdata <- subset(allfilesdata, SCENARIO %in% scenarios_selected)
      allfilesdata <- subset(allfilesdata, MODEL %in% models_selected)
      
      #time frame
      allfilesdata <- subset(allfilesdata, YEAR>=yearmin & YEAR<=yearmax)
      #clean data
      #allfilesdata <- as.data.frame(allfilesdata)[!is.na(allfilesdata$value)]
     
       if(is.null(regions)) regions <- "World"
      
      if(regions[1]=="World" | length(regions)==1){#if only World is displayed or only one region, show files with colors
        p <- ggplot(subset(allfilesdata, REGION %in% regions),aes(YEAR,value,colour=SCENARIO, linetype=MODEL)) + geom_line(stat="identity", size=1.5) + xlab("year") + ylab(unitplot) + xlim(yearmin,yearmax)
        if(ylim_zero) p <- p + ylim(0, NA)
        #legends:
        p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL))
      }else{
        p <- ggplot(subset(allfilesdata, REGION %in% regions),aes(YEAR,value,colour=interaction(REGION, MODEL), linetype=SCENARIO)) + geom_line(stat="identity", size=1.5) + xlab("year") + ylab(unitplot) + xlim(yearmin,yearmax) + facet_grid(. ~ REGION)
        #p <- p + geom_line(data=subset(allfilesdata, n %in% regions & str_detect(file, "historical")),aes(year,value,colour=n, linetype=file), stat="identity", size=1.0)
        #legends:
        p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL, nrow = 2), linetype=guide_legend(title=NULL))
        
      }
      print(p + labs(title=variable))
      if(save_plot) saveplot(variable)
  })
    
    
    
    
    
    
    # MAIN CODE FOR PLOT GENERATION  
    output$iiasadb_compaRly <- renderPlotly({
      ylim_zero <- input$ylim_zero
      variable <- input$variable_selected
      if(is.null(variable)) variable <- variables[1]
      
      #get data
      #remove years with zero obs
      allfilesdata <- subset(iiasadb_snapshot, VARIABLE==variable)
      #convert to witchplotR format
      allfilesdata <- as.data.frame(allfilesdata)[,colSums(is.na(allfilesdata))<nrow(allfilesdata)] #drop years without observations
      allfilesdata <- melt(allfilesdata, id.vars = c("VARIABLE", "UNIT", "REGION", "SCENARIO", "MODEL"), variable.name = "YEAR")
      allfilesdata$YEAR <- as.integer(as.character(allfilesdata$YEAR))
      unitplot <- unique(allfilesdata$UNIT)[1]
      
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
      allfilesdata <- subset(allfilesdata, SCENARIO %in% scenarios_selected)
      allfilesdata <- subset(allfilesdata, MODEL %in% models_selected)
      
      #time frame
      allfilesdata <- subset(allfilesdata, YEAR>=yearmin & YEAR<=yearmax)
      #clean data
      #allfilesdata <- as.data.frame(allfilesdata)[!is.na(allfilesdata$value)]
      
      if(is.null(regions)) regions <- "World"
      
      if(regions[1]=="World" | length(regions)==1){#if only World is displayed or only one region, show files with colors
        p <- ggplot(subset(allfilesdata, REGION %in% regions),aes(YEAR,value,colour=SCENARIO, linetype=MODEL)) + geom_line(stat="identity", size=1.5) + xlab("year") + ylab(unitplot) + xlim(yearmin,yearmax)
        if(ylim_zero) p <- p + ylim(0, NA)
        #legends:
        p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL))
      }else{
        p <- ggplot(subset(allfilesdata, REGION %in% regions),aes(YEAR,value,colour=interaction(REGION, MODEL), linetype=SCENARIO)) + geom_line(stat="identity", size=1.5) + xlab("year") + ylab(unitplot) + xlim(yearmin,yearmax) + facet_grid(. ~ REGION)
        #p <- p + geom_line(data=subset(allfilesdata, n %in% regions & str_detect(file, "historical")),aes(year,value,colour=n, linetype=file), stat="identity", size=1.0)
        #legends:
        p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL, nrow = 2), linetype=guide_legend(title=NULL))
        
      }
      p_dyn <- p + theme(legend.position = "none") + labs(title=variable)
      print(p_dyn)
      suppressWarnings(ggplotly()) #to be done: fix error "argument 1 is not a vector", shoudl be done by plotly package
    })
    
    
    


    
    
})