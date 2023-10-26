#Create gdxcompaR based on iiasa forma csv or xlsx files

# Define server 
shinyServer(function(input, output, session) {
    #some global flags
    verbose = FALSE
    
    #get list of variables
    regions <- unique(iiasadb_snapshot$REGION)
    models <- unique(iiasadb_snapshot$MODEL)
    variables <- unique(iiasadb_snapshot$VARIABLE)
    variables <- sort(variables)
    variable_atstart <- ifelse("Population" %in% variables, "Population", variables[1])
    scenarios <- unique(iiasadb_snapshot$SCENARIO)

    #Scenario selector
    output$select_scenarios <- renderUI({
      selectInput("scenarios_selected", "Select scenarios", scenarios, size=length(scenarios), selectize = F, multiple = T, selected = scenarios)
    })  
    
    #Variable selector
    output$select_variable <- renderUI({
    selectInput("variable_selected", "Select variable", variables, size=1, selectize = F, multiple = F, selected = variable_atstart)
    })  
    variable_selected_reactive <- reactive({input$variable_selected})
  

    #MODEL selector
    output$select_models <- renderUI({
      selectInput("models_selected", "Select models", models, size=length(models), selectize = F, multiple = T, selected = models)
    })  
    
    #REGION selector
    output$select_regions <- renderUI({
      regions_for_selector <- regions
    selectInput("regions_selected", "Select regions", regions_for_selector, size=1, selectize = F, multiple = F, selected = "World")
    })
    
    #Compare models or scenarios
    # output$compare_models_scenarios <- renderUI({
    #   compare_models_scenarios_selector <- "Scenarios"
    #   radioButtons("choice_models_scenarios", "Use color for", c("Scenarios", "Models"),selected = "Scenarios", inline=T) 
    # })

    observeEvent(input$button_saveplotdata, {
      variable <- input$variable_selected
      print("Current plot saved in subdirectory 'graphs'")
      saveplot(variable, width = 14, height = 7)
    })
    
    #Additional selector for specific Panels
    
    

    # MAIN CODE FOR PLOT GENERATION  
    output$iiasadb_compaRplot <- renderPlot({
      ylim_zero <- input$ylim_zero
      variable <- input$variable_selected
      if(is.null(variable)) variable <- variables[1]
      #get data
      allfilesdata <- subset(iiasadb_snapshot, VARIABLE==variable)
      unitplot <- unique(allfilesdata$UNIT)[1]
      #add historical data
      allfilesdata <- add_historical_values(allfilesdata, varname = variable, check_calibration = T, iiasadb = T, verbose = F)

      #get input from sliders/buttons
      yearmin = input$yearmin
      yearmax = input$yearmax
      regions <- input$regions_selected
      models_selected <- input$models_selected
      #get all possible scenarios
      scenarios_selected <- input$scenarios_selected
      #select scenarios
      allfilesdata <- subset(allfilesdata, SCENARIO %in% c(scenarios_selected, "historical"))
      allfilesdata <- subset(allfilesdata, !(MODEL %in% setdiff(models, models_selected)))
      
      #time frame
      allfilesdata <- subset(allfilesdata, YEAR>=yearmin & YEAR<=yearmax)
      #clean data
      allfilesdata <- subset(allfilesdata, !is.na(value))
     
      if(is.null(regions)) regions <- "World"
      
      if(regions[1]=="World" | length(regions)==1){#if only World is displayed or only one region, show files with colors
        if(length(models_selected)==1){
          p <- ggplot(subset(allfilesdata, REGION %in% regions & SCENARIO!="historical"),aes(YEAR,value,colour=SCENARIO)) + geom_line(stat="identity", linewidth=1.5) + xlab("") + ylab(unitplot) + xlim(yearmin,yearmax)
        }else{
          p <- ggplot(subset(allfilesdata, REGION %in% regions & SCENARIO!="historical"),aes(YEAR,value,colour=MODEL, linetype=SCENARIO)) + geom_line(stat="identity", size=1.5) + xlab("") + ylab(unitplot) + xlim(yearmin,yearmax)
        }
        p <- p + geom_line(data=subset(allfilesdata, REGION %in% regions & SCENARIO=="historical"), aes(YEAR,value, linetype=MODEL), stat="identity", linewidth=1.0, colour = "black")
        if(ylim_zero) p <- p + ylim(0, NA)
        #legends:
        p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL), linetype=guide_legend(title=NULL))
       }else{
        p <- ggplot(subset(allfilesdata, REGION %in% regions & SCENARIO!="historical"),aes(YEAR,value,colour=interaction(REGION, MODEL), linetype=SCENARIO)) + geom_line(stat="identity", linewidth=1.5) + xlab("year") + ylab(unitplot) + xlim(yearmin,yearmax) + facet_grid(. ~ REGION)
        p <- p + geom_line(data=subset(allfilesdata, REGION %in% regions & SCENARIO=="historical"), aes(YEAR,value,colour=REGION, linetype=MODEL), stat="identity", linewidth=1.0)
        if(ylim_zero) p <- p + ylim(0, NA)
        #legends:
        p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL, nrow = 2), linetype=guide_legend(title=NULL))
      }
      print(p + labs(title=variable))
      })
    
    
    
 
    
    
    
    
    # MAIN CODE FOR PLOT GENERATION  
    output$iiasadb_compaRly <- renderPlotly({
      ylim_zero <- input$ylim_zero
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
      scenarios_selected <- input$scenarios_selected
      
      #select scenarios
      allfilesdata <- subset(allfilesdata, SCENARIO %in% c(scenarios_selected, "historical"))
      allfilesdata <- subset(allfilesdata, MODEL %in% c(models_selected, "historical"))
      
      #time frame
      allfilesdata <- subset(allfilesdata, YEAR>=yearmin & YEAR<=yearmax)
      #clean data
      allfilesdata <- subset(allfilesdata, !is.na(value))
      
      if(is.null(regions)) regions <- "World"
      
      if(length(models_selected)==1){
        color_aesthetics <- "SCENARIO"
        linetype_aesthetics <- "MODEL"
      }else{
        color_aesthetics <- "MODEL"
        linetype_aesthetics <- "SCENARIO"
      } 
      
      if(regions[1]=="World" | length(regions)==1){#if only World is displayed or only one region, show files with colors
        if(length(models_selected)==1){
          p <- ggplot(subset(allfilesdata, REGION %in% regions & SCENARIO!="historical"),aes(YEAR,value,colour=SCENARIO)) + geom_line(stat="identity", linewidth=1) + xlab("") + ylab(unitplot) + xlim(yearmin,yearmax)
        }else{
          p <- ggplot(subset(allfilesdata, REGION %in% regions & SCENARIO!="historical"),aes(YEAR,value,colour=MODEL, linetype=SCENARIO)) + geom_line(stat="identity", linewidth=1) + xlab("") + ylab(unitplot) + xlim(yearmin,yearmax)
        }
        p <- p + geom_line(data=subset(allfilesdata, REGION %in% regions & SCENARIO=="historical"), aes(YEAR,value), stat="identity", linewidth=1.0, colour = "black")
        if(ylim_zero) p <- p + ylim(0, NA)
        #legends:
        p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL))
      }else{
        p <- ggplot(subset(allfilesdata, REGION %in% regions & SCENARIO!="historical"),aes(YEAR,value,colour=interaction(REGION, MODEL), linetype=SCENARIO)) + geom_line(stat="identity", linewidth=1.5) + xlab("year") + ylab(unitplot) + xlim(yearmin,yearmax) + facet_grid(. ~ REGION)
        p <- p + geom_line(data=subset(allfilesdata, REGION %in% regions & SCENARIO=="historical"), aes(YEAR,value,colour=REGION), stat="identity", linewidth=1.0)
        if(ylim_zero) p <- p + ylim(0, NA)
        #legends:
        p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL, nrow = 2), linetype=guide_legend(title=NULL))
      }
      p_dyn <- p + theme(legend.position = "none") + labs(title=variable)
      print(p_dyn)
      suppressWarnings(ggplotly()) #to be done: fix error "argument 1 is not a vector", shoudl be done by plotly package
    })
    
    
    


    
    
})

