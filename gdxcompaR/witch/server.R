# Define server 

#require packages if online deployed
if(deploy_online){
  suppressPackageStartupMessages(require(tidyverse))
  require(plotly)
  add_historical_values <- function(x, varname, check_calibration, iiasadb, verbose){return(x)}
  get_witch <- function(variable, check_calibration, field){return(allvariables[[variable]])}
} 

shinyServer(function(input, output, session) {
  
    #some global flags
    verbose = FALSE
    
    # Get list of variables and parameters in all files
    list_of_variables <- NULL
    for(f in filelist){
      .gdx <- gdx(paste(file.path(fullpathdir[1], f),".gdx",sep=""))
      # Select all variables and parameters 
      #  -> with "t" in their domain names
      #  -> with dimension <= 2
      for (item in c("variables", "parameters")) {
        info_item <- .gdx[[item]]
        info_item <- info_item[info_item$dim <= 3,]
        info_item <- info_item[sapply(info_item$domnames, 
                                      function(x) "t" %in% x),]
        list_of_variables <- c(list_of_variables, info_item$name)
      }
    }
    list_of_variables <- unique(list_of_variables)
    list_of_variables <- c(sort(str_subset(list_of_variables, "^[:upper:]")), 
                           sort(str_subset(list_of_variables, "^[:lower:]")))

    #Scenario selector
    output$select_scenarios <- renderUI({
    selectInput(inputId = "scenarios_selected", 
                label = "Scenarios:", 
                choices = unname(scenlist),
                size = length(scenlist), 
                selectize = FALSE, 
                multiple = TRUE,
                selected = unname(scenlist)) # Select all scenarios by default
    })
    
    
    #Variable selector
    output$select_variable <- renderUI({
      pickerInput(
        inputId = "variable_selected",
        label = "Variable:", 
        choices = list_of_variables,
        selected = "Q_EMI",
        options = list(
          `live-search` = TRUE)
      )
    })
    variable_selected_reactive <- reactive({input$variable_selected})
    
    #Display selected variable and set
    output$varname <- renderText({  
      paste0(variable_selected_reactive(),
            "|", str_trunc(paste(input$additional_set_id_selected, 
                                 collapse=","), 20),
            ifelse(is.null(input$additional_set_id_selected2) | 
                     input$additional_set_id_selected2 == "na" ,
                   "",
                   paste0("|", str_trunc(paste(input$additional_set_id_selected2, 
                                               collapse=","), 20))),
            "|", str_trunc(paste(input$regions_selected, 
                                 collapse=","), 10))
    }) 

    #REGION selector
    output$select_regions <- renderUI({
      regions_for_selector <- list(Aggregate = list("World", "EU"),  
                                   `Native regions` = witch_regions)
    selectInput(inputId = "regions_selected", 
                label = "Regions:", 
                regions_for_selector, 
                size = max(10, length(regions_for_selector)), 
                selectize = FALSE, 
                multiple = TRUE,
                selected = "World")
    })
  
    observeEvent(input$button_saveplotdata, {
      variable <- input$variable_selected
      print("Current plot saved in subdirectory 'graphs'")
      saveplot(variable, width = 14, height = 7)
    })

    # MAIN CODE FOR PLOT GENERATION  
    output$gdxompaRplot <- renderPlot({
      assign("historical", input$add_historical, envir = .GlobalEnv)
      ylim_zero <- input$ylim_zero
      field_show <- input$field
      #plotly_dynamic <- input$plotly_dynamic
      variable <- input$variable_selected
      if(is.null(variable)) variable <- list_of_variables[1]
      #get data
      afd <- get_witch(variable, check_calibration=T, field = field_show)
      if(verbose) print(str_glue("Variable {variable} loaded."))
      #get the name of the additional set
      additional_sets <- setdiff(colnames(afd), c(file_group_columns, "pathdir", "t", "n", "value"))
      #extract additional set elements
      if(length(additional_sets)==0){additional_set_id="na"; set_elements = "na"; additional_set_id2="na"; set_elements2 = "na"}
      else if(length(additional_sets)==1)
      {
        additional_set_id <- additional_sets[1]
        set_elements <- unique(tolower(as.data.frame(afd)[, match(additional_set_id, colnames(afd))]))
        set_elements <- sort(set_elements)
        additional_set_id2 <- "na"
        set_elements2 <- "na"
      }
      else if(length(additional_sets)==2)
      {
        additional_set_id <- additional_sets[1]
        set_elements <- unique(tolower(as.data.frame(afd)[, match(additional_set_id, colnames(afd))]))
        set_elements <- sort(set_elements)
        additional_set_id2 <- additional_sets[2] 
        set_elements2 <- unique(tolower(as.data.frame(afd)[, match(additional_set_id2, colnames(afd))]))
        set_elements2 <- sort(set_elements2)
      }

      #Selector for additional set
      output$choose_additional_set <- renderUI({
        variable <- variable_selected_reactive()
        if (is.null(variable)) {
          variable <- list_of_variables[1]
        }
        sel <- input$additional_set_id_selected
        if (is.null(sel)) {
          if ("co2_ffi" %in% set_elements) {
            sel <- "co2_ffi"
          } else {
            sel <- set_elements[1]
          }
        }
        size_elements <- min(length(set_elements), 5)
        selectInput(inputId = "additional_set_id_selected", 
                    label = "Indices 1:", 
                    choices = set_elements, 
                    size = size_elements, 
                    selectize = FALSE, 
                    multiple = TRUE,
                    selected = sel)
      })
      #Selector for additional set #2
      output$choose_additional_set2 <- renderUI({
        variable <- variable_selected_reactive()
        if(is.null(variable)) variable <- list_of_variables[1]
        sel2 <- input$additional_set_id_selected2
        size_elements2 <- min(length(set_elements2), 5)
        selectInput(inputId = "additional_set_id_selected2", 
                    label = "Indices 2:", 
                    choices = set_elements2, 
                    size = size_elements2, 
                    selectize = FALSE, 
                    multiple = TRUE, 
                    selected = sel2)
      })
      
      #get input from sliders/buttons
      yearlim <- input$yearlim
      additional_set_selected <- input$additional_set_id_selected
      additional_set_selected2 <- input$additional_set_id_selected2
      regions <- input$regions_selected
      scenarios <- input$scenarios_selected
      
      #in case they have not yet been set, set to default values
      if (is.null(regions)) {
        regions <- display_regions
      }
      if (is.null(additional_set_selected)) {
        additional_set_selected <- set_elements[1]
      }
      if((additional_set_id!="na" & additional_set_selected[1]=="na") | !(additional_set_selected[1] %in% set_elements)) additional_set_selected <- set_elements[1] 
      if(is.null(additional_set_selected2)) additional_set_selected2 <- set_elements2[1]
      if((additional_set_id2!="na" & additional_set_selected2[1]=="na") | !(additional_set_selected2[1] %in% set_elements2)) additional_set_selected2 <- set_elements2[1] 
      
      # SUBSET data and PLOT
      #choose additional selected element
      if(additional_set_id!="na"){
        afd[[additional_set_id]] <- tolower(afd[[additional_set_id]]) # to fix erroneous gams cases (y and Y etc.)
        afd <- subset(afd, get(additional_set_id) %in% additional_set_selected)
        afd[[additional_set_id]] <- NULL #remove additional set column
        #afd$t <- as.character(afd$t)
        if(length(additional_set_selected) >1) afd <- afd %>% group_by_at(setdiff(names(afd), "value")) %>% summarize(value=sum(value), .groups = 'drop')
        }
      if(additional_set_id2!="na"){
        afd[[additional_set_id2]] <- tolower(afd[[additional_set_id2]]) # to fix erroneous gams cases (y and Y etc.)
        afd <- subset(afd, get(additional_set_id2) %in% additional_set_selected2)
        afd[[additional_set_id2]] <- NULL #remove additional set column
        if(length(additional_set_selected2) >1) afd <- afd %>% group_by_at(setdiff(names(afd), "value")) %>% summarize(value=sum(value), .groups = 'drop')
      }
     
      #time frame
      if (input$time_filter) {
        afd <- subset(afd, ttoyear(t) >= yearlim[1] & ttoyear(t) <= yearlim[2])
      }
      #clean data
      afd <- afd %>% filter(!is.na(value))
      
      #Computation of World/global sum/average
      #now based on meta param to guess max, mean, sum
      if(nrow(afd)>0){
        afd_global <- afd
        afd_global$n <- NULL
        if(variable %in% default_meta_param()$parameter){
          if(default_meta_param()[parameter==variable & type=="nagg"]$value=="sum"){
            afd_global <- afd_global %>% group_by_at(setdiff(names(afd_global), "value")) %>% summarize(value=sum(value), .groups = 'drop')
            }else if(default_meta_param()[parameter==variable & type=="nagg"]$value=="mean"){
              afd_global <- afd_global %>% group_by_at(setdiff(names(afd_global), "value")) %>% summarize(value=mean(value), .groups = 'drop')
            }
            }else{
              afd_global <- afd_global %>% group_by_at(setdiff(names(afd_global), "value")) %>% summarize(value=sum(value), .groups = 'drop')
            }
      afd_global <- afd_global %>% mutate(n = "World") %>% as.data.frame()
      afd <- rbind(afd, afd_global[, names(afd)])
      }
      #same for EU
      if(nrow(afd)>0){
        eu <- get_witch("eu"); if(!exists("eu")) eu_regions <- c("europe") else eu_regions <- unique(eu$n)
        afd_global <- afd %>% filter(n %in% eu_regions)
        afd_global$n <- NULL
        if(variable %in% default_meta_param()$parameter){
          if(default_meta_param()[parameter==variable & type=="nagg"]$value=="sum"){
            afd_global <- afd_global %>% group_by_at(setdiff(names(afd_global), "value")) %>% summarize(value=sum(value), .groups = 'drop')
          }else if(default_meta_param()[parameter==variable & type=="nagg"]$value=="mean"){
            afd_global <- afd_global %>% group_by_at(setdiff(names(afd_global), "value")) %>% summarize(value=mean(value), .groups = 'drop')
          }
        }else{
          afd_global <- afd_global %>% group_by_at(setdiff(names(afd_global), "value")) %>% summarize(value=sum(value), .groups = 'drop')
        }
        afd_global <- afd_global %>% mutate(n = "EU") %>% as.data.frame()
        afd <- rbind(afd, afd_global[, names(afd)])
      }
      
      #scenarios, potentially add stochastic scenarios to show
      afd <- subset(afd, file %in% c(scenarios, paste0(scenarios, "(b1)"),paste0(scenarios, "(b2)"), paste0(scenarios, "(b3)")) | str_detect(file, "historical") | str_detect(file, "valid"))
    
      #Unit conversion
      unit_conv <- unit_conversion(variable)
      afd$value <- afd$value * unit_conv$convert   
      afd$year <- ttoyear(afd$t)
      
      # If only World/EU is displayed or only one region, show files with colors
      if ( length(regions)==1 | (length(regions) == 1 & regions[1] %in% c("World","EU"))) {
        p <- ggplot(subset(afd, n %in% regions & 
                             !str_detect(file, "historical") & 
                             !str_detect(file, "valid")),
                    aes(ttoyear(t), value, colour=file)) + 
          geom_line(stat="identity", linewidth=1.5) + 
          xlab(NULL) + 
          ylab(unit_conv$unit) + 
          coord_cartesian(xlim = yearlim)
        
        # Add a horizontal line at y=0
        if(ylim_zero) {
          p <- p + geom_hline(yintercept = 0, alpha = 0.5)
        }
        
        p <- p + geom_line(data=subset(afd, n %in% regions & str_detect(file, "historical")),aes(year,value,colour=file), stat="identity", linewidth=1.0, linetype="solid")
        p <- p + geom_point(data=subset(afd, n %in% regions & str_detect(file, "valid")),aes(year,value,colour=file), size=4.0, shape=18)
        #legends:
        p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL))
      }else{
        p <- ggplot(subset(afd, n %in% regions & 
                             !str_detect(file, "historical") & 
                             !str_detect(file, "valid")),
                    aes(ttoyear(t), value, colour=n, linetype=file)) + 
          geom_line(stat="identity", linewidth=1.5) + 
          xlab(NULL) + 
          ylab(unit_conv$unit) + 
          scale_colour_manual(values = region_palette) + 
          coord_cartesian(xlim = yearlim) 

        # Add a horizontal line at y=0
        if(ylim_zero) {
          p <- p + geom_hline(yintercept = 0, alpha = 0.5)
        }
        
        p <- p + geom_line(data=subset(afd, n %in% regions & str_detect(file, "historical")),aes(year, value, colour=n, group=interaction(n, file)), linetype = "solid", stat="identity", linewidth=1.0)
        p <- p + geom_point(data=subset(afd, n %in% regions & str_detect(file, "valid")),aes(year, value, colour=n, shape=file), size=4.0)
        #legends:
        p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL, nrow = 2), linetype=guide_legend(title=NULL))
        
      }
      if(length(fullpathdir)!=1){p <- p + facet_grid(. ~ pathdir)}
      print(p + labs(title=variable))
  })
    
    
    
    # MAIN CODE FOR PLOTLY GENERATION (copied from standard ggplot)  
    output$gdxompaRplotly <- renderPlotly({
      assign("historical", input$add_historical, envir = .GlobalEnv)
      ylim_zero <- input$ylim_zero
      field_show <- input$field
      plotly_dynamic <- input$plotly_dynamic
      variable <- input$variable_selected
      if(is.null(variable)) variable <- list_of_variables[1]
      #get data
      afd <- get_witch(variable, check_calibration=TRUE, field = field_show)
      if(verbose) print(str_glue("Variable {variable} loaded."))
      #get the name of the additional set
      additional_sets <- setdiff(colnames(afd), c(file_group_columns, "pathdir", "t", "n", "value"))
      #extract additional set elements
      if(length(additional_sets)==0){additional_set_id="na"; set_elements = "na"; additional_set_id2="na"; set_elements2 = "na"}
      else if(length(additional_sets)==1)
      {
        additional_set_id <- additional_sets[1]
        set_elements <- unique(tolower(as.data.frame(afd)[, match(additional_set_id, colnames(afd))]))
        additional_set_id2="na"; set_elements2 = "na"
      }
      else if(length(additional_sets)==2)
      {
        additional_set_id <- additional_sets[1]
        set_elements <- unique(tolower(as.data.frame(afd)[, match(additional_set_id, colnames(afd))]))
        additional_set_id2 <- additional_sets[2] 
        set_elements2 <- unique(tolower(as.data.frame(afd)[, match(additional_set_id2, colnames(afd))]))
      }
      
      #Selector for additional set
      output$choose_additional_set <- renderUI({
        variable <- variable_selected_reactive()
        if(is.null(variable)) variable <- list_of_variables[1]
        sel <- input$additional_set_id_selected
        size_elements <- min(length(set_elements), 5)
        selectInput("additional_set_id_selected", "Additional set element", set_elements, size=size_elements, selectize = F, multiple = T, selected = sel)
      })
      #Selector for additional set #2
      output$choose_additional_set2 <- renderUI({
        variable <- variable_selected_reactive()
        if(is.null(variable)) variable <- list_of_variables[1]
        sel2 <- input$additional_set_id_selected2
        size_elements2 <- min(length(set_elements2), 5)
        selectInput("additional_set_id_selected2", "Additional set element 2", set_elements2, size=size_elements2, selectize = F, multiple = T, selected = sel2)
      })
      
      #get input from sliders/buttons
      yearlim <- input$yearlim
      additional_set_selected <- input$additional_set_id_selected
      additional_set_selected2 <- input$additional_set_id_selected2
      regions <- input$regions_selected
      scenarios <- input$scenarios_selected
      
      #in case they have not yet been set, set to default values
      if(is.null(regions)) regions <- display_regions
      if(is.null(additional_set_selected)) additional_set_selected <- set_elements[1]
      if((additional_set_id!="na" & additional_set_selected[1]=="na") | !(additional_set_selected[1] %in% set_elements)) additional_set_selected <- set_elements[1] 
      if(is.null(additional_set_selected2)) additional_set_selected2 <- set_elements2[1]
      if((additional_set_id2!="na" & additional_set_selected2[1]=="na") | !(additional_set_selected2[1] %in% set_elements2)) additional_set_selected2 <- set_elements2[1] 
      
      # SUBSET data and PLOT
      #choose additional selected element
      if(additional_set_id!="na"){
        afd[[additional_set_id]] <- tolower(afd[[additional_set_id]]) # to fix erroneous gams cases (y and Y etc.)
        afd <- subset(afd, get(additional_set_id) %in% additional_set_selected)
        afd[[additional_set_id]] <- NULL #remove additional set column
        #afd$t <- as.character(afd$t)
        if(length(additional_set_selected) >1) afd <- afd %>% group_by_at(setdiff(names(afd), "value")) %>% summarize(value=sum(value), .groups = 'drop')
      }
      if(additional_set_id2!="na"){
        afd[[additional_set_id2]] <- tolower(afd[[additional_set_id2]]) # to fix erroneous gams cases (y and Y etc.)
        afd <- subset(afd, get(additional_set_id2) %in% additional_set_selected2)
        afd[[additional_set_id2]] <- NULL #remove additional set column
        if(length(additional_set_selected2) >1) afd <- afd %>% group_by_at(setdiff(names(afd), "value")) %>% summarize(value=sum(value), .groups = 'drop')
      }
      
      #time frame
      afd <- subset(afd, ttoyear(t)>= yearlim[1] & ttoyear(t) <= yearlim[2])
      #clean data
      afd <- afd %>% filter(!is.na(value))
      
      #Computation of World/glboal sum/average
      #now based on meta param to guess max, mean, sum
      if(nrow(afd)>0){
        afd_global <- afd
        afd_global$n <- NULL
        if(variable %in% default_meta_param()$parameter){
          if(default_meta_param()[parameter==variable & type=="nagg"]$value=="sum"){
            afd_global <- afd_global %>% group_by_at(setdiff(names(afd_global), "value")) %>% summarize(value=sum(value), .groups = 'drop')
          }else if(default_meta_param()[parameter==variable & type=="nagg"]$value=="mean"){
            afd_global <- afd_global %>% group_by_at(setdiff(names(afd_global), "value")) %>% summarize(value=mean(value), .groups = 'drop')
          }else{
            afd_global <- afd_global %>% group_by_at(setdiff(names(afd_global), "value")) %>% summarize(value=sum(value), .groups = 'drop')
          }
        }
        afd_global <- afd_global %>% mutate(n = "World") %>% as.data.frame()
        afd <- rbind(afd, afd_global[, names(afd)])
      }
      
      #scenarios, potentially add stochastic scenarios to show
      afd <- subset(afd, file %in% c(scenarios, paste0(scenarios, "(b1)"),paste0(scenarios, "(b2)"), paste0(scenarios, "(b3)")) | str_detect(file, "historical") | str_detect(file, "valid"))
      
      #Unit conversion
      unit_conv <- unit_conversion(variable)
      afd$value <- afd$value * unit_conv$convert   
      afd$year <- ttoyear(afd$t)
      
      if(regions[1]=="World" | regions[1]=="EU" | length(regions)==1){#if only World is displayed or only one region, show files with colors
        p_dyn <- ggplot(subset(afd, n %in% regions & (!str_detect(file, "historical") & !str_detect(file, "valid"))),aes(year,value,colour=file)) + geom_line(stat="identity", linewidth=1.5) + xlab(NULL) + ylab(unit_conv$unit) + xlim(yearlim[1],yearlim[2])
        
        # Add a horizontal line at y=0
        if(ylim_zero) {
          p <- p + geom_hline(yintercept = 0, alpha = 0.5)
        }
        
        if("valid" %in% unique(afd %>% filter(n %in% regions))$file) p_dyn <- p_dyn + geom_point(data=subset(afd, n %in% regions & str_detect(file, "valid")),aes(year,value,colour=file), size=4.0, shape=18)

        #legends:
        p_dyn <- p_dyn + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL))
      }else{
        p_dyn <- ggplot(subset(afd, n %in% regions & (!str_detect(file, "historical") & !str_detect(file, "valid"))),aes(year,value,colour=n, linetype=file)) + geom_line(stat="identity", linewidth=1.5) + xlab(NULL) + ylab(unit_conv$unit) + scale_colour_manual(values = region_palette) + xlim(yearlim[1],yearlim[2])
        #if("historical" %in% unique(afd %>% filter(n %in% regions))$file) p_dyn <- p_dyn + geom_line(data=subset(afd, n %in% regions & str_detect(file, "historical")),aes(ttoyear(t),value,colour=n), linetype = "solid", stat="identity", size=1.0)
        if("valid" %in% unique(afd %>% filter(n %in% regions))$file) p_dyn <- p_dyn + geom_point(data=subset(afd, n %in% regions & str_detect(file, "valid")),aes(year,value, shape=file), size=4.0)
        #legends:
        p_dyn <- p_dyn + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL, nrow = 2), linetype=guide_legend(title=NULL))
      }
      if(length(fullpathdir)!=1){p_dyn <- p_dyn + facet_grid(. ~ pathdir)}
      p_dyn <- p_dyn + theme(legend.position = "none")
      print(p_dyn)
      if(length(ggplot_build(p_dyn)$data[[1]]) > 0) ggplotly()
    })
    
    
    output$inequalityplot <- renderPlot({
      #get input from sliders/buttons
      variable_ineq <- input$variable_selected
      yearlim <- input$yearlim
      additional_set_selected <- input$additional_set_id_selected
      additional_set_selected2 <- input$additional_set_id_selected2
      regions <- input$regions_selected
      scenarios <- input$scenarios_selected
      inequality_plot_type_selected <- input$inequality_plot_type_selected
      inequality_value_share <- input$inequality_value_share
      plot_inequality(variable = variable_ineq, plot_type = inequality_plot_type_selected, value_share = inequality_value_share, quantile_set = "dist", regions = regions[1], years = seq(yearlim[1], yearlim[2]), years_lorenz = range(yearlim[1], yearlim[2]), scenplot = scenarios)
    })
    

    output$Diagnostics <- renderPlot({
      #get input from sliders/buttons
      yearlim <- input$yearlim
      additional_set_selected <- input$additional_set_id_selected
      additional_set_selected2 <- input$additional_set_id_selected2
      regions <- input$regions_selected
      scenarios <- input$scenarios_selected
      diagnostics_plots(scenplot = scenarios)
    })
    
    
    output$energymixplot <- renderPlot({
      #get input from sliders/buttons
      yearlim <- input$yearlim
      additional_set_selected <- input$additional_set_id_selected
      additional_set_selected2 <- input$additional_set_id_selected2
      regions <- input$regions_selected
      scenarios <- input$scenarios_selected
      mix_plot_type_selected <- input$mix_plot_type_selected
      mix_y_value_selected <- input$mix_y_value_selected
      Primary_Energy_Mix(PES_y = mix_y_value_selected, regions = regions[1], years = seq(yearlim[1], yearlim[2], 1), plot_type = mix_plot_type_selected, scenplot = scenarios)
    })
    
    output$electricitymixplot <- renderPlot({
      #get input from sliders/buttons
      yearlim <- input$yearlim
      additional_set_selected <- input$additional_set_id_selected
      additional_set_selected2 <- input$additional_set_id_selected2
      regions <- input$regions_selected
      scenarios <- input$scenarios_selected
      mix_plot_type_selected <- input$mix_plot_type_selected
      mix_y_value_selected <- input$mix_y_value_selected
      Electricity_Mix(Electricity_y = mix_y_value_selected, regions = regions[1], years = seq(yearlim[1], yearlim[2], 1), plot_type = mix_plot_type_selected, scenplot = scenarios)
    })
  
    output$investmentplot <- renderPlot({
      #get input from sliders/buttons
      yearlim <- input$yearlim
      additional_set_selected <- input$additional_set_id_selected
      additional_set_selected2 <- input$additional_set_id_selected2
      regions <- input$regions_selected
      scenarios <- input$scenarios_selected
      Investment_Plot(regions="World", scenplot = scenarios)
    })
    
    output$policycostplot <- renderPlot({
      #get input from sliders/buttons
      yearlim <- input$yearlim
      additional_set_selected <- input$additional_set_id_selected
      additional_set_selected2 <- input$additional_set_id_selected2
      regions <- input$regions_selected
      scenarios <- input$scenarios_selected
      Policy_Cost(discount_rate=5, regions=regions, bauscen = scenarios[1], show_numbers=TRUE, tmax=yeartot(yearlim[2]))
    })
    
    output$intensityplot <- renderPlot({
      #get input from sliders/buttons
      yearlim <- input$yearlim
      additional_set_selected <- input$additional_set_id_selected
      additional_set_selected2 <- input$additional_set_id_selected2
      regions <- input$regions_selected
      scenarios <- input$scenarios_selected
      Intensity_Plot(years=c(yearlim[2], yearlim[2]-50), regions = regions, year0=2010, scenplot = scenarios, animate_plot = FALSE)
    })
    
    output$impactmap <- renderPlot({
      #get input from sliders/buttons
      yearlim <- input$yearlim
      additional_set_selected <- input$additional_set_id_selected
      additional_set_selected2 <- input$additional_set_id_selected2
      regions <- input$regions_selected
      scenarios <- input$scenarios_selected
      
      t_map = yeartot(yearlim[2]); bau_scen = scenarios[1]
      Q <- get_witch("Q")
      impact_map_data <- Q %>% filter(iq=="y" & t==t_map) %>% group_by(n, pathdir) %>% mutate(value = -((value/sum(value[file==bau_scen]))-1)*100) %>% filter(is.finite(value))
      scen <- scenarios[2]
        witchmap(impact_map_data, file_report=scen, t_report=t_map, mapcolor="Reds", map_name="Impact Map", map_legend = str_glue("GDP loss [%] in {scen}."))
    })
    
    output$climate_plot <- renderPlot({
      #get input from sliders/buttons
      yearlim <- input$yearlim
      additional_set_selected <- input$additional_set_id_selected
      additional_set_selected2 <- input$additional_set_id_selected2
      regions <- input$regions_selected
      scenarios <- input$scenarios_selected
      climate_plot(scenplot = scenarios)
    })
    
    output$SCC_plot <- renderPlot({
      #get input from sliders/buttons
      yearlim <- input$yearlim
      additional_set_selected <- input$additional_set_id_selected
      additional_set_selected2 <- input$additional_set_id_selected2
      regions <- input$regions_selected
      scenarios <- input$scenarios_selected
      scc_normalization_region <- input$scc_normalization_region
      SCC_plot(scenplot = scenarios, regions = regions, normalization_region = scc_normalization_region)
    })

    
    
})