# Define server 
shinyServer(function(input, output, session) {
  
    #some global flags
    verbose = FALSE
    save_plot = FALSE
    
    #get list of variables and parameters in all files
    # list_of_variables <- NULL
    # for(f in filelist){
    #   .gdx <- gdx(paste(file.path(fullpathdir[1], f),".gdx",sep=""))
    #   list_of_variables <- c(list_of_variables, all_items(.gdx)$variables, all_items(.gdx)$parameters)
    # }
    # list_of_variables <- unique(list_of_variables)
    # #list_of_variables <- c(sort(str_subset(list_of_variables, "^[:upper:]")), sort(str_subset(list_of_variables, "^[:lower:]")))
    list_of_variables <- c("Q", "Q_EN", "Q_FUEL", "Q_OUT", "Q_EMI", "K", "K_EN", "I_EN", "I", "FPRICE", "MCOST_INV", "COST_EMI", "MCOST_EMI", "CPRICE", "MCOST_FUEL", "TEMP", "TRF", "OMEGA", "Q_FEN", "Q_IN", "ykali", "tpes", "carbonprice", "emi_cap", "l")
    
    #Scenario selector
    output$select_scenarios <- renderUI({
      selectInput("scenarios_selected", "Select scenarios", scenlist, size=length(scenlist), selectize = F, multiple = T, selected = scenlist)
    })  
    
    #Variable selector
    output$select_variable <- renderUI({
    selectInput("variable_selected", "Select variable", list_of_variables, size=1, selectize = F, multiple = F, selected = list_of_variables[1])
    })  
    variable_selected_reactive <- reactive({input$variable_selected})
    
    #Display selected variable and set
    output$varname <- renderText({  
      paste("Variable:", variable_selected_reactive()," Element 1:", paste(input$additional_set_id_selected, collapse=","), " Element 2:", paste(input$additional_set_id_selected2, collapse=","))
    }) 

    #REGION selector
    output$select_regions <- renderUI({
      regions_for_selector <- c(witch_regions, "World")
    selectInput("regions_selected", "Select regions", regions_for_selector, size=length(regions_for_selector), selectize = F, multiple = T, selected = witch_regions)
    })
  
    #Additional selector for specific Panels
    
    

    # MAIN CODE FOR PLOT GENERATION  
    output$gdxompaRplot <- renderPlot({
      assign("historical", input$add_historical, envir = .GlobalEnv)
      ylim_zero <- input$ylim_zero
      #plotly_dynamic <- input$plotly_dynamic
      variable <- input$variable_selected
      if(is.null(variable)) variable <- list_of_variables[1]
      #get data
      afd <- get_witch_simple(variable, check_calibration=TRUE, results = "return")
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
      yearmin = input$yearmin
      yearmax = input$yearmax
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
      afd <- subset(afd, ttoyear(t)>=yearmin & ttoyear(t)<=yearmax)
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
      afd <- rbind(afd, afd_global[,c("t","n","value",file_group_columns, "pathdir")])
      }
      
      #scenarios, potentially add stochastic scenarios to show
      afd <- subset(afd, file %in% c(scenarios, paste0(scenarios, "(b1)"),paste0(scenarios, "(b2)"), paste0(scenarios, "(b3)")) | str_detect(file, "historical") | str_detect(file, "valid"))
    
      #Unit conversion
      unit_conv <- unit_conversion(variable)
      afd$value <- afd$value * unit_conv$convert   
      afd$year <- ttoyear(afd$t)
      
      if(regions[1]=="World" | length(regions)==1){#if only World is displayed or only one region, show files with colors
        p <- ggplot(subset(afd, n %in% regions & (!str_detect(file, "historical") & !str_detect(file, "valid"))),aes(ttoyear(t),value,colour=file)) + geom_line(stat="identity", size=1.5) + xlab("year") + ylab(unit_conv$unit) + xlim(yearmin,yearmax)
        if(ylim_zero) p <- p + ylim(0, NA)
        p <- p + geom_line(data=subset(afd, n %in% regions & str_detect(file, "historical")),aes(year,value,colour=file), stat="identity", size=1.0, linetype="solid")
        p <- p + geom_point(data=subset(afd, n %in% regions & str_detect(file, "valid")),aes(year,value,colour=file), size=4.0, shape=18)
        #legends:
        p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL))
      }else{
        p <- ggplot(subset(afd, n %in% regions & (!str_detect(file, "historical") & !str_detect(file, "valid"))),aes(ttoyear(t),value,colour=n, linetype=file)) + geom_line(stat="identity", size=1.5) + xlab("year") + ylab(unit_conv$unit) + scale_colour_manual(values = region_palette) + xlim(yearmin,yearmax)
        p <- p + geom_line(data=subset(afd, n %in% regions & str_detect(file, "historical")),aes(year, value, colour=n, group=interaction(n, file)), linetype = "solid", stat="identity", size=1.0)
        p <- p + geom_point(data=subset(afd, n %in% regions & str_detect(file, "valid")),aes(year, value, colour=n, shape=file), size=4.0)
        #legends:
        p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL, nrow = 2), linetype=guide_legend(title=NULL))
        
      }
      if(length(fullpathdir)!=1){p <- p + facet_grid(. ~ pathdir)}
      print(p + labs(title=variable))
      if(save_plot) saveplot(variable)
  })
    
    
    
    # MAIN CODE FOR PLOTLY GENERATION (copied from standard ggplot)  
    output$gdxompaRplotly <- renderPlotly({
      assign("historical", input$add_historical, envir = .GlobalEnv)
      ylim_zero <- input$ylim_zero
      plotly_dynamic <- input$plotly_dynamic
      variable <- input$variable_selected
      if(is.null(variable)) variable <- list_of_variables[1]
      #get data
      afd <- get_witch_simple(variable, check_calibration=TRUE, results = "return")
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
      yearmin = input$yearmin
      yearmax = input$yearmax
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
      afd <- subset(afd, ttoyear(t)>=yearmin & ttoyear(t)<=yearmax)
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
        afd <- rbind(afd, afd_global[,c("t","n","value",file_group_columns, "pathdir")])
      }
      
      #scenarios, potentially add stochastic scenarios to show
      afd <- subset(afd, file %in% c(scenarios, paste0(scenarios, "(b1)"),paste0(scenarios, "(b2)"), paste0(scenarios, "(b3)")) | str_detect(file, "historical") | str_detect(file, "valid"))
      
      #Unit conversion
      unit_conv <- unit_conversion(variable)
      afd$value <- afd$value * unit_conv$convert   
      afd$year <- ttoyear(afd$t)
      
      if(regions[1]=="World" | length(regions)==1){#if only World is displayed or only one region, show files with colors
        p_dyn <- ggplot(subset(afd, n %in% regions & (!str_detect(file, "historical") & !str_detect(file, "valid"))),aes(year,value,colour=file)) + geom_line(stat="identity", size=1.5) + xlab("year") + ylab(unit_conv$unit) + xlim(yearmin,yearmax)
        if(ylim_zero) p_dyn <- p_dyn + ylim(0, NA)
        #p_dyn <- p_dyn + geom_line(data=subset(afd, n %in% regions & str_detect(file, "historical")),aes(year,value,colour=file), stat="identity", size=1.0, linetype="solid")
        p_dyn <- p_dyn + geom_point(data=subset(afd, n %in% regions & str_detect(file, "valid")),aes(year,value,colour=file), size=4.0, shape=18)
        #legends:
        p_dyn <- p_dyn + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL))
      }else{
        p_dyn <- ggplot(subset(afd, n %in% regions & (!str_detect(file, "historical") & !str_detect(file, "valid"))),aes(year,value,colour=n, linetype=file)) + geom_line(stat="identity", size=1.5) + xlab("year") + ylab(unit_conv$unit) + scale_colour_manual(values = region_palette) + xlim(yearmin,yearmax)
        #p_dyn <- p_dyn + geom_line(data=subset(afd, n %in% regions & str_detect(file, "historical")),aes(ttoyear(t),value,colour=n), linetype = "solid", stat="identity", size=1.0)
        p_dyn <- p_dyn + geom_point(data=subset(afd, n %in% regions & str_detect(file, "valid")),aes(year,value, shape=file), size=4.0)
        #legends:
        p_dyn <- p_dyn + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL, nrow = 2), linetype=guide_legend(title=NULL))
      }
      if(length(fullpathdir)!=1){p_dyn <- p_dyn + facet_grid(. ~ pathdir)}
      p_dyn <- p_dyn + theme(legend.position = "none")
      print(p_dyn)
      suppressWarnings(ggplotly()) #to be done: fix error "argument 1 is not a vector", shoudl be done by plotly package
      
    })
    
    
    output$inequalityplot <- renderPlot({
      #get input from sliders/buttons
      variable_ineq <- input$variable_selected
      yearmin = input$yearmin
      yearmax = input$yearmax
      additional_set_selected <- input$additional_set_id_selected
      additional_set_selected2 <- input$additional_set_id_selected2
      regions <- input$regions_selected
      scenarios <- input$scenarios_selected
      inequality_plot_type_selected <- input$inequality_plot_type_selected
      inequality_value_share <- input$inequality_value_share
      plot_inequality(variable = variable_ineq, plot_type = inequality_plot_type_selected, value_share = inequality_value_share, quantile_set = "dist", regions = regions[1], years = seq(yearmin, yearmax), years_lorenz = range(yearmin, yearmax), scenplot = scenarios)
    })
    

    output$Diagnostics <- renderPlot({
      #get input from sliders/buttons
      yearmin = input$yearmin
      yearmax = input$yearmax
      additional_set_selected <- input$additional_set_id_selected
      additional_set_selected2 <- input$additional_set_id_selected2
      regions <- input$regions_selected
      scenarios <- input$scenarios_selected
      diagnostics_plots(scenplot = scenarios)
    })
    
    
    output$energymixplot <- renderPlot({
      #get input from sliders/buttons
      yearmin = input$yearmin
      yearmax = input$yearmax
      additional_set_selected <- input$additional_set_id_selected
      additional_set_selected2 <- input$additional_set_id_selected2
      regions <- input$regions_selected
      scenarios <- input$scenarios_selected
      mix_plot_type_selected <- input$mix_plot_type_selected
      mix_y_value_selected <- input$mix_y_value_selected
      Primary_Energy_Mix(PES_y = mix_y_value_selected, regions = regions[1], years = seq(yearmin, yearmax, 1), plot_type = mix_plot_type_selected, scenplot = scenarios)
    })
    
    output$electricitymixplot <- renderPlot({
      #get input from sliders/buttons
      yearmin = input$yearmin
      yearmax = input$yearmax
      additional_set_selected <- input$additional_set_id_selected
      additional_set_selected2 <- input$additional_set_id_selected2
      regions <- input$regions_selected
      scenarios <- input$scenarios_selected
      mix_plot_type_selected <- input$mix_plot_type_selected
      mix_y_value_selected <- input$mix_y_value_selected
      Electricity_Mix(Electricity_y = mix_y_value_selected, regions = regions[1], years = seq(yearmin, yearmax, 1), plot_type = mix_plot_type_selected, scenplot = scenarios)
    })
  
    output$investmentplot <- renderPlot({
      #get input from sliders/buttons
      yearmin = input$yearmin
      yearmax = input$yearmax
      additional_set_selected <- input$additional_set_id_selected
      additional_set_selected2 <- input$additional_set_id_selected2
      regions <- input$regions_selected
      scenarios <- input$scenarios_selected
      Investment_Plot(regions="World", scenplot = scenarios)
    })
    
    output$policycostplot <- renderPlot({
      #get input from sliders/buttons
      yearmin = input$yearmin
      yearmax = input$yearmax
      additional_set_selected <- input$additional_set_id_selected
      additional_set_selected2 <- input$additional_set_id_selected2
      regions <- input$regions_selected
      scenarios <- input$scenarios_selected
      Policy_Cost(discount_rate=5, regions=regions, bauscen = scenarios[1], show_numbers=TRUE, tmax=yeartot(yearmax))
    })
    
    output$intensityplot <- renderPlot({
      #get input from sliders/buttons
      yearmin = input$yearmin
      yearmax = input$yearmax
      additional_set_selected <- input$additional_set_id_selected
      additional_set_selected2 <- input$additional_set_id_selected2
      regions <- input$regions_selected
      scenarios <- input$scenarios_selected
      Intensity_Plot(years=c(yearmax, yearmax-50), regions = regions, year0=2010, scenplot = scenarios, animate_plot = FALSE)
    })
    
    output$impactmap <- renderPlot({
      #get input from sliders/buttons
      yearmin = input$yearmin
      yearmax = input$yearmax
      additional_set_selected <- input$additional_set_id_selected
      additional_set_selected2 <- input$additional_set_id_selected2
      regions <- input$regions_selected
      scenarios <- input$scenarios_selected
      
      t_map = yeartot(yearmax); bau_scen = scenarios[1]
      get_witch_simple("Q")
      impact_map_data <- Q %>% filter(iq=="y" & t==t_map) %>% group_by(n, pathdir) %>% mutate(value = -((value/sum(value[file==bau_scen]))-1)*100) %>% filter(is.finite(value))
      scen <- scenarios[2]
        witchmap(impact_map_data, file_report=scen, t_report=t_map, mapcolor="Reds", map_name="Impact Map", map_legend = str_glue("GDP loss [%] in {scen}."))
    })
    
    output$climate_plot <- renderPlot({
      #get input from sliders/buttons
      yearmin = input$yearmin
      yearmax = input$yearmax
      additional_set_selected <- input$additional_set_id_selected
      additional_set_selected2 <- input$additional_set_id_selected2
      regions <- input$regions_selected
      scenarios <- input$scenarios_selected
      climate_plot(scenplot = scenarios)
    })
    
    output$SCC_plot <- renderPlot({
      #get input from sliders/buttons
      yearmin = input$yearmin
      yearmax = input$yearmax
      additional_set_selected <- input$additional_set_id_selected
      additional_set_selected2 <- input$additional_set_id_selected2
      regions <- input$regions_selected
      scenarios <- input$scenarios_selected
      scc_normalization_region <- input$scc_normalization_region
      SCC_plot(scenplot = scenarios, regions = regions, normalization_region = scc_normalization_region)
    })

    
    
})