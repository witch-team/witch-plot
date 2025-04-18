# Define server
shinyServer(function(input, output, session) {
  # some global flags
  verbose <- FALSE
  save_plot <- FALSE
  growth_rate <- FALSE

  # get list of variables and parameters in all files
  list_of_variables <- NULL
  for (f in filelist) {
    .gdx <- gdx(paste(file.path(fullpathdir[1], f), ".gdx", sep = ""))
    list_of_variables <- c(list_of_variables, all_items(.gdx)$variables)
    list_of_variables <- c(list_of_variables, all_items(.gdx)$parameters) # also all parameters
  }
  list_of_variables <- unique(list_of_variables)
  list_of_variables <- c(sort(str_subset(list_of_variables, "^[:upper:]")), sort(str_subset(list_of_variables, "^[:lower:]")))
  list_of_variables <- str_subset(list_of_variables, pattern = "_t$") #FIDELIO keep only time dependent variables

  # Scenario selector
  output$select_scenarios <- renderUI({
    selectInput("scenarios_selected", "Select scenarios", unname(scenlist), size = length(scenlist), selectize = F, multiple = T, selected = unname(scenlist))
  })

  # Variable selector
  output$select_variable <- renderUI({
    selectInput("variable_selected", "Select variable", list_of_variables, size = 1, selectize = F, multiple = F, selected = list_of_variables[1])
  })
  variable_selected_reactive <- reactive({
    input$variable_selected
  })
  
  # Reactively update variable selector
  variable_input <- reactive({
    return(input$variable_selected)
  })

  # Display selected variable and set
  output$varname <- renderText({
    paste("Variable:", variable_selected_reactive(), " Element:", paste(input$additional_set_id_selected, collapse = ","))
  })

  # Display selected variable and set
  output$varname2 <- renderText({
    paste("Variable:", variable_selected_reactive(), " Element:", paste(input$additional_set_id_selected, collapse = ","))
  })

  # REGION selector
  output$select_regions <- renderUI({
    regions_for_selector <- c(witch_regions, "World")
    selectInput("regions_selected", "Select regions", regions_for_selector, size = min(17, length(regions_for_selector)), selectize = F, multiple = T, selected = witch_regions)
  })

  observeEvent(input$button_saveplotdata, {
    variable <- input$variable_selected
    print("Current plot saved in subdirectory 'graphs'")
    saveplot(variable, width = 14, height = 7)
  })

  # Additional selector for specific Panels



  # MAIN CODE FOR PLOT GENERATION
  output$gdxcompaRplot <- renderPlot({
    assign("historical", input$add_historical, envir = .GlobalEnv)
    ylim_zero <- input$ylim_zero
    field_show <- input$field
    growth_rate <- input$growth_rate
    # plotly_dynamic <- input$plotly_dynamic
    variable <- input$variable_selected
    if (is.null(variable)) variable <- list_of_variables[1]
    # get data
    afd <- get_witch(variable, check_calibration = TRUE, field = field_show)
    if (verbose) print(str_glue("Variable {variable} loaded."))
    # get the name of the additional set
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
      variable <- variable_input()
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
      variable <- variable_input()
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
    # get input from sliders/buttons
    yearlim <- input$yearlim
    additional_set_selected <- input$additional_set_id_selected
    regions <- input$regions_selected
    scenarios <- input$scenarios_selected

    # in case they have not yet been set, set to default values
    if (is.null(regions)) regions <- display_regions
    if (is.null(additional_set_selected)) additional_set_selected <- set_elements[1]
    if ((additional_set_id != "na" & additional_set_selected[1] == "na") | !(additional_set_selected[1] %in% set_elements)) additional_set_selected <- set_elements[1]

    # SUBSET data and PLOT
    # choose additional selected element
    if (additional_set_id != "na") {
      afd[[additional_set_id]] <- tolower(afd[[additional_set_id]]) # to fix erroneous gams cases (y and Y etc.)
      afd <- subset(afd, get(additional_set_id) %in% additional_set_selected)
      afd[[additional_set_id]] <- NULL # remove additional set column
      # afd$t <- as.character(afd$t)
      if (length(additional_set_selected) > 1) {
        afd <- afd %>%
          group_by_at(setdiff(names(afd), "value")) %>%
          summarize(value = sum(value), .groups = "drop")
      }
    }

    # time frame
    afd <- subset(afd, ttoyear(t) >= yearlim[1] & ttoyear(t) <= yearlim[2])
    # clean data
    afd <- afd %>% filter(!is.na(value))

    # Computation of World/global sum/average
    # now based on meta param to guess max, mean, sum
    if (nrow(afd) > 0) {
      afd_global <- afd
      afd_global$n <- NULL
      if (variable %in% default_meta_param()$parameter) {
        if (default_meta_param()[parameter == variable & type == "nagg"]$value == "sum") {
          afd_global <- afd_global %>%
            group_by_at(setdiff(names(afd_global), "value")) %>%
            summarize(value = sum(value), .groups = "drop")
        } else if (default_meta_param()[parameter == variable & type == "nagg"]$value == "mean") {
          afd_global <- afd_global %>%
            group_by_at(setdiff(names(afd_global), "value")) %>%
            summarize(value = mean(value), .groups = "drop")
        }
      } else {
        afd_global <- afd_global %>%
          group_by_at(setdiff(names(afd_global), "value")) %>%
          summarize(value = sum(value), .groups = "drop")
      }
      afd_global <- afd_global %>%
        mutate(n = "World") %>%
        as.data.frame()
      afd <- rbind(afd, afd_global[, names(afd)])
    }

    # in case growth rates
    if (growth_rate) {
      afd <- afd %>%
        group_by_at(setdiff(names(afd), c("t", "value"))) %>%
        arrange(t) %>%
        mutate(year = ttoyear(t), growthrate = ((value / lag(value))^(1 / (year - lag(year))) - 1) * 100) %>%
        select(-year, -value) %>%
        dplyr::rename(value = growthrate) %>%
        mutate(value = ifelse(is.na(value), 0, value)) %>%
        ungroup()
    }
    
    #all upper case except for world
    afd$n <- ifelse(afd$n == "World", "World", toupper(afd$n))


    # scenarios, potentially add stochastic scenarios to show
    afd <- subset(afd, file %in% c(scenarios, paste0(scenarios, "(b1)"), paste0(scenarios, "(b2)"), paste0(scenarios, "(b3)")) | str_detect(file, "historical") | str_detect(file, "valid"))

    # Unit conversion
    unit_conv <- unit_conversion(variable)
    if (growth_rate) unit_conv$unit <- " % p.a."
    unit_conv$convert <- 1
    afd$value <- afd$value * unit_conv$convert
    afd$year <- ttoyear(afd$t)

    if (regions[1] == "World" | length(regions) == 1) { # if only World is displayed or only one region, show files with colors
      p <- ggplot(subset(afd, n %in% regions & (!str_detect(file, "historical") & !str_detect(file, "valid"))), aes(ttoyear(t), value, colour = file)) +
        geom_line(stat = "identity", linewidth = 1.5) +
        xlab("year") +
        ylab(unit_conv$unit) +
        xlim(yearlim[1], yearlim[2])
      if (ylim_zero) p <- p + ylim(0, NA)
      p <- p + geom_line(data = subset(afd, n %in% regions & str_detect(file, "historical")), aes(year, value, colour = file), stat = "identity", linewidth = 1.0, linetype = "solid")
      # legends:
      p <- p + theme(text = element_text(size = 16), legend.position = "bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title = element_blank()) + guides(color = guide_legend(title = NULL))
    } else {
      p <- ggplot(subset(afd, n %in% regions & (!str_detect(file, "historical") & !str_detect(file, "valid"))), aes(ttoyear(t), value, colour = n, linetype = file)) +
        geom_line(stat = "identity", linewidth = 1.5) +
        xlab("year") +
        ylab(unit_conv$unit) +
        scale_colour_manual(values = region_palette) +
        xlim(yearlim[1], yearlim[2])
      p <- p + geom_line(data = subset(afd, n %in% regions & str_detect(file, "historical")), aes(year, value, colour = n, group = interaction(n, file)), linetype = "solid", stat = "identity", linewidth = 1.0)
      # legends:
      p <- p + theme(text = element_text(size = 16), legend.position = "bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title = element_blank()) + guides(color = guide_legend(title = NULL, nrow = 2), linetype = guide_legend(title = NULL))
    }
    if (length(fullpathdir) != 1) {
      p <- p + facet_grid(. ~ pathdir)
    }
    if(nrow(afd)>0) print(p + labs(title = variable))
  })



  # RICE50x Stacked area plot
  output$gdxcompaRstackedplot <- renderPlot({
    assign("historical", input$add_historical, envir = .GlobalEnv)
    ylim_zero <- input$ylim_zero
    variable <- input$variable_selected
    if (is.null(variable)) variable <- list_of_variables[1]
    # get data
    afd <- get_witch(variable, check_calibration = TRUE)
    if (verbose) print(str_glue("Variable {variable} loaded."))
    # get the name of the additional set
    additional_sets <- setdiff(colnames(afd), c(file_group_columns, "pathdir", "t", "n", "value"))
    # extract additional set elements
    if (length(additional_sets) == 0) {
      additional_set_id <- "na"
      set_elements <- "na"
      additional_set_id2 <- "na"
      set_elements2 <- "na"
    } else if (length(additional_sets) == 1) {
      additional_set_id <- additional_sets[1]
      set_elements <- unique(tolower(as.data.frame(afd)[, match(additional_set_id, colnames(afd))]))
    } else if (length(additional_sets) == 2) {
      additional_set_id <- additional_sets[1]
      set_elements <- unique(tolower(as.data.frame(afd)[, match(additional_set_id, colnames(afd))]))
    }

    # Selector for additional set
    output$choose_additional_set <- renderUI({
      variable <- variable_selected_reactive()
      if (is.null(variable)) variable <- list_of_variables[1]
      sel <- input$additional_set_id_selected
      size_elements <- min(length(set_elements), 5)
      selectInput("additional_set_id_selected", "Additional set element", set_elements, size = size_elements, selectize = F, multiple = T, selected = sel)
    })

    # get input from sliders/buttons
    yearlim <- input$yearlim
    additional_set_selected <- input$additional_set_id_selected
    regions <- input$regions_selected
    scenarios <- input$scenarios_selected

    # in case they have not yet been set, set to default values
    if (is.null(regions)) regions <- display_regions
    if (is.null(additional_set_selected)) additional_set_selected <- set_elements[1]
    if ((additional_set_id != "na" & additional_set_selected[1] == "na") | !(additional_set_selected[1] %in% set_elements)) additional_set_selected <- set_elements[1]

    # SUBSET data and PLOT
    # choose additional selected element
    if (additional_set_id != "na") {
      afd[[additional_set_id]] <- tolower(afd[[additional_set_id]]) # to fix erroneous gams cases (y and Y etc.)
      afd <- subset(afd, get(additional_set_id) %in% additional_set_selected)
      afd[[additional_set_id]] <- NULL # remove additional set column
      if (length(additional_set_selected) > 1) {
        afd <- afd %>%
          group_by_at(setdiff(names(afd), "value")) %>%
          summarize(value = sum(value), .groups = "drop")
      }
    }

    # time frame
    afd <- subset(afd, ttoyear(t) >= yearlim[1] & ttoyear(t) <= yearlim[2])
    # clean data
    afd <- afd %>% filter(!is.na(value))

    # scenarios, potentially add stochastic scenarios to show
    afd <- subset(afd, file %in% c(scenarios, paste0(scenarios, "(b1)"), paste0(scenarios, "(b2)"), paste0(scenarios, "(b3)")) | str_detect(file, "historical") | str_detect(file, "valid"))

    # remove duplicate historical/validation data
    # afd <- subset(afd, file %in% c(scenarios, str_subset(unique(afd$file), "valid")[1], str_subset(unique(afd$file), "historical")[1]))
    # for figure on files add historical for evrey scenario
    afd_hist <- subset(afd, file %in% c(str_subset(unique(afd$file), "historical")[1]))
    afd <- subset(afd, file %in% c(scenarios))
    for (scen in scenarios)
    {
      afd_hist$file <- scen
      if (scen == scenarios[1]) {
        afd_hist_temp <- afd_hist
      } else {
        afd_hist_temp <- rbind(afd_hist_temp, afd_hist)
      }
    }
    afd <- rbind(afd, afd_hist)

    # Unit conversion
    unit_conv <- unit_conversion(variable)
    afd$value <- afd$value * unit_conv$convert
    afd$year <- ttoyear(afd$t)

    p_stacked <- ggplot(subset(afd, n %in% regions & (!str_detect(file, "historical") & !str_detect(file, "valid"))), aes(ttoyear(t), value, fill = n)) +
      geom_area(stat = "identity", size = 1.5) +
      xlab("year") +
      ylab(unit_conv$unit) +
      scale_fill_manual(values = region_palette) +
      xlim(yearlim[1], yearlim[2])
    # p_stacked <- p_stacked + geom_area(data=subset(afd, n %in% regions & str_detect(file, "historical")),aes(year, value, fill=n), linetype = "solid", stat="identity", size=1.0)
    # p_stacked <- p_stacked + geom_area(data=subset(afd, n %in% regions & str_detect(file, "valid")),aes(year, value, fill=n), size=4.0)
    # legends:
    p_stacked <- p_stacked + theme(text = element_text(size = 16), legend.position = "bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title = element_blank()) + guides(fill = guide_legend(title = NULL, nrow = 2))
    if (!is.null(scenarios)) p_stacked <- p_stacked + facet_wrap(. ~ file)
    print(p_stacked + labs(title = variable))
  })







  # MAIN CODE FOR PLOTLY GENERATION (copied from standard ggplot)  
  output$gdxompaRplotly <- renderPlotly({
    assign("historical", input$add_historical, envir = .GlobalEnv)
    ylim_zero <- input$ylim_zero
    growth_rate <- input$growth_rate
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
      variable <- variable_input()
      if(is.null(variable)) variable <- list_of_variables[1]
      sel <- input$additional_set_id_selected
      size_elements <- min(length(set_elements), 5)
      selectInput("additional_set_id_selected", "Additional set element", set_elements, size=size_elements, selectize = F, multiple = T, selected = sel)
    })
    #Selector for additional set #2
    output$choose_additional_set2 <- renderUI({
      variable <- variable_input()
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
    
    # in case growth rates
    if (growth_rate) {
      afd <- afd %>%
        group_by_at(setdiff(names(afd), c("t", "value"))) %>%
        arrange(t) %>%
        mutate(year = ttoyear(t), growthrate = ((value / lag(value))^(1 / (year - lag(year))) - 1) * 100) %>%
        select(-year, -value) %>%
        dplyr::rename(value = growthrate) %>%
        ungroup()
    }
    
    #scenarios, potentially add stochastic scenarios to show
    afd <- subset(afd, file %in% c(scenarios, paste0(scenarios, "(b1)"),paste0(scenarios, "(b2)"), paste0(scenarios, "(b3)")) | str_detect(file, "historical") | str_detect(file, "valid"))
    
    #Unit conversion
    unit_conv <- unit_conversion(variable)
    afd$value <- afd$value * unit_conv$convert   
    afd$year <- ttoyear(afd$t)
    
    if(regions[1]=="World" | regions[1]=="EU" | length(regions)==1){#if only World is displayed or only one region, show files with colors
      p_dyn <- ggplot(subset(afd, n %in% regions & (!str_detect(file, "historical") & !str_detect(file, "valid"))),aes(year,value,colour=file)) + geom_line(stat="identity", linewidth=1.5) + xlab(NULL) + ylab(unit_conv$unit) + xlim(yearlim[1],yearlim[2])
      
      if(nrow(afd %>% filter(n %in% regions & str_detect(file, "historical"))) > 0 ) p_dyn <- p_dyn + geom_line(data=subset(afd, n %in% regions & str_detect(file, "historical")),aes(year,value,colour=file), stat="identity", linewidth=1.0, linetype="solid")
      if(nrow(afd %>% filter(n %in% regions & str_detect(file, "valid"))) > 0 ) p_dyn <- p_dyn + geom_point(data=subset(afd, n %in% regions & str_detect(file, "valid")),aes(year,value,colour=file), size=4.0, shape=18)
      
      # Add a horizontal line at y=0
      if(ylim_zero) {
        p <- p + geom_hline(yintercept = 0, alpha = 0.5)
      }
      
      if("valid" %in% unique(afd %>% filter(n %in% regions))$file) p_dyn <- p_dyn + geom_point(data=subset(afd, n %in% regions & str_detect(file, "valid")),aes(year,value,colour=file), size=4.0, shape=18)
      
      #legends:
      p_dyn <- p_dyn + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL))
    }else{
      p_dyn <- ggplot(subset(afd, n %in% regions & (!str_detect(file, "historical") & !str_detect(file, "valid"))),aes(year,value,colour=n, linetype=file)) + geom_line(stat="identity", linewidth=1.5) + xlab(NULL) + ylab(unit_conv$unit) + scale_colour_manual(values = region_palette) + xlim(yearlim[1],yearlim[2])
      if("historical" %in% unique(afd %>% filter(n %in% regions))$file) p_dyn <- p_dyn + geom_line(data=subset(afd, n %in% regions & str_detect(file, "historical")),aes(ttoyear(t),value,colour=n), linetype = "solid", stat="identity", size=1.0)
      if("valid" %in% unique(afd %>% filter(n %in% regions))$file) p_dyn <- p_dyn + geom_point(data=subset(afd, n %in% regions & str_detect(file, "valid")),aes(year,value, shape=file), size=4.0)
      #legends:
      p_dyn <- p_dyn + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL, nrow = 2), linetype=guide_legend(title=NULL))
    }
    if(length(fullpathdir)!=1){p_dyn <- p_dyn + facet_grid(. ~ pathdir)}
    p_dyn <- p_dyn + theme(legend.position = "none")
    print(p_dyn)
    if(length(ggplot_build(p_dyn)$data[[1]]) > 0) ggplotly()
  })
  


  output$diagnostics <- renderPlot({
    # get input from sliders/buttons
    variable <- input$variable_selected
    yearlim <- input$yearlim
    scenarios <- input$scenarios_selected

    get_witch("elapsed")
    if (!exists("elapsed")) elapsed <- data.frame(file = scenlist, value = 0)
    get_witch("Y")
    get_witch("TATM")
    get_witch("MIU")
    get_witch("l")
    # get_witch("DAMFRAC")
    # compute Gini index
    gini <- Y %>%
      left_join(l %>% rename(pop = value), by = c("t", "n", "file", "pathdir")) %>%
      group_by(t, file, pathdir) %>%
      summarize(value = reldist::gini(value / pop, weights = pop))
    # style
    diagplot <- ggarrange(
      ggplot(elapsed %>% filter(file %in% scenarios)) +
        geom_bar(aes(file, value, fill = file), stat = "identity") +
        ylab("Run time (minutes)") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
        scale_y_time(labels = function(l) strftime(l, "%M:%S")),
      ggarrange(
        ggplot(MIU %>% group_by(t, file, pathdir) %>% summarise(value = mean(value)) %>% filter(file %in% scenarios)) +
          geom_line(aes(ttoyear(t), value, color = file), size = 1) +
          ylab("MIU") +
          xlab(""),
        ggplot(Y %>% filter(file %in% scenarios) %>% group_by(t, file, pathdir) %>% summarise(value = sum(value))) +
          geom_line(aes(ttoyear(t), value, color = file), size = 1) +
          ylab("GDP [T$]") +
          xlab(""),
        ncol = 2, common.legend = T, legend = "none"
      ),
      ggarrange(
        ggplot(TATM %>% filter(file %in% scenarios & !is.na(value))) +
          geom_line(aes(ttoyear(t), value, color = file), size = 1) +
          ylab("TATM") +
          xlab(""),
        ggplot(gini %>% filter(file %in% scenarios)) +
          geom_line(aes(ttoyear(t), value, color = file), size = 1) +
          ylab("Gini index") +
          xlab("") +
          ylim(0, 1),
        ncol = 2, common.legend = T, legend = "none"
      ),
      nrow = 3, common.legend = T, legend = "bottom"
    )
    print(diagplot)
  })


 })
