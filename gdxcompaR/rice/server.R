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
        selected = "E",
        options = list(
          `live-search` = TRUE)
      )
    })
    variable_selected_reactive <- reactive({input$variable_selected})

  # Display selected variable and set
  output$varname <- renderText({
    paste("Variable:", variable_selected_reactive(), " Element:", paste(input$additional_set_id_selected, collapse = ","))
  })

    #REGION selector
    output$select_regions <- renderUI({
      regions_for_selector <- list(Aggregate = list("World"),  
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
      p <- p + geom_point(data = subset(afd, n %in% regions & str_detect(file, "valid")), aes(year, value, colour = file), size = 4.0, shape = 18)
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
      p <- p + geom_point(data = subset(afd, n %in% regions & str_detect(file, "valid")), aes(year, value, colour = n, shape = file), size = 4.0)
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
    field_show <- input$field
    variable <- input$variable_selected
    if (is.null(variable)) variable <- list_of_variables[1]
    # get data
    afd <- get_witch(variable, check_calibration = TRUE, field = field_show)
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
    if(nrow(afd)>0) print(p_stacked + labs(title = variable))
  })








  # MAIN CODE FOR PLOTLY GENERATION (copied from standard ggplot)
  output$gdxompaRplotly <- renderPlotly({
    assign("historical", input$add_historical, envir = .GlobalEnv)
    ylim_zero <- input$ylim_zero
    field_show <- input$field
    growth_rate <- input$growth_rate
    plotly_dynamic <- input$plotly_dynamic
    variable <- input$variable_selected
    if (is.null(variable)) variable <- list_of_variables[1]
    # get data
    afd <- get_witch(variable, check_calibration = TRUE, field = field_show)
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
      additional_set_id2 <- "na"
      set_elements2 <- "na"
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
      # afd$t <- as.character(afd$t)
      if (length(additional_set_selected) > 1) {
        afd <- afd %>%
          group_by_at(setdiff(names(afd), "value")) %>%
          summarize(value = sum(value))
      }
    }

    # time frame
    afd <- subset(afd, ttoyear(t) >= yearlim[1] & ttoyear(t) <= yearlim[2])
    # clean data
    afd <- afd %>% filter(!is.na(value))

    # Computation of World/glboal sum/average
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
        ungroup()
    }

    # scenarios, potentially add stochastic scenarios to show
    afd <- subset(afd, file %in% c(scenarios, paste0(scenarios, "(b1)"), paste0(scenarios, "(b2)"), paste0(scenarios, "(b3)")) | str_detect(file, "historical") | str_detect(file, "valid"))

    # Unit conversion
    unit_conv <- unit_conversion(variable)
    if (growth_rate) unit_conv$unit <- " % p.a."
    unit_conv$convert <- 1
    afd$value <- afd$value * unit_conv$convert
    afd$year <- ttoyear(afd$t)

    if (regions[1] == "World" | length(regions) == 1) { # if only World is displayed or only one region, show files with colors
      p_dyn <- ggplot(subset(afd, n %in% regions & (!str_detect(file, "historical") & !str_detect(file, "valid"))), aes(year, value, colour = file)) +
        geom_line(stat = "identity", size = 1.5) +
        xlab("year") +
        ylab(unit_conv$unit) +
        xlim(yearlim[1], yearlim[2])
      if (ylim_zero) p_dyn <- p_dyn + ylim(0, NA)
      # p_dyn <- p_dyn + geom_line(data=subset(afd, n %in% regions & str_detect(file, "historical")),aes(year,value,colour=file), stat="identity", size=1.0, linetype="solid")
      p_dyn <- p_dyn + geom_point(data = subset(afd, n %in% regions & str_detect(file, "valid")), aes(year, value, colour = file), size = 4.0, shape = 18)
      # legends:
      p_dyn <- p_dyn + theme(text = element_text(size = 16), legend.position = "bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title = element_blank()) + guides(color = guide_legend(title = NULL))
    } else {
      p_dyn <- ggplot(subset(afd, n %in% regions & (!str_detect(file, "historical") & !str_detect(file, "valid"))), aes(year, value, colour = n, linetype = file)) +
        geom_line(stat = "identity", linewidth = 1.5) +
        xlab("year") +
        ylab(unit_conv$unit) +
        scale_colour_manual(values = region_palette) +
        xlim(yearlim[1], yearlim[2])
      # if("historical" %in% unique(allfilesdata %>% filter(n %in% regions))$file) p_dyn <- p_dyn + geom_line(data=subset(afd, n %in% regions & str_detect(file, "historical")),aes(ttoyear(t),value,colour=n), linetype = "solid", stat="identity", size=1.0)
      if("valid" %in% unique(allfilesdata %>% filter(n %in% regions))$file) p_dyn <- p_dyn + geom_point(data = subset(afd, n %in% regions & str_detect(file, "valid")), aes(year, value, shape = file), size = 4.0)
      # legends:
      p_dyn <- p_dyn + theme(text = element_text(size = 16), legend.position = "bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title = element_blank()) + guides(color = guide_legend(title = NULL, nrow = 2), linetype = guide_legend(title = NULL))
    }
    if (length(fullpathdir) != 1) {
      p_dyn <- p_dyn + facet_grid(. ~ pathdir)
    }
    p_dyn <- p_dyn + theme(legend.position = "none")
    if(nrow(afd)>0) print(p_dyn)
    if(length(ggplot_build(p_dyn)$data[[1]]) > 0) ggplotly()
  })



  output$gdxcompaRmap <- renderPlot({
    # get input from sliders/buttons
    variable <- input$variable_selected
    yearlim <- input$yearlim
    scenarios <- input$scenarios_selected
    map_new(variable, yearmap = yearlim[2], scenplot = scenarios, title = str_glue("{variable} in {yearlim[2]}"))
  })


  output$diagnostics <- renderPlot({
    # get input from sliders/buttons
    variable <- input$variable_selected
    yearlim <- input$yearlim
    scenarios <- input$scenarios_selected

    elapsed <- get_witch("elapsed")
    if (!exists("elapsed")) elapsed <- data.frame(file = scenlist, value = 0)
    Y <- get_witch("Y")
    TATM <- get_witch("TATM")
    MIU <- get_witch("MIU")
    l <- get_witch("l")
    # compute Gini index
    gini <- Y %>%
      left_join(l %>% rename(pop = value), by = c("t", "n", "file", "pathdir")) %>%
      group_by(t, file, pathdir) %>%
      summarize(value = reldist::gini(value / pop, weights = pop))
    # style
    diagplot <- list()
    for(p in subdir){
    diagplot[[p]] <- ggarrange(
      ggplot(elapsed %>% filter(file %in% scenarios & pathdir==p)) +
        geom_bar(aes(file, value, fill = file), stat = "identity") +
        ylab("Run time (minutes)") + ylim(0, max(elapsed$value)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) + ggtitle(p) +
        scale_y_continuous(limits = c(0, max(elapsed$value)), labels = function(l) strftime(as.POSIXct(l, origin = "1970-01-01"), "%M:%S"))
        ,
      ggarrange(
        ggplot(MIU %>% group_by(t, file, pathdir) %>% summarise(value = mean(value)) %>% filter(file %in% scenarios & pathdir==p)) +
          geom_line(aes(ttoyear(t), value, color = file), linewidth = 1) +
          ylab("MIU") +
          xlab(""),
        ggplot(Y %>% filter(file %in% scenarios & pathdir==p) %>% group_by(t, file, pathdir) %>% summarise(value = sum(value))) +
          geom_line(aes(ttoyear(t), value, color = file), linewidth = 1) +
          ylab("GDP [T$]") +
          xlab(""),
        ncol = 2, common.legend = T, legend = "none"
      ),
      ggarrange(
        ggplot(TATM %>% filter(file %in% scenarios & pathdir==p & !is.na(value))) +
          geom_line(aes(ttoyear(t), value, color = file), linewidth = 1) +
          ylab("TATM") +
          xlab(""),
        ggplot(gini %>% filter(file %in% scenarios & pathdir==p)) +
          geom_line(aes(ttoyear(t), value, color = file), linewidth = 1) +
          ylab("Gini index") +
          xlab("") +
          ylim(0, 1),
        ncol = 2, common.legend = T, legend = "none"
      ),
      nrow = 3, common.legend = T, legend = "bottom"
    )
    }
    diagplot_all <- ggarrange(plotlist = diagplot, ncol = length(diagplot), common.legend = T)
    print(diagplot_all)
  })


  output$inequalityplot <- renderPlot({
    # get input from sliders/buttons
    variable_ineq <- input$variable_selected
    yearlim <- input$yearlim
    regions <- input$regions_selected
    scenarios <- input$scenarios_selected
    inequality_plot_type_selected <- input$inequality_plot_type_selected
    inequality_value_share <- input$inequality_value_share
    plot_inequality(variable = variable_ineq, plot_type = inequality_plot_type_selected, value_share = inequality_value_share, quantile_set = "dist", regions = regions[1], years = seq(yearlim[1], yearlim[2]), years_lorenz = range(yearlim[1], yearlim[2]), scenplot = scenarios)
  })

  output$tatmplot <- renderPlot({
    yearlim <- input$yearlim
    scenarios <- input$scenarios_selected
    gridded_temp_map(yearplot = yearlim[2], scenplot = scenarios, pathadj = "../../")
  })
  
  
  output$iterationplot <- renderPlot({
    yearlim <- input$yearlim
    scenarios <- input$scenarios_selected
    regions <- input$regions_selected
    viter <- get_witch("viter")
    # Assuming viter is your dataframe and 'value' is the column with actual values
    # First, group by the variables that define your sequences
    viter <- viter %>% group_by(n, file, pathdir, v, iter) %>% arrange(t) %>% mutate(seen_nonzero = cumsum(value != 0) > 0) %>% complete(t) %>% mutate(value = ifelse(is.na(value) & !seen_nonzero, 0, value)) %>% select(-seen_nonzero) %>% ungroup()
    viter <- viter %>% group_by(n, file, pathdir, v, iter) %>% summarise(value = mean(value[ttoyear(t) >= yearlim[1] & ttoyear(t) <= yearlim[2]]))
    viter <- viter %>% filter(file %in% scenarios)
    if(regions[1]!="World") viter <- viter %>% filter(n %in% regions)
    p_iter <- ggplot(viter) + geom_line(aes(iter, value, color=n, group=n)) + facet_grid(v ~ file, scales = "free_y") + theme(legend.position = "none")
    print(p_iter)
    #ggplotly()
  })
  
   
  
})
