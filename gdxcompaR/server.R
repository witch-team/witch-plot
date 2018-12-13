require(shiny)

# Define server 
shinyServer(function(input, output, session) {
  
    #some global flags
    save_plot = FALSE
    line2005 = TRUE
    verbose = FALSE
    
    
    #get list of variables
    mygdx <- gdx(paste(pathdir[1], filelist[1],".gdx",sep=""))
    list_of_variables <- c(all_items(mygdx)$variables, all_items(mygdx)$parameters)
    #now instead by hand
    list_of_variables <- c("Q", "Q_EN", "Q_FUEL", "Q_OUT", "Q_EMI", "K", "K_EN", "I_EN", "I", "FPRICE", "MCOST_INV", "COST_EMI", "MCOST_EMI", "CPRICE", "CUM_SAV", "TEMP", "TRF","Q_FEN", "Q_IN", "ykali", "tpes", "carbonprice", "emi_cap", "l")
    # preload additional set elements for all variables
    additional_set_list <- list(); additional_set_list2 <- list()
    for(var in list_of_variables){
      additional_sets <- setdiff(names(mygdx[var]), c("t", "n", "value"))
      if(length(additional_sets)==0){additional_set="na"; set_elements = "na"; additional_set2="na"; set_elements2 = "na"}
      else if(length(additional_sets)==1)
      {
        additional_set <- additional_sets[1]
        set_elements <- unique(tolower(mygdx[var][, match(additional_set, colnames(mygdx[var]))]))
        additional_set2="na"; set_elements2 = "na"
      }
      else if(length(additional_sets)==2)
      {
        additional_set <- additional_sets[1]
        set_elements <- unique(tolower(mygdx[var][, match(additional_set, colnames(mygdx[var]))]))
        additional_set2 <- additional_sets[2] 
        set_elements2 <- unique(tolower(mygdx[var][, match(additional_set2, colnames(mygdx[var]))]))
      }
      lv <- list(var=list(additional_set_id=additional_set, set_elements=set_elements)); names(lv) <- var
      additional_set_list = c(additional_set_list, lv)
      lv2 <- list(var=list(additional_set_id2=additional_set2, set_elements2=set_elements2)); names(lv2) <- var
      additional_set_list2 = c(additional_set_list2, lv2)
    }
    
    
    #Scenario selector
    output$select_scenarios <- renderUI({
      selectInput("scenarios_selected", "Select scenarios", scenlist, size=length(scenlist), selectize = F, multiple = T, selected = scenlist)
    })  
    
    #Variable selector
    output$select_variable <- renderUI({
    selectInput("variable_selected", "Select variable", list_of_variables, size=1, selectize = F, multiple = F, selected = list_of_variables[1])
    })  
    variable_selected_reactive <- reactive({input$variable_selected})
    #variable_selected_reactive <- function(){return(input$variable_selected)} #works also
    
    
    #Display selected variable and set
    output$varname <- renderText({  
      paste("Variable:",variable_selected_reactive()," Element 1:", input$additional_set_id_selected, " Element 2:", input$additional_set_id_selected2)
    }) 
  

    #REGION selector
    output$select_regions <- renderUI({
      regions_for_selector <- c(witch_regions, "World")
    selectInput("regions_selected", "Select regions", regions_for_selector, size=length(regions_for_selector), selectize = F, multiple = T, selected = witch_regions)
    })
  
    #Selector for additional set
    output$choose_additional_set <- renderUI({
      variable <- variable_selected_reactive()
      if(is.null(variable)) variable <- list_of_variables[1]
      additional_set_id <- additional_set_list[[variable]]$additional_set_id
      set_elements <- additional_set_list[[variable]]$set_elements
      sel <- input$additional_set_id_selected
      size_elements <- min(length(set_elements), 5)
      selectInput("additional_set_id_selected", "Additional set element", set_elements, size=size_elements, selectize = F, multiple = F, selected = sel)
      })
    #Selector for additional set #2
    output$choose_additional_set2 <- renderUI({
      variable <- variable_selected_reactive()
      if(is.null(variable)) variable <- list_of_variables[1]
      additional_set_id2 <- additional_set_list2[[variable]]$additional_set_id2
      set_elements2 <- additional_set_list2[[variable]]$set_elements2
      sel2 <- input$additional_set_id_selected2
      size_elements2 <- min(length(set_elements2), 5)
      selectInput("additional_set_id_selected2", "Additional set element 2", set_elements2, size=size_elements2, selectize = F, multiple = F, selected = sel2)
    })
    


    # MAIN CODE FOR PLOT GENERATION  
    output$gdxompaRplot <- renderPlot({
      variable <- input$variable_selected
      if(is.null(variable)) variable <- list_of_variables[1]
      #get data
      get_witch_simple(variable, check_calibration=TRUE)
      if(verbose) print(str_glue("Variable {variable} loaded."))
      allfilesdata <- get(variable)
      #print(str(allfilesdata))
      #get the name of the additional set
      additional_set_id <- additional_set_list[[variable]]$additional_set_id
      set_elements <- additional_set_list[[variable]]$set_elements
      additional_set_id2 <- additional_set_list2[[variable]]$additional_set_id2
      set_elements2 <- additional_set_list2[[variable]]$set_elements2
    
      #get input from sliders/buttons
      yearmin = input$yearmin
      yearmax = input$yearmax
      additional_set_selected <- input$additional_set_id_selected
      additional_set_selected2 <- input$additional_set_id_selected2
      regions <- input$regions_selected
      scenarios <- input$scenarios_selected
      
      #in case they have not yet been set, set to defult values
      if(is.null(regions)) regions <- display_regions
      if(is.null(additional_set_selected)) additional_set_selected <- set_elements[1]
      if((additional_set_id!="na" & additional_set_selected=="na") | !(additional_set_selected %in% set_elements)) additional_set_selected <- set_elements[1] 
      if(is.null(additional_set_selected2)) additional_set_selected2 <- set_elements2[1]
      if((additional_set_id2!="na" & additional_set_selected2=="na") | !(additional_set_selected2 %in% set_elements2)) additional_set_selected2 <- set_elements2[1] 
      
      # SUBSET data and PLOT
      #choose additional selected element
      if(additional_set_id!="na"){
        allfilesdata[[additional_set_id]] <- tolower(allfilesdata[[additional_set_id]]) # to fix erroneous gams cases (y and Y etc.)
        allfilesdata <- subset(allfilesdata, get(additional_set_id)==additional_set_selected)
        allfilesdata[[additional_set_id]] <- NULL #remove additional set column
      }
      if(additional_set_id2!="na"){
        allfilesdata[[additional_set_id2]] <- tolower(allfilesdata[[additional_set_id2]]) # to fix erroneous gams cases (y and Y etc.)
        allfilesdata <- subset(allfilesdata, get(additional_set_id2)==additional_set_selected2)
        allfilesdata[[additional_set_id2]] <- NULL #remove additional set column
      }
     
       #time frame
      allfilesdata <- subset(allfilesdata, ttoyear(t)>=yearmin & ttoyear(t)<=yearmax)
      
      #clean data
      allfilesdata <- allfilesdata[!is.na(allfilesdata$value)]
      
      #Computation of World/glboal sum/average
      #now based on meta param to guess max, mean, sum
      if(nrow(allfilesdata) >0){
      if(variable %in% default_meta_param()$parameter){find_aggregation = default_meta_param()[parameter==variable & type=="nagg"]$value}else{find_aggregation="sum"}
      allfilesdata_global <- aggregate(value~t+file+pathdir, data=allfilesdata, find_aggregation)
      allfilesdata_global$n <- "World"
      allfilesdata <- rbind(allfilesdata, allfilesdata_global[,c("t","n","value","file", "pathdir")])
      }
      
      #scenarios, potentially add stochastic scenarios to show
      allfilesdata <- subset(allfilesdata, file %in% c(scenarios, paste0(scenarios, "(b1)"),paste0(scenarios, "(b2)"), paste0(scenarios, "(b3)"), "historical"))
    
      #Unit conversion
      unit_conversion <- unit_conversion(variable)
      allfilesdata$value <- allfilesdata$value * unit_conversion$convert   
      
      if(regions[1]=="World" | length(regions)==1){#if only World is displayed or only one region, show files with colors
        p <- ggplot(subset(allfilesdata, n %in% regions & file!="historical"),aes(ttoyear(t),value,colour=file)) + geom_line(stat="identity", size=1.5) + xlab("year") + ylab(unit_conversion$unit) + xlim(yearmin,yearmax)
        p <- p + geom_line(data=subset(allfilesdata, n %in% regions & file=="historical"),aes(ttoyear(t),value,colour=file), stat="identity", size=1.0, linetype="solid")
        #legends:
        p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL))
      }else{
        p <- ggplot(subset(allfilesdata, n %in% regions & file!="historical"),aes(ttoyear(t),value,colour=n, linetype=file)) + geom_line(stat="identity", size=1.5) + xlab("year") + ylab(unit_conversion$unit) + scale_colour_manual(values = region_palette) + xlim(yearmin,yearmax)
        p <- p + geom_line(data=subset(allfilesdata, n %in% regions & file=="historical"),aes(ttoyear(t),value,colour=n), stat="identity", size=1.0, linetype="solid")
        #legends:
        p <- p + theme(text = element_text(size=16), legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()) + guides(color=guide_legend(title=NULL, nrow = 1), linetype=guide_legend(title=NULL, nrow = 2))
        
      }
      if(length(pathdir)!=1){p <- p + facet_grid(. ~ pathdir)}
      if(line2005){p <- p + geom_vline(size=0.5,aes(xintercept=2005), linetype="solid", color="grey")}
      
      #format and print plot
      print(p + labs(title=variable))
      
      if(save_plot){saveplot(variable)}
    })


    output$energymixplot <- renderPlot({
      #get input from sliders/buttons
      yearmin = input$yearmin
      yearmax = input$yearmax
      additional_set_selected <- input$additional_set_id_selected
      additional_set_selected2 <- input$additional_set_id_selected2
      regions <- input$regions_selected
      scenarios <- input$scenarios_selected
      Primary_Energy_Mix(PES_y = "value", regions = regions[1], years = seq(yearmin, yearmax, 1), plot_type = "area", scenplot = scenarios)
    })
    
    output$electricitymixplot <- renderPlot({
      #get input from sliders/buttons
      yearmin = input$yearmin
      yearmax = input$yearmax
      additional_set_selected <- input$additional_set_id_selected
      additional_set_selected2 <- input$additional_set_id_selected2
      regions <- input$regions_selected
      scenarios <- input$scenarios_selected
      Electricity_Mix(Electricity_y = "value", regions = regions[1], years = seq(yearmin, yearmax, 1), plot_type = "area", scenplot = scenarios)
    })
  
  
  
  
  
  
  
})