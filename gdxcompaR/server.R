#Specify variable to load
variable = "Q_FUEL"


library(shiny)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output, session) {
  
  # Expression that generates a plot of the distribution. The expression

  #some global flags
  save_plot = FALSE
  line2005 = TRUE
  
  
  #get list of variables
  mygdx <- gdx(paste(pathdir[1], filelist[1],".gdx",sep=""))
  list_of_variables <- c(all_items(mygdx)$variables, all_items(mygdx)$parameters)
  #now instead by hand
  list_of_variables <- c("Q", "Q_EN", "Q_FUEL", "Q_OUT", "Q_EMI", "K", "K_EN", "I_EN", "I", "FPRICE", "emi_cap", "ctax", "MCOST_INV", "COST_EMI", "MCOST_EMI", "CPRICE", "CUM_SAV", "TEMP", "TRF", "tpes", "tpes_kali", "ei", "ei_kali", "Q_FEN", "Q_IN", "ykali")
  
  #Scenario selector
  output$select_scenarios <- renderUI({
    selectInput("scenarios_selected", "Select scenarios", scenlist, size=length(scenlist), selectize = F, multiple = T, selected = scenlist)
  })  
  
  #Variable selector
  output$select_variable <- renderUI({
  selectInput("variable_selected", "Select variable", list_of_variables, size=1, selectize = F, multiple = F, selected = list_of_variables[1])
  })  
  variable_selected_reactive <- reactive({
    input$variable_selected
  })
  
  #Display selected variable and set
  output$varname <- renderText({  
    paste("Variable:",variable_selected_reactive()," Element 1:", input$additional_set_id_selected, " Element 2:", input$additional_set_id_selected2)
  }) 
  


    
    #REGION selector
    output$select_regions <- renderUI({
      regions_for_selector <- c(witch_regions, "World")
    selectInput("regions_selected", "Select regions", regions_for_selector, size=length(regions_for_selector), selectize = F, multiple = T, selected = witch_regions)
    })
  


# MAIN CODE FOR PLOT GENERATION  
output$gdxompaRplot <- renderPlot({

    variable <- input$variable_selected
    
    #get data updated
    get_witch_simple(variable, check_calibration=TRUE)
    allfilesdata <- get(variable)
    #print(str(allfilesdata))
    #get the name of the additional set
    additional_sets <- setdiff(colnames(allfilesdata), c("file", "pathdir", "t", "n", "value"))
    #extract additional set elements
    if(length(additional_sets)==0){additional_set="na"; set_elements = "na"; additional_set2="na"; set_elements2 = "na"}
    else if(length(additional_sets)==1)
    {
      additional_set <- additional_sets[1]
      set_elements <- unique(tolower(as.data.frame(allfilesdata)[, match(additional_set, colnames(allfilesdata))]))
      additional_set2="na"; set_elements2 = "na"
    }
    else if(length(additional_sets)==2)
    {
      additional_set <- additional_sets[1]
      set_elements <- unique(tolower(as.data.frame(allfilesdata)[, match(additional_set, colnames(allfilesdata))]))
      additional_set2 <- additional_sets[2] 
      set_elements2 <- unique(tolower(as.data.frame(allfilesdata)[, match(additional_set2, colnames(allfilesdata))]))
    }
 



   
     
    #Selector for additional set
    output$choose_additional_set <- renderUI({
      sel <- input$additional_set_id_selected
      #print(paste("in setselector", sel))
      size_elements <- min(length(set_elements), 10)
      selectInput("additional_set_id_selected", "Additional set element", set_elements, size=size_elements, selectize = F, multiple = F, selected = sel) 
      })  
    
    #Selector for additional set
    output$choose_additional_set2 <- renderUI({
      sel2 <- input$additional_set_id_selected2
      #print(paste("in setselector", sel))
      size_elements2 <- min(length(set_elements2), 10)
      selectInput("additional_set_id_selected2", "Additional set element 2", set_elements2, size=size_elements2, selectize = F, multiple = F, selected = sel2) 
    })  

      
  #get input from sliders/buttons
  yearmin = input$yearmin
  yearmax = input$yearmax
  additional_set_id <- input$additional_set_id_selected
  additional_set_id2 <- input$additional_set_id_selected2
  regions <- input$regions_selected
  scenarios <- input$scenarios_selected


  
  # SUBSET data and PLOT
  
  #choose additional selected element
  if(additional_set_id!="na"){
    allfilesdata[[additional_set]] <- tolower(allfilesdata[[additional_set]]) # to fix erroneous gams cases (y and Y etc.)
    allfilesdata <- subset(allfilesdata, get(additional_set)==additional_set_id)
    allfilesdata[[additional_set]] <- NULL #remove additional set column
  }
  if(additional_set_id2!="na"){
    allfilesdata[[additional_set2]] <- tolower(allfilesdata[[additional_set2]]) # to fix erroneous gams cases (y and Y etc.)
    allfilesdata <- subset(allfilesdata, get(additional_set2)==additional_set_id2)
    allfilesdata[[additional_set2]] <- NULL #remove additional set column
  }
  #time frame
  allfilesdata <- subset(allfilesdata, ttoyear(t)>=yearmin & ttoyear(t)<=yearmax)
  
  #regional computations of World
  #now based on meta param to guess max, mean, sum
  if(variable %in% default_meta_param()$parameter){find_aggregation = default_meta_param()[parameter==variable & type=="nagg"]$value}else{find_aggregation="sum"}
  allfilesdata_global <- aggregate(value~t+file+pathdir, data=allfilesdata, find_aggregation)
  allfilesdata_global$n <- "World"
  allfilesdata <- rbind(allfilesdata, allfilesdata_global)
  
  #scenarios, potentially add stochastic scenarios to show
  allfilesdata <- subset(allfilesdata, file %in% c(scenarios, paste0(scenarios, "(b1)"),paste0(scenarios, "(b2)"), paste0(scenarios, "(b3)"), "historical"))

  #Unit conversion
  unit_conversion <- unit_conversion(variable)
  allfilesdata$value <- allfilesdata$value * unit_conversion$convert   
  
  if(regions[1]=="World"){#if only World is displayed, show files with colors
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
})