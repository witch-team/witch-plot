#Specify variable to load
variable = "Q_FUEL"
unit = ""
convert=1 # 0.0036   


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
  list_of_variables <- c("Q", "Q_EN", "Q_FUEL", "Q_OUT", "Q_EMI", "K", "K_EN", "I_EN", "l", "FPRICE")
  
  
  #Variable selector
  output$select_variable <- renderUI({
  selectInput("variable_selected", "Select variable", list_of_variables, size=1, selectize = F, multiple = F, selected = list_of_variables[1])
  })  
  variable_selected_reactive <- reactive({
    input$variable_selected
  })
  #Display selected variable and set
  output$varname <- renderText({  
    paste("Variable:",variable_selected_reactive()," Element:", input$additional_set_id_selected)
  }) 
  


  #get data first time
  get_witch_simple(variable, check_calibration=TRUE)
  allfilesdata <- get(variable)
  #get the name of the additional set
  additional_set <- setdiff(colnames(allfilesdata), c("file", "pathdir", "t", "n", "value"))
  #extract additional set elements
  if(length(additional_set)==0){additional_set="na"; set_elements = "na"}else{set_elements <- unique(tolower(as.data.frame(allfilesdata)[, grep(additional_set, colnames(allfilesdata))]))}
    
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
    
    #get the name of the additional set
    additional_set <- setdiff(colnames(allfilesdata), c("file", "pathdir", "t", "n", "value"))
    #extract additional set elements
    if(length(additional_set)==0){additional_set="na"; set_elements = "na"}else
    {set_elements <- unique(tolower(as.data.frame(allfilesdata)[, grep(additional_set, colnames(allfilesdata))]))}
 



   
     
    #Selector for additional set
    output$choose_additional_set <- renderUI({
      sel <- input$additional_set_id_selected
      size_elements <- min(length(set_elements), 10)
      selectInput("additional_set_id_selected", "Additional set element", set_elements, size=size_elements, selectize = F, multiple = F, selected = sel) 
      })  
    

  
      
  #get input from sliders/buttons
  yearmin = input$yearmin
  yearmax = input$yearmax
  additional_set_id <- input$additional_set_id_selected
  regions <- input$regions_selected
  
  

  
  # SUBSET data and PLOT
  
  #choose additional selected element
  if(additional_set_id!="na"){
    allfilesdata[[additional_set]] <- tolower(allfilesdata[[additional_set]]) # to fix erroneous gams cases (y and Y etc.)
    allfilesdata <- subset(allfilesdata, get(additional_set)==additional_set_id)
    allfilesdata[[additional_set]] <- NULL #remove additional set column
  }
  #time frame
  allfilesdata <- subset(allfilesdata, ttoyear(t)>=yearmin & ttoyear(t)<=yearmax)
  #Unit conversion
  allfilesdata$value <- allfilesdata$value * convert 
  #regional computations of World
  allfilesdata_global <- aggregate(value~t+file+pathdir, data=allfilesdata, sum)
  allfilesdata_global$n <- "World"
  allfilesdata <- rbind(allfilesdata, allfilesdata_global)

  p <- ggplot(subset(allfilesdata, n %in% regions & file!="calibration"),aes(ttoyear(t),value,colour=n, line_type=file)) + geom_line(stat="identity", size=1.5) + xlab("year") + ylab(unit) + scale_colour_manual(values = region_palette) + xlim(yearmin,yearmax)
  p <- p + geom_line(data=subset(allfilesdata, n %in% regions & file=="calibration"),aes(ttoyear(t),value,colour=n), stat="identity", size=1.0)
  if(length(pathdir)!=1){p <- p + facet_grid(pathdir ~ .)}
  if(line2005){p <- p + geom_vline(size=0.5,aes(xintercept=2005), linetype="solid", color="grey")}
  
  #format and print plot
  print(p + labs(title=variable) + theme(text = element_text(size=16), legend.position="right", legend.direction = "vertical", legend.key = element_rect(colour = NA), legend.title=element_blank()))
  
  if(save_plot){saveplot(variable)}
  
  
  })
})