#Specify variable to load
variable = "Q_FUEL"
unit = "EJ"
convert=0.0036   


library(shiny)



# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  #

  

  #additional_set="na"  #NA means get all!
  #additional_set_id="na"
  #regions=witch_regions
  
  
  #some global flags
  save_plot = FALSE
  line2005 = TRUE
  
  #get data
  get_witch_simple(variable, check_calibration=TRUE)
  allfilesdata <- get(variable)
  #get the name of the additional set
  additional_set <- setdiff(colnames(allfilesdata), c("file", "pathdir", "t", "n", "value"))
  #extract additional set elements
  set_elements <- unique(as.data.frame(allfilesdata)[, grep(additional_set, colnames(allfilesdata))])

  
  
  output$choose_additional_set <- renderUI({
    selectInput("additional_set_id_selected", "Additional set element", set_elements, size=length(set_elements), selectize = F, multiple = F)
  })  
  
  output$select_regions <- renderUI({
    selectInput("regions_selected", "Select regions", witch_regions, size=length(witch_regions), selectize = F, multiple = T, selected = witch_regions)
  })
  
  output$gdxompaRplot <- renderPlot({
    
      
  #get input from sliders/buttons
  yearmin = input$yearmin
  yearmax = input$yearmax
  additional_set_id <- input$additional_set_id_selected
  regions <- input$regions_selected
  
  
  
  #now run the gdxcompare plot function
  
  #choose subsets
  if(additional_set!="na"){
    allfilesdata <- subset(allfilesdata, get(additional_set)==additional_set_id)
  }
  #time frame
  allfilesdata <- subset(allfilesdata, ttoyear(t)>=yearmin & ttoyear(t)<=yearmax)
  #Unit conversion
  allfilesdata$value <- allfilesdata$value * convert 
  
  p <- ggplot(subset(allfilesdata, n %in% regions & file!="calibration"),aes(ttoyear(t),value,colour=n, line_type=file)) + geom_line(stat="identity", size=1.5) + xlab("year") + ylab(unit) + scale_colour_manual(values = region_palette) + xlim(yearmin,yearmax)
  p <- p + geom_line(data=subset(allfilesdata, n %in% regions & file=="calibration"),aes(ttoyear(t),value,colour=n), stat="identity", size=1.0)
  if(length(pathdir)!=1){p <- p + facet_grid(pathdir ~ .)}
  if(line2005){p <- p + geom_vline(size=0.5,aes(xintercept=2005), linetype="solid", color="grey")}
  print(p)
  if(save_plot){saveplot(variable)}
  })
})