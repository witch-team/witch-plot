library(shiny)
library(shinythemes)

# Define UI
shinyUI(fluidPage(theme = shinytheme("superhero"), pageWithSidebar(

  
  # Application title
  headerPanel("WITCH - gdxcompaR"),
  
  # Sidebar with a slider of years and set elements
  sidebarPanel(
    uiOutput("select_variable"),
    #actionButton("chgvar", "Update variable", icon("refresh")),
    sliderInput("yearmin", 
                "Start year:", 
                min = 1970,
                max = 2100, 
                value = 1980,
                step = 10),
    sliderInput("yearmax", 
                "End year:", 
                min = 1970,
                max = 2150, 
                value = 2100,
                step  = 10),
    uiOutput("choose_additional_set"),
    uiOutput("choose_additional_set2"),
    uiOutput("select_scenarios"),
    uiOutput("select_regions")
),
    

  
  # Show the plot
  mainPanel(
    h1(textOutput("varname")),
    plotOutput("gdxompaRplot", width = "100%", height = "800px", hover=hoverOpts(id="plot_hover", delay = 100, delayType = "debounce"))
    
    
  )
)))