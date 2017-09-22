library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("gdxcompaR"),
  
  # Sidebar with a slider of years and set elements
  sidebarPanel(
    sliderInput("yearmin", 
                "Start year:", 
                min = 1850,
                max = 2100, 
                value = 1980,
                step = 5),
    sliderInput("yearmax", 
                "End year:", 
                min = 1990,
                max = 2100, 
                value = 2100,
                step  = 5),
    uiOutput("choose_additional_set"),
    uiOutput("select_regions")
),
    

  
  # Show the plot
  mainPanel(
    plotOutput("gdxompaRplot")
  )
))