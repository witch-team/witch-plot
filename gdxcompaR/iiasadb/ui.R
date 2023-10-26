# Define UI

#load data if not running locally
deploy_online <<- F
if(!exists("iiasadb_snapshot")){
  load("iiasadb_snapshot.Rdata", envir = .GlobalEnv)
  #Install and load packages
  require_package <- function(package){
    suppressPackageStartupMessages(require(package,character.only=T, quietly = TRUE))  
  }
  pkgs <- c('data.table', 'stringr', 'countrycode', 'ggplot2', 'ggpubr', 'scales', 'RColorBrewer', 'dplyr', 'openxlsx', 'gsubfn', 'tidyr', 'rlang', 'shiny', 'shinythemes', 'plotly', 'purrr', 'reldist', 'tidytidbits', 'forcats', 'arrow')
  res <- lapply(pkgs, require_package)
  deploy_online <<- T
} 


shinyUI(fluidPage(
  tags$style("
    body {
    -moz-transform: scale(0.8, 0.8); /* Moz-browsers */
    zoom: 0.8; /* Other non-webkit browsers */
    zoom: 80%; /* Webkit browsers */
}              "),
  
  pageWithSidebar(
    
  # Application title
  headerPanel("iiasadb gdxcompaR"),
  
  # Sidebar with a slider of years and set elements
  sidebarPanel(
      #fileInput("iiasadb_file", "Choose (zipped) iiasadb CSV File", accept = c(".csv", ".zip")),
    #shinyFilesButton('files', label='File select', title='Please select a file', multiple=FALSE),
    
    uiOutput("select_variable"),
    #actionButton("chgvar", "Update variable", icon("refresh")),
    sliderInput("yearmin", 
                "Start year:", 
                min = 1970,
                max = 2100, 
                value = 1990,
                step = 10),
    sliderInput("yearmax", 
                "End year:", 
                min = 1990,
                max = 2150, 
                value = 2100,
                step  = 10),
    

    uiOutput("select_scenarios"),
    uiOutput("select_models"),
    uiOutput("select_regions"),
    #div(style="display:inline-block",uiOutput("compare_models_scenarios")),     
    div(style="display:inline-block",checkboxInput("ylim_zero", " Set y-axis limit to zero", value = F)),
    div(style="display:inline-block",actionButton("button_saveplotdata", "Save Plot"))
    
    
),
    

  # Show the plot
  mainPanel(
  tabsetPanel(type = "tabs", id = "tabs",
                tabPanel("iiasadb_compaR", id = "iiasadb_compaR", h2(textOutput("varname")),plotOutput("iiasadb_compaR", width = "100%", height = "80vh")),
                tabPanel("iiasadb_compaRly", id = "iiasadb_compaRly", plotlyOutput("iiasadb_compaRly", width = "100%", height = "80vh")),
                
    )
  )
)))