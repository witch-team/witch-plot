# Define UI
#shinyUI(fluidPage(theme = shinytheme("superhero"), pageWithSidebar(
shinyUI(fluidPage(pageWithSidebar(
    
  
  # Application title
  headerPanel("IIASAdb gdxcompaR"),
  
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
    

    uiOutput("select_scenarios_1"),
    uiOutput("select_scenarios_2"),
    uiOutput("select_scenarios_3"),
    uiOutput("select_scenarios_4"),
    uiOutput("select_models"),
    uiOutput("select_regions"),
    div(style="display:inline-block",checkboxInput("ylim_zero", "Set y-axis limit to zero", value = F))
    #div(style="display:inline-block",actionButton("button_saveplotdata", "Save Graph and Data"))
    
    
),
    

  # Show the plot
  mainPanel(
  tabsetPanel(type = "tabs", id = "tabs",
                tabPanel("iiasadb_compaR", id = "iiasadb_compaR", h2(textOutput("varname")),plotOutput("iiasadb_compaRplot", width = "100%", height = "80vh")),
                tabPanel("iiasadb_compaRly (BETA)", id = "iiasadb_compaRly", plotlyOutput("iiasadb_compaRly", width = "100%", height = "80vh"))
                
    )
  )
)))