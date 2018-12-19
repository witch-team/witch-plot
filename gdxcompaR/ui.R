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
                value = 1990,
                step = 10),
    sliderInput("yearmax", 
                "End year:", 
                min = 1990,
                max = 2150, 
                value = 2100,
                step  = 10),
    uiOutput("choose_additional_set"),
    uiOutput("choose_additional_set2"),
    uiOutput("select_scenarios"),
    uiOutput("select_regions"),
    checkboxInput("add_historical", "Add historical values", FALSE)
),
    

  
  # Show the plot
  mainPanel(
    tabsetPanel(type = "tabs", id = "tabs",
                tabPanel("gdxcompaR", id = "gdxcompaR", h2(textOutput("varname")),plotOutput("gdxompaRplot", width = "100%", height = "80vh", hover=hoverOpts(id="plot_hover", delay = 100, delayType = "debounce"))),
                tabPanel("Energy Mix", id = "Energy Mix", 
                         selectInput("mix_y_value_selected", "Plot value or share:", c("value", "share") , size=1, selectize = F, multiple = F, selected = "value"), 
                         selectInput("mix_plot_type_selected", "Plot Type:", c("area", "line", "bar") , size=1, selectize = F, multiple = F, selected = "area"),
                         h2("Energy Mix"),plotOutput("energymixplot", width = "100%", height = "80vh")),
                tabPanel("Electricity Mix", id = "Electricity Mix", 
                         selectInput("mix_y_value_selected", "Plot value or share:", c("value", "share") , size=1, selectize = F, multiple = F, selected = "value"), 
                         selectInput("mix_plot_type_selected", "Plot Type:", c("area", "line", "bar") , size=1, selectize = F, multiple = F, selected = "area"),
                         h2("Electricity Mix"),plotOutput("electricitymixplot", width = "100%", height = "80vh")),
                tabPanel("Investment", id = "Investment", h2("Investment"),plotOutput("investmentplot", width = "100%", height = "80vh")),
                tabPanel("Policy Cost", id = "Policy Cost", h2("Policy Cost"),p("Select BAU scenario under 'scenarios'."),plotOutput("policycostplot", width = "100%", height = "80vh")),
                tabPanel("Intensity Plot", id = "Intensity Plot", h2("Energy and Carbon Intensity"),plotOutput("intensityplot", width = "100%", height = "80vh"))

    )
  )
)))