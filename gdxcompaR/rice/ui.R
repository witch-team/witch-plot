# Define UI
header_ui <- headerPanel("RICE50+ gdxcompaR")

sidebar_ui <- sidebarPanel(
  uiOutput("select_scenarios"),
  uiOutput("select_variable"),
  uiOutput("choose_additional_set"),
  uiOutput("select_regions"),
  sliderInput("yearlim", 
              "Time", 
              min = 1970,
              max = 2300,
              value = c(1990,2100),
              step = 5),
  div(style="display:inline-block",checkboxInput("add_historical", "Add historical", value = T)),
  div(style="display:inline-block",
      checkboxInput("ylim_zero",
                    "ymin=0",
                    value = FALSE)),
  div(style="display:inline-block",checkboxInput("growth_rate", "Show growth rates", value = F)),
  div(style="display:inline-block",radioButtons("field", "", choiceNames = c("l","up","lo"), choiceValues = c("l","up","lo"), inline = TRUE)),
  div(style="display:inline-block",actionButton("button_saveplotdata", "Save Plot"))
)
    
tabs_ui <- tabsetPanel(type = "tabs", id = "tabs",    
                tabPanel("gdxcompaR", id = "gdxcompaR", h2(textOutput("varname")),plotOutput("gdxcompaRplot", width = "100%", height = "80vh")),
                tabPanel("Diagnostics", id = "diagnostics", plotOutput("diagnostics", width = "100%", height = "80vh")),
                tabPanel("gdxcompaRly (BETA)", id = "gdxcompaRly", plotlyOutput("gdxompaRplotly", width = "100%", height = "80vh")),
                tabPanel("gdxcompaR stacked", id = "gdxcompaR_stacked", h2(textOutput("varname2")),plotOutput("gdxcompaRstackedplot", width = "100%", height = "80vh")),
                tabPanel("gdxcompaR MAP", id = "gdxcompaR_map", plotOutput("gdxcompaRmap", width = "100%", height = "80vh")),
                tabPanel("Inequality", id = "Inequality", 
                         div(style="display:inline-block",selectInput("inequality_plot_type_selected", "Plot Type:", c("quantiles", "gini", "lorenz_curve", "distribution") , size=1, selectize = F, multiple = F, selected = "Quantiles")),
                         div(style="display:inline-block",selectInput("inequality_value_share", "Plot value or share:", c("value", "share") , size=1, selectize = F, multiple = F, selected = "value")), 
                         h2("Inequality Plots"),plotOutput("inequalityplot", width = "100%", height = "80vh")),
                tabPanel("Temperature Map", id = "tatm_plot", plotOutput("tatmplot", width = "100%", height = "80vh"))  
)

ui <- fluidPage(
  
  pageWithSidebar(
    
    # Application title
    header_ui,
    
    # Sidebar with a slider of years and set elements
    sidebar_ui,
    
    # Show the plots
    mainPanel(tabs_ui)
    
))

shinyUI(ui)
