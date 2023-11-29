# Define UI
#shinyUI(fluidPage(theme = shinytheme("superhero"), pageWithSidebar(
shinyUI(fluidPage(
  
  pageWithSidebar(
  
  # Application title
  headerPanel("RICE gdxcompaR"),
  
  # Sidebar with a slider of years and set elements
  sidebarPanel(
    uiOutput("select_variable"),
    #actionButton("chgvar", "Update variable", icon("refresh")),
    sliderInput("yearlim", 
                "Time", 
                min = 1970,
                max = 2300,
                value = c(1990,2200),
                step = 10),
    uiOutput("choose_additional_set"),
    uiOutput("select_scenarios"),
    uiOutput("select_regions"),
    div(style="display:inline-block",checkboxInput("add_historical", "Add historical values", value = T)),
    div(style="display:inline-block",checkboxInput("ylim_zero", "Set y-axis limit to zero", value = F)),
    div(style="display:inline-block",checkboxInput("growth_rate", "Show growth rates", value = F)),
    div(style="display:inline-block",actionButton("button_saveplotdata", "Save Plot"))
),
    

  # Show the plot
  mainPanel(
    tabsetPanel(type = "tabs", id = "tabs",
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
  )
)))
