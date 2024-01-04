# WITCH gdxcompaR

#if deploying online be sure to run plotgdx_witch.R first, and then execute create_witch_plot_online() to create and save the snapshot.
#then run and "publish" the shiny app

#only if deployed online
deploy_online <<- F
if(!exists("witch_folder")){
  load(file="allvariables.Rdata", envir = .GlobalEnv)
  #Install and load packages
  require_package <- function(package){
    suppressPackageStartupMessages(require(package,character.only=T, quietly = TRUE))  
  }
  pkgs <- c('data.table', 'stringr', 'countrycode', 'ggplot2', 'ggpubr', 'scales', 'RColorBrewer', 'dplyr', 'openxlsx', 'gsubfn', 'tidyr', 'rlang', 'shiny', 'shinythemes', 'plotly', 'purrr', 'reldist', 'tidytidbits', 'forcats', 'arrow')
  res <- lapply(pkgs, require_package)
  deploy_online <<- T
} 

header_ui <- headerPanel("WITCH gdxcompaR")

sidebar_ui <- sidebarPanel(
  uiOutput("select_scenarios"),
  uiOutput("select_variable"),
  uiOutput("choose_additional_set"),
  uiOutput("choose_additional_set2"),
  uiOutput("select_regions"),
  sliderInput("yearlim", 
              "Time", 
              min = 1970,
              max = 2150,
              value = c(1990,2100),
              step = 5),
  div(style="display:inline-block",
      checkboxInput("time_filter", 
                    "Time filter", 
                    value = TRUE)),
  div(style="display:inline-block",
      checkboxInput("add_historical", 
                    "Historical", 
                    value = TRUE)),
  div(style="display:inline-block",
      checkboxInput("ylim_zero",
                    "ymin=0",
                    value = FALSE)),
  div(style="display:inline-block",radioButtons("field", "", choiceNames = c("l","up","lo"), choiceValues = c("l","up","lo"), inline = TRUE)),
  div(style="display:inline-block",actionButton("button_saveplotdata", "Save Plot"))
)

tabs_ui <- tabsetPanel(type = "tabs", id = "tabs",
                       
                       tabPanel("gdxcompaR", id = "gdxcompaR", h3(textOutput("varname")),plotOutput("gdxompaRplot", width = "100%", height = "80vh")),
                       tabPanel("gdxcompaRly (BETA)", id = "gdxcompaRly", plotlyOutput("gdxompaRplotly", width = "100%", height = "80vh")),
                       
                       tabPanel("Inequality", id = "Inequality", 
                                div(style="display:inline-block",selectInput("inequality_plot_type_selected", "Plot Type:", c("quantiles", "gini", "lorenz_curve", "distribution") , size=1, selectize = F, multiple = F, selected = "Quantiles")),
                                div(style="display:inline-block",selectInput("inequality_value_share", "Plot value or share:", c("value", "share") , size=1, selectize = F, multiple = F, selected = "value")), 
                                h2("Inequality Plots"),plotOutput("inequalityplot", width = "100%", height = "80vh")),
                       
                       tabPanel("Diagnostics", id = "Diagnostics", h2("Diagnostics of model runs"),plotOutput("Diagnostics", width = "100%", height = "80vh")),
                       tabPanel("Energy Mix", id = "Energy Mix", 
                                div(style="display:inline-block",selectInput("mix_y_value_selected", "Plot value or share:", c("value", "share") , size=1, selectize = F, multiple = F, selected = "value")), 
                                div(style="display:inline-block",selectInput("mix_plot_type_selected", "Plot Type:", c("area", "line", "bar") , size=1, selectize = F, multiple = F, selected = "area")),
                                h2("Energy Mix"),plotOutput("energymixplot", width = "100%", height = "80vh")),
                       tabPanel("Electricity Mix", id = "Electricity Mix", 
                                div(style="display:inline-block",selectInput("mix_y_value_selected", "Plot value or share:", c("value", "share") , size=1, selectize = F, multiple = F, selected = "value")), 
                                div(style="display:inline-block",selectInput("mix_plot_type_selected", "Plot Type:", c("area", "line", "bar") , size=1, selectize = F, multiple = F, selected = "area")),
                                h2("Electricity Mix"),plotOutput("electricitymixplot", width = "100%", height = "80vh")),
                       tabPanel("Investment", id = "Investment", h2("Investment"),plotOutput("investmentplot", width = "100%", height = "80vh")),
                       tabPanel("Policy Cost", id = "Policy Cost", h2("Policy Cost"),p("Select BAU scenario under 'scenarios'."),plotOutput("policycostplot", width = "100%", height = "80vh")),
                       tabPanel("Intensity Plot", id = "Intensity Plot", h2("Energy and Carbon Intensity"),plotOutput("intensityplot", width = "100%", height = "80vh")),
                       tabPanel("Impact Map", id = "Impact Map", h2("GDP Impact [% loss wrt BAU]"),plotOutput("impactmap", width = "100%", height = "80vh")),
                       tabPanel("Climate", id = "climate", h2("The Climate"),plotOutput("climate_plot", width = "100%", height = "80vh")),
                       tabPanel("SCC", id = "SCC", 
                                div(style="display:inline-block",selectInput("scc_normalization_region", "Normalization region:", c("World", witch_regions) , size=1, selectize = F, multiple = F, selected = "World")),
                                h2("Social Cost of Carbon"),plotOutput("SCC_plot", width = "100%", height = "80vh"))
                       
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
