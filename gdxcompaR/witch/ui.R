# Define UI
#shinyUI(fluidPage(theme = shinytheme("superhero"), pageWithSidebar(
shinyUI(fluidPage(pageWithSidebar(
    
  
  # Application title
  headerPanel("WITCH gdxcompaR"),
  
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
    div(style="display:inline-block",checkboxInput("add_historical", "Add historical values", value = T)),
    div(style="display:inline-block",checkboxInput("ylim_zero", "Set y-axis limit to zero", value = F))
    #div(style="display:inline-block",checkboxInput("plotly_dynamic", "Dynamic plot(!)", value = F))
),
    

  # Show the plot
  mainPanel(
    tabsetPanel(type = "tabs", id = "tabs",

                tabPanel("gdxcompaR", id = "gdxcompaR", h2(textOutput("varname")),plotOutput("gdxompaRplot", width = "100%", height = "80vh")),
                tabPanel("gdxcompaRly (BETA)", id = "gdxcompaRly", plotlyOutput("gdxompaRplotly", width = "100%", height = "80vh")),
                
                tabPanel("Inequality", id = "Inequality", 
                         div(style="display:inline-block",selectInput("inequality_plot_type_selected", "Plot Type:", c("quantiles", "gini", "lorenz_curve", "distribution") , size=1, selectize = F, multiple = F, selected = "Quantiles")),
                         div(style="display:inline-block",selectInput("inequality_value_share", "Plot value or share:", c("value", "share") , size=1, selectize = F, multiple = F, selected = "value")), 
                         h2("Inequality Plots"),plotOutput("inequalityplot", width = "100%", height = "80vh")),
                
                tabPanel("Diagnostics", id = "Diagnostics", h2("Diagnostics of model runs"),plotOutput("Diagnostics", width = "100%", height = "80vh")),
                tabPanel("Energy Mix", id = "Energy Mix", 
                         div(style="display:inline-block",selectInput("mix_y_value_selected", "Plot value or share:", c("value", "share") , size=1, selectize = F, multiple = F, selected = "value")), 
                         div(style="display:inline-block",selectInput("mix_plot_type_selected", "Plot Type:", c("area", "line", "bar") , size=1, selectize = F, multiple = F, selected = "line")),
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
  )
)))