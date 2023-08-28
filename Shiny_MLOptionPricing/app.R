require(shiny)
require(shinyjs)
require(dplyr)
require(tidyr)
require(keras)

# For deploying to ShinyApps.io - Comment out the below if deploying locally
# require(reticulate)
# # Create a virtual environment selecting your desired python version
# virtualenv_create(envname = "python_environment", python= "python3")
# # Explicitly install python libraries that you want to use, e.g. pandas, numpy
# virtualenv_install("python_environment", packages = c('pandas','numpy'))
# # Select the virtual environment
# use_virtualenv("python_environment", required = TRUE)

# Load all of the models for exp_no given by folder names within models folder
models = list()
means  = list()
stds   = list()
dirs_exp = list.dirs("models", recursive = FALSE)
for (exp in dirs_exp) {
    i = substr(exp, nchar(exp), nchar(exp))
    models[[i]] = load_model_tf(paste0("models/", i))
    means[[i]]  = readRDS(paste0("means_", i, ".RDS"))
    stds[[i]]  = readRDS(paste0("stds_", i, ".RDS"))
}

# Source R functions
source("HelperFns.R")

# Define the UI
ui <- fluidPage(
    useShinyjs(),
    titlePanel("Machine Learning Option Pricing Model Demonstration", "Machine Learning Option Pricing"),
    sidebarLayout( # Sidebar layout with input and output definitions
        sidebarPanel( # Sidebar panel for enabling inputs
            width=4,
            fluidPage(
                fluidRow(actionButton("Go", "Go!", style = "float: right;"),
                    selectInput("Exp", "Choose experiment",
                                list("1. Inputs: 710k, S/K, T; Noise: 0.0"=1, "2. Inputs: 710k, S/K, T, r, sigma; Noise: 0.0"=2,
                                     "3. Inputs: 710k, S/K, T, r, sigma, b, eta; Noise: 0.0"=3, "4. Inputs: 710k, S/K, T, r, sigma, b, eta; Noise: 0.1"=4,
                                     "5. Inputs: 710k, S/K, T, r, sigma, b, eta; Noise: 0.4"=5, "6. Inputs: 710k, S/K, T, r, sigma, b, eta, dummy; Noise: 0.4"=6,
                                     "7. Inputs: 710k, S/K, T, r, sigma, b, eta, dummy; Noise: 0.1"=7, "8. Inputs: 710k, S/K, T, r, sigma, b, eta, dummy; Noise: 0.4"=8),
                                1, selectize=TRUE, width=400),
                    h2("Choose inputs for option to value")),
                fluidRow(column(width=6),
                    column(width=6, p(style = "text-align: right; font-weight: bold;", "Trained Range"))),
                GetNumericalInputRow(8, 4, "S", "S: Underlying price", 100, 50, 200, "text-align: right; vertical-align: bottom;", "0.5*K-2*K"),
                GetNumericalInputRow(8, 4, "T", "T: Time to maturity (years)", 1, 0, 5, "text-align: right; vertical-align: bottom;", "0-2.5"),
                GetNumericalInputRow(8, 4, "K", "K: Strike price", 100, 50, 200, style = "text-align: right; vertical-align: bottom;", "0.5*S-2*S"),
                GetNumericalInputRow(8, 4, "r", "r: Risk-free rate (annual %)", 5, 0, 20, "text-align: right; vertical-align: bottom;", "1-12"),
                GetNumericalInputRow(8, 4, "sigma", "sigma: Volatility (annual %)", 20, 0, 75, "text-align: right; vertical-align: bottom;", "5-50"),
                GetNumericalInputRow(8, 4, "b", "b: Barrier (% of strike)", 90, 0, 99, style = "text-align: right; vertical-align: bottom;", "60-99"),
                GetNumericalInputRow(8, 4, "eta", "eta: Barrier decay (annual %)", 10, 0, 50, "text-align: right; vertical-align: bottom;", "0-30"),
                GetNumericalInputRow(8, 4, "dummy", "Dummy variable", 3, 0, 100, style = "text-align: right; vertical-align: bottom;", "1-5")

            )
        ),
        mainPanel( # Main panel for displaying outputs
            width=8,
            tabsetPanel( # Tabset panel for all tabs with outputs
                tabPanel("ML vs BSM Prices",
                    fluidPage(fluidRow(textOutput("ErrCheck")),
                        fluidRow(h5("Comparing prices for ML and BSM models given the experiment number and inputs values supplied opposite:"),
                            htmlOutput("MLPrice"),
                            htmlOutput("BSMPrice"),
                            h5("Charts comparing prices for ML and BSM models given the experiment number and inputs values supplied opposite, but varying each of the inputs across the trained range:")),
                        fluidRow(plotOutput("ValsSK", height=500)),
                        fluidRow(
                            column(width = 6,
                                plotOutput("ValsT", height=250),
                                plotOutput("Valsv", height=250),
                                plotOutput("Valseta", height=250)),
                            column(width = 6,
                                plotOutput("Valsr", height=250),
                                plotOutput("Valsb", height=250),
                                plotOutput("Valsdummy", height=250))
                        )
                    )
                ),
                tabPanel("Tracking",
                    fluidPage(
                        fluidRow(h4("Charts tracking the values of the elements of a hedging strategy", style = "text-align: center"),
                            column(width = 6, p("Using ML model prices", style = "text-align: center")),
                            column(width = 6, p("Using BSM model prices", style = "text-align: center"))),
                        fluidRow(p("Underlying Price Simulation 1:"),
                            column(width = 6, plotOutput("TrackML1", height=320)),
                            column(width = 6, plotOutput("TrackBSM1", height=320))),
                        fluidRow(column(width = 6, plotOutput("TrackMLBSM1", height=240))),
                        fluidRow(p("Underlying Price Simulation 2:"),
                            column(width = 6, plotOutput("TrackML2", height=320)),
                            column(width = 6, plotOutput("TrackBSM2", height=320))),
                        fluidRow(column(width = 6, plotOutput("TrackMLBSM2", height=240))),
                        fluidRow(p("Underlying Price Simulation 3:"),
                            column(width = 6, plotOutput("TrackML3", height=320)),
                            column(width = 6, plotOutput("TrackBSM3", height=320))),
                        fluidRow(column(width = 6, plotOutput("TrackMLBSM3", height=240)))
                    )
                )
            )
        )
    )
)

# Define server logic required to produce required results
server <- function(input, output) {
    tb = reactiveVal(NULL)
    
##############################################
# General event observing and non-tab-specific
    
    observeEvent(input$Exp, { # Check for changes in Experiment to reset/disable input fields as appropriate
        #shinyjs::reset('r'); shinyjs::reset('sigma'); shinyjs::reset('b'); shinyjs::reset('eta'); shinyjs::reset('dummy')
        if (input$Exp == 1) {shinyjs::disable('r'); shinyjs::disable('sigma'); shinyjs::disable('b'); shinyjs::disable('eta'); shinyjs::disable('dummy')}
        else if (input$Exp == 2) {shinyjs::enable('r'); shinyjs::enable('sigma'); shinyjs::disable('b'); shinyjs::disable('eta'); shinyjs::disable('dummy')}
        else if (input$Exp == 3 | input$Exp == 4 | input$Exp == 5) {shinyjs::enable('r'); shinyjs::enable('sigma'); shinyjs::enable('b'); shinyjs::enable('eta'); shinyjs::disable('dummy')}
        else {shinyjs::enable('r'); shinyjs::enable('sigma'); shinyjs::enable('b'); shinyjs::enable('eta'); shinyjs::enable('dummy')}
    })
    
###################################################
# Comparisons on ML and BSM Prices given User Input
    observeEvent(input$Go, {
        tb = tibble(Exp=input$Exp, S=input$S, K=input$K, T=input$T, r=input$r/100, sigma=input$sigma/100, b=input$b/100, eta=input$eta/100, dummy=input$dummy)
        output$MLPrice <- renderText({
            paste0("ML Model Price: <b>", format(MLPricePerExp(tb$Exp, models, means, stds, tb$S, tb$K, tb$T, tb$r/100, tb$sigma/100, tb$b/100, tb$eta/100, tb$dummy), digits=4))
        })        
        output$BSMPrice <- renderText({
            paste0("BSM Model Price: <b>", format(BSMPricePerExp(tb$Exp, tb$S, tb$K, tb$T, tb$r, tb$sigma, tb$b, tb$eta), digits=4))
        })
        output$ValsSK <- renderPlot({
            PlotComps(tb$Exp, "S_K", models, means, stds, tb$S, tb$K, tb$T, tb$r, tb$sigma, tb$b, tb$eta, tb$dummy)
        })
        output$ValsT <- renderPlot({
            PlotComps(tb$Exp, "T", models, means, stds, tb$S, tb$K, tb$T, tb$r, tb$sigma, tb$b, tb$eta, tb$dummy)
        })
        output$Valsr <- renderPlot({
            if (tb$Exp > 1) {PlotComps(tb$Exp, "r", models, means, stds, tb$S, tb$K, tb$T, tb$r, tb$sigma, tb$b, tb$eta, tb$dummy, TRUE)}
        })
        output$Valsv <- renderPlot({
            if (tb$Exp > 1) {PlotComps(tb$Exp, "v", models, means, stds, tb$S, tb$K, tb$T, tb$r, tb$sigma, tb$b, tb$eta, tb$dummy, TRUE)}
        })
        output$Valsb <- renderPlot({
            if (tb$Exp > 2) {PlotComps(tb$Exp, "b", models, means, stds, tb$S, tb$K, tb$T, tb$r, tb$sigma, tb$b, tb$eta, tb$dummy, TRUE)}
        })
        output$Valseta <- renderPlot({
            if (tb$Exp > 2) {PlotComps(tb$Exp, "eta", models, means, stds, tb$S, tb$K, tb$T, tb$r, tb$sigma, tb$b, tb$eta, tb$dummy, TRUE)}
        })
        output$Valsdummy <- renderPlot({
            if (tb$Exp > 5) {PlotComps(tb$Exp, "dummy", models, means, stds, tb$S, tb$K, tb$T, tb$r, tb$sigma, tb$b, tb$eta, tb$dummy)}
        })
    })

#################################################
# Comparisons of Tracking Errors using ML and BSM
    
    observeEvent(input$Go, {
        tb = tibble(Exp=input$Exp, S=input$S, K=input$K, T=input$T, r=input$r/100, sigma=input$sigma/100, b=input$b/100, eta=input$eta/100, dummy=input$dummy)
        track_path1 = GetTrackingPath(tb$Exp, models, means, stds, tb$S, tb$K, tb$b, tb$eta, tb$r, tb$sigma, tb$dummy, tb$T, 1)
        track_path2 = GetTrackingPath(tb$Exp, models, means, stds, tb$S, tb$K, tb$b, tb$eta, tb$r, tb$sigma, tb$dummy, tb$T, 2)
        track_path3 = GetTrackingPath(tb$Exp, models, means, stds, tb$S, tb$K, tb$b, tb$eta, tb$r, tb$sigma, tb$dummy, tb$T, 3)
        # output$ErrCheck = renderText({
        #     track_path1$S
        # })
        output$TrackML1 <- renderPlot({
            PlotTrackingElements(tb$Exp, models, means, stds, tb$S, tb$K, tb$b, tb$eta, tb$r, tb$sigma, tb$dummy, tb$T, track_path1, "ML")
        })
        output$TrackBSM1 <- renderPlot({
            PlotTrackingElements(tb$Exp, models, means, stds, tb$S, tb$K, tb$b, tb$eta, tb$r, tb$sigma, tb$dummy, tb$T, track_path1, "BSM")
        })
        output$TrackMLBSM1 <- renderPlot({
            PlotHedgeComps(tb$Exp, models, means, stds, tb$S, tb$K, tb$b, tb$eta, tb$r, tb$sigma, tb$dummy, tb$T, track_path1)
        })
        
        output$TrackML2 <- renderPlot({
            PlotTrackingElements(tb$Exp, models, means, stds, tb$S, tb$K, tb$b, tb$eta, tb$r, tb$sigma, tb$dummy, tb$T, track_path2, "ML")
        })
        output$TrackBSM2 <- renderPlot({
            PlotTrackingElements(tb$Exp, models, means, stds, tb$S, tb$K, tb$b, tb$eta, tb$r, tb$sigma, tb$dummy, tb$T, track_path2, "BSM")
        })
        output$TrackMLBSM2 <- renderPlot({
            PlotHedgeComps(tb$Exp, models, means, stds, tb$S, tb$K, tb$b, tb$eta, tb$r, tb$sigma, tb$dummy, tb$T, track_path2)
        })
        
        output$TrackML3 <- renderPlot({
            PlotTrackingElements(tb$Exp, models, means, stds, tb$S, tb$K, tb$b, tb$eta, tb$r, tb$sigma, tb$dummy, tb$T, track_path3, "ML")
        })
        output$TrackBSM3 <- renderPlot({
            PlotTrackingElements(tb$Exp, models, means, stds, tb$S, tb$K, tb$b, tb$eta, tb$r, tb$sigma, tb$dummy, tb$T, track_path3, "BSM")
        })
        output$TrackMLBSM3 <- renderPlot({
            PlotHedgeComps(tb$Exp, models, means, stds, tb$S, tb$K, tb$b, tb$eta, tb$r, tb$sigma, tb$dummy, tb$T, track_path3)
        })
    })
}

shinyApp(ui, server)