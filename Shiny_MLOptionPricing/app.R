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

# Load all of the models for the given exp_no
exp_no = 1
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
    # App title ----
    titlePanel("Hello TensorFlow!"),
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(
            selectInput("Exp", "Choose experiment", list(1, 2, 3, 4, 5, 6, 7), 1),
            h2("Choose inputs for option to value"),
            h4("Market Data Inputs"),
            numericInput("S", "Underlying price", 100, 50, 200),
            numericInput("r", "Risk-free rate (annual %)", 5, 0, 20),
            numericInput("sigma", "Underlying volatility (annual %)", 20, 0, 50),
            h4("Contract Terms Inputs"),
            numericInput("T", "Time to maturity (years)", 1, 0, 2.5),
            numericInput("K", "Strike price", 100, 50, 200),
            numericInput("b", "Barrier", 0, 0, 0.99),
            numericInput("eta", "Barrier decay (annual %)", 0, 0, 30),
            numericInput("dummy", "Dummy variable", 0, 0, 100)
        ),
        # Main panel for displaying outputs ----
        mainPanel(
            h5("Model Price:", textOutput(outputId = "MLPrice", inline=TRUE)),
            h5("BSM Price:", textOutput(outputId = "BSMPrice", inline=TRUE))
        )
    )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
    observeEvent(input$Exp, {
        shinyjs::reset('r'); shinyjs::reset('sigma'); shinyjs::reset('b'); shinyjs::reset('eta'); shinyjs::reset('dummy')
        if(input$Exp == 1) {shinyjs::disable('r'); shinyjs::disable('sigma'); shinyjs::disable('b'); shinyjs::disable('eta'); shinyjs::disable('dummy')}
        else if (input$Exp == 2) {shinyjs::enable('r'); shinyjs::enable('sigma'); shinyjs::disable('b'); shinyjs::disable('eta'); shinyjs::disable('dummy')}
        else if (input$Exp == 3) {shinyjs::enable('r'); shinyjs::enable('sigma'); shinyjs::enable('b'); shinyjs::enable('eta'); shinyjs::disable('dummy')}
        else if (input$Exp == 4) {shinyjs::enable('r'); shinyjs::enable('sigma'); shinyjs::enable('b'); shinyjs::enable('eta'); shinyjs::disable('dummy')}
        else if (input$Exp == 5) {shinyjs::enable('r'); shinyjs::enable('sigma'); shinyjs::enable('b'); shinyjs::enable('eta'); shinyjs::disable('dummy')}
        else {shinyjs::enable('r'); shinyjs::enable('sigma'); shinyjs::enable('b'); shinyjs::enable('eta'); shinyjs::enable('dummy')}
    })
    output$MLPrice <- renderText({
        if(input$Exp == 1) {input_data = tibble(S_K=input$S/input$K, T_t=input$T)}
        else if (input$Exp == 2) {input_data = tibble(S_K=input$S/input$K, T_t=input$T, r=input$r/100, sigma=input$sigma/100)}
        else if (input$Exp == 3) {input_data = tibble(S_K=input$S/input$K, T_t=input$T, r=input$r/100, sigma=input$sigma/100, b=input$b, eta=input$eta/100)}
        else if (input$Exp == 4) {input_data = tibble(S_K=input$S/input$K, T_t=input$T, r=input$r/100, sigma=input$sigma/100, b=input$b, eta=input$eta/100)}
        else if (input$Exp == 5) {input_data = tibble(S_K=input$S/input$K, T_t=input$T, r=input$r/100, sigma=input$sigma/100, b=input$b, eta=input$eta/100)}
        else {input_data = tibble(S_K=input$S/input$K, T_t=input$T, r=input$r/100, sigma=input$sigma/100, b=input$b, eta=input$eta, dummy=input$dummy)}
        input_data = scale_data(input_data, means[[input$Exp]], stds[[input$Exp]])
        format(predict(models[[input$Exp]], input_data) * input$K, digits=4)
    })
    output$BSMPrice <- renderText({
        if(input$Exp == 1 | input$Exp == 2) {price = BSM_EU_call(input$S, input$K, 0, 0, input$r/100, input$sigma/100, 0, input$T)}
        #else if (input$Exp == 2) {price = BSM_EU_call(input$S, input$K, 0, 0, input$r/100, input$sigma/100, 0, input$T)}
        else {price = BSM_EU_call_barrier(input$S, input$K, input$b, input$eta/100, input$r/100, input$sigma/100, 0, input$T)}
        format(price, digits=4)
    })
}

shinyApp(ui, server)