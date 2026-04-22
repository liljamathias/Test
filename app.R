# Demo Shiny app: interactive Kaplan-Meier curves.
#
# Run from the repo root with:
#   shiny::runApp(".")

library(shiny)
library(plotly)
library(survival)

source("R/interactive_km.R")

datasets <- list(
  lung    = survival::lung,
  veteran = survival::veteran
)

strata_choices <- list(
  lung    = c("(none)", "sex", "ph.ecog"),
  veteran = c("(none)", "trt", "celltype")
)

ui <- fluidPage(
  titlePanel("Interactive Kaplan-Meier curves"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Dataset", choices = names(datasets), selected = "lung"),
      selectInput("strata",  "Stratify by", choices = strata_choices$lung),
      checkboxInput("risk_table", "Show risk table", value = TRUE),
      helpText("Hover a curve to see time, S(t) and 95% CI.")
    ),
    mainPanel(
      plotlyOutput("km", height = "560px")
    )
  )
)

server <- function(input, output, session) {

  observeEvent(input$dataset, {
    updateSelectInput(session, "strata", choices = strata_choices[[input$dataset]])
  })

  dat <- reactive({
    d <- datasets[[input$dataset]]
    # survival::lung uses status 1/2 (censor/event); normalise to 0/1.
    if (input$dataset == "lung") d$status <- as.integer(d$status == 2)
    d
  })

  output$km <- renderPlotly({
    d <- dat()
    interactive_km(
      d,
      time       = "time",
      event      = "status",
      strata     = if (input$strata == "(none)") NULL else input$strata,
      risk_table = input$risk_table
    )
  })
}

shinyApp(ui, server)
