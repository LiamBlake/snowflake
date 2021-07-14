library(shiny)

source("growth.R")

cstate <- initialise(0.9)

# Define UI for application
ui <- fluidPage(

  # Application title
  titlePanel("Snowflake Growth Simulation"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      "Parameters",
      sliderInput(
        "a",
        "Rate of water transfer:",
        min = 0,
        max = 1,
        value = 1
      ),
      sliderInput(
        "k",
        "Initial water:",
        min = 0,
        max = 1,
        value = 0.9
      ),
      sliderInput(
        "gamma",
        "Added water:",
        min = 0,
        max = 1,
        value = 0.01
      ),
      checkboxInput(
        "show_values",
        "Show water vapour levels",
        value = FALSE
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      # Simulate button
      actionButton("do", "Grow the snowflake!"),
      plotOutput("snowflake")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Approach to get iteration in app, from
  # https://gist.github.com/trestletech/8608815
  rv <- reactiveValues(cstate = cstate, loop = 0)

  # Plot of state
  output$snowflake <- plot_cells(rv$cstate)

  observeEvent(input$do, {
    # Reset and start growth loop
    # Don't reset unless the loop is not currently in progress
    if (rv$loop < 1) {
      rv$cstate <- initialise(0.9)
      rv$loop <- 1
    }
  })

  # The actual loop
  observe({
    isolate({
      if (rv$loop > 0) {
        # Check for stopping condition
        if (check_stop(rv$cstate)) {
          rv$loop <- 0
        } else {

          # Iterate, plot
          rv$cstate <- step(input$a, input$gamma, rv$cstate)
          output$snowflake <- plot_cells(rv$cstate)
          rv$loop <- rv$loop + 1
        }
      }
    })

    # Check stopping condition - if loop continues, schedule
    # for later so the plot can be drawn again.
    if (check_stop(isolate(rv$cstate))) {
      rv$loop <- -1
    } else {
      invalidateLater(0, session)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
