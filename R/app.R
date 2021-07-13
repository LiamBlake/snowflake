library(shiny)

source("growth.R")

# This is the state of the system, which must persist across server loops
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
server <- function(input, output) {
  # Initialisation
  output$snowflake <- plot_cells(cstate)

  # Approach to get iteration in app, from
  # https://stackoverflow.com/a/61867175
  rv <- reactiveValues(loop = 0)

  observeEvent(input$do, {
    # Reset and start growth loop
    # Don't reset unless the loop is not currently in progress
    if (rv$loop < 1) {
      cstate <<- initialise(0.9)
      rv$loop <- 1
    }
  })

  # The actual loop
  observeEvent(rv$loop, {
    if (rv$loop > 0) {
      # Check for stopping condition
      if (check_stop(cstate)) {
        rv$loop <- 0
      } else {

        # Iterate, plot
        cstate <<- step(input$a, input$gamma, cstate)
        output$snowflake <- plot_cells(cstate)
        rv$loop <- rv$loop + 1
        print(cstate)
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
