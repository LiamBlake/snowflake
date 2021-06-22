library(shiny)

source("growth.R")

# Indicates whether the snowflake is currently being grown
default_k <- 0.1
growing <- FALSE
state <- initialise(default_k)

# Define UI for application
ui <- fluidPage(

  # Application title
  titlePanel("Snowflake Growth Simulation"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      "Parameters",
      sliderInput("a",
        "Rate of water transfer:",
        min = 0,
        max = 1,
        value = 0.1
      ),
      sliderInput("k",
        "Initial water:",
        min = 0,
        max = 1,
        value = default_k
      ),
      sliderInput("gamma",
        "Added water:",
        min = 0,
        max = 1,
        value = 0.1
      ),
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
  output$snowflake <- renderPlot({
    # Draw the current state of the snowflake
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$k + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = "darkgray", border = "white")
  })
  observeEvent(input$do, {
    # Trigger the growth process
    if (!growing) {
      growing <- TRUE
      state <- initialise(input$k)
    }
  })

  growing <- FALSE

  # Perform the growth
  if (growing) {
    state <- step(input$a, input$gamma, state)
  }

  # Check stopping condition
  if (check_stop(state)) {
    growing <- FALSE
  }
}

# Run the application
shinyApp(ui = ui, server = server)
