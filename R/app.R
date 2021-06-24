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

# Helper function to create the polygon for each hexagon
hexagon <- function(x, y, unitcell = 1, col = col) {
  polygon(c(
    x, x, x + unitcell / 2, x + unitcell, x + unitcell,
    x + unitcell / 2
  ), c(
    y + unitcell * 0.125,
    y + unitcell * 0.875,
    y + unitcell * 1.125,
    y + unitcell * 0.875,
    y + unitcell * 0.125,
    y - unitcell * 0.125
  ),
  col = col, border = "grey"
  )
}

# Define server logic
server <- function(input, output) {
  output$snowflake <- renderPlot(
    {
      frozen_cells <- is_frozen(state)

      x <- as.vector(frozen_cells)

      nrows <- nrow(frozen_cells)
      ncols <- ncol(frozen_cells)

      # Initiate the plot window
      plot(0, 0,
        type = "n", axes = FALSE, xlim = c(0, ncols),
        ylim = c(0, nrows), xlab = "", ylab = "", asp = 1
      )

      color_codes <- rep("#FFFFFF", length(x))
      for (i in 1:length(x)) {
        if (x[i]) color_codes[i] <- "#3288BD"
      }

      offset <- 0.5 # offset for the hexagons when moving up a row
      for (row in 1:nrows) {
        for (column in 0:(ncols - 1)) {
          hexagon(column + offset, row - 1, col = color_codes[row + nrows * column])
        }
        offset <- ifelse(offset, 0, 0.5)
      }
    },
    width = 500,
    height = 500
  )

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
