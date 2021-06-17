library(shiny)

# Define UI for application that draws a histogram
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
                        value = 0.1),
            sliderInput("k",
                        "Initial water:",
                        min = 0,
                        max = 1,
                        value = 0.1),
            sliderInput("gamma",
                        "Added water:",
                        min = 0,
                        max = 1,
                        value = 0.1),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            # Simulate button
            actionButton("do", "Grow the snowflake!"),
           plotOutput("distPlot")
        )
    )
)

# Define server logic
server <- function(input, output) {
    observeEvent(input$do, {print("test")})
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$k + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
