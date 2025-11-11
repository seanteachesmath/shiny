library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Discrete-time population models"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "alpha",
        withMathJax(helpText("\\(\\alpha = \\)")),
        min = 0,
        max = 1,
        value = 0.1,
        step = 0.01
      ),
      sliderInput(
        "beta",
        withMathJax(helpText("\\(\\beta = \\)")),
        min = 0,
        max = 100,
        value = 30,
        step = 1
      ),
      sliderInput(
        "sigma",
        withMathJax(helpText("\\(\\sigma = \\)")),
        min = 0,
        max = 1,
        value = 0.1,
        step = 0.01
      ),
            sliderInput(
        "V0",
        withMathJax(helpText("\\(V_{0} = \\)")),
        min = 0,
        max = 100,
        value = 10,
        step = 1
      ),
            sliderInput(
        "F0",
        withMathJax(helpText("\\(F_{0} = \\)")),
        min = 0,
        max = 100,
        value = 10,
        step = 1
      ),
      sliderInput(
        "tmax",
        withMathJax(helpText("\\(t_{\\text{max}} = \\)")),
        min = 10,
        max = 100,
        value = 50,
        step = 10
      ),
    ),
    # Show a plot of the generated distribution
    mainPanel(withMathJax(
      helpText("")
    ), 
    plotOutput("plot1")),
    plotOutput("plot2"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  observe({
    ## parameters
    (parms <- c(
      alpha = input$alpha,
      beta = input$beta,
      sigma = input$sigma
    ))
    ## vector of time steps
    (times <- seq(0, input$tmax, by = 0.1))
    ## initial conditions
    (xstart <- c(
      V0 = input$V0,
      F0 = input$F0,
    ))
    
    A <- matrix(c(alpha, beta, sigma, 0), nrow = 2, ncol = 2, byrow = TRUE)
    N <- matrix(c(input$V0, input$F0), nrow = 2, ncol = 1, byrow = TRUE)
    eig <- eigen(A)
    
    idx <- which(eig$values == max(eig$values))
    
    sol <- N
    for(i in 2:tmax){
      sol <- cbind(sol, A%*%sol[, i - 1])
    }
    # # # # # # # # # # # # # # # #
    
    output$plot1 <- renderPlot({

      par(mar = c(4.1, 5.1, 0.8, 0.8), xaxs = 'i', yaxs = 'i')
      matplot(t(sol), type = 'l', las = 1, ylim = c(0, 10*ceiling(max(sol)/10)), lty = 1, lwd = 2, xlab = "Time", ylab = "Abundance")
      legend("topright", c("vegetative", "flowering"), col = c(1, 2), lwd = 2)      
    })
    
    output$plot2 <- renderPlot({
      par(mar = c(4.1, 5.1, 0.8, 0.8), xaxs = 'i', yaxs = 'i')
      plot(t(sol), type = 'l', las = 1, ylim = c(0, 10*ceiling(max(sol)/10)), lty = 1, lwd = 2, xlab = "Vegetative", ylab = "Flowering")
      points(sol[, 1], pch = 19)
      legend("topright", c("Dynamics", "Initial Condition"), col = c(1, 1), lwd = 2, lty = c(1, NA), pch = c(NA, 19))      
      if(eig$values[idx] > 0){
        abline(a = 0, b = eig$vectors[2, idx]/eig$vectors[1, idx])
      }
    })
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
