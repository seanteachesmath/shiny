# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(deSolve)
library(shiny)

ui <- fluidPage(# Application title
  titlePanel("Predator-Prey Dynamics"),
  
  # Sidebar with a slider input for initial conditions and parameters
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "H0",
        withMathJax(helpText("Initial Prey, \\(H_0 = \\)")),
        min = 0,
        max = 500,
        value = 30,
        step = 5
      ),
      sliderInput(
        "L0",
        withMathJax(helpText("Initial Predator, \\(L_0 = \\)")),
        min = 0,
        max = 500,
        value = 30,
        step = 5
      ),
      sliderInput(
        "alpha",
        withMathJax(helpText("\\(\\alpha = \\)")),
        min = 0,
        max = 2,
        value = 1.1,
        step = 0.1
      ),
      sliderInput(
        "beta",
        withMathJax(helpText("\\(\\beta = \\)")),
        min = 1e-10,
        max = 0.5,
        value = 0.05,
        step = 0.01
      ),
      sliderInput(
        "vareps",
        withMathJax(helpText(
          "\\(\\varepsilon =\\)"
        )),
        min = 0,
        max = 1,
        value = 0.01,
        step = 0.001
      ),
      sliderInput(
        "delta",
        withMathJax(helpText("\\(\\delta = \\)")),
        min = 1e-10,
        max = 2,
        value = 0.5,
        step = 0.01
      ),
    ),
    
    # Show background and add plot
    mainPanel(withMathJax(
      helpText(
        "The standard predator-prey model is given by \\begin{align*}\\dfrac{dH}{dt} & = \\alpha H - \\beta HL\\\\\\dfrac{dL}{dt} & = \\varepsilon \\beta HL - \\delta L\\end{align*} with an initial condition \\(x_0\\) and choice of parameter \\(r\\). The bottom graph is a rendition of what is called 'a solution in the phase plane'. For certain changes the graph scales might recalculate."
      )# end helpText
    ),# end MathJax
    plotOutput("plot1")
    )# end main panel
  )# end sidebar
  )# end input

# Define server logic required to draw a histogram
server <- function(input, output) {
  observe({
    ## parameters
      parms <-
        c(
          alpha = input$alpha,
          beta = input$beta,
          epsilon = input$vareps,
          delta = input$delta
        )
      
    ## vector of time steps
    (times <- seq(0, 500, by = 0.1))
      
    ## initial conditions
    (xstart <- c(H = input$H0, L = input$L0))
    
    # a Predator-Prey model
    PPmod <- function(t, x, parms) {
      with(as.list(c(parms, x)), {
        dH <- alpha * H - beta * H * L
        dL <- epsilon * beta * H * L - delta * L
        list(c(dH, dL))
      })
    } ## end model
    
    # # # # # # # # # # # # # # # #
    
    
    ## plot 2
    out <- as.data.frame(lsoda(xstart, times, PPmod, parms))
    ## compute plot limits using relevant variables
    xmax <- 100 * ceiling(max(out$H) / 100)
    ymax <- 100 * ceiling(max(out$L) / 100)
    
    output$plot1 <- renderPlot({
      par(mar = c(4.1, 5.1, 0.8, 0.8),
          mfrow = c(1, 2) ## you probably want c(1, 1)
          )
      
      
      # # # # # # # # # # # # # # # #
      ## solution plot
      matplot(out$time, out[, c('H', 'L')], type = 'l', lwd = 2, col = c('blue', 'red'), lty = 1, ylim = c(0, max(c(xmax, ymax))), xlab = 'Time', ylab = 'Population densities (number/area)', las = 1)
      legend('topleft', c('Prey (H)', 'Predator (L)', 'Initial Value (below)'), col = c('blue', 'red', "black"), lty = c(1, 1, NA), pch = c(NA, NA, 19), merge = T, bg = 'white')
      
      ## phase plane, you probably *don't* want this at all
      plot(out$H, out$L, type = 'l', xlim = c(0, xmax), ylim = c(0, ymax), xlab = 'Prey (H)', ylab = 'Predator (L)', las = 1, lwd = 2)
      points(input$H0, input$L0, pch = 19)
    })
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
