library(deSolve)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Enzyme-Substrate Kinetics"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "S0",
        withMathJax(helpText("Initial Substrate, \\(S_0 = \\)")),
        min = 0,
        max = 100,
        value = 30,
        step = 1
      ),
      sliderInput(
        "E0",
        withMathJax(helpText("Initial Free Enzyme, \\(E_0 = \\)")),
        min = 0,
        max = 100,
        value = 30,
        step = 1
      ),
      sliderInput(
        "I0",
        withMathJax(helpText("Initial Inhibitor, \\(I_0 = \\)")),
        min = 0,
        max = 100,
        value = 0,
        step = 1
      ),
      sliderInput(
        "tmax",
        withMathJax(helpText("\\(t_{\\text{max}} = \\)")),
        min = 100,
        max = 300,
        value = 100,
        step = 10
      ),
      # sliderInput("k1", withMathJax(helpText("\\(k_{1} = \\)")),
      #             min = 0, max = 0.5, value = 0.05, step = 0.01),
      # sliderInput("k1m", withMathJax(helpText("\\(k_{-1} = \\)")),
      #             min = 0, max = 0.5, value = 0.05, step = 0.01),
      # sliderInput("kp", withMathJax(helpText("\\(k_{p} = \\)")),
      #             min = 0, max = 0.5, value = 0.05, step = 0.01),
      # sliderInput("k3", withMathJax(helpText("\\(k_{3} = \\)")),
      #             min = 0, max = 0.5, value = 0.0, step = 0.01),
      # sliderInput("k3m", withMathJax(helpText("\\(k_{-3} = \\)")),
      #             min = 0, max = 0.5, value = 0.0, step = 0.01),
      #      ),
      #      sidebarPanel(
      # sliderInput("S0", withMathJax(helpText("Initial Substrate, \\(S_0 = \\)")), min = 0, max = 100, value = 30, step = 1),
      # sliderInput("E0", withMathJax(helpText("Initial Free Enzyme, \\(E_0 = \\)")),  min = 0, max = 100,   value = 30,   step = 1),
      # sliderInput("I0", withMathJax(helpText("Initial Inhibitor, \\(I_0 = \\)")), min = 0, max = 100, value = 0, step = 1),
      sliderInput(
        "k1",
        withMathJax(helpText("\\(k_{1} = \\)")),
        min = 0,
        max = 0.5,
        value = 0.05,
        step = 0.01
      ),
      sliderInput(
        "k1m",
        withMathJax(helpText("\\(k_{-1} = \\)")),
        min = 0,
        max = 0.5,
        value = 0.05,
        step = 0.01
      ),
      sliderInput(
        "kp",
        withMathJax(helpText("\\(k_{p} = \\)")),
        min = 0,
        max = 0.5,
        value = 0.05,
        step = 0.01
      ),
      sliderInput(
        "k3",
        withMathJax(helpText("\\(k_{3} = \\)")),
        min = 0,
        max = 0.5,
        value = 0.0,
        step = 0.01
      ),
      sliderInput(
        "k3m",
        withMathJax(helpText("\\(k_{-3} = \\)")),
        min = 0,
        max = 0.5,
        value = 0.0,
        step = 0.01
      ),
    ),
    # Show a plot of the generated distribution
    mainPanel(withMathJax(
      helpText(
        "Below are results of a the dynamics of an enzyme-catalyzed reaction in the presence of an inhibitor. The defaults assume a reaction **without** inhibitors. You are encouraged to systematically experiment with initial conditions, then with parameters.

                           In the absence of inhibitors, the \\(SC\\)-plane (here \\(SC_{1}\\)-plane) is shown corresponding to the solution to the original model in the numerically-generated phase plane (no nullclines)."
      )
    ), plotOutput("plot1"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  observe({
    ## parameters
    (parms <- c(
      k1 = input$k1,
      k1m = input$k1m,
      kp = input$kp,
      k3 = input$k3,
      k3m = input$k3m
    ))
    ## vector of time steps
    (times <- seq(0, input$tmax, by = 0.1))
    ## initial conditions
    (xstart <- c(
      S = input$S0,
      E = input$E0,
      C1 = 0,
      P = 0,
      I = input$I0,
      C2 = 0
    ))
    
    
    # a Predator-Prey model
    PPmod <- function(t, x, parms) {
      with(as.list(c(parms, x)), {
        dS <- -k1 * S * E + k1m * C1
        dE <- -k1 * S * E + (k1m + kp) * C1 - k3 * I * E + k3m * C2
        dC1 <- k1 * S * E - (k1m + kp) * C1
        dP <- kp * C1
        dI <- -k3 * I * E + k3m * C2
        dC2 <- k3 * I * E - k3m * C2
        list(c(dS, dE, dC1, dP, dI, dC2))
      })
    }
    
    # # # # # # # # # # # # # # # #
    
    
    ## plot 2
    
    
    out <- as.data.frame(lsoda(xstart, times, PPmod, parms))
    xmax <- ymax <- 100 * ceiling(max(out[, c("S", "E", "I", "C1", "C2")]) / 100)
    
    output$plot1 <- renderPlot({
      layout(matrix(c(1, 2, 3, 4, 5, 5), byrow = TRUE, ncol = 2))
      par(mar = c(4.1, 5.1, 0.8, 0.8))
      
      
      # # # # # # # # # # # # # # # #
      ## plot 1
      
      plot(
        out[, c("time", "E")],
        type = 'l',
        lty = 1,
        ylim = c(0, max(c(xmax, ymax))),
        xlab = 'Time',
        ylab = "Free Enzyme\nConcentration",
        las = 1,
        lwd = 2,
        font.lab = 2,
        cex = 1.35
      )
      matplot(
        out$time,
        out[, c('S', 'I')],
        type = 'l',
        lwd = 2,
        col = c('blue', 'red'),
        lty = 1,
        ylim = c(0, max(c(xmax, ymax))),
        xlab = 'Time',
        ylab = "Substrate and Inhibitor\nConcentration",
        las = 1,
        font.lab = 2,
        cex = 1.35
      )
      legend(
        "topright",
        c("Free Substrate", "Free Inhibitor"),
        col = c("blue", "red"),
        lty = 1,
        lwd = 2
      )
      matplot(
        out$time,
        out[, c('C1', 'C2')],
        type = 'l',
        lwd = 2,
        col = c('green', 'gray'),
        lty = 1,
        ylim = c(0, max(c(xmax, ymax))),
        xlab = 'Time',
        ylab = 'Complex',
        las = 1,
        font.lab = 2,
        cex = 1.35
      )
      legend(
        "topright",
        c("Enzyme:Substrate complex", "Enzyme:Inhibitor complex"),
        col = c('green', 'gray'),
        lty = 1,
        lwd = 2
      )
      plot(
        out[, c("time", "P")],
        type = 'l',
        lty = 1,
        ylim = c(0, max(c(xmax, ymax))),
        xlab = 'Time',
        ylab = "Product\nConcentration",
        las = 1,
        lwd = 2,
        font.lab = 2,
        cex = 1.35
      )
      if (input$I0 == 0) {
        plot(
          out[, c("S", "C1")],
          type = 'l',
          lwd = 2,
          las = 1,
          xlab = "Substrate\nConcentration",
          ylab = "Enzyme:Substrate Complex\nConcentration",
          font.lab = 2,
          cex = 1.35,
          xlim = c(0, 10 * ceiling(input$S0 / 10))
        )
        points(input$S0, 0, pch = 19)
        legend("topright", "Intial condition", pch = 19)
      } else{
        plot(
          NULL,
          xlim = c(0, 1),
          ylim = c(0, 1),
          axes = F,
          xlab = "",
          ylab = ""
        )
      }
      
    })
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
