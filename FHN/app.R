library(shiny)
library(deSolve)

ui <- fluidPage(

    # Application title
    titlePanel("FitzHugh-Nagumo Model"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("v0",
                        withMathJax(helpText("\\(v_{0} = \\)")),
                        min = 0,
                        max = 1,
                        value = 0.1,
                        step = 0.01),
            sliderInput("a",
                        withMathJax(helpText("\\(a = \\)")),
                        min = 0,
                        max = 1,
                        value = 0.1,
                        step = 0.01),
            sliderInput("Iapp",
                        withMathJax(helpText("\\(I_{\\text{app}} = \\)")),
                        min = 0,
                        max = 1,
                        value = 0.1,
                        step = 0.01),
            sliderInput("eps",
                        withMathJax(helpText("\\(\\varepsilon = \\)")),
                        min = 0,
                        max = 0.1,
                        value = 0.01,
                        step = 0.01),
            sliderInput("gamma",
                        withMathJax(helpText("\\(\\gamma = \\)")),
                        min = 0,
                        max = 10,
                        value = 1, 
                        step = 0.1)
        ),

        mainPanel(
           plotOutput("solsPlot")#,
#           plotOutput("phasePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
observe({
  FHNmod <- function(t, x, parms) {
    with(as.list(c(parms, x)), {
      dv <- -v*(v - a)*(v - 1) - w + Iapp
      dw <- eps*(v - gamma*w)
      list(c(dv, dw))
    })}
  
  parms <- c(a = input$a, Iapp = input$Iapp, eps = input$eps, gamma = input$gamma) ## parameters
  times <- seq(0, 100, by=0.1) ## vector of time steps
  xstart <- c(v = input$v0, w = 0) ## initial conditions
  out <- as.data.frame(lsoda(xstart, times, FHNmod, parms))
  vnull <- function(x){with(as.list(c(parms, x)), -x*(x-input$a)*(x-1) + input$Iapp)}
  vr <- range(out$v)
  wr <- range(out$w)
  yr <- range(c(wr, vnull(seq(0, 1, by = 0.1))))
  
    output$solsPlot <- renderPlot({
  matplot(out$time, out[, c("v", "w")], ylim = range(c(vr, wr)), col = c("black", "red"), lty = 1, lwd = 2, type = 'l', las = 1, xlab = "Time", ylab = "V, w")
    })
    
    output$phasePlot <- renderPlot({
      plot(out[, c("v", "w")], ylim = yr, col = c("black"), lty = 1, lwd = 2, type = 'l', las = 1)
      plot(vnull, xlim = vr, ylim = yr, col = "blue", lty = 1, lwd = 2, add = TRUE)
      plot(function(x){with(as.list(c(parms, x)), x/input$gamma)}, xlim = vr, col = "red", lty = 1, lwd = 2, add = TRUE)
    })
})
}

# Run the application 
shinyApp(ui = ui, server = server)
