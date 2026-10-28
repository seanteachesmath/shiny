library(shiny)
ui <- shinyUI(fluidPage(
    
  # Application title
  titlePanel("data and trendlines"),
    
  # Sidebar with a slider input
  sidebarLayout(
    sidebarPanel(
      withMathJax(helpText("Depending on your choice, you will pick either the slope for the linear trend of the form \\(y = ax\\) or the rate parameter for the exponential trend of the form \\(y = e^{ax}\\).")),
      selectInput("func", "Trend",
          c("Linear" = "linear",
          "Exponential" = "exp")),
      
     sliderInput(input="par",
          withMathJax(helpText("\\(a\\)")),
          min = 0,
          max = 3,
          value = 1,
          step = 0.01),
          ),
    mainPanel(
      plotOutput("plot1")
    )
)))

server <- function(input, output) {
output$plot1 <- renderPlot({
    if(input$func=="linear"){

    x <- c(1.4, 2, 4.7)
    y <- c(2.8, 4, 7.8)
    
    best <- coef(lm(y ~ 0 + x))
    f <- function(x)input$par*x
    err <- function(a){
      out <- 0
      for(i in 1:length(x)){out <- out + (y[i]-a*x[i])^2}
      return(out)
    }
    par(mfrow=c(1, 2), xaxs='i', yaxs='i', mar=c(4.1, 4.1, 1.1, 1.1))
    #layout(matrix(c(1,2), nrow=2, ncol=1))
    plot(x, y, xlim=c(0,10), ylim=c(0, 10), xlab="", ylab="", las=1, pch=19, cex=1)
    mtext("Initial Area (sq. mm)", 1, font=2, line=2)
    mtext("Final Area (sq. mm)", 2, font=2, line=2)
    abline(a=0, b=input$par, lwd=2)
    for(i in 1:length(x)){
      segments(x[i], y[i], x[i], f(x[i]), col=2)
    }
    
  }else if(input$func=="exp"){
    x <- c(1.3, 4.2, 5.1)
    y <- c(1.3, 2.8, 4.4)
    best <- coef(lm(log(y) ~ 0 + x))
    f <- function(x)exp(input$par*x)
    err <- function(a){
      out <- 0
      for(i in 1:length(x)){out <- out + (log(y[i])-a*x[i])^2}
      return(out)
    }
    par(mfrow=c(1, 2), xaxs='i', yaxs='i', mar=c(4.1, 4.1, 1.1, 1.1))
    #layout(matrix(c(1,2), nrow=2, ncol=1))
    plot(x, y, xlim=c(0,10), ylim=c(0, 10), xlab="", ylab="", las=1, pch=19, cex=1)
    plot(function(x)exp(input$par*x), xlim=c(0,10), lwd=2, add=T)
    mtext("x", 1, font=2, line=2)
    mtext("y = f(x)", 2, font=2, line=2)

    for(i in 1:length(x)){
      segments(x[i], y[i], x[i], f(x[i]), col=2)
    }
    
  }

        plot(err, xlim=c(0, 3), ylim=c(0, max(err(0), err(3))), las=1, xlab="", ylab="", lwd=2)
        mtext("parameter value (a)", 1, font=2, line=2)
        mtext("Sum of squared errors", 2, font=2, line=2.5)
        points(input$par, err(input$par), pch=19, col=4, cex=2, lwd=2)
        points(best, err(best), pch=21, col=2, cex=2, lwd=2)
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)