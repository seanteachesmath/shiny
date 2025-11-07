library(shiny)
ui <- shinyUI(fluidPage(
    
    # Application title
    titlePanel("a function and tangent lines"),
    
    # Sidebar with a slider input
    sidebarLayout(
        sidebarPanel(
            withMathJax("Choose the 'base point' for the tangent line and the value of \\(h\\) (or \\(\\Delta x\\)) that separates the base point and the second point."),
            HTML("With a fixed base point, you can vary the distance between the points and watch the secant line approach the tangent line.
         <br>
         <br>"),
            sliderInput(inputId="slidera",
                        "base point",
                        min = 0,
                        max = 4,
                        value = 1,
                        step = 0.5) ,
            #  ),
            #  sidebarPanel(
            sliderInput(inputId="sliderb",
                        "zoom",
                        min = 0,
                        max = 25,
                        value = 0,
                        step = 5)
        ),
        mainPanel(plotOutput("plot1"))
    )))


server <- function(input, output) {
    
    Fun <- function(x){x^2}#{exp(0.3*x)}
    
    f <- function(x){2*x}#{0.3*exp(0.3*x)}
    
    output$plot1 <- renderPlot({
        par(xaxs='i', yaxs='i')
        low <- input$slidera - 1/(1+input$sliderb)
        high <- input$slidera + 1/(1+input$sliderb)
        plot(Fun, xlim=c(low, high), ylim=c(Fun(low), Fun(high)), las=1, ylab="function, tangent", lwd=3)
        grid()
        abline(a = Fun(input$slidera)-f(input$slidera)*input$slidera, b =f(input$slidera), col='blue', lwd=2, lty=3)
        #x <- seq(0, input$slider, by=0.05)
        #  polygon(c(x, rev(x)), c(0*x, f(rev(x))), col="gray")
        text(1,8, bquote(m[tan]==.(round(f(input$slidera), 3))), pos=4, col='blue')
        points(input$slidera, Fun(input$slidera), pch=19, cex=1.0)
        
    })
    
    # output$plot2 <- renderPlot({
    #  plot(F, xlim=c(-1, 3), las=1, ylab="F (the c.d.f)")
    #segments(input$slider, 0, input$slider, F(input$slider))
    #points(input$slider, F(input$slider), pch=19)
    #text(input$slider,F(input$slider), paste("(", input$slider, ", ",F(input$slider), ")", sep=''), pos=4)
    #})
    
}

# Run the application 
shinyApp(ui = ui, server = server)