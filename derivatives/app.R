library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Visualization of critical and inflection points"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            withMathJax(helpText("Below you will visualize critial and inflection points for the function \\(f(x) = x^3 -3x^2 + 2x\\) or \\(g(x) = \\sin(\\pi x)\\).")),
            #withMathJax(helpText("Below you will visualize Riemann sums used to approximate the value of \\(\\int_{a}^{b} f(x)\\,dx\\) for your choice of limits of integration \\(a\\) and \\(b\\), function \\(f(x) = x^2\\), \\(f(x) = x^3\\), or \\(f(x) = \\sin(x)\\), number of rectangles \\(n\\), and rectangle rule.")),
            
            selectInput("func", "Which function?",
                        c("Polynomial" = "poly",
                          "Trig" = "trig")),
        
            selectInput("type", "Type of point",
                               c("Critical" = "crit",
                                 "Inflection" = "infl",
                                 "Both" = "both")),
        ),


        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

server <- function(input, output) {
    output$distPlot <- renderPlot({
        
        #if(input$m==1 | input$b==0){ym <- 10}else{ym <- abs(input$b/(input$m-1));ym <- 5*ceiling(ym/5)}
        
        par(mfrow=c(1,1), mar=c(4.1, 4.1, 0.8, 0.8), xaxs='i', yaxs='i')
        
        if(input$func=="poly"){
            f <- function(x) x^3 - 3*x^2 + 2*x
            fp <- function(x) 3*x^2-6*x+2
            fpp <- function(x) 6*x-6
            a <- c((3 - sqrt(3))/3, (3 + sqrt(3))/3)
            p <- 1
            
        }else{
            f <- function(x) sin(pi*x)
            a <- (2*(-1:5) + 1)*(1/2)
            p <- (2*(-1:5))*(1/2)
            
        }
        plot(f, xlim=c(-1, 3), ylim = c(-3, 3), las=1, axes=F, lwd=2, xlab="", ylab="")
        axis(1, pos=0)
        axis(2, pos=0, las=1)
        mtext("x", 4, at=0, font=2, cex=1.25, las=1, line=0.2)
        mtext("y", 3, at=0, font=2, cex=1.25, line=0.2)
        box()
        legend("topright", c("Function", "Critical point(s)", "Inflection point(s)"), lty=c(1, NA, NA), pch=c(NA, 19, 19), col=c(1, 2, 4), lwd=c(2, NA, NA))
        
        
        if(input$type=="crit"){
            points(c(a), f(c(a)), pch=19, col=2)
        } else if(input$type=="infl"){
            points(c(p), f(p), pch=19, col=4)
        } else {
            points(c(a), f(c(a)), pch=19, col=2)
            points(c(p), f(p), pch=19, col=4)
        }
            
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)