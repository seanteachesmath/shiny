# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that computes Riemann sums
ui <- fluidPage(

    # Application title
    titlePanel("Riemann sum calculator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("rule", "Choose integration rule", choices = c("left", "right", "midpoint")),
            sliderInput("limits",
                        "limits",
                        min = 0,
                        max = 50,
                        value = c(0, 1),
                        step = 0.1),
            #sliderInput("b",
            #            "Upper limits:",
            #            min = 0,
            #            max = 50,
            #            value = 10,
            #            step = 0.1),
            sliderInput("n",
                        "Number of rectangles:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),# ends sidebarPanel

        # Show a plot of the generated distribution
        mainPanel(
          withMathJax(
            helpText("Choose a range that represents your limits of integration. To the left choose values for \\(a\\), \\(b\\), and \\(n\\) to approximate \\[\\int_{a}^{b} xe^{-x}\\,dx.\\]")
            ),
           plotOutput("rectangles")
        )# end mainPanel
    )# ends sidebarLayout
)# ends fluidPage

# Define server logic required to graph a function and rectangles for area approximation
server <- function(input, output) {

    output$rectangles <- renderPlot({
        # generate bins based on input$bins from ui.R
        f <- function(x)x*exp(-x)
        
        # this will let us use the sliders by their assigned variable name rather than require "input$" each time
        a <- input$limits[1]
        b <- input$limits[2]
        n <- input$n
        
        # initialize area, find 'dx', and start the plot
        area <- 0
        dx <- (b - a)/n
        plot(f, xlim = c(min(c(0, a)), b), ylim = c(0, 0.5), las = 1, lwd = 2)
        
        # step over each subinterval
        for(i in 1:n){
          # we added this late to incorporate other rules, the impact on the code is minimal
          # if anything we probably should have defined the point xstar sooner to minimize change of errors
          if(input$rule == "left"){
            xstar <- a + (i - 1)*dx
          }else if(input$rule == "right"){
            xstar <- a + (i)*dx  
          }else{
            xstar <- a + (i - 1/2)*dx
          }
          
          # codes the user-selected rectangle
          area <- area + f(xstar)*dx
          # calculates polygon positions
          xs <- c(a + (i - 1)*dx,  a + i*dx, a + i*dx, a + (i - 1)*dx)
          ys <- c(0, 0, f(xstar), f(xstar))
          polygon(xs, ys, density = 20)
        }
        # to add our answer, we plotted a point to orient outselves around the graph
        # points(b, 0.3)
        text(b, 0.45, paste("Estimate = ", area), pos = 2)
        text(b, 0.4, paste("Actual = ", integrate(f, lower = a, upper = b)$value), pos = 2)
        # more clever would be to calculate the max value of f(x) on the interval and use that rather than 0.45, 0.4
        # I changed those a bit after class
        # then if we allowed sliders for additional parameters in the function, the graph and text would update
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
