library(shiny)
library(diffeqr)

    FHNmod <- function(t, x, parms) {
      with(as.list(c(parms, x)), {
        dv <- -v*(v - a)*(v - 1) - w + Iapp
        dw <- eps*(v - gamma*w)
        list(c(dv, dw))
      })}
    
    parms <- c(a = input$a, Iapp = input$Iapp, eps = input$eps, gamma = input$gamma) ## parameters
    times <- seq(0, 100, by=0.1) ## vector of time steps
    xstart <- c(v = input$v0, w = 0) ## initial conditions
    #out <- as.data.frame(lsoda(xstart, times, FHNmod, parms))
    de <- diffeqr::diffeq_setup()
    prob <- de$ODEProblem(FHNmod, xstart, times, params)
    out <- de$solve(prob)
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
 