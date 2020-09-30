library(shiny)

function(input, output) {
  
  getmdlnow <- reactive({input$model})
  getb0M1now <- reactive({input$b0M1})
  getb0M2now <- reactive({input$b0M2})
  getb1M2now <- reactive({input$b1M2})
  
  output$plot1 <- renderPlot({
    mdlnow <- getmdlnow()
    b0M1now <- getb0M1now()
    b0M2now <- getb0M2now()
    b1M2now <- getb1M2now()
    print(b0M1now)
    print(b0M2now)
    print(b1M2now)
    print(input$model)
    
    if(mdlnow == "M1") {
      yhat <- b0M1now
    }
    if(mdlnow == "M2") {
      X <- cbind(1, DT$hours)
      B <- rbind(b0M2now, b1M2now)
      yhat <- X %*% B
    }
    
    GG <- data.table::copy(DT)
    
    # Compute predictions and errors
    GG[, yhat := yhat]
    GG[, e := y - yhat]
    
    # Compute square coordinates
    GG[, xmin := id]
    GG[, xmax := id + abs(e)/20*3]
    GG[, ymin := min(y, yhat), id]
    GG[, ymax := max(y, yhat), id]
    
    # MSE & RMSE
    My <- mean(GG$y)
    n <- nrow(GG)
    p <- ifelse(mdlnow == "M1", 1, 2)
    
    SS <- sum(GG$e**2)
    MSE <- GG[, sum((y - yhat)**2)/(n - p)]
    RMSE <- sqrt(MSE)
    
    print(MSE)
    print(RMSE)
    
    gg <- ggplot(GG) +
      # Required to prepare plot
      geom_point(aes(student, y), color = green, size = 4, alpha = 0)
    
    if(input$plotSS) {
      gg <- gg + geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                           fill = grey, alpha = .6) +
        annotate("text", x = 1, y = 20,
                 label = paste0("SCE = ", round(SS, 2)))
    }
    if(input$plote) {
      gg <- gg + geom_segment(aes(x = student, xend = student, y = y, yend = yhat),
                              color = red, size = 1)
    }
    if(input$plotyhat) {
      gg <- gg + 
        geom_point(aes(student, yhat), color = green, size = 4)
    }
    if(input$plotyhat & mdlnow == "M1") {
      gg <- gg + 
        geom_hline(yintercept = b0M1now,
                   color = green, alpha = .8, size = 1)
    }
    if(input$ploty) {
      gg <- gg + geom_point(aes(student, y), color = blue, size = 4)
    }
    
    # gg <- gg + geom_hline(yintercept = My + RMSE, color = "red")
    
    gg <- gg +
      scale_y_continuous(breaks = 0:20) +
      expand_limits(x = 8) +
      coord_fixed(.15, xlim = c(1, 8), ylim = c(0, 20)) +
      labs(y = "notes", x = "étudiant(e)") +
      apatheme
    
    return(gg)
  })
  
  output$plot2 <- renderPlot({
    mdlnow <- getmdlnow()
    b0M1now <- getb0M1now()
    
    print(b0M1now)
    print(input$model)
    
    X <- matrix(1, nrow = nrow(DT))
    b0 <- round(seq(0, 20, by = .2), 2)
    B <- matrix(b0, ncol = length(b0))
    y <- DT$y
    yHat <- X %*% B
    SS <- colSums((y - yHat)**2)
    GG <- data.table(SS, b0)
    GGnow <- GG[b0 == b0M1now]
    
    gg <- ggplot(GG, aes(b0, SS)) +
      geom_line(color = grey, size = 1.2) +
      geom_segment(data = GGnow, aes(x = b0M1now, xend = b0M1now, y = 0, yend = SS),
                   color = "grey40", alpha = .8, size = .8, linetype = "dashed") +
      geom_segment(data = GGnow, aes(x = 0, xend = b0M1now, y = SS, yend = SS),
                   color = "grey40", alpha = .8, size = .8, linetype = "dashed") +
      geom_point(data = GGnow, aes(b0M1now, SS), colour = redblood, size = 3) +
      scale_x_continuous(breaks = seq(0, 20, by = 2), limits = c(0, 20)) +
      labs(x = expression(italic("b"["0"])), y = "SCE") +
      apatheme
    
    return(gg)
  })
  
  output$plot3 <- renderPlot({
    mdlnow <- getmdlnow()
    b0M2now <- getb0M2now()
    b1M2now <- getb1M2now()
    cat("b0=", b0M2now, "\n")
    cat("b1=", b1M2now, "\n")
    cat("Model=", mdlnow, "\n")
    
    X <- cbind(1, DT$hours)
    b0 <- c(4.559, round(seq(0, 20, by = .2), 3))   # include best estimate of b0
    b1 <- c(3.382, round(seq(-10, 10, by = .2), 3)) # include best estimate of b1
    B <- as.matrix(t(expand.grid(b0 = b0, b1 = b1)))
    y <- DT$y
    yHat <- X %*% B
    SS <- colSums((y - yHat)**2)
    GG <- data.table(SS, t(B))
    GGnow <- GG[b0 == b0M2now & b1 == b1M2now]
    
    print(GGnow)
    
    X <- c(0, 0.01, 0.1, 0.5, 1.2**(0:6))
    X <- scales::rescale(X, to = 0:1)

    breaks <- c(quantile(GG$SS, X))
    breaks <- breaks[order(breaks)]
    
    ggplot(GG, aes(x = b0, y = b1, z = SS)) +
      geom_contour_filled(breaks = breaks) +
      geom_segment(data = GGnow, aes(x = 0, xend = b0M2now, y = b1M2now, yend = b1M2now),
                   color = "grey90", alpha = .8, size = .8, linetype = "dashed") +
      geom_segment(data = GGnow, aes(x = b0M2now, xend = b0M2now, y = -10, yend = b1M2now),
                   color = "grey90", alpha = .8, size = .8, linetype = "dashed") +
      geom_point(data = GGnow, aes(b0M2now, b1M2now), colour = "white", size = 3) +
      coord_cartesian(xlim = c(0, 20), ylim = c(-10, 10)) +
      scale_fill_brewer(name = "SCE", palette = "Spectral") +
      apatheme
  })
  
}