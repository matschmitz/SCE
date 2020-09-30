library(shiny)

fluidPage(theme = "myCSS.css",
          
          tags$head(
            HTML('
        <script src="https://polyfill.io/v3/polyfill.min.js?features=es6">
        </script><script id="MathJax-script" async src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"></script>'
            )),
          h1("Estimation des paramètres"),
          
          fluidRow(
            column(width = 2, wellPanel(
              selectInput("model", "Modèle :",
                          c("b0" = "M1",
                            "b0 + b1*heures" = "M2")),
              conditionalPanel(
                condition = "input.model == 'M1'",
                tags$text("$$notes = \\hat{notes} + e$$"),
                tags$text("$$\\hat{notes} = \\textit{b}_{0}$$"),
                sliderInput("b0M1", "\\(\\textit{b}_{0}\\)",
                            min = 0, max = 20, value = 4, step = .2, animate = TRUE),
              ),
              conditionalPanel(
                condition = "input.model == 'M2'",
                tags$text("$$notes = \\hat{notes} + e$$"),
                tags$text("$$\\hat{notes} = \\textit{b}_{0} + \\textit{b}_{1}heures$$"),
                sliderInput("b0M2", "\\(\\textit{b}_{0}\\)",
                            min = 0, max = 20, value = 10, step = .2, animate = TRUE),
                sliderInput("b1M2", "\\(\\textit{b}_{1}\\)",
                            min = -10, max = 10, value = 0, step = .2, animate = TRUE),
              ),
              
              checkboxInput('ploty', tags$text('Donées', id = "y"), value = TRUE),
              checkboxInput('plotyhat', tags$text('Prédictions', id = "yhat")),
              checkboxInput('plote', tags$text('Erreurs', id = "error")),
              checkboxInput('plotSS', tags$text('SCE', id = "SS")),
            )),
            column(width = 7,
                   plotOutput("plot1", width = "100%"),
            ),
            column(width = 3,
                   conditionalPanel(condition = "input.plotSS == true & input.model == 'M1'",
                                    plotOutput("plot2", width = "100%")
                   ),
                   conditionalPanel(condition = "input.plotSS == true & input.model == 'M2'",
                                    plotOutput("plot3", width = "100%")
                   )
            )
          )
)