ui <- navbarPage("Calculadora Financeira",
                 theme = shinytheme("slate"),
                 navbarMenu("Basics",
                            tabPanel("Future Value",
                                     numericInput("c11",
                                                  "Capital Inicial",
                                                  value = 100),
                                     numericInput("jur1",
                                                  "Taxa de juros ao ano",
                                                  value = 0.1),
                                     numericInput("per1",
                                                  "Período em anos",
                                                  value = 1),
                                     selectInput("ki1", "Tipo de Juros",
                                                 list("--","Compostos", "Simples")),
                                     verbatimTextOutput("r1")),
                            tabPanel("Juros",
                                     numericInput("c12",
                                                  "Capital Inicial",
                                                  value = 100),
                                     numericInput("vf2",
                                                  "Valor Futuro",
                                                  value = 110),
                                     numericInput("per2",
                                                  "Período em anos",
                                                  value = 1),
                                     selectInput("ki2", "Tipo de Juros",
                                                 list("--","Compostos", "Simples")),
                                     verbatimTextOutput("r2")),
                            tabPanel("Período",
                                     numericInput("c13",
                                                  "Capital Inicial",
                                                  value = 100),
                                     numericInput("vf3",
                                                  "Valor Futuro",
                                                  value = 110),
                                     numericInput("jur3",
                                                  "Taxa de juros ao ano",
                                                  value = 0.1),
                                     selectInput("ki3", "Tipo de Juros",
                                                 list("--","Compostos", "Simples")),
                                     verbatimTextOutput("r3")),
                            tabPanel("Capital Inicial",
                                     numericInput("vf4",
                                                  "Valor Futuro",
                                                  value = 110),
                                     numericInput("jur4",
                                                  "Taxa de juros ao ano",
                                                  value = 0.1),
                                     numericInput("per4",
                                                  "Período em anos",
                                                  value = 1),
                                     selectInput("ki4", "Tipo de Juros",
                                                 list("--","Compostos", "Simples")),
                                     verbatimTextOutput("r4"))
                            )
)

server <- function(input, output) {
  vf <- reactiveValues(doText = FALSE)
  j  <- reactiveValues(doText = FALSE)
  p  <- reactiveValues(doText = FALSE)
  vp <- reactiveValues(doText = FALSE)
  
     ###########################     Basics   ##############################
  ###########################     Future Value   ##############################
 
   observeEvent(input$ki1, {
    vf$doText <- input$ki1
  })
  
  output$r1 <- renderText({
    if (vf$doText == FALSE) return()
    isolate({
      data <-
        if(input$ki1 == "Compostos"){
          vf$data <- paste("Valor Futuro:", (input$c11)*(1 + (input$jur1))^(input$per1))
        } else {
          vf$data <- paste("Valor Futuro:", (input$c11) + (input$c11)*(input$jur1)*(input$per1))
        }
    })
  })
  
  ###########################     Basics   ##############################
  ###########################     JUROS   ##############################
  
  observeEvent(input$ki2, {
    j$doText <- input$ki2
  })
  
  output$r2 <- renderText({
    if (j$doText == FALSE) return()
    isolate({
      data <-
        if(input$ki2 == "Compostos"){
          j$data <- paste("Juros:", (input$vf2/input$c12)^(1/input$per2) - 1)
        } else {
          j$data <- paste("Juros:", ((input$vf2 - input$c12)/input$c12)/input$per2)
        }
    })
  })
  
  ###########################     Basics   ##############################
  ###########################    PERIODO   ##############################
  
  observeEvent(input$ki3, {
    p$doText <- input$ki3
  })
  
  output$r3 <- renderText({
    if (p$doText == FALSE) return()
    isolate({
      data <-
        if(input$ki3 == "Compostos"){
          p$data <- paste("Período:", log(input$vf3/input$c13)/log(1+input$jur3))
        } else {
          p$data <- paste("Período:", (input$vf3 - input$c13)/(input$jur3*input$c13))
        }
    })
  })
  
  ###########################     Basics   ##############################
  ###########################    PRESENT VALUE   ##############################
  
  observeEvent(input$ki4, {
    vp$doText <- input$ki4
  })
  
  output$r4 <- renderText({
    if (vp$doText == FALSE) return()
    isolate({
      data <-
        if(input$ki4 == "Compostos"){
          vp$data <- paste("Valor Presente:", input$vf4/((1+input$jur4)^input$per4))
        } else {
          vp$data <- paste("Valor Presente:", (input$vf4/(1 + (input$jur4)*(input$per4))))
        }
    })
  })
}



shinyApp(ui, server)