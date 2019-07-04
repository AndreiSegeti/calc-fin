#SERVER WITH MULTIPLE TABS

server <- function(input, output){
  v <- reactiveValues(giveResult = FALSE)
  
  observeEvent(input$go, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    v$giveResult <- input$go
  })
  
  observeEvent(input$tabset, {
    v$giveResult <- FALSE
  })  
  
#ObserveEvents do Cálculo do Valor Futuro
  observeEvent(input$com1, {
    v$data <- paste("Juros:", (input$vf1/input$c11)^(1/input$per1) - 1)
  })
  
  observeEvent(input$sim1, {
    v$data <- paste("Juros:", ((input$vf1 - input$c11)/input$c11)/input$per1)
  })
#ObserveEvents do Cálculo do Juros
  observeEvent(input$com2, {
    v$data <- paste("Juros:", (input$vf2/input$c12)^(1/input$per2) - 1)
  })
  
  observeEvent(input$sim2, {
    v$data <- paste("Juros:", ((input$vf2 - input$c12)/input$c12)/input$per2)
  })
  
#Restart
  observeEvent(input$res, {
    v$data <- NULL
  })
  
#Output

  output$resultado <- renderText({
    if (v$giveResult == FALSE) return()
    
    isolate({
      data <- if (input$tabset == "Valor Futuro") {
        paste(v$data)
      } else {
        paste(v$data)
      }
      paste(data)
    })
  })
  
}

shinyApp(ui, server)