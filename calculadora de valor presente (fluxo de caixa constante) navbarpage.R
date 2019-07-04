ui <- navbarPage("Calculadora Financeira",
                 theme = shinytheme("slate"),
                 navbarMenu("Intermediary", 
                            tabPanel("Present Value with Cash Flow",
                                     numericInput("ricf", "Constant Cash Flow", value = 100),
                                     numericInput("rin", "Number of entries",  value = 5),
                                     numericInput("rij", "Interest", value = 0.1),
                                     actionButton("cal1", label = "Calculate"),
                                     verbatimTextOutput("ri1")
                            ),
                            tabPanel("NPV",
                                     numericInput("ricf2", "Constant Cash Flow", value = 100),
                                     numericInput("rin2", "Number of entries",  value = 5),
                                     numericInput("rij2", "Interest", value = 0.1),
                                     numericInput("ric0", "Payment", value = -400),
                                     actionButton("rical2", label = "Calculate"),
                                     verbatimTextOutput("ri2")
                            )
                 )
)

server <- function(input, output){
  vpc <- reactiveValues(data = NULL)
  nv <-  reactiveValues(data = NULL)
  
  observeEvent(input$cal1, {
    dis.fac <- (1 + input$rij)
    vet.dis.fac <- rep(0, input$rin)
    cash <- rep(input$ricf, input$rin)
    for (i in 1:input$rin){
      vet.dis.fac[i] <- (dis.fac)^i
    }
    vpc$data <- sum(data.frame(cash/vet.dis.fac))  
  })
  
  output$ri1 <- renderText({
    paste(vpc$data)
  })
  
  observeEvent(input$rical2, {
    dis.fac <- (1 + input$rij2)
    vet.dis.fac <- rep(0, input$rin2)
    cash <- rep(input$ricf2, input$rin2)
    for (i in 1:input$rin2){
      vet.dis.fac[i] <- (dis.fac)^i
    }
    nv$data <- (input$ric0 + sum(data.frame(cash/vet.dis.fac)))
  })
  
  output$ri2 <- renderText({
    paste(nv$data)
  })
} 
  
shinyApp(ui, server)    