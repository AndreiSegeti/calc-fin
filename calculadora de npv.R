library(shiny)

ui <- fluidPage(
  sidebarPanel(
    numericInput("c0", "Payment", value = -400),
    numericInput("n", "Number of entries",  value = 5),
    numericInput("j", "Interest", value = 0.1),
    numericInput("cf", "Constant Cash Flow", value = 100),
    actionButton("go", "Calcular Valor Presente Líquido")),
  mainPanel(
    verbatimTextOutput("resultadopv")
  )
)

server <- function(input, output) {
  v <- reactiveValues(data = NULL)
  d <- reactiveValues(data2 = NULL)
  
  observeEvent(input$go, {
    dis.fac <- (1 + input$j)
    vet.dis.fac <- rep(0, input$n)
    cash <- rep(input$cf, input$n)
    for (i in 1:input$n){
      vet.dis.fac[i] <- (dis.fac)^i
    }
    v$data <-  sum(data.frame(cash/vet.dis.fac)) 
    d$data2 <- input$c0
  })
  
  output$resultadopv <- renderText({
    
    paste("Net Present Value:", d$data2 + v$data)
    
  })
}

shinyApp(ui, server)
