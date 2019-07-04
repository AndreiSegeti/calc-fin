library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id = "ad",
                  tabPanel("Present Value",
                           numericInput("cf", "Constant Cash Flow", value = 100),
                           numericInput("n", "Number of entries",  value = 5),
                           numericInput("j", "Interest", value = 0.1)),
                  tabPanel("Net Present Value",
                           numericInput("cf", "Constant Cash Flow", value = 100),
                           numericInput("n", "Number of entries",  value = 5),
                           numericInput("j", "Interest", value = 0.1),
                           numericInput("c0", "Payment", value = -400))
        
      ),
      actionButton("cal", label = "Calculate")
    ),
    mainPanel(
     verbatimTextOutput("result")
    )
  )
)

server <- function(input, output, session) {
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$cal, {
    if (input$ad == "Present Value") {
      dis.fac <- (1 + input$j)
      vet.dis.fac <- rep(0, input$n)
      cash <- rep(input$cf, input$n)
      for (i in 1:input$n){
        vet.dis.fac[i] <- (dis.fac)^i
      }
      v$data <- sum(data.frame(cash/vet.dis.fac))  
    }
    
    if(input$ad == "Net Present Value") {
      dis.fac <- (1 + input$j)
      vet.dis.fac <- rep(0, input$n)
      cash <- rep(input$cf, input$n)
      for (i in 1:input$n){
        vet.dis.fac[i] <- (dis.fac)^i
      }
      v$data <- (input$c0 + sum(data.frame(cash/vet.dis.fac)))
      }
   
     })
  
    output$result <- renderText({
      paste(v$data)
    })
}

shinyApp(ui, server)
