library(shiny)

ui <- fluidPage(
  sidebarPanel(
    numericInput("cf", "Constant Cash Flow", value = 100),
    numericInput("n", "Number of entries",  value = 5),
    numericInput("j", "Interest", value = 0.1)),
  mainPanel(
    verbatimTextOutput("resultadopv")
  )
)

server <- function(input, output) {
 
    output$resultadopv <- renderText({
    dis.fac <- (1 + input$j)
    vet.dis.fac <- rep(0, input$n)
    cash <- rep(input$cf, input$n)

     for (i in 1:input$n){
    vet.dis.fac[i] <- (dis.fac)^i
                          }
    paste("Present Value:", sum(data.frame(cash/vet.dis.fac)))
    
    })
}

shinyApp(ui, server)



  