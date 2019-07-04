library(shiny)
ui <- fluidPage(
  sidebarPanel("Valores necessários",
               numericInput("c12",
                            "Capital Inicial",
                            value = 100),
               numericInput("vf2",
                            "Valor Futuro",
                            value = 110),
               numericInput("per2",
                            "Período em anos",
                            value = 1),
               selectInput("ki2", "Escolha o tipo de juros:",
                           list("--", "Compostos", "Simples")),
               actionButton("res", "Recomeçar")
  ),
  mainPanel("Resultados",
            verbatimTextOutput("jurs2"))
)

server <- function(input, output){
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$ki2, {
    if (input$ki2 == "Compostos") {
     v$data <-  paste("Juros:", (input$vf2/input$c12)^(1/input$per2) - 1)
    } else {
     v$data <- paste("Juros:", ((input$vf2 - input$c12)/input$c12)/input$per2)
    }
  })
  observeEvent(input$res, {
    v$data <- NULL
  })
  
  output$jurs2 <- renderText({
    if (is.null(v$data)) return()
    paste(v$data)
  })
}

shinyApp(ui, server) 
