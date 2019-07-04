ui <- fluidPage(
  sidebarPanel("Valores necessários",
               numericInput("c11",
                         "Capital Inicial",
                         value = 100),
               numericInput("jur1",
                         "Taxa de juros ao ano",
                         value = 0.1),
               numericInput("per1",
                         "Período em anos",
                         value = 1),
              selectInput("ki", "Escolha o tipo de juros:",
                          list("Compostos", "Simples")),
               actionButton("res", "Recomeçar")
  ),
  mainPanel("Resultados",
            textOutput("jurs1"))
)

server <- function(input, output){
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$ki, {
    if (input$ki == "Compostos") {
           v$data <- paste("Juros:", (input$c11)*(1 + (input$jur1))^(input$per1))
    } else {
           v$data <- paste("Juros:", (input$c11) + (input$c11)*(input$jur1)*(input$per1))
    }
    })
  
  observeEvent(input$res, {
    v$data <- NULL
  })
  
  output$jurs1 <- renderText({
    if (is.null(v$data)) return()
    paste(v$data)
  })
}

shinyApp(ui, server)
