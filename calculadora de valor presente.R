ui <- fluidPage(
  sidebarPanel(
    "Valores necessários",
    numericInput("vf", "Valor futuro",
                 value = 110),
    numericInput("jur", "Taxa de Juros",
                 value = 0.1),
    numericInput("per", "Período",
                 value = 1),
    selectInput("ki", "Escolha o tipo de Juros:",
                list("--", "Compostos", "Simples"))
  ),
  mainPanel(
    verbatimTextOutput("resultado")
  )
)

server <- function(input, output) {
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$ki, {
    if (input$ki == "Compostos") {
      v$data <- paste(input$vf/((1+input$jur)^input$per))
    } else {
      v$data <- paste((input$vf/(1 + (input$jur)*(input$per)))
    }
  })
  
  output$resultado <- renderText({
    paste(v$data)
  })
}

shinyApp(ui, server)