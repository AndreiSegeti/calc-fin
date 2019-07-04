library(shiny)
ui <- fluidPage(
  sidebarPanel("Valores necessários",
               textInput("c1",
                         "Capital Inicial",
                         value = 100),
               textInput("jur",
                         "Taxa de juros ao ano",
                         value = 0.1),
               textInput("per",
                         "Período em anos",
                         value = 1),
               actionButton("com", "Juros Compostos"),
               actionButton("sim", "Juros Simples"),
               actionButton("res", "Recomeçar")
               ),
  mainPanel("Resultados",
            verbatimTextOutput("jurs"))
)

server <- function(input, output){
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$sim, {
    v$data <- paste("Juros:", as.numeric(input$c1) + as.numeric(input$c1)*as.numeric(input$jur)*as.numeric(input$per))
  })
  
  observeEvent(input$com, {
    v$data <- paste("Juros:", as.numeric(input$c1)*(1 + as.numeric(input$jur))^as.numeric(input$per))
  }) 
  
  observeEvent(input$res, {
    v$data <- NULL
  })
  
  output$jurs <- renderText({
    if (is.null(v$data)) return()
    paste(v$data)
  })
}

shinyApp(ui, server)
