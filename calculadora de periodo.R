library(shiny)
 ui <- fluidPage(
   sidebarPanel(
      "Valores necessários",
      numericInput("c13", "Capital inicial",
                   value = 100),
      numericInput("vf", "Valor futuro",
                   value = 110),
      numericInput("jur", "Taxa de Juros",
                   value = 0.1),
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
       v$data <- paste(log(input$vf/input$c13)/log(1+input$jur))
     } else {
       v$data <- paste((input$vf - input$c13)/(input$jur*input$c13))
     }
   })
   
   output$resultado <- renderText({
     paste(v$data)
   })
 }
 
 shinyApp(ui, server)
 