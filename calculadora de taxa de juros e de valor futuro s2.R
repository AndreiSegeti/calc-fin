library(shinythemes)
library(shiny)
ui <- fluidPage(
  theme = shinytheme("slate"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id = "tabset",
               tabPanel("Valor Futuro",
                  numericInput("c11",
                            "Capital Inicial",
                            value = 100),
                  numericInput("jur1",
                            "Taxa de juros ao ano",
                            value = 0.1),
                  numericInput("per1",
                            "Período em anos",
                            value = 1)
               ),
               tabPanel("Juros",
                  numericInput("c12",
                            "Capital Inicial",
                            value = 100),
                  numericInput("vf2",
                            "Valor Futuro",
                            value = 110),
                  numericInput("per2",
                            "Período em anos",
                            value = 1)
               )
    ),
    selectInput("ki", "Tipo de Juros",
                list("--","Compostos", "Simples"))
  ),
  mainPanel(
    verbatimTextOutput("result")
     )
  )
)
server <- function(input, output){
  v <- reactiveValues(doText = FALSE)
  
   observeEvent(input$ki, {
    v$doText <- input$ki
  })
 
   observeEvent(input$tabset, {
    v$doText <- FALSE
  })
   
   
   
   output$result <- renderText({
     if (v$doText == FALSE) return()
     isolate({
       data <- if(input$tabset == "Valor Futuro") {
         if(input$ki == "Compostos"){
           v$data <- paste("Juros:", (input$c11)*(1 + (input$jur1))^(input$per1))
         } else {
           v$data <- paste("Juros:", (input$c11) + (input$c11)*(input$jur1)*(input$per1))
         }
       } else {
         if(input$ki == "Compostos"){
           v$data <- paste("Juros:", (input$vf2/input$c12)^(1/input$per2) - 1)
         } else {
           v$data <- paste("Juros:", ((input$vf2 - input$c12)/input$c12)/input$per2)
         }
       }
       paste(data)
     })
   })
}

shinyApp(ui, server)
