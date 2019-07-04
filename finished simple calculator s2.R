library(shinythemes)
library(shiny)

ui <- fluidPage(
#Adiciona um tema à página.
theme = shinytheme("slate"),
  #Adiciona o Layout da barra lateral (permite multiplas tabs)
  sidebarLayout(
   sidebarPanel(
    #Aqui se define o tabset e suas subsessões
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
                        value = 1)),
       tabPanel("Juros",
        numericInput("c12",
                      "Capital Inicial",
                       value = 100),
         numericInput("vf2",
                       "Valor Futuro",
                         value = 110),
          numericInput("per2",
                        "Período em anos",
                          value = 1)),
         tabPanel("Período",
          numericInput("c13",
                        "Capital Inicial",
                          value = 100),
           numericInput("vf3",
                         "Valor Futuro",
                           value = 110),
            numericInput("jur3",
                          "Taxa de Juros",
                           value = 0.1)),
         tabPanel("Capital Inicial",
           numericInput("vf4",
                         "Valor Futuro",
                          value = 110),
            numericInput("jur4",
                          "Taxa de Juros",
                           value = 0.1),
            numericInput("per4",
                        "Período em anos",
                        value = 1))
      ), #tabset
      selectInput("ki", "Tipo de Juros",
       list("--","Compostos", "Simples"))
   ),#sidebarlayout
        mainPanel(
          verbatimTextOutput("resultado")
        ) 
     ) #sidebarlayout    
)#fluidpage
server <- function(input, output){
  v <- reactiveValues(doText = FALSE)
  
  observeEvent(input$ki, {
    v$doText <- input$ki
  })
  
  observeEvent(input$tabset, {
    v$doText <- FALSE
  })

  output$resultado <- renderText({
    if (v$doText == FALSE) return()
    isolate({
      data <- if(input$tabset == "Valor Futuro") {
        if(input$ki == "Compostos"){
          v$data <- paste("Valor Futuro:", (input$c11)*(1 + (input$jur1))^(input$per1))
        } else {
          v$data <- paste("Valor Futuro:", (input$c11) + (input$c11)*(input$jur1)*(input$per1))
        }
      }
      
      data <-  if(input$tabset == "Juros") {
        if(input$ki == "Compostos") {
          v$data <- paste("Juros:", (input$vf2/input$c12)^(1/input$per2) - 1)
        } else {
          v$data <- paste("Juros:", ((input$vf2 - input$c12)/input$c12)/input$per2)
        }
      }
        
      data <- if(input$tabset == "Período") {
        if(input$ki == "Compostos") {
          v$data <- paste("Período:", log(input$vf3/input$c13)/log(1+input$jur3))
        } else {
          v$data <- paste("Período:", (input$vf3 - input$c13)/(input$jur3*input$c13))
        }
      }
      
      data <- if(input$tabset == "Capital Inicial") {
        if(input$ki == "Compostos") {
          v$data <- paste("Valor Presente:", input$vf4/((1+input$jur4)^input$per4))
        } else {
          v$data <- paste("Valor Presente:", (input$vf4/(1 + (input$jur4)*(input$per4))))
        }
      }
      
      paste(v$data)
      
    })
  })
}

shinyApp(ui, server)
