#UI WITH MULTIPLE TABS

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id = "tabset",
                  tabPanel("Valor futuro",
                           textInput("c11",
                                     "Capital Inicial",
                                     value = 100),
                           textInput("jur1",
                                     "Taxa de juros ao ano",
                                     value = 0.1),
                           textInput("per1",
                                     "Período em anos",
                                     value = 1),
                           actionButton("com1", "Juros Compostos"),
                           actionButton("sim1", "Juros Simples"),
                           actionButton("res", "Recomeçar")
                  ),
                  tabPanel("Taxa de juros",
                           numericInput("c12",
                                        "Capital Inicial",
                                        value = 100),
                           numericInput("vf2",
                                        "Valor Futuro",
                                        value = 110),
                           numericInput("per2",
                                        "Período em anos",
                                        value = 1),
                           actionButton("com2", "Calcular Juros Compostos"),
                           actionButton("sim2", "Calcular Juros Simples"),
                           actionButton("res", "Recomeçar")
                  )
      ),
      actionButton("go", "Plot")
    ),
    mainPanel(
      verbatimTextOutput("resultado")
    )
  )
)