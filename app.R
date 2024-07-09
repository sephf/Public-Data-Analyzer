#paquetes necesarios

#install.packages("shiny")
#install.packages("ggplot2")
#install.packages("data.table")
#install.packages("dplyr")
#install.packages("readxl")

library(shiny)
library(ggplot2)
library(data.table)
library(dplyr)
library(readxl)

#límite de tamaño de carga de archivos a 1 GB
options(shiny.maxRequestSize = 1024^3)

ui <- fluidPage(
  titlePanel("Análisis de Datos de Entidad Pública"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Cargar Datos", accept = c(".csv",".xls",".xlsx")),
      uiOutput("variable_ui"),
      actionButton("analyze", "Analizar")
    ),
    mainPanel(
      plotOutput("plot"),
      tableOutput("summary")
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file1)
    if (endsWith(input$file1$name, ".csv")) {
      df <- fread(input$file1$datapath)
    } else if (endsWith(input$file1$name, ".xls") || endsWith(input$file1$name, ".xlsx")) {
      df <- read_excel(input$file1$datapath)
    } else {
      return(NULL)
    }
    
    updateSelectInput(session, "variable", choices = names(df))
    return(df)
  })
  
  output$variable_ui <- renderUI({
    selectInput("variable", "Selecciona la Variable", choices = NULL)
  })
  
  observe({
    req(data())
    updateSelectInput(session, "variable", choices = names(data()))
  })
  
  observeEvent(input$analyze, {
    req(input$variable)
    selected_data <- data()[[input$variable]]
   
     output$plot <- renderPlot({
      req(input$variable, data())
      selected_data <- data()[[input$variable]]
      
      if (is.numeric(selected_data)) {
        ggplot(data(), aes_string(x = input$variable)) + 
          geom_histogram(binwidth = 10) +
          theme_minimal()
      } else {
        ggplot(data()) +
          geom_bar(aes_string(x = input$variable)) +
          theme_minimal()
      }
    })
    
    
    output$summary <- renderTable({
      if (is.numeric(selected_data)) {
        as.data.frame(t(summary(selected_data)))
      } else {
        as.data.frame(table(selected_data))
      }
    })
  })
}

shinyApp(ui = ui, server = server)