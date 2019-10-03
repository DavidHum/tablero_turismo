## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(data.table)
library(plotly)

if (!exists("turistas")) {
  turistas <-
    data.table::fread("data_clean/tourist_clean_2018.csv")
}

turistas <-
  data.table::fread("data_clean/tourist_clean_2018.csv")

aeropuerto_list <- 
  unique(turistas$aeropuerto)


header <- dashboardHeader(title = "Entrada de Turistas")
sidebar <- dashboardSidebar(
  selectizeInput(
    inputId = "aeropuertos",
    label = "Selecciona un Aeropuerto",
    choices = aeropuerto_list,
    selected = "AEROPUERTO INTERNACIONAL DE LAS AMERICAS",
    multiple = TRUE
  ),
  selectInput(
    inputId = "continentes",
    label = "Selecciona un continente",
    choices = unique(turistas$continentes),
    selected = "AMERICA CENTRAL Y EL CARIBE",
    multiple = TRUE,
    selectize = FALSE
  )
)
body <-
  dashboardBody(fluidRow(valueBoxOutput("valuebox_total_tour")),
                fluidRow(box(plotlyOutput("plot")),
                         box(plotlyOutput("plot2"))))

ui <- dashboardPage(header,
                    sidebar,
                    body)

server <- function(input, output, session) {
  base_turistas <- reactive({
    resultado <-
      turistas %>%
      filter(aeropuerto %in% input$aeropuertos)
    
    if (!is_null(input$continentes)) {
      resultado <-
        resultado %>% filter(continentes %in% input$continentes)
    }
    resultado
  })

  output$plot <- renderPlotly({
    data_top10_nacionalidad <-
      base_turistas() %>%
      group_by(nacionalidad) %>%
      summarize(cantidad = sum(cantidad)) %>%
      arrange(desc(cantidad)) %>%
      head(10)
    
    # data_barras[, .(cantidad = sum(cantidad)),
    #             by = nacionalidad][order(-cantidad)] %>% head(10)
    data_top10_nacionalidad %>%
      plot_ly(
        x = ~ cantidad,
        y = ~ reorder(nacionalidad, cantidad),
        name = "Cantidad de Turistas por Nacionalidad",
        type = "bar",
        orientation = "h",
        marker = list(
          color = 'rgba(50, 171, 96, 0.6)',
          line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1)
        )
      ) %>%
      layout(
        title = "Top 10 Nacionalidad Turistas",
        yaxis = list(
          showgrid = FALSE,
          showline = FALSE,
          showticklabels = TRUE
        ),
        xaxis = list(
          zeroline = FALSE,
          showline = FALSE,
          showticklabels = TRUE,
          showgrid = TRUE
        )
      )
    
    
  })
  output$plot2 <- renderPlotly({
    data_lineas_fecha <- base_turistas() %>%
      group_by(fecha) %>%
      summarize(cantidad = sum(cantidad))
    
    plot_ly(
      data = data_lineas_fecha,
      x = ~ fecha,
      y = ~ cantidad,
      name = "Por donde entran los turistas?",
      type = "scatter",
      mode = "lines+markers"
    ) %>%
      layout(
        title = "Cantidad de turistas por Mes",
        yaxis = list(
          showgrid = FALSE,
          showline = FALSE,
          showticklabels = TRUE
        ),
        xaxis = list(
          zeroline = FALSE,
          showline = FALSE,
          showticklabels = TRUE,
          showgrid = TRUE
        )
      )
  })
  output$valuebox_total_tour <- renderValueBox({
    valueBox(
      base_turistas() %>%
        summarize(scales::comma(sum(cantidad))) %>% unlist(),
      "Total de Turistas"
    )
  })
  
}

shinyApp(ui, server)