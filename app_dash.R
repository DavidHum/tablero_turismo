## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(data.table)
library(plotly)

source('funciones.R')

if (!exists("turistas")) {
  turistas <- juntar_dataframes(list.files("data_clean"))
}

aeropuerto_list <-
  turistas %>% 
  distinct(id_aeropuerto, aeropuerto) %>% 
  select(id_aeropuerto) %>%
  unlist() %>% 
  as.list() %>% 
  set_names( distinct(turistas, id_aeropuerto, aeropuerto)$aeropuerto)

aeropuerto_list$`Todos` <- 99

continentes_list <-
  turistas %>% 
  distinct(id_continentes, continentes) %>% 
  select(id_continentes) %>%
  unlist() %>% 
  as.list() %>% 
  set_names( distinct(turistas, id_continentes, continentes)$continentes)

continentes_list$`Todos` <- 99

year_list <-
  turistas %>% distinct(year) %>% arrange(year) %>% unlist() %>% as.list() %>% set_names()

year_list$`Todos` <- 99

listado_meses = as.list(1:12) %>% set_names(distinct(turistas, meses, meses_numero)$meses)

listado_meses$`Todos` <- 99

header <- dashboardHeader(title = "Entrada de Turistas")

sidebar <- dashboardSidebar(
  selectInput(
    inputId = "aeropuertos",
    label = "Selecciona un Aeropuerto",
    choices = aeropuerto_list,
    selected = 99,
    selectize = FALSE,
    multiple = TRUE
  ),
  selectInput(
    inputId = "continentes",
    label = "Selecciona un continente",
    choices = continentes_list,
    selected = 99,
    multiple = TRUE,
    selectize = FALSE
  ),
  selectInput(
    inputId = "year",
    label = "Year:",
    choices = year_list,
    selected = 99,
    size = 4,
    selectize = FALSE
  ),
  selectInput(
    inputId = "month",
    label = "Mes:",
    choices = listado_meses,
    selected = 99,
    size = 13,
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
      turistas
    
    if (any(input$aeropuertos != 99)) {
      resultado <-
        turistas %>%
        filter(id_aeropuerto %in% input$aeropuertos)
    }
    
    if (any(input$continentes != 99)) {
      resultado <-
        resultado %>% filter(id_continentes %in% input$continentes)
    }
    
    if (input$year != 99) {
      resultado <-
        resultado %>% filter(year == input$year)
    }
    
    if (input$month != 99) {
      resultado <-
        resultado %>% filter(meses_numero == input$month)
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
    valueBox(base_turistas() %>%
               summarize(scales::comma(sum(cantidad))) %>% unlist(),
             "Total de Turistas")
  })
  
}

shinyApp(ui, server)