## app.R ##
library(shiny)
library(shinydashboard)
library(shinyWidgets)
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
  set_names(distinct(turistas, id_aeropuerto, aeropuerto)$aeropuerto)

continentes_list <- unique(turistas$continentes)
  # turistas %>%
  # distinct(id_continentes, continentes) %>%
  # select(id_continentes) %>%
  # unlist() %>%
  # as.list() %>%
  # set_names(distinct(turistas, id_continentes, continentes)$continentes)

year_list <-
  turistas %>% distinct(year) %>% arrange(year) %>% unlist() %>% as.list() %>% set_names()

listado_meses = as.list(1:12) %>% set_names(distinct(turistas, meses, meses_numero)$meses)

header <- dashboardHeader(title = "Entrada de Turistas")

sidebar <- dashboardSidebar(
  pickerInput(
    inputId = "aeropuertos",
    label = "Selecciona un Aeropuerto",
    choices = aeropuerto_list,
    options = list(
      `actions-box` = TRUE,
      size = 10,
      `selected-text-format` = "count > 3"
    ),
    multiple = TRUE
  ),
  # pickerInput(
  #   inputId = "continentes",
  #   label = "Selecciona un continente",
  #   choices = continentes_list,
  #   selected = 1,
  #   multiple = TRUE,
  #   options = list(
  #     `actions-box` = TRUE,
  #     size = 10,
  #     `selected-text-format` = "count > 3"
  #   )
  # ),
  sliderTextInput(
    inputId = "year",
    label = "Year:",
    grid = TRUE,
    choices = year_list,
    selected = 2015:2016
  ),
  pickerInput(
    inputId = "month",
    label = "Mes:",
    choices = listado_meses,
    options = list(
      `actions-box` = TRUE,
      size = 10,
      `selected-text-format` = "count > 3"
    ),
    multiple = TRUE
  )
)
body <-
  dashboardBody(fluidRow(valueBoxOutput("valuebox_total_tour")),
                fluidRow(box(plotlyOutput("plot"),  box(uiOutput(
                  "back"
                ))),
                box(plotlyOutput("plot2"))))

ui <- dashboardPage(header,
                    sidebar,
                    body)

server <- function(input, output, session) {
  continente_activo <- reactiveVal()
  
  base_turistas_barras <- reactive({
    resultado <-
      turistas
    
    if (any(input$aeropuertos != 99)) {
      resultado <-
        turistas %>%
        filter(id_aeropuerto %in% input$aeropuertos)
    }
    
    if (!any(input$year == 99)) {
      resultado <-
        resultado %>% filter(year >= input$year[1], year <= input$year[2])
    }
    
    if (!any(input$month == 99)) {
      resultado <-
        resultado %>% filter(meses_numero %in% input$month)
    }
    
    if (!length(continente_activo())) {
      resultado <- resultado %>% 
        group_by(continentes) %>%
        summarize(cantidad = sum(cantidad)) %>%
        arrange(desc(cantidad)) %>%
        head(10)
    }
    else {
      resultado <- resultado %>%
        filter(continentes %in% continente_activo()) %>% 
        group_by(nacionalidad) %>%
        summarize(cantidad = sum(cantidad)) %>%
        arrange(desc(cantidad)) %>%
        head(10)
    }
    
    # resultado %>%
    #   group_by(nacionalidad) %>%
    #   summarize(cantidad = sum(cantidad)) %>%
    #   arrange(desc(cantidad)) %>%
    #   head(10)
  })
  
  output$plot <- renderPlotly({
    base_turistas_barras <-
      setNames(base_turistas_barras(), c("y", "x"))
    plot_ly(base_turistas_barras) %>%
      add_bars(
        x = ~ x,
        y = ~ reorder(y,x),
        name = "Cantidad de Turistas por Nacionalidad",
        orientation = "h",
        marker = list(
          color = 'rgba(50, 171, 96, 0.6)',
          line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1)
        )
      ) %>%
      layout(
        title = "Cantidad de Turistas" %||% continente_activo(),
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
  
  observe({
    cd <- event_data("plotly_click")$y
    if (isTRUE(cd %in% continentes_list))
      continente_activo(cd)
  })
  
  # populate back button if category is chosen
  output$back <- renderUI({
    if (length(continente_activo()))
      actionButton("clear", "Back", icon("chevron-left"))
  })
  
  # clear the chosen category on back button press
  observeEvent(input$clear, continente_activo(NULL))


base_turistas_lineas <- reactive({
  resultado <-
    turistas
  
  if (any(input$aeropuertos != 99)) {
    resultado <-
      turistas %>%
      filter(id_aeropuerto %in% input$aeropuertos)
  }
  
  if (!length(continente_activo())) {
    resultado <- resultado %>% mutate(nacionalidad = continentes)
  }
  else {
    resultado <- resultado %>%
      filter(continentes %in% continente_activo())
  }
  
  if (!any(input$year == 99)) {
    resultado <-
      resultado %>% filter(year >= input$year[1], year <= input$year[2])
  }
  
  if (!any(input$month == 99)) {
    resultado <-
      resultado %>% filter(meses_numero %in% input$month)
  }
  
  resultado %>%
    group_by(fecha) %>%
    summarize(cantidad = sum(cantidad))
})

output$plot2 <- renderPlotly({
  plot_ly(
    data = base_turistas_lineas(),
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
    base_turistas_barras() %>%
      summarize(scales::comma(sum(cantidad))) %>% unlist(),
    "Total de Turistas"
  )
})

}

shinyApp(ui, server)
