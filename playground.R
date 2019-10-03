library(janitor)
library(tidyverse)
library(lubridate)
library(tidylog)
library(data.table)
library(rbokeh)


tourist_casi_clean %>%
  filter(aeropuerto %in% input$aeropuertos) %>%
  group_by(continentes, fecha) %>%
  summarise(cantidad = sum(cantidad, na.rm = TRUE)) %>%
  plot_ly(
    x = ~ fecha,
    y = ~ cantidad,
    type = 'scatter',
    mode = "lines+markers",
    color = ~ continentes
  )

tourist_casi_clean %>%
  filter(continentes %in% input$continentes) %>%
  group_by(aeropuerto, fecha) %>%
  summarise(cantidad = sum(cantidad, na.rm = TRUE)) %>% 
  plot_ly(
    data = .,
    x = ~ fecha,
    y = ~ cantidad,
    type = 'scatter',
    mode = "lines+markers",
    color = ~ aeropuerto
  )

# Preguntas ------
tourist_casi_clean <-
  fread("data_clean/tourist_clean_2018.csv", sep = ",")

tourist_casi_clean[cantidad > 0, .(cantidad = sum(cantidad)), by = c("fecha", "continentes")][order(-cantidad)] %>%
  plot_ly(x = ~ fecha,
          y = ~ cantidad) %>%
  add_lines(linetype = ~ continentes, color = ~ continentes)

# TODO Pais de donde mas se reciben visitantes ------

tourist_casi_clean[cantidad > 0, .(cantidad = sum(cantidad)), by = nacionalidad][order(-cantidad)] %>% head(5) %>%
  plot_ly(
    x = ~ cantidad,
    y = ~ reorder(nacionalidad, cantidad),
    type = "bar",
    orientation = "h",
    marker = list(
      color = 'rgba(50, 171, 96, 0.6)',
      line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1)
    )
  ) %>%
  layout(
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

# TODO Aeropuertos con mas visitantes

tourist_casi_clean[cantidad > 0, .(cantidad = sum(cantidad)), by = aeropuerto][order(-cantidad)]

# TODO Meses con mas visitantes y hacer drill down por continente y aeropuerto

tourist_casi_clean[cantidad > 0, .(cantidad = sum(cantidad)), by = .(fecha)] %>%
  plot_ly(
    x = ~ ymd(fecha),
    y = ~ cantidad,
    type = "scatter",
    mode = "lines+markers",
    color = "Dark2"
  )

# TODO Cantidad de visitantes total

tourist_casi_clean[cantidad > 0, .(cantidad = sum(cantidad))]

# TODO Poder filtrar por year, hay que agregar 2015 >