# Para obtener y limpiar data para formato bonito
# Je Je


# paquetes ----------------------------------------------------------------
library(janitor)
library(tidyverse)
library(lubridate)
library(tidylog)
library(data.table)
library(rbokeh)

# Descargar la data desde BCRD --------------------------------------------
anno = "2019"

tourist_url <-
  glue::glue("https://cdn.bancentral.gov.do/documents/estadisticas/sector-turismo/documents/lleg_nacionalidad_{anno}.xls")

p1f <- tempfile()
download.file(tourist_url, p1f, mode = "wb")

col_names <-
  c(
    "aeropuerto",
    # "del_1",
    # "del_2",
    "nacionalidad",
    "total",
    "enero",
    "febrero",
    "marzo",
    "abril",
    "mayo",
    "junio",
    "julio",
    "agosto",
    "septiembre",
    "octubre",
    "noviembre",
    "diciembre"
  )

tourist_xlsx <-
  readxl::read_excel(p1f,
                     sheet = 1,
                     skip = 3,
                     col_names = col_names)
# tourist_xlsx <- select(tourist_xlsx, -del_1, -del_2)

write.csv(tourist_xlsx, file = glue::glue("data_raw/llegada_{anno}.csv"), row.names = FALSE)

# Saber todas las filas que dicen aeropuerto y descartar todo lo que no se encuentre dentro
# De esos rangos
# Eliminamos todas las columnas y filas que esten en blanco (NA)
# Nos quedamos solo con los nombres de los paises y las distincion de residentes,
# No residentes, y extranjeros
# Buscamos forma de estandarizar los nombres de los aeropuertos
# Ya sea teniendo un archivo aparte con los nombres o creando codigos
# Damos formato a los meses, que tengan fecha numerica y alfabetica
# Luego descargamos los ultimos 5 year y generamos un documento con todo eso
# Y creamos aplicacion shiny dashboard, claro que hay que aprender a usarla

tourist_xlsx <- read_csv(file = glue::glue("data_raw/llegada_{anno}.csv"))

meses_num <- c(
  "enero" = 1,
  "febrero" = 2,
  "marzo" = 3,
  "abril" = 4,
  "mayo" = 5,
  "junio" = 6,
  "julio" = 7,
  "agosto" = 8,
  "septiembre" = 9,
  "octubre" = 10,
  "noviembre" = 11,
  "diciembre" = 12
)

tourist_xlsx <-
  remove_empty(tourist_xlsx, which = c("rows", "cols"))

row_to_extract <-
  str_which(tourist_xlsx$aeropuerto,
            "AEROPUERTO")

tourist_xlsx <- tourist_xlsx[row_to_extract[1]:nrow(tourist_xlsx), ]

row_to_elim <-
  str_which(tourist_xlsx$aeropuerto,
            "LLEGADA")

tourist_xlsx <- tourist_xlsx[-row_to_elim, ]

tourist_xlsx$residente <- tourist_xlsx$nacionalidad

indices <- grep(pattern = c("*RESIDENTE"),
                x = tourist_xlsx$residente)

tourist_xlsx[-indices, "residente"] <- NA
tourist_xlsx[indices, "nacionalidad"] <- NA

continentes <- c(
  "AMERICA DEL NORTE",
  "AMERICA CENTRAL Y EL CARIBE",
  "AMERICA DEL SUR",
  "ASIA",
  "EUROPA",
  "RESTO DEL MUNDO"
)

tourist_xlsx$continentes <- tourist_xlsx$nacionalidad
tourist_xlsx[!tourist_xlsx$continentes %in% continentes, "continentes"] <-
  NA

elimnar <-  c(
  "AMERICA DEL NORTE",
  "AMERICA CENTRAL Y EL CARIBE",
  "AMERICA DEL SUR",
  "ASIA",
  "EUROPA",
  "RESTO DEL MUNDO",
  "*Cifras sujetas a rectificación.",
  "NACIONALIDAD",
  "TOTAL"
)

# aeropuertos <-
#   tourist_casi_clean %>%
#   distinct(aeropuerto) %>%
#   filter(!is.na(aeropuerto)) %>%
#   mutate(aeropuerto = trimws(str_remove_all(aeropuerto, "[,0-9]")))

# aeropuertos

tourist_casi_clean <- tourist_xlsx %>%
  fill(residente, .direction = "down") %>%
  fill(aeropuerto,
       .direction = "down") %>%
  fill(continentes,
       .direction = "down") %>%
  filter(!is.na(nacionalidad),
         !is.na(residente),
         !nacionalidad %in% elimnar) %>%
  mutate(
    aeropuerto = trimws(str_remove_all(aeropuerto, "[,0-9]")),
    continentes = if_else(
      nacionalidad == "DOMINICANOS",
      "AMERICA CENTRAL Y EL CARIBE",
      continentes
    ),
    nacionalidad = if_else(
      nacionalidad == "DOMINICANOS",
      "Republica Dominicana",
      nacionalidad
    )
  ) %>%
  gather(key = meses,
         value = cantidad,
         -1:-2,
         -residente:-continentes) %>%
  filter(
    meses != "total",
    !is.na(cantidad),
    residente == "NO RESIDENTES",
    nacionalidad != "EXTRANJEROS"
  ) %>%
  mutate(
    meses_numero = meses_num[meses],
    year = as.numeric(anno),
    fecha = ymd(paste0(
      as.character(year),
      str_pad(
        as.character(meses_numero),
        width = 2,
        side = "left",
        pad = "0"
      ),
      "01"
    )),
    cantidad = as.numeric(cantidad)
  )
tourist_casi_clean
  
write_csv(tourist_casi_clean, glue::glue("data_clean/tourist_clean_{anno}.csv") )

# tourist_casi_clean$meses_num <- meses_num[tourist_casi_clean$meses]
skimr::skim(tourist_casi_clean)

tourist_casi_clean %>%
  group_by(fecha, continentes) %>%
  summarise(cantidad = sum(cantidad, na.rm = TRUE)) %>%
  ggplot(aes(x = fecha,
             y = cantidad,
             colour = continentes)) +
  geom_line()

setDT(tourist_casi_clean)
tourist_casi_clean[nacionalidad == "Republica Dominicana", sum(cantidad)]

# diamonds %>%
#   plot_ly(x = ~cut) %>%
#   add_histogram() %>%
#   group_by(cut) %>%
#   summarise(n = n()) %>%
#   add_text(
#     text = ~scales::comma(n), y = ~n,
#     textposition = "top middle",
#     cliponaxis = FALSE
#   )


tourist_casi_clean %>%
  mutate(fecha = ymd(fecha)) %>%
  group_by(continentes, fecha) %>%
  summarise(cantidad = sum(cantidad, na.rm = TRUE)) %>%
  plot_ly(x = ~ fecha,
          y = ~ cantidad) %>%
  add_lines(linetype = ~ continentes)

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

tourist_casi_clean[cantidad > 0, .(cantidad = sum(cantidad)), by = .(fecha, continentes, nacionalidad, aeropuerto)][order(-cantidad)]

# TODO Cantidad de visitantes total

tourist_casi_clean[cantidad > 0, .(cantidad = sum(cantidad))]

# TODO Poder filtrar por year, hay que agregar 2015 >


trace_0 <- rnorm(100, mean = 5)
trace_1 <- rnorm(100, mean = 0)
trace_2 <- rnorm(100, mean = -5)
x <- c(1:100)

data <- data.frame(x, trace_0, trace_1, trace_2)

p <-
  plot_ly(
    data,
    x = ~ x,
    y = ~ trace_0,
    name = 'trace 0',
    type = 'scatter',
    mode = 'lines'
  ) %>%
  add_trace(y = ~ trace_1,
            name = 'trace 1',
            mode = 'lines+markers') %>%
  add_trace(y = ~ trace_2,
            name = 'trace 2',
            mode = 'markers')

p


top5 <- txhousing %>%
  group_by(city) %>%
  summarise(m = mean(sales, na.rm = TRUE)) %>%
  arrange(desc(m)) %>%
  top_n(5)

tx5 <- semi_join(txhousing, top5, by = "city")

plot_ly(tx5, x = ~ date, y = ~ median) %>%
  add_lines(linetype = ~ city)
# Bokeh ####
pl <- figure() %>% ly_lines(tx5$date, tx5$median, color = tx5$city)
pl

p <- figure() %>%
  ly_lines(cars$speed, cars$dist) %>%
  ly_abline(-17.6, 3.9)
p

tourist_casi_clean %>%
  # filter(continentes %in% input$continentes) %>%
  group_by(aeropuerto, fecha) %>%
  summarise(cantidad = sum(cantidad, na.rm = TRUE))
2015:2019

map(2015:2019,limpiar_data)

turistass <- juntar_dataframes(list.files("data_clean"))

turistass %>% 
  distinct(aeropuerto)

as.list(1:12) %>% set_names(distinct(turistass, meses, meses_numero)$meses)

str_replace_all(turistass$aeropuerto, c("AEROPUERTO INTERNACIONAL" = "", "DE" = "") )

aeropuertos <- turistass %>% 
  distinct(aeropuerto) %>%
  unlist()

aeropuerto_lists <-
  as.list(1:length(aeropuertos)) %>% set_names(aeropuertos)
aeropuerto_lists$`Todos` <- 99

aeropuerto_lists["Todos"]

turistas %>%
  filter(aeropuerto %in% input$aeropuertos)

turistass %>% 
  distinct(id_continentes, continentes) %>% 
  select(id_continentes) %>%
  unlist() %>% 
  as.list() %>% 
  set_names( distinct(turistass, id_continentes, continentes)$continentes)

any()
