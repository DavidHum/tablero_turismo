limpiar_data <- function(anno = "2018") {
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
  
  tourist_xlsx <- readr::read_csv(file = glue::glue("data_raw/llegada_{anno}.csv"))
  
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
    janitor::remove_empty(tourist_xlsx, which = c("rows", "cols"))
  
  row_to_extract <-
    stringr::str_which(tourist_xlsx$aeropuerto,
              "AEROPUERTO")
  
  tourist_xlsx <- tourist_xlsx[row_to_extract[1]:nrow(tourist_xlsx), ]
  
  row_to_elim <-
    stringr::str_which(tourist_xlsx$aeropuerto,
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
  
  tourist_casi_clean <- tourist_xlsx %>%
    tidyr::fill(residente, .direction = "down") %>%
    tidyr::fill(aeropuerto,
         .direction = "down") %>%
    tidyr::fill(continentes,
         .direction = "down") %>%
    dplyr::filter(!is.na(nacionalidad),
           !is.na(residente),
           !nacionalidad %in% elimnar) %>%
    dplyr::mutate(
      aeropuerto = trimws(stringr::str_remove_all(aeropuerto, "[,0-9]")),
      continentes = dplyr::if_else(
        nacionalidad == "DOMINICANOS",
        "AMERICA CENTRAL Y EL CARIBE",
        continentes
      ),
      nacionalidad = dplyr::if_else(
        nacionalidad == "DOMINICANOS",
        "Republica Dominicana",
        nacionalidad
      )
    ) %>%
    tidyr::gather(key = meses,
           value = cantidad,
           -1:-2,
           -residente:-continentes) %>%
    dplyr::filter(
      meses != "total",
      !is.na(cantidad),
      residente == "NO RESIDENTES",
      nacionalidad != "EXTRANJEROS"
    ) %>%
    dplyr::mutate(
      meses_numero = meses_num[meses],
      year = as.numeric(anno),
      fecha = lubridate::ymd(paste0(
        as.character(year),
        stringr::str_pad(
          as.character(meses_numero),
          width = 2,
          side = "left",
          pad = "0"
        ),
        "01"
      )),
      cantidad = as.numeric(cantidad),
      aeropuerto = str_trim(str_replace_all(aeropuerto, c("AEROPUERTO INTERNACIONAL" = "", "DE" = "") ), side = "both"),
      aeropuerto = str_to_sentence(aeropuerto),
      continentes = str_to_sentence(continentes),
      meses = str_to_sentence(meses),
      id_aeropuerto = group_indices(., aeropuerto),
      id_continentes = group_indices(., continentes)
    )
  
  readr::write_csv(tourist_casi_clean, glue::glue("data_clean/tourist_clean_{anno}.csv") )
}

juntar_dataframes <- function(listado_archivos){
  
  turistas <- map_dfr(listado_archivos, function(x) read_csv(file = paste("data_clean", x, sep = "/")) )
  
  turistas
}