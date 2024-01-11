library(dplyr)
library(lubridate)
library(purrr)
library(ggplot2)
library(rvest)
library(stringr)
library(polite)


source("funciones.R")
# options(scipen = 99999)


#http://datosretc.mma.gob.cl/dataset

#descarga los archivos disponibles en el sitio de retc, que son un archivo por año, 
#y los guarda en la carpeta datos/scraping
#estos datos son desactualizados, pero sirven como datos históricos complementarios a los obtenidos
#por medio de transparencia



#obtener enlaces desde el sitio
url <- "http://datosretc.mma.gob.cl/dataset/generacion-municipal-de-residuos-no-peligrosos"

sitio <- session(url) |> 
  read_html()

#obtener links de cada pagina de residuos (hay 1 por año)
enlaces_sitios <- sitio |> 
  html_elements("a") |> 
  html_attr("href") |> 
  str_subset("generacion(_|-)municipal.*resource")

# http://datosretc.mma.gob.cl/dataset/generacion-municipal-de-residuos-no-peligrosos/resource/61347d33-6cbf-4f6a-9771-8b319b516ab8

# obtener enlaces de datos ----

#por cada pagina, ingresar y obtener el dataset descargable
enlaces_archivos <- map(enlaces_sitios, ~{
  # .x <- enlaces_sitios[[1]]
  enlace <- paste0("http://datosretc.mma.gob.cl", .x)
  
  message("scraping ", enlace)
  sitio_residuo <- session(enlace) |> 
    read_html()
  
  Sys.sleep(3)
  
  #archivo
  archivo <- sitio_residuo |> 
    html_elements("a") |> 
    html_attr("href") |> 
    str_subset("\\.csv|\\.xls")
  
  return(archivo)
})

enlaces_archivos

enlaces_archivos_2 <- enlaces_archivos |> unlist() |> unique()


# descargar ----
walk(enlaces_archivos, ~{
  # .x <- enlaces_archivos
  nombre_archivo <- str_extract(.x, "download\\/.*\\..*$") |> str_remove("download/")
  message("descargando ", nombre_archivo)
  
  download.file(.x, destfile = paste0("reciclaje/datos/scraping/", nombre_archivo))
  Sys.sleep(3)
})


#obtener rutas de archivos
archivos_scraping <- fs::dir_ls("reciclaje/datos/scraping/") |> str_subset("gm-sinader")

# cargar archivos ----
datos <- map(archivos_scraping, ~{
  message("cargando ", .x) 
.x = archivos_scraping[2]
  datos <- try(readr::read_csv2(.x, col_types = readr::cols(.default = "c")))
  
  # dado que algunos archivos vienen con otra codificación:
  if ("try-error" %in% class(datos)) {
    message("error, intentando otra codificación")
    datos <- readr::read_csv2(.x, col_types = readr::cols(.default = "c"), locale = readr::locale(encoding = "latin1"))
  }
  return(datos)
})


# limpiar datos ----
residuos <- datos |> 
  list_rbind() |> 
  select(año, cut_comuna = id_comuna, cantidad_toneladas,
         tratamiento = tratamiento_nivel_3) |> 
  #re-anexar comunas
  mutate(cut_comuna = as.numeric(cut_comuna)) |> 
  mutate(toneladas = readr::parse_number(cantidad_toneladas, locale = readr::locale(decimal_mark = ","))) |> 
  select(-cantidad_toneladas) |> 
  left_join(cargar_comunas(), join_by(cut_comuna)) |> 
  filter(!is.na(comuna)) |> 
  mutate(tipo_tratamiento = case_when(tratamiento %in% c("Compostaje", "Preparación para reutilización", "Reciclaje", "Valorización") | stringr::str_detect(tratamiento, "(R|r)ecicl") ~ "Valorización",
                                              .default = "Eliminación"))



# guardar ----
readr::write_csv(residuos, "reciclaje/datos/retc_residuos_reciclaje.csv")
