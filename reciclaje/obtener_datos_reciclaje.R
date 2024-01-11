library(dplyr)
library(lubridate)
library(stringr)
library(purrr)
library(ggplot2)
source("capca_funciones.r")
options(scipen = 99999)


#http://datosretc.mma.gob.cl/dataset

#descarga los archivos disponibles en el sitio de retc, que son un archivo por año, 
#y los guarda en la carpeta datos/scraping
#estos datos son desactualizados, pero sirven como datos históricos complementarios a los obtenidos
#por medio de transparencia

library(rvest)
library(stringr)
library(purrr)
library(polite)

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

#descargar todos los archivos
if (!dir.exists("residuos/datos")) dir.create("residuos/datos")
if (!dir.exists("residuos/datos/scraping/")) dir.create("residuos/datos/scraping/")

walk(enlaces_archivos |> unlist() |> unique(), ~{
  # .x <- enlaces_archivos
  nombre_archivo <- str_extract(.x, "download\\/.*\\..*$") |> str_remove("download/")
  message("descargando ", nombre_archivo)
  
  download.file(.x, destfile = paste0("residuos/datos/scraping/", nombre_archivo))
  Sys.sleep(5)
})



#unir ----

#obtener rutas de archivos
archivos_scraping <- fs::dir_ls("residuos/datos/scraping/") |> str_subset("gm-sinader")

#cargar archivos
datos <- map(archivos_scraping, readr::read_csv2)

#limpiar datos
residuos_historico <- datos |> 
  list_rbind() |> 
  select(año, cut_comuna = id_comuna, cantidad_toneladas,
         tratamiento = tratamiento_nivel_3) |> 
  #re-anexar comunas
  mutate(cut_comuna = as.character(cut_comuna)) |> 
  left_join(isdt_cargar_cut_comunas(), join_by(cut_comuna)) |> 
  filter(!is.na(comuna)) |> 
  mutate(valorizacion_eliminacion = case_when(tratamiento %in% c("Compostaje", "Preparación para reutilización", "Reciclaje", "Valorización") | stringr::str_detect(tratamiento, "Reciclaje") ~ "Valorización",
                                              .default = "Eliminación"))

# residuos_historico |> 
#   count(tratamiento) |> print(n=Inf)
# 
# residuos_historico |> count(valorizacion_eliminacion)

unique(residuos_historico$año)

residuos_historico_2 <- residuos_historico |> 
  mutate(fecha = lubridate::ymd(paste(año, "01", "01"))) |> 
  select(fecha, cut_region, region, cut_comuna, comuna, 
         tratamiento, valorizacion_eliminacion,
         toneladas = cantidad_toneladas) |> 
  filter(cut_comuna %in% cut_comunas_capca())

# guardar ----
arrow::write_parquet(residuos_historico_2, "residuos/resultados/residuos_solidos_historicos.parquet")
