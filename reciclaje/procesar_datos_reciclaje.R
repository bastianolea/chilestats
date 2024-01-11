library(dplyr)
library(purrr)
library(lubridate)
source("capca_funciones.r")

#este archivo viene con algunos puntos de datos que están en unidades equivocadas, por ejemplo, en vez de 10 (toneladas), ponen 10000 (kilos),
#entonces hay qeu detectar datos outliers, y dividirlos por la magnitud de diferencia que tienen respecto de la media

#cargar el archivo recibido desde Transparencia
retc0 <- readxl::read_excel("residuos/datos/BBDD_generador_municipal_mensual.xlsx") |> 
  janitor::clean_names() |> 
  mutate(cantidad_toneladas = as.integer(cantidad_toneladas)) |> 
  mutate(fecha = ymd(paste(ano, mes, 1))) |> 
  left_join(isdt_cargar_cut_comunas() |> select(-region), join_by(comuna)) |> #filter(is.na(cut_comuna))
  arrange(fecha, region, comuna)

#dependiendo de si la base contiene la columna
if ("valorizacion_eliminacion" %in% names(retc0)) {
  retc1b <- retc0 |> 
    #ignorar tratamiento de residuos (dejar eliminacion/valorización)
    filter(valorizacion_eliminacion %in% c("Eliminación", "Valorización")) |>
    mutate(valorizacion_eliminacion = recode(valorizacion_eliminacion, "NULL" = "Eliminación"))
  
} else if ("tratamiento" %in% names(retc0)) {
  retc1b <- retc0 |> 
    mutate(valorizacion_eliminacion = case_when(tratamiento %in% c("Compostaje", "Preparación para reutilización", "Reciclaje", "Valorización") | stringr::str_detect(tratamiento, "Reciclaje") ~ "Valorización",
                                                .default = "Eliminación"))
}

retc1 <- retc1b |> 
  group_by(fecha, region, cut_region, comuna, cut_comuna, valorizacion_eliminacion) |> 
  summarize(toneladas = sum(cantidad_toneladas), .groups = "drop") |> 
  capca_corregir_comunas_faltantes() |> 
  filter(cut_comuna %in% cut_comunas_capca())

#resolver problema de unidades
retc <- retc1 |>   
  mutate(año = year(fecha)) |> 
  group_by(comuna, valorizacion_eliminacion, año) |> 
  #sacar 
  mutate(toneladas_efectivas = ifelse(toneladas == 0, NA, toneladas),
         toneladas_med_comuna = median(toneladas_efectivas, na.rm = T),
         toneladas_quin_comuna = quantile(toneladas_efectivas, probs = .6, na.rm = T)) |> #percentil 60%
  group_by(comuna, valorizacion_eliminacion) |> 
  #si el valor es 10 veces mayor que la media comunal, significa que está en miles
  mutate(problema_unidades = case_when(toneladas > toneladas_quin_comuna*10 ~ TRUE)) |> 
  #ver cuantas veces mas grande que la media es
  mutate(problema_cuanto = ifelse(problema_unidades == TRUE, toneladas/toneladas_quin_comuna, NA)) |> 
  #dividir el outlier por la magnitud de su diferencia (si el valor es 1000 veces mas grande que la media, dividir por 1000)
  mutate(toneladas_corregidas = case_when(problema_unidades == TRUE ~ toneladas/problema_cuanto, .default = toneladas)) |> 
  #lo mismo que lo anterior, pero por si son menores (Renca 13128 Eliminación 2022-02-01 5)
  mutate(problema_unidades_menor = case_when(toneladas < toneladas_quin_comuna/10 ~ TRUE)) |> 
  mutate(problema_menor_cuanto = ifelse(problema_unidades_menor == TRUE, toneladas_quin_comuna/toneladas, NA)) |> 
  mutate(toneladas_corregidas = case_when(problema_unidades_menor == TRUE ~ toneladas*problema_menor_cuanto, .default = toneladas_corregidas)) |> 
  ungroup() |> 
  #ordenar
  arrange(cut_region, cut_comuna, valorizacion_eliminacion, fecha)

# retc |> 
#   filter(comuna == "Renca", valorizacion_eliminacion == "Eliminación") |> select(comuna, fecha, toneladas, problema_unidades_menor, problema_menor_cuanto, toneladas_corregidas) |> print(n=Inf)

# # retc |> select(ano, mes, region, comuna,
# #                 valorizacion_eliminacion, tratamiento, cantidad_toneladas) |> 
# #   #filter(valorizacion_eliminacion == "Eliminación") |> 
# #   filter(comuna == "Panguipulli")
#   
# #visualizar problema de unidades
# retc |> 
#   filter(comuna == "Panguipulli") |> 
#   filter(valorizacion_eliminacion %in% c("Eliminación", "Valorización")) |> 
#   group_by(fecha, comuna, valorizacion_eliminacion) |> 
#   summarize(toneladas = sum(toneladas, na.rm = T)) |> 
#   ggplot(aes(fecha, toneladas, col = valorizacion_eliminacion)) +
#   geom_point()
# 
# retc |> 
#   filter(comuna == "Panguipulli") |> 
#   filter(valorizacion_eliminacion %in% c("Eliminación")) |> 
#   group_by(fecha, comuna, valorizacion_eliminacion) |> 
#   summarize(toneladas = sum(toneladas, na.rm = T)) |> 
#   ggplot(aes(fecha, toneladas, fill = valorizacion_eliminacion)) +
#   geom_col() +
#   geom_text(aes(label = toneladas), angle = 90, size = 3)
# #claramente es un problema de unidades
# 
# 
# # #resolver problema de unidades viendo si el valor es 10 veces maypr que la media, en cuyo caso, dividir por 1000
# # retc |> 
# #   select(fecha, comuna, valorizacion_eliminacion, cantidad_toneladas) |> 
# #   filter(comuna == "Panguipulli") |> 
# #   filter(valorizacion_eliminacion %in% c("Eliminación")) |> 
# #   arrange(fecha, valorizacion_eliminacion) |> 
# #   #group_by(fecha, comuna, valorizacion_eliminacion) |> 
# #   ungroup() |>
# #   # rowwise() |> 
# #   # mutate(cantidad_toneladas = case_when(cantidad_toneladas > lag(cantidad_toneladas)*7 ~ cantidad_toneladas/1000, 
# #   #                                       .default = cantidad_toneladas)) |> 
# #   group_by(comuna) |> 
# #   mutate(toneladas_med_comuna = median(cantidad_toneladas)) |> 
# #   ungroup() |> 
# #   mutate(unidades = case_when(cantidad_toneladas > toneladas_med_comuna*10 ~ TRUE)) |> 
# #   mutate(cantidad_toneladas_2 = case_when(unidades == TRUE ~ cantidad_toneladas/1000, .default = cantidad_toneladas)) |> 
# #   print(n=Inf)
# 
# 
# #revisar ----
# #comparar variable original de toneladas versus la corregida por unidades
# #por ejemplo, panguipulli, Lanco, Mafil
# #original
# retc |> 
#   filter(region == "Los Ríos", valorizacion_eliminacion == "Eliminación") |> 
#   ggplot(aes(comuna, toneladas)) +
#   geom_boxplot() + facet_wrap(~comuna, scales = "free")
# #corregida
# retc |> 
#   filter(region == "Los Ríos", valorizacion_eliminacion == "Eliminación") |> 
#   ggplot(aes(comuna, toneladas_corregidas)) +
#   geom_boxplot() + facet_wrap(~comuna, scales = "free")
# 
# #original
# retc |> 
#   filter(comuna == "Panguipulli") |> 
#   ggplot(aes(fecha, toneladas, col = valorizacion_eliminacion)) +
#   geom_point() +
#   ggrepel::geom_text_repel(aes(label = toneladas), size = 3)
# #corregida
# retc |> 
#   filter(comuna == "Panguipulli") |> 
#   ggplot(aes(fecha, toneladas_corregidas, col = valorizacion_eliminacion)) +
#   geom_point() +
#   ggrepel::geom_text_repel(aes(label = toneladas_corregidas), size = 3)



#guardar ----
if (!dir.exists("residuos/resultados")) dir.create("residuos/resultados")
arrow::write_parquet(retc, "residuos/resultados/residuos_solidos.parquet")



# # proporcionar ----

#los datos históricos vienen con una observación por año, por lo que la idea es expandir los datos anuales a mensuales, y darles el movimiento que tienen en la serie mensual

residuos_mensual <- arrow::read_parquet("residuos/resultados/residuos_solidos.parquet")
residuos_historico <- arrow::read_parquet("residuos/resultados/residuos_solidos_historicos.parquet")

# library(ggplot2)
# 
# residuos_mensual |>
#   isdt_filtrar_años_incompletos() |>
#   filter(comuna == "El Quisco") |>
#   ggplot(aes(fecha, toneladas_corregidas, col = valorizacion_eliminacion)) +
#   geom_line()
# 
# residuos_historico |>
#   filter(comuna == "El Quisco") |>
#   ggplot(aes(fecha, toneladas, col = valorizacion_eliminacion)) +
#   geom_line()

# 
# residuos_historico |>
#   filter(comuna == "El Quisco") |>
#   group_by(fecha, region, cut_region, comuna, cut_comuna, valorizacion_eliminacion) |>
#   summarize(toneladas = sum(toneladas)) |>
#   isdt_expandir_años(año_min = 2014, año_max = 2021) |>
#   tidyr::fill(everything(), .direction = "down") |>
#   print(n=100) |>
#   ggplot(aes(fecha, toneladas, col = valorizacion_eliminacion)) +
#   geom_line()


#mensualizar el registro histórico anual
residuos_historico_2 <- residuos_historico |>
  # filter(comuna == "Lo Prado") |>
  group_by(fecha, region, cut_region, comuna, cut_comuna, valorizacion_eliminacion) |>
  summarize(toneladas = sum(toneladas), .groups = "drop") |> #???
  group_by(fecha, region, cut_region, comuna, cut_comuna, valorizacion_eliminacion) |> 
  # capca_expandir_años(año_min = 2014, año_max = 2020) |>
  # tidyr::fill(everything(), .direction = "down") |> 
  mutate(toneladas = toneladas/12)

residuos_historico_2 |>
  filter(comuna == "Lo Prado")

#completar meses de los años
residuos_historico_mensual <- map_df(comunas_capca(), ~ {
  residuos_historico_2 |> 
    filter(comuna == .x) |>
    # filter(comuna == "Renca") |> 
    # capca_expandir_años(año_min = 2014, año_max = 2020) |> print(n=Inf)
    # group_by(comuna, valorizacion_eliminacion) |> 
    # tidyr::fill(everything(), .direction = "down")
    arrange(cut_region, cut_comuna, fecha) |> 
    group_by(comuna, cut_comuna, region, cut_region, valorizacion_eliminacion) |> 
    capca_completar_años() |> 
    tidyr::fill(toneladas, .direction = "down")
})

# residuos_historico_mensual |>
# filter(comuna == "Renca") |> 
#   print(n=Inf)
# 
# residuos_historico_mensual |>
#   filter(comuna == "Lo Prado") |> 
#   print(n=Inf)

residuos_solidos_unidos <- bind_rows(residuos_historico_mensual |> mutate(fuente = "historico"),
          residuos_mensual |> select(fecha, region, cut_region, comuna, cut_comuna, valorizacion_eliminacion, toneladas = toneladas_corregidas)  |> mutate(fuente = "mensual")
) |> 
  #eliminar datos del historico si existen en el mensual
  mutate(fuente = forcats::fct_relevel(fuente, "historico", "mensual")) |>
  group_by(comuna, cut_comuna, region, cut_region, valorizacion_eliminacion, fecha) |>
  slice_max(fuente)

# residuos_solidos_unidos |> 
#   filter(valorizacion_eliminacion == "Eliminación") |> 
#   filter(comuna == "Renca") |>
#   # filter(year(fecha) == 2021) |> 
#   # print(n=Inf)
#     ggplot(aes(fecha, toneladas, col = comuna)) +
#     geom_line()


if (!dir.exists("residuos/resultados")) dir.create("residuos/resultados")
arrow::write_parquet(residuos_solidos_unidos, "residuos/resultados/residuos_solidos_unidos.parquet")



# residuos_solidos_unidos |> 
#   filter(comuna == "Renca") |> 
#   filter(valorizacion_eliminacion == "Eliminación") |> 
#   print(n=Inf)

# residuos_solidos_unidos |>
#   filter(comuna == "Renca") |>
#   ggplot(aes(fecha, toneladas, col = valorizacion_eliminacion)) +
#   geom_line()

# #interpolar ---- 
# residuos_historico_interpolada <- residuos_historico_2 |> 
#   mutate(toneladas = case_when(month(fecha) != 1 ~ NA, .default = toneladas)) |> #dejar 1 solo dato por mes
#   mutate(toneladas = case_when(month(fecha) == 1 & is.na(toneladas) ~ 0, .default = toneladas)) |> #rellenar con cero si la comuna no tiene datos
#   arrange(cut_comuna, valorizacion_eliminacion, fecha) |> 
#     group_by(cut_comuna) %>%
#     mutate(time = seq(1, n())) %>%
#     mutate(toneladas_interpolada = approx(time, toneladas, time, method = "linear")$y) %>%
#     tidyr::fill(toneladas) |>
#     select(-time)
# 
# residuos_historico_interpolada |> 
#   mutate(toneladas = toneladas_interpolada) |> 
# bind_rows(residuos_mensual |> select(fecha, region, cut_region, comuna, cut_comuna, valorizacion_eliminacion, toneladas = toneladas_corregidas)
# ) |> 
#   filter(comuna == "Pirque") |>
#   ggplot(aes(fecha, toneladas, col = valorizacion_eliminacion)) +
#   geom_line()
