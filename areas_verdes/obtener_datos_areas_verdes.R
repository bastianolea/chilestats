library(sinimr)
library(dplyr)

#hay una función mala en el paquete sinimr, que no permite que se descarguen datos de 2022, por lo que hay que hacer esto apra agregar el año
fixInNamespace(getyear, pos="package:sinimr") #agregar 2022 y 2023

get_sinim_vars(376)

# Superficie Total (m2) de Plazas Existentes en la Comuna (a contar del 2010) MTS² 4037
superficie_plazas <- get_sinim(4037, 2019:2022, moncorr = FALSE, truevalue = FALSE)

# Superficie Total (m2) de Parques Urbanos Existentes en la Comuna (a contar del 2010) MTS² 4039
superficie_parques <- get_sinim(4039, 2019:2022, moncorr = FALSE, truevalue = FALSE)

# Superficie en M2 de Areas Verdes con Mantenimiento MTS²
superficie_areas_verdes <- get_sinim(512, 2019:2022, moncorr = FALSE, truevalue = FALSE)

# limpiar 
superficie_plazas_2 <- superficie_plazas |> as_tibble() |> rename(cut_comuna = code, comuna = municipality, año = year, valor = value)
superficie_parques_2 <- superficie_parques |> as_tibble() |> rename(cut_comuna = code, comuna = municipality, año = year, valor = value) 
superficie_areas_verdes_2 <- superficie_areas_verdes |> as_tibble() |> rename(cut_comuna = code, comuna = municipality, año = year, valor = value)

#guardar datos brutos
readr::write_csv2(superficie_plazas_2, "areas_verdes/datos/sinim_areas_verdes_superficie_plazas.csv")
readr::write_csv2(superficie_parques_2, "areas_verdes/datos/sinim_areas_verdes_superficie_parques.csv")
readr::write_csv2(superficie_areas_verdes_2, "areas_verdes/datos/sinim_areas_verdes_superficie_mantenida.csv")
