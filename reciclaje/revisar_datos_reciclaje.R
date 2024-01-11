library(dplyr)
library(lubridate)

residuos <- arrow::read_parquet("residuos/resultados/residuos_solidos_unidos.parquet")

residuos

residuos |> 
  filter(comuna == "Renca", 
         year(fecha) == 2021)
