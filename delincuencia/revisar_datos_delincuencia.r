library(dplyr)
library(ggplot2)

delincuencia <- arrow::read_parquet("delincuencia/datos/cead_delincuencia.parquet")

delincuencia |> 
  filter(comuna == "Estación Central")
