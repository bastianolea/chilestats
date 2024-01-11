delincuencia <- arrow::read_parquet("delincuencia/resultados/delincuencia_2010-2023.parquet")


delincuencia |> 
  filter(comuna == "Estación Central")
