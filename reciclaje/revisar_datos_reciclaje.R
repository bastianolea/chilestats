library(dplyr)
library(lubridate)

reciclaje <- readr::read_csv("reciclaje/datos/retc_residuos_reciclaje.csv")

reciclaje

reciclaje |> 
  filter(comuna == "Buin", 
         año == 2015)
