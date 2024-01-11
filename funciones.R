cargar_comunas <- function() readr::read_csv2("comunas_cut_chile.csv", col_types = c("i", "c", "i", "c"))

