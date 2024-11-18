###MASTERFILE###

#Cargar paquetes
pacman::p_load(tidyverse, haven, summarytools,quarto)


#3_armado_bbdd---------------------------------------------------------------
source("Scripts/3_armado_bbdd.R")

#4_pivot
source("Scripts/4_pivot.R")

#5_analisis--------------------------------------------------------------------
source("Scripts/5_analisis.R")

#6 Renderizar Reporte Quarto
quarto::quarto_render("Quarto/Reporte.qmd")


