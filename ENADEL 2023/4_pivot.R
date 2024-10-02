WENA WENA REPOSITORIO
 wenaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
 
 mmmmmmvale
# --------------------------- 
# script: 4_pivot.R
#
# objetivo: Mostrar la transformación de wide a long y visceversa
#
# autora: antoniaaguileracarrasco@gmail.com
#
# fecha creación: Julio 2024
# ---------------------------
# Notas:
#   
#
# ---------------------------
# Paquetes: 
paquetes <- c("tidyverse", "dplyr", "plyr", "knitr", "haven", "readxl", "Hmisc")
#invisible(lapply(paquetes, install.packages, character.only = TRUE))
invisible(lapply(paquetes, library, character.only = TRUE))
# ---------------------------

# ------------------------------------------------------------------------------------ #
# ----------------------------------- WIDE TO LONG ----------------------------------- #
# ------------------------------------------------------------------------------------ #
enadel_wide <- read_dta("data/clean/enadel_wide.dta")
# 5.820 filas y 881 columnas

wide_unicas <- enadel_wide |> 
  select(folio, fact_emp, sector, region_trab, tam_trab, tam_vent, trabtot, trabval, tam_val,
         p0, p6,
         a2, a2_agroa1, a2_agroa2, a2_agroa3, a2_agroa4, a3, a4, starts_with("a5r"), a6,
         b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12,
         b3_pub_planta, b4_pub_planta, b5_pub_planta, b6_pub_planta,
         b3_pub_contrata, b4_pub_contrata, b5_pub_contrata, b6_pub_contrata,
         b3_pub_honorarios, b4_pub_honorarios, b5_pub_honorarios, b6_pub_honorarios,
         b3_pub_codigot, b4_pub_codigot, b5_pub_codigot, b6_pub_codigot,
         c0, c1, c2,
         starts_with("d1r"), starts_with("d2r"),
         tot_contratadosu12, tot_vacantesu12, tot_totactuales)

wide_mult <- enadel_wide |> 
  select(folio,starts_with(c("oficio4", "cargo", "tareas", "contratadosu12", "vacantesu12",
                             "dificultad1", "dificultad2", "dificultad3", "tipo", "orden",
                             "contrato_o_vacante",
                             "totactuales", "mujeresactuales", "vacantesprox12", "exp", "educ",
                             "licencia", "certificado", "requisito", "canal1", "canal2", "canal3", "creadou3")))

# PIVOT LONGER: hago el reshape sólo para las que necesitan
long_mult <- wide_mult |> 
  pivot_longer(cols = -folio, names_to = c(".value", "tipo_cat", "puesto"), names_sep = "_") |> 
  subset(!(cargo == "" & tareas == ""))

enadel_long_test <- full_join(long_mult, wide_unicas, by = "folio")
# 14.898 filas y 107 columnas

# ------------------------------------------------------------------------------------ #
# ----------------------------------- LONG TO WIDE ----------------------------------- #
# ------------------------------------------------------------------------------------ #
enadel_long <- read_dta("data/clean/enadel_long.dta")
# 14.898 filas y 107 columnas

long_unicas <- enadel_long |> 
  select(folio, fact_emp, sector, region_trab, tam_trab, tam_vent, trabtot, trabval, tam_val,
         p0, p6,
         a2, a2_agroa1, a2_agroa2, a2_agroa3, a2_agroa4, a3, a4, starts_with("a5r"), a6,
         b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12,
         b3_pub_planta, b4_pub_planta, b5_pub_planta, b6_pub_planta,
         b3_pub_contrata, b4_pub_contrata, b5_pub_contrata, b6_pub_contrata,
         b3_pub_honorarios, b4_pub_honorarios, b5_pub_honorarios, b6_pub_honorarios,
         b3_pub_codigot, b4_pub_codigot, b5_pub_codigot, b6_pub_codigot,
         c0, c1, c2,
         starts_with("d1r"), starts_with("d2r"),
         tot_contratadosu12, tot_vacantesu12, tot_totactuales) |> 
  distinct()

long_mult <- enadel_long |>
  select(folio,starts_with(c("tipo_cat", "puesto", 
                             "oficio4", "cargo", "tareas", "contratadosu12", "vacantesu12",
                             "dificultad1", "dificultad2", "dificultad3", "tipo", "orden",
                             "contrato_o_vacante",
                             "totactuales", "mujeresactuales", "vacantesprox12", "exp", "educ",
                             "licencia", "certificado", "requisito", "canal1", "canal2", "canal3", "creadou3")))

# PIVOT WIDER: hago el reshape sólo para las que necesitan
wide_mult <- long_mult |> 
  pivot_wider(
    names_from = c(tipo_cat, puesto),
    values_from = c(oficio4, cargo, tareas, contratadosu12, vacantesu12,
                    contrato_o_vacante,
                    dificultad1, dificultad2, dificultad3,
                    totactuales, mujeresactuales, vacantesprox12, exp, educ,
                    licencia, certificado, requisito, canal1, canal2, canal3, creadou3),
    names_glue = "{.value}_t{tipo_cat}_{puesto}"
  ) |> ungroup() |> 
  dplyr::rename(contrato_o_vacante_t1 = contrato_o_vacante_t1_p1,
                contrato_o_vacante_t2 = contrato_o_vacante_t1_p2,
                contrato_o_vacante_t3 = contrato_o_vacante_t1_p3,
                contrato_o_vacante_t4 = contrato_o_vacante_t1_p4) |> 
  select(folio, matches("^oficio.*$"), matches("^cargo.*$"), matches("^tareas.*$"), matches("^contratadosu12.*$"), matches("^vacantesu12.*$"), 
         contrato_o_vacante_t1, contrato_o_vacante_t2, contrato_o_vacante_t3, contrato_o_vacante_t4, 
         matches("^dificultad"),
         matches("^totactuales_t4_.*$"), matches("^mujeresactuales_t4_.*$"), matches("^vacantesprox12_t4_.*$"),
         matches("^exp_t4_.*$"), matches("^educ_t4_.*$"), 
         matches("^licencia_t4_.*$"), matches("^certificado_t4_.*$"), matches("^requisito_t4_.*$"), matches("^requisito_t5_.*$"),
         matches("canal1_t4_.*$"), matches("canal2_t4_.*$"), matches("canal3_t4_.*$"),
         matches("creadou3_t4_.*$"))
# no todas las combinaciones de tipos y puestos existen

# ordenar 
custom_sort <- function(x) {
  x_t <- str_extract(x, "t\\d+")
  x_p <- str_extract(x, "p\\d+") 
  x_t_num <- as.integer(gsub("\\D+", "", x_t))
  x_p_num <- as.integer(gsub("\\D+", "", x_p))
  order(x_t_num, x_p_num)
}

custom_sort2 <- function(x,y){
  names <- names(select(x,starts_with(y)))
  sorted <- names[custom_sort(names)]
  return(sorted)
}
 
# join
enadel_wide_test <- full_join(wide_mult, long_unicas, by = "folio") |> 
  select(folio, fact_emp, sector, region_trab, tam_trab, tam_vent, trabtot, trabval, tam_val, # 9
         p0, p6, # 2
         a2, a2_agroa1, a2_agroa2, a2_agroa3, a2_agroa4, a3, a4, starts_with("a5r"), a6, # 8+16
         b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, # 12
         b3_pub_planta, b4_pub_planta, b5_pub_planta, b6_pub_planta, # 4
         b3_pub_contrata, b4_pub_contrata, b5_pub_contrata, b6_pub_contrata, # 4
         b3_pub_honorarios, b4_pub_honorarios, b5_pub_honorarios, b6_pub_honorarios, # 4
         b3_pub_codigot, b4_pub_codigot, b5_pub_codigot, b6_pub_codigot, # 4
         c0, c1, c2, contrato_o_vacante_t1, contrato_o_vacante_t2, contrato_o_vacante_t3, contrato_o_vacante_t4, # 7
         custom_sort2(modc_wide, "oficio4"), # 77
         custom_sort2(modc_wide, "cargo"), # 77
         custom_sort2(modc_wide, "tareas_t1"), # 6  
         custom_sort2(modc_wide, "tareas_t2"), # 7
         custom_sort2(modc_wide, "tareas_t3"), # 8 
         custom_sort2(modc_wide, "tareas_t4"), # 16
         custom_sort2(modc_wide, "contratadosu12"), # 77
         custom_sort2(modc_wide, "vacantesu12"), # 77
         custom_sort2(modc_wide, "dificultad"), # 231, 77*3
         custom_sort2(modc_wide, "exp_t4"), # 16
         custom_sort2(modc_wide, "educ_t4"), # 16
         custom_sort2(modc_wide, "licencia_t4"), # 16
         custom_sort2(modc_wide, "certificado_t4"), # 16
         custom_sort2(modc_wide, "requisito_t4"), # 16
         custom_sort2(modc_wide, "requisito_t5"), # 25
         custom_sort2(modc_wide, "totactuales_t4"), # 16
         custom_sort2(modc_wide, "mujeresactuales_t4"), # 16
         custom_sort2(modc_wide, "vacantesprox12_t4"), # 16
         custom_sort2(modc_wide, "canal1_t4"), # 16
         custom_sort2(modc_wide, "canal2_t4"), # 16
         custom_sort2(modc_wide, "canal3_t4"), # 16
         custom_sort2(modc_wide, "creadou3_t4"), # 16
         starts_with("d1r"), # 6
         starts_with("d2r"),
         tot_contratadosu12, tot_vacantesu12, tot_totactuales)


