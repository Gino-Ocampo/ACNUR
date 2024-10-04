# --------------------------- 
# script: 2_validar_vacantes.R
#
# objetivo: Validar el total de vacantes no llenadas
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
paquetes <- c("tidyverse", "dplyr", "plyr", "knitr", "haven", "readxl", "Hmisc", "labelled")

invisible(lapply(paquetes, library, character.only = TRUE)) #Para no ver el mensaje típico que aparece cuando cargas un paquete con library(), como "cargando namespace" o mensajes de advertencia.
# ---------------------------

# ---------------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------------- #
# ---------------------------------- TRABAJO BBDD ---------------------------------- #
# ---------------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------------- #

raw <- read_dta("data/raw/enadel2023.dta")
nrow(raw) # 5820
ncol(raw) # 834
length(unique(raw$folio))
folio_raw <- unique(raw$folio)
# 5820 folios 

# ---------------------------------------------------------------------------------- #
# -------------------------------- COLUMNAS ÚNICAS --------------------------------- #
# ---------------------------------------------------------------------------------- #
moda <- raw |> 
  dplyr::rename(c0 = c_1_2 ) |>
  dplyr::rename(c1 = c_1   ) |>
  dplyr::rename(c2 = c_2   ) |> 
  select(folio, fact_emp, region_trab, tam_trab, tam_vent, trabtot,
         p0, p6,
         a2, a2_agroa1, a2_agroa2, a2_agroa3, a2_agroa4, a3,  a4, a5r1, a5r2, a5r3, a5r4, a5r5, a5r6, a5r7, a5r8, a5r9, a5r10, a5r11, a5r12, a5r13, a5r14, a5r15, a5r16, a6,
         c0, c1, c2, c_a1, pub_c_a1, c_b1, c_c1,
         d1r1, d1r2, d1r3, d1r4, d1r5, d1r6, d2r1, d2r2, d2r3, d2r4, d2r5, d2r6, d2r7, d2r8, d2r9 )


# ---------------------------------------------------------------------------------- #
# ------------------------------------ MÓDULO B ------------------------------------ #
# ---------------------------------------------------------------------------------- #
# --- PRIVADAS
modb_priv <- raw |> 
  select(folio, trabtot, tam_trab, starts_with(c("dirr", "contr", "subcontr", "honr"))) |> 
  dplyr::mutate(
    total_priv = rowSums(across(c(dirr1, contr1, subcontr1, honr1)), na.rm = TRUE),
    total_priv_sinsubcontr = rowSums(across(c(dirr1, contr1, honr1)), na.rm = TRUE),
    sector_priv = ifelse(total_priv>0, "Privado", "Público")
  ) |> 
  dplyr::rename(b1 = dirr1) |> 
  dplyr::rename(b2 = dirr2) |> 
  dplyr::rename(b3 = contr1) |> 
  dplyr::rename(b4 = contr2) |> 
  dplyr::rename(b5 = contr3) |> 
  dplyr::rename(b6 = contr4) |> 
  dplyr::rename(b7 = contr5) |> 
  dplyr::rename(b8 = contr6) |> 
  dplyr::rename(b9 = subcontr1) |> 
  dplyr::rename(b10 = subcontr2) |> 
  dplyr::rename(b11 = honr1) |> 
  dplyr::rename(b12 = honr2) 

# --- PÚBLICAS
modb_pub <- raw |> 
  select(folio, starts_with(c("pub_jefr", "pub_contr", "pub_honr", "pub_codigo"))) |> 
  dplyr::rename(b3_pub_contrata = pub_contr1) |> 
  dplyr::rename(b4_pub_contrata = pub_contr2) |> 
  dplyr::rename(b5_pub_contrata = pub_contr3) |> 
  dplyr::rename(b6_pub_contrata = pub_contr4) |> 
  dplyr::rename(b3_pub_planta = pub_jefr1) |> 
  dplyr::rename(b4_pub_planta = pub_jefr2) |> 
  dplyr::rename(b5_pub_planta = pub_jefr3) |> 
  dplyr::rename(b6_pub_planta = pub_jefr4) |> 
  dplyr::rename(b3_pub_codigot = pub_codigor1) |> 
  dplyr::rename(b4_pub_codigot = pub_codigor2) |> 
  dplyr::rename(b5_pub_codigot = pub_codigor3) |> 
  dplyr::rename(b6_pub_codigot = pub_codigor4) |> 
  dplyr::rename(b3_pub_honorarios = pub_honr1) |> 
  dplyr::rename(b4_pub_honorarios = pub_honr2) |> 
  dplyr::rename(b5_pub_honorarios = pub_honr3) |> 
  dplyr::rename(b6_pub_honorarios = pub_honr4) |> 
  dplyr::mutate(
    total_pub = rowSums(across(c(b3_pub_contrata, b3_pub_planta, b3_pub_codigot, b3_pub_honorarios)), na.rm = TRUE),
    total_pub_sinct = rowSums(across(c(b3_pub_contrata, b3_pub_planta, b3_pub_honorarios)), na.rm = TRUE),
    sector_pub = ifelse(total_pub>0, "Público", "Privado")
  )  

# --- JOIN 
modb <- full_join(modb_priv, modb_pub, by = "folio") |> 
  mutate(sector = ifelse(sector_priv == sector_pub, sector_priv, "Sin Sector")) |>   
  mutate(total_trabajadores = ifelse(sector == "Público", total_pub, total_priv),
         total_trabajadores_sin = ifelse(sector == "Público", total_pub_sinct, total_priv_sinsubcontr)) |> 
  select(folio, sector, starts_with("b"), trabtot, total_trabajadores, total_trabajadores_sin, tam_trab) 

# --- VALIDAR TOTRAB
modb <- modb |> 
  mutate(validacion_tot = (trabtot == total_trabajadores),
       validacion_sin = (trabtot == total_trabajadores_sin))

table(modb$sector, modb$validacion_tot)
table(modb$validacion_sin, modb$sector)
# se revisan estas 154 empresas no validadas 

noval <- modb|> subset(validacion_sin == F)
# revisar ambas validaciones 
table(noval$validacion_sin, noval$validacion_tot)
# hay 3 empresas que consideran a los subcontratados dentro de trabtot

# revisar diferencia entre trabtot y el cálculo 
noval <- noval |> 
  dplyr::mutate(delta = trabtot-total_trabajadores_sin,
                nacount = rowSums(across(c("b1", "b3", "b9", "b11"), ~is.na(.))),
                val_na = (abs(nacount) == abs(delta)),
                cat_val = case_when(validacion_tot == T ~ 1,
                                    val_na == T ~ 2,
                                    TRUE ~ 3))

table(noval$cat_val)
write_dta(noval, file.path("data/clean/folios_novalidados.dta"))
# hay 3 casos donde trabtot coincide con el total de trabajadores incluyendo subcontrato
# hay 61 casos (al menos) donde es probable que se hayan sumado los NA (codificados como -1). 
# Esto porque la cantidad de NA coincide con la diferencia entre trabtot y total_trabajadores_sin. Se sugiere conservar total_trabajadores_sin.
# hay 91 casos donde no se pudo validar
noval <- noval |> select(folio, cat_val)

# --- LIMPIAR TRABVAL FINAL Y RECONSTRUIR TAM_TRAB --- # 
modb_val <- left_join(modb, noval, by = "folio") |> 
  mutate(trabval = case_when(validacion_sin == T ~ total_trabajadores_sin,
                             validacion_sin == F & cat_val == 1 ~ total_trabajadores_sin,
                             validacion_sin == F & cat_val == 2 ~ total_trabajadores_sin,
                             validacion_sin == F & cat_val == 3 ~ total_trabajadores),
         flag_trab = trabval != trabtot,
         tam_val = case_when(trabval<50 ~ 2,
                             between(trabval,50,199) ~ 3,
                             trabval>=200 ~ 4),
         flag_tam = tam_trab != tam_val) 

# --- VALIDAR CLASIFICACIÓN DEL TAMAÑO DE EMPRESA --- #
table(modb_val$tam_trab, modb_val$tam_val)
modb_val |> subset(flag_tam == 1) |> select(folio, trabtot, trabval, tam_trab, tam_val, sector)

modb_val <- modb_val |> 
  select(!c("trabtot", "tam_trab", "flag_trab", "flag_tam", "cat_val", "total_trabajadores_sin", 
            "validacion_tot", "validacion_sin", "total_trabajadores"))
rm(modb, modb_priv, modb_pub, noval)

# ---------------------------------------------------------------------------------- #
# ------------------------------- MÓDULO C1: DEMANDA ------------------------------- #
# ---------------------------------------------------------------------------------- #
# estas inicialmente van a estar en formato long, para después reshape a wide 

# --- DIRECTORES PRIVADOS
modc_directores_priv <- raw |> 
  select(folio, matches("^c_a2")) |> 
  rename_with(~ str_replace(., "^c_a2_oficio4_a(.*)$", "oficio4_p\\1")) |> 
  rename_with(~ str_replace(., "^c_a2r2a(.*)$", "cargo_p\\1"))  |> 
  rename_with(~ str_replace(., "^c_a2r3a(.*)$", "tareas_p\\1")) |> 
  rename_with(~ str_replace(., "^c_a2r4a(.*)$", "contratadosu12_p\\1")) |>
  rename_with(~ str_replace(., "^c_a2r5a(.*)$", "vacantesu12_p\\1")) |>
  rename_with(~ str_replace(., "^c_a2r6a(.*)$", "dificultad1_p\\1")) |>
  rename_with(~ str_replace(., "^c_a2r7a(.*)$", "dificultad2_p\\1")) |>
  rename_with(~ str_replace(., "^c_a2r8a(.*)$", "dificultad3_p\\1")) |> 
  pivot_longer(cols = -folio, 
               names_to = c(".value", "puesto"),
               names_sep = "_") |> 
  mutate(tipo = "Directores") |>
  arrange(folio, oficio4, cargo, tareas, contratadosu12) |> 
  group_by(folio) |> 
  dplyr::mutate(orden = row_number()) |> 
  subset(!(cargo == "" & tareas == ""))
# 594 obs, checked

# --- DIRECTORES_PÚBLICOS 
modc_directores_pub <- raw |> 
  select(folio, matches("^pub_c_a2")) |> 
  rename_with(~ str_replace(., "^pub_c_a2_oficio4_a(.*)$", "oficio4_p\\1")) |> 
  rename_with(~ str_replace(., "^pub_c_a2r2a(.*)$", "cargo_p\\1"))  |> 
  rename_with(~ str_replace(., "^pub_c_a2r3a(.*)$", "tareas_p\\1")) |> 
  rename_with(~ str_replace(., "^pub_c_a2r4a(.*)$", "contratadosu12_p\\1")) |>
  rename_with(~ str_replace(., "^pub_c_a2r5a(.*)$", "vacantesu12_p\\1")) |>
  rename_with(~ str_replace(., "^pub_c_a2r6a(.*)$", "dificultad1_p\\1")) |>
  rename_with(~ str_replace(., "^pub_c_a2r7a(.*)$", "dificultad2_p\\1")) |>
  rename_with(~ str_replace(., "^pub_c_a2r8a(.*)$", "dificultad3_p\\1")) |> 
  mutate(oficio4_p1 = NA) |> 
  pivot_longer(cols = -folio, 
               names_to = c(".value", "puesto"),
               names_sep = "_") |> 
  mutate(tipo = "Jefatura/ADP") |>
  arrange(folio, oficio4, cargo, tareas, contratadosu12) |> 
  group_by(folio) |> 
  dplyr::mutate(orden = row_number()) |> 
  subset(!(cargo == "" & tareas == ""))
# 144 obs, checked

# --- ELEMENTALES
modc_elementales <- raw |> 
  select(folio, matches("^c_b2")) |> 
  rename_with(~ str_replace(., "^c_b2_oficio4_a(.*)$", "oficio4_p\\1")) |> 
  rename_with(~ str_replace(., "^c_b2r2a(.*)$", "cargo_p\\1"))  |> 
  rename_with(~ str_replace(., "^c_b2r3a(.*)$", "tareas_p\\1")) |> 
  rename_with(~ str_replace(., "^c_b2r4a(.*)$", "contratadosu12_p\\1")) |>
  rename_with(~ str_replace(., "^c_b2r5a(.*)$", "vacantesu12_p\\1")) |>
  rename_with(~ str_replace(., "^c_b2r6a(.*)$", "dificultad1_p\\1")) |>
  rename_with(~ str_replace(., "^c_b2r7a(.*)$", "dificultad2_p\\1")) |>
  rename_with(~ str_replace(., "^c_b2r8a(.*)$", "dificultad3_p\\1")) |> 
  pivot_longer(cols = -folio, 
               names_to = c(".value", "puesto"),
               names_sep = "_") |> 
  mutate(tipo = "Ocupaciones Elementales") |> 
  arrange(folio, oficio4, cargo, tareas, contratadosu12) |> 
  group_by(folio) |> 
  dplyr::mutate(orden = row_number()) |> 
  subset(!(cargo == "" & tareas == ""))
# 2914 obs, checked

# --- OTROS CARGOS
modc_otros_cargos <- raw |> 
  select(folio, matches(c("^c_c2", 
                          "^c_c3", 
                          "^c_c4r(.*)a0", "^c_c4r(.*)a1", "^c_c4r(.*)a2", "^c_c4r(.*)a3", "^c_c4r(.*)a4", "^c_c4r(.*)a5$", "^c_c4r(.*)a6$", "^c_c4r(.*)a7$",
                          "^c_c5r(.*)a0$", "^c_c5r(.*)a1$", 
                          "^c_c6r(.*)a1$", "^c_c6r(.*)a2$", "^c_c6r(.*)a3$", 
                          "^c_c8r(.*)a1$", "^c_c8r(.*)a2$", "^c_c8r(.*)a3$", 
                          "^c_c9r(.*)$"))) |> 
  rename_with(~ str_replace(., "^c_c2_oficio4_a(.*)$", "oficio4_p\\1")) |> 
  rename_with(~ str_replace(., "^c_c2_(.*)$", "cargo_p\\1"))  |> 
  rename_with(~ str_replace(., "^c_c3r(.*)$", "tareas_p\\1")) |> 
  rename_with(~ str_replace(., "^c_c4r(.*)a0$", "contratadosu12_p\\1")) |>
  rename_with(~ str_replace(., "^c_c4r(.*)a1$", "vacantesu12_p\\1")) |>
  rename_with(~ str_replace(., "^c_c4r(.*)a2$", "dificultad1_p\\1")) |>
  rename_with(~ str_replace(., "^c_c4r(.*)a3$", "dificultad2_p\\1")) |> 
  rename_with(~ str_replace(., "^c_c4r(.*)a4$", "dificultad3_p\\1")) |> 
  rename_with(~ str_replace(., "^c_c4r(.*)a5$", "totactuales_p\\1")) |> 
  rename_with(~ str_replace(., "^c_c4r(.*)a6$", "mujeresactuales_p\\1")) |> 
  rename_with(~ str_replace(., "^c_c4r(.*)a7$", "vacantesprox12_p\\1")) |> 
  rename_with(~ str_replace(., "^c_c5r(.*)a0$", "exp_p\\1")) |> 
  rename_with(~ str_replace(., "^c_c5r(.*)a1$", "educ_p\\1")) |> 
  rename_with(~ str_replace(., "^c_c6r(.*)a1$", "licencia_p\\1")) |> 
  rename_with(~ str_replace(., "^c_c6r(.*)a2$", "certificado_p\\1")) |> 
  rename_with(~ str_replace(., "^c_c6r(.*)a3$", "requisito_p\\1")) |> 
  rename_with(~ str_replace(., "^c_c8r(.*)a1$", "canal1_p\\1")) |> 
  rename_with(~ str_replace(., "^c_c8r(.*)a2$", "canal2_p\\1")) |> 
  rename_with(~ str_replace(., "^c_c8r(.*)a3$", "canal3_p\\1")) |> 
  rename_with(~ str_replace(., "^c_c9r(.*)$", "creadou3_p\\1")) |> 
  pivot_longer(cols = -folio, 
               names_to = c(".value", "puesto"),
               names_sep = "_") |> 
  mutate(tipo = "Otros Cargos") |> 
  arrange(folio, oficio4, cargo, tareas, contratadosu12) |> 
  group_by(folio) |> 
  dplyr::mutate(orden = row_number()) |> 
  subset(!(cargo == "" & tareas == ""))
# 8307 obs 

# --- CONSTRUCCIÓN 
modc_construccion <- raw |> 
  select(folio, matches(c("^c_construccionr(.*)a0$", "^c_construccionr(.*)a1$", "^c_construccionr(.*)a2$", "^c_construccionr(.*)a3$", "^c_construccionr(.*)a4$", "^c_construccionr(.*)a5$",  "^c_construccionr(.*)a6$"))) |> 
  rename_with(~ str_replace(., "^c_construccionr(.*)a0$", "oficio4_p\\1")) |> 
  rename_with(~ str_replace(., "^c_construccionr(.*)a1$", "contratadosu12_p\\1")) |>
  rename_with(~ str_replace(., "^c_construccionr(.*)a2$", "vacantesu12_p\\1")) |>
  rename_with(~ str_replace(., "^c_construccionr(.*)a3$", "dificultad1_p\\1")) |>
  rename_with(~ str_replace(., "^c_construccionr(.*)a4$", "dificultad2_p\\1")) |> 
  rename_with(~ str_replace(., "^c_construccionr(.*)a5$", "dificultad3_p\\1")) |> 
  rename_with(~ str_replace(., "^c_construccionr(.*)a6$", "requisito_p\\1")) |> 
  pivot_longer(cols = -folio, 
               names_to = c(".value", "puesto"),
               names_sep = "_") |> 
  mutate(tipo = "Construcción",
         cargo = case_when(puesto == "p1" ~ "Encargados de Obra",
                           puesto == "p2" ~ "Capataces",
                           puesto == "p3" ~ "Electrónicos, electromecánicos e instrumentistas",
                           puesto == "p4" ~ "Laboratoristas",
                           puesto == "p5" ~ "Electricistas (técnicos y/o maestros)",
                           puesto == "p6" ~ "Ingenieros, prevencionistas, arqueólogos, otros profesionales",
                           puesto == "p7" ~ "Operadores planta asfalto y áridos",
                           puesto == "p8" ~ "Operadores de maquinaria pesada",
                           puesto == "p9" ~ "Operadores de maquinaria liviana",
                           puesto == "p10" ~ "Trazadores",
                           puesto == "p11" ~ "Mecánicos",
                           puesto == "p12" ~ "Soldadores",
                           puesto == "p13" ~ "Enfierradores",
                           puesto == "p14" ~ "Albañiles",
                           puesto == "p15" ~ "Concreteros",
                           puesto == "p16" ~ "Carpinteros",
                           puesto == "p17" ~ "Pintores",
                           puesto == "p18" ~ "Baldoseros y ceramistas",
                           puesto == "p19" ~ "Tuberos y peradores de termofusión",
                           puesto == "p20" ~ "Sanitarios y gásfiteres",
                           puesto == "p21" ~ "Instaladores de gas",
                           puesto == "p22" ~ "Otros maestros de primera y segunda",
                           puesto == "p23" ~ "Buzos",
                           puesto == "p24" ~ "Bodegueros y cardcheckers",
                           puesto == "p25" ~ "Obreros y jornales"),
         tareas = "") |> 
  arrange(folio, oficio4, cargo, tareas, contratadosu12) |> 
  group_by(folio) |> 
  dplyr::mutate(orden = row_number()) |> 
  subset(!is.na(oficio4))

# --- AGRO
modc_agro <- raw |> 
  select(folio, matches(c("^c_agror(.*)a0$", "^c_agror(.*)a1$", "^c_agror(.*)a2$", "^c_agror(.*)a3$", "^c_agror(.*)a4$", "^c_agror(.*)a5$"))) |> 
  rename_with(~ str_replace(., "^c_agror(.*)a0$", "oficio4_p\\1")) |> 
  rename_with(~ str_replace(., "^c_agror(.*)a1$", "contratadosu12_p\\1")) |>
  rename_with(~ str_replace(., "^c_agror(.*)a2$", "vacantesu12_p\\1")) |>
  rename_with(~ str_replace(., "^c_agror(.*)a3$", "dificultad1_p\\1")) |>
  rename_with(~ str_replace(., "^c_agror(.*)a4$", "dificultad2_p\\1")) |> 
  rename_with(~ str_replace(., "^c_agror(.*)a5$", "dificultad3_p\\1")) |> 
  pivot_longer(cols = -c(folio), 
               names_to = c(".value", "puesto"),
               names_sep = "_") |> 
  mutate(tipo = "Agro",
         cargo = case_when(puesto == "p1" ~ "Obrero agrícola de siembra, viveros",
                           puesto == "p2" ~ "Obrero agrícola de poda, raleo",
                           puesto == "p3" ~ "Obrero agrícola de riego, aplicación de agroquímicos",
                           puesto == "p4" ~ "Obrero agrícola de cosecha",
                           puesto == "p5" ~ "Obrero agrícola de packing frutícola, bodega, estabilización, embotellado",
                           puesto == "p6" ~ "Obrero agrícola de almácigos",
                           puesto == "p7" ~ "Obrero pecuario de ordeña",
                           puesto == "p8" ~ "Obrero pecuario de crianza, alimentación, pastoreo",
                           puesto == "p9" ~ "Obrero agrícola de manejo reproductivo y sanitario",
                           puesto == "p10" ~ "Obrero agroindustrial gestión de cría y engorda",
                           puesto == "p11" ~ "Obrero agroindustrial en matadero",
                           puesto == "p12" ~ "Obrero agroindustrial limpieza, control plagas y enfermedades",
                           puesto == "p13" ~ "Obrero forestal de siembra",
                           puesto == "p14" ~ "Obrero forestal de cosecha",
                           puesto == "p15" ~ "Obrero forestal en labores de aserrador"),
         tareas = "")|> 
  arrange(folio, oficio4, cargo, tareas, contratadosu12) |> 
  group_by(folio) |> 
  dplyr::mutate(orden = row_number()) |> 
  subset(!is.na(oficio4))

# --- MOD C LONG
modc_long <- rbind(modc_directores_priv, modc_directores_pub, modc_elementales, modc_otros_cargos, modc_construccion, modc_agro) |> 
  mutate(tareas = trimws(tareas),
         cargo  = trimws(cargo),
         tipo_cat = case_when(tipo == "Directores" ~ 1,
                              tipo == "Jefatura/ADP"~ 2,
                              tipo == "Ocupaciones Elementales" ~ 3,
                              tipo == "Otros Cargos" ~ 4,
                              tipo == "Construcción" ~ 5,
                              tipo == "Agro" ~ 6)) |> 
  arrange(folio, oficio4, cargo, tareas, contratadosu12, tipo)|> ungroup()

rm(modc_agro, modc_construccion, modc_directores_priv, modc_directores_pub, modc_elementales, modc_otros_cargos)
length(unique(modc_long$folio))
# 4995 folios 
nrow(modc_long)
# 14073 obs

# --- MOD C WIDE 
modc_wide <- modc_long |> 
  pivot_wider(
    names_from = c(tipo_cat, puesto),
    values_from = c(oficio4, cargo, tareas, contratadosu12, vacantesu12,
                    dificultad1, dificultad2, dificultad3, tipo, orden,
                    totactuales, mujeresactuales, vacantesprox12, exp, educ,
                    licencia, certificado, requisito, canal1, canal2, canal3, creadou3),
    names_glue = "{.value}_t{tipo_cat}_{puesto}"
  ) |> ungroup()




# ---------------------------------------------------------------------------------- #
# ----------------------------------- ENADEL LONG ---------------------------------- #
# ---------------------------------------------------------------------------------- #
enadel_base <- full_join(moda, modb_val, by = "folio") 

# --- join
# la bbdd de cargos tiene 75, 73 sin contar "cargo estándar" ni "oficio1"
# yo deberia tener 73 (bbdd) + 8 (nuevas que introduje:sector, trabval, tam_val, tipo_cat, puesto, tot_contratadosu12, tot_vacantesu12, tot_totactuales) 
# + 29 (que no incluye cargos: 15 de d1r y d2r, p0, 7 de c*, 4 a2_agro*, exp, educ) = 107
enadel_long <- full_join(enadel_base, modc_long, by = "folio") |> 
  select(folio, fact_emp, sector, region_trab, tam_trab, tam_vent, trabtot, trabval, tam_val,
         p0, p6,
         a2, a2_agroa1, a2_agroa2, a2_agroa3, a2_agroa4, a3, a4, starts_with("a5r"), a6,
         b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12,
         b3_pub_planta, b4_pub_planta, b5_pub_planta, b6_pub_planta,
         b3_pub_contrata, b4_pub_contrata, b5_pub_contrata, b6_pub_contrata,
         b3_pub_honorarios, b4_pub_honorarios, b5_pub_honorarios, b6_pub_honorarios,
         b3_pub_codigot, b4_pub_codigot, b5_pub_codigot, b6_pub_codigot,
         c0, c1, c2, c_a1, pub_c_a1, c_b1, c_c1,
         tipo_cat, puesto, oficio4, cargo, tareas, contratadosu12, vacantesu12,
         dificultad1, dificultad2, dificultad3, totactuales, mujeresactuales,
         vacantesprox12, exp, educ, licencia, certificado, requisito, canal1, canal2, canal3,
         creadou3,
         starts_with("d1r"), starts_with("d2r")) |> 
  group_by(folio) |> 
  dplyr::mutate(tot_contratadosu12 = sum(contratadosu12, na.rm = T),
         tot_vacantesu12 = sum(vacantesu12, na.rm = T),
         tot_totactuales = sum(totactuales, na.rm = T),
         contrato_o_vacante = case_when(tipo_cat == 1 ~ c_a1,
                                        tipo_cat == 2 ~ pub_c_a1,
                                        tipo_cat == 3 ~ c_b1,
                                        tipo_cat == 4 ~ c_c1,
                                        tipo_cat == 5 ~ NA ,
                                        tipo_cat == 6 ~ NA )) |> 
  select(!c(c_a1, pub_c_a1, c_b1, c_c1)) |> 
  ungroup() 

# --- fix labels
label(enadel_long$trabval)            <- "Trabajadores Totales, validado"
label(enadel_long$tam_val)            <- "Tamaño Empresa según cantidad de trabajadores, validado"
label(enadel_long$contrato_o_vacante) <- "En los últimos 12 meses, ¿Contrató personas nuevas o tuvo vacantes no llenadas para el cargo?"
label(enadel_long$sector)             <- "Sector (Privado o Público)"
label(enadel_long$tipo_cat)           <- "Categoría de tipo de cargo"
label(enadel_long$puesto)             <- "Número de puesto por tipo de cargo"
label(enadel_long$oficio4)            <- "Código de Oficio, 4 dígitos"
label(enadel_long$cargo)              <- "Cargo"
label(enadel_long$tareas)             <- "Principales tareas del cargo"
label(enadel_long$contratadosu12)     <- "Contratados en el cargo en los últimos 12 meses"
label(enadel_long$vacantesu12)        <- "Vacantes no llenadas en los últimos 12 meses"
label(enadel_long$dificultad1)        <- "Dificultad de Contratación 1"
label(enadel_long$dificultad2)        <- "Dificultad de Contratación 2"
label(enadel_long$dificultad3)        <- "Dificultad de Contratación 3"
label(enadel_long$totactuales)        <- "Contratados Actuales"
label(enadel_long$mujeresactuales)    <- "Mujeres contratadas"
label(enadel_long$vacantesprox12)     <- "Vacantes próximos 12 meses"
label(enadel_long$exp)                <- "Años de experiencia mínimo"
label(enadel_long$educ)               <- "Nivel educacional requerido"
label(enadel_long$licencia)           <- "Licencias"
label(enadel_long$certificado)        <- "Certificaciones"
label(enadel_long$requisito)          <- "Requisitos"
label(enadel_long$canal1)             <- "Canal 1"
label(enadel_long$canal2)             <- "Canal 2"
label(enadel_long$canal3)             <- "Canal 3"
label(enadel_long$creadou3)           <- "Cargos creados en los últimos 3 años"
label(enadel_long$tot_contratadosu12) <- "Total trabajadores contratados en los últimos 12 meses, por empresa"
label(enadel_long$tot_vacantesu12)    <- "Total vacantes no llenadas en los últimos 12 meses, por empresa"
label(enadel_long$tot_totactuales)    <- "Total personas contratadas actualmente, por ocupación (T4)"

# --- set value labels 
enadel_long <- enadel_long |> set_value_labels(tipo_cat = c("Directores y gerentes" = 1,
                                                             "Jefatura/ADP" = 2,
                                                             "Ocupaciones Elementales" = 3,
                                                             "Otros Cargos" = 4,
                                                             "Construcción" = 5,
                                                             "Agro"         = 6),
                                               tam_val = c("Pequeña" = 2, 
                                                           "Mediana" = 3, 
                                                           "Grande"  = 4))

# --- guardar
enadel_long <- write_dta(enadel_long, file.path("data/clean/enadel_long.dta"))
rm(modc_long, moda, modb_val, raw)

# ---------------------------------------------------------------------------------- #
# ----------------------------------- ENADEL WIDE ---------------------------------- #
# ---------------------------------------------------------------------------------- #

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
enadel_wide <- full_join(enadel_base, modc_wide, by = "folio") |> 
  select(folio, fact_emp, sector, region_trab, tam_trab, tam_vent, trabtot, trabval, tam_val, # 9
         p0, p6, # 2
         a2, a2_agroa1, a2_agroa2, a2_agroa3, a2_agroa4, a3, a4, starts_with("a5r"), a6, # 8+16
         b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, # 12
         b3_pub_planta, b4_pub_planta, b5_pub_planta, b6_pub_planta, # 4
         b3_pub_contrata, b4_pub_contrata, b5_pub_contrata, b6_pub_contrata, # 4
         b3_pub_honorarios, b4_pub_honorarios, b5_pub_honorarios, b6_pub_honorarios, # 4
         b3_pub_codigot, b4_pub_codigot, b5_pub_codigot, b6_pub_codigot, # 4
         c0, c1, c2, c_a1, pub_c_a1, c_b1, c_c1, # 7
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
         starts_with("d2r")) |> 
  dplyr::mutate(tot_contratadosu12 = rowSums(across(starts_with("contratadosu12")), na.rm = TRUE), 
                tot_vacantesu12 = rowSums(across(starts_with("vacantesu12")), na.rm = TRUE),
                tot_totactuales = rowSums(across(starts_with("totactuales")), na.rm = TRUE)) |> 
  mutate(contrato_o_vacante_t1 = c_a1,
         contrato_o_vacante_t2 = pub_c_a1,
         contrato_o_vacante_t3 = c_b1,
         contrato_o_vacante_t4 = c_c1) |> 
  select(!c(c_a1, pub_c_a1, c_b1, c_c1))

# --- fix labels
label(enadel_wide$trabval)   <- "Trabajadores Totales, validado"
label(enadel_wide$tam_val)   <- "Tamaño Empresa según cantidad de trabajadores, validado"
label(enadel_wide$sector)    <- "Sector (Privado o Público)"
label(enadel_wide$tot_contratadosu12)    <- "Total trabajadores contratados en los últimos 12 meses, por empresa"
label(enadel_wide$tot_vacantesu12)       <- "Total vacantes no llenadas en los últimos 12 meses, por empresa"
label(enadel_wide$tot_totactuales)       <- "Total personas contratadas actualmente, por ocupación (T4)"

labels_list <- list(
  t1 = "Director y/ gerente",
  t2 = "Jefatura/ADP",
  t3 = "Ocupacion Elemental",
  t4 = "Otros Cargos",
  t5 = "Construcción",
  t6 = "Agro"
)

p_ranges <- list(
  t1 = 1:6,
  t2 = 1:7,
  t3 = 1:8,
  t4 = 1:16,
  t5 = 1:25,
  t6 = 1:15
)

varnames <- c("oficio4", "cargo", "tareas", "contratadosu12", 
              "vacantesu12", "dificultad1", "dificultad2", "dificultad3", "exp", "educ", "licencia", "certificado",
              "requisito", "totactuales", "mujeresactuales", "vacantesprox12", "canal1", "canal2", "canal3", "creadou3")
for (x in varnames) {
  for (t in names(labels_list)) {
    for (p in p_ranges[[t]]) {
      var_name <- paste0(x,"_", t, "_p", p)
      
      label_text <- paste(labels_list[[t]], p, ": ",x)
      
      if (var_name %in% names(enadel_wide)) {
        attr(enadel_wide[[var_name]], "label") <- label_text
      }
    }
  }
}

# set value labels
enadel_wide <- enadel_wide |> 
  set_value_labels(tam_val = c("Pequeña" = 2,
                               "Mediana" = 3,
                               "Grande"  = 4))
# guardar wide
write_dta(enadel_wide, file.path("data/clean/enadel_wide.dta"))
