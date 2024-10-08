# --------------------------- 
# script: 5_analisis.R
#
# objetivo: Validar el total de vacantes no llenadas
#
# autora: antoniaaguileracarrasco@gmail.com
#
# fecha creación: Agosto 2024
# fecha última modificación: Septiembre 2024
# ---------------------------
# Paquetes: 
pacman::p_load("tidyverse", "dplyr", "plyr", "knitr", "haven", "readxl", "writexl", "Hmisc", "survey", "labelled", "archive")

# ---------------------------

# ---------------------------------------------------------------------------------- #
# ------------------------------------ PREÁMBULO ----------------------------------- #
# ---------------------------------------------------------------------------------- #
region <- seq(1,16)

enadel_wide <- readr::read_rds("Datos/enadel_wide.rds")
enadel_long <- readr::read_rds("Datos/enadel_long.rds")


# svy
options(survey.lonely.psu="adjust")
design_wide <- svydesign(ids = ~folio, strata = ~estrato_cm, weights = ~fact_emp, data = enadel_wide, nest = TRUE)
design_long_tot <- svydesign(ids = ~folio, strata = ~estrato_cm, weights = ~fact_emp, data = enadel_long, nest = TRUE)
design_t5   <- subset(design_long_tot, tipo_cat == 5)
design_t6   <- subset(design_long_tot, tipo_cat == 6)
design_long <- subset(design_long_tot, tipo_cat%in%c(1, 2, 3, 4))


# ---------------------------------------------------------------------------------- #
# -------------------------------------- STATS ------------------------------------- #
# ---------------------------------------------------------------------------------- #

# ----------------------- CARACTERIZACIÓN GENERAL DE LA BBDD ----------------------- #
{
  # --- cuadro resumen 
  # cantidad de empresas 
  tot_folios <- svytotal(~empresas, design_wide, na.rm = TRUE)[1]
  tot_folios_muestra <- length(unique(enadel_wide$folio)) # 5820 empresas 
  
  # total trabajadores 
  tot_trab <- svytotal(~trabval, design_wide, na.rm = TRUE)[1]
  tot_trab_muestra <- sum(enadel_wide$trabval)
  
  # empresas por sector
  folios_sector <- svyby(~empresas, ~sector, design_wide, svytotal, na.rm = TRUE)[1:2]
  folios_sector_muestra <- enadel_wide |> group_by(sector) |> 
    dplyr::summarise(tot_folios = n())
  
  # trabajadores por sector
  trab_sector <- svyby(~trabval, ~sector, design_wide, svytotal, na.rm = TRUE)[1:2]
  trab_sector_muestra <- enadel_wide |> group_by(sector) |> 
    dplyr::summarise(tot_trab = sum(trabval))
  
  cuadro_resumen <- data.frame(tipo = c(folios_sector$sector, "Total"),
                               empresas_muestra = c(folios_sector_muestra$tot_folios, tot_trab),
                               empresas_expandidos = c(folios_sector$empresas, tot_folios),
                               trab_muestra = c(trab_sector_muestra$tot_trab, tot_trab_muestra),
                               trab_expandidos = c(trab_sector$trabval, tot_trab))
    
  # --- % por region 
  # empresas por region
  folios_region <- svyby(~empresas, ~region_trab, design_wide, svytotal, na.rm = TRUE)[1:2]
  
  orden_regiones <- c(
    "Arica y Parinacota", "Tarapacá", "Antofagasta", "Atacama", 
    "Coquimbo", "Valparaíso", "Metropolitana", "O'Higgins", 
    "Maule", "Ñuble", "Biobío", "Araucanía", 
    "Los Ríos", "Los Lagos", "Aysén", "Magallanes"
  )
  
  # trabajadores por region 
  trab_region <- svyby(~trabval, ~region_trab, design_wide, svytotal, na.rm = TRUE)[1:2]
  
  pc_region <- data.frame (region =  names(attr(folios_region$region_trab, "labels")),
                           empresas = folios_region$empresas,
                           trabajadores = trab_region$trabval) |> 
    mutate(pc_empresas = empresas/sum(empresas),
           pc_trabajadores = trabajadores/sum(trabajadores),
           region= factor(region, levels = orden_regiones)
           ) |>
    arrange(region) 
  
  # --- % por tamaño
  # empresas por tamaño
  tam_tot <- svyby(~empresas, ~tam_val, design_wide, svytotal, na.rm = TRUE)[1:2]
  # trabajadores por tamaño
  tam_trab_tot <- svyby(~trabval, ~tam_val, design_wide, svytotal, na.rm = TRUE)[1:2]
  
  pc_tam <- data.frame (tam_val = tam_tot$tam_val,
                           empresas = tam_tot$empresas,
                           trabajadores = tam_trab_tot$trabval) |> 
    mutate(pc_empresas = empresas/sum(empresas),
           pc_trabajadores = trabajadores/sum(trabajadores),
           tam_label = case_when(tam_val == 2 ~ "Pequeña", 
                                 tam_val == 3 ~ "Mediana", 
                                 tam_val == 4 ~ "Grande")) |> 
    select(tam_label, empresas, trabajadores, pc_empresas, pc_trabajadores)
  
  # --- % por tamaño de ventas
  # empresas por tamaño
  tamvent_tot <- svyby(~empresas, ~tam_vent, design_wide, svytotal, na.rm = TRUE)[1:2]
  # trabajadores por tamaño
  tamvent_trab_tot <- svyby(~trabval, ~tam_vent, design_wide, svytotal, na.rm = TRUE)[1:2]
  
  pc_tamvent <- data.frame (tam_vent = val_labels(tamvent_tot$tam_vent),
                            empresas = tamvent_tot$empresas,
                            trabajadores = tamvent_trab_tot$trabval) |> 
    mutate(pc_empresas = empresas/sum(empresas),
           pc_trabajadores = trabajadores/sum(trabajadores)) |> 
    select(tam_vent, empresas, trabajadores, pc_empresas, pc_trabajadores)
  
  
  # --- % por sector 
  # empresas por sector
  acteco_tot <- svyby(~empresas, ~a2, design_wide, svytotal, na.rm = TRUE)[1:2]
  # trabajadores por sector
  acteco_trab_tot <- svyby(~trabval, ~a2, design_wide, svytotal, na.rm = TRUE)[1:2]
  
  pc_acteco <- data.frame (act_eco = names(attr(acteco_tot$a2, "labels")[1:13]),
                           empresas = acteco_tot$empresas,
                           trabajadores = acteco_trab_tot$trabval) |> 
    mutate(pc_empresas = empresas/sum(empresas),
           pc_trabajadores = trabajadores/sum(trabajadores)) |> 
    select(act_eco, empresas, trabajadores, pc_empresas, pc_trabajadores)

}
# CHECK

# -------------------------- CONTRATADOS Y VACANTES GENERAL ------------------------ #
{
  # --- vacantes no llenadas tot
  # En los últimos 12 meses, ¿Su empresa contrató personas nuevas?
  contratos_u12 <- svyby(~empresas, ~c1, design_wide, svytotal, na.rm = TRUE)[1:2]
  # En los últimos 12 meses, ¿Tuvo vacantes que no pudo llenar?
  vacantes_nollenadas <- svyby(~empresas, ~c2, design_wide, svytotal, na.rm = TRUE)[1:2]
  
  vacantes_nollenadas_tot <- data.frame(label = c("SI", "NO"),
                                        contratos_u12 = contratos_u12$empresas, 
                                        vac_nollenadas_u12 = vacantes_nollenadas$empresas)
  vacantes_nollenadas_tot
  
  # --- contratos_sector últimos 12 meses
  contratos_sector <- svyby(~empresas, ~c1+a2, design_wide, svytotal, na.rm = TRUE)[1:3] |>
    pivot_wider(names_from = "c1", values_from = "empresas", names_glue = "empresas_{c1}") |> 
    mutate(tot_empresas = empresas_1+empresas_2) |> 
    dplyr::rename(convacu12_1 = empresas_1) |> 
    dplyr::rename(convacu12_2 = empresas_2)
  unique_oficio <- unique(contratos_sector$a2)
  labels_oficio <- attr(contratos_sector$a2, "labels")
  matched_labels <- names(labels_oficio)[match(unique_oficio, labels_oficio)]
  contratos_sector$sector_label <- matched_labels[match(contratos_sector$a2, unique_oficio)]
  
  # --- vacantes no llenadas por sector
  vnll_sector <- svyby(~empresas, ~c2+a2, design_wide, svytotal, na.rm = TRUE)[1:3] |>
    pivot_wider(names_from = "c2", values_from = "empresas", names_glue = "empresas_{c2}") |> 
    dplyr::rename(vacnollen_1 = empresas_1) |> 
    dplyr::rename(vacnollen_2 = empresas_2) |> 
    left_join(contratos_sector, by = c("a2")) |> 
    select(a2, tot_empresas, convacu12_1, convacu12_2, vacnollen_1, vacnollen_2) |> 
    mutate(pc_vacu12 = convacu12_1/sum(convacu12_1),
           pc_vacnollen = vacnollen_1/sum(vacnollen_1))
}

# ********************************************************************************** #
# -------------------------- CONTRATADOS ÚLTIMOS 12 MESES -------------------------- #
# ********************************************************************************** #

# ------------------ CONTRATACIONES ÚLTIMOS 12 MESES POR OCUPACIÓN ----------------- #
{
  cu12_total <- svytotal(~contratadosu12, design_long, na.rm = TRUE)
  cv(cu12_total)
  
  # por ocupación
  cu12 <- svyby(~contratadosu12, ~oficio4, design_long, svytotal, na.rm = TRUE) |> arrange(desc(contratadosu12))
  cu12$cv <- cv(cu12)
  # labels
  unique_oficio <- unique(cu12$oficio4)
  labels_oficio <- attr(cu12$oficio4, "labels")
  matched_labels <- names(labels_oficio)[match(unique_oficio, labels_oficio)]
  cu12$oficio_label <- matched_labels[match(cu12$oficio, unique_oficio)]
  
  # por ocupación y sector 
  cu12_sector <- svyby(~contratadosu12, ~oficio4+a2, design_long, svytotal, na.rm = TRUE) |> arrange(desc(contratadosu12))
  cu12_sector$cv <- cv(cu12_sector)
  # labels oficio
  unique_oficio <- unique(cu12_sector$oficio4)
  labels_oficio <- attr(cu12_sector$oficio4, "labels")
  matched_labels <- names(labels_oficio)[match(unique_oficio, labels_oficio)]
  cu12_sector$oficio_label <- matched_labels[match(cu12_sector$oficio, unique_oficio)]
  # labels sector
  unique_sector <- unique(cu12_sector$a2)
  labels_sector <- attr(cu12_sector$a2, "labels")
  matched_labels <- names(labels_sector)[match(unique_sector, labels_sector)]
  cu12_sector$sector_label <- matched_labels[match(cu12_sector$a2, unique_sector)]
  
  cu12_sector <- cu12_sector |> arrange(sector_label, desc(contratadosu12)) |> 
    subset(contratadosu12>0) |> select(sector_label, a2, oficio_label, oficio4, contratadosu12, se, cv)
}

# -------------------- CONSTRUCCIÓN: CONTRATADOS POR OCUPACIÓN --------------------- #
{
  cu12_t5_total <- svytotal(~contratadosu12, design_t5, na.rm = TRUE)
  cv(cu12_t5_total)
  
  # por ocupación
  cu12_t5 <- svyby(~contratadosu12, ~cargo, design_t5, svytotal, na.rm = TRUE) |> 
    mutate(orden = case_when(cargo == "Encargados de Obra"~1,
                             cargo == "Capataces"~2,
                             cargo == "Electrónicos, electromecánicos e instrumentistas"~3,
                             cargo == "Laboratoristas"~4,
                             cargo == "Electricistas (técnicos y/o maestros)"~5,
                             cargo == "Ingenieros, prevencionistas, arqueólogos, otros profesionales"~6,
                             cargo == "Operadores planta asfalto y áridos"~7,
                             cargo == "Operadores de maquinaria pesada"~8,
                             cargo == "Operadores de maquinaria liviana"~9,
                             cargo == "Trazadores"~10,
                             cargo == "Mecánicos"~11,
                             cargo == "Soldadores"~12,
                             cargo == "Enfierradores"~13,
                             cargo == "Albañiles"~14,
                             cargo == "Concreteros"~15,
                             cargo == "Carpinteros"~16,
                             cargo == "Pintores"~17,
                             cargo == "Baldoseros y ceramistas"~18,
                             cargo == "Tuberos y peradores de termofusión"~19,
                             cargo == "Sanitarios y gásfiteres"~20,
                             cargo == "Instaladores de gas"~21,
                             cargo == "Otros maestros de primera y segunda"~22,
                             cargo == "Buzos"~23,
                             cargo == "Bodegueros y cardcheckers"~24,
                             cargo == "Obreros y jornales"~25)) |> 
    arrange(orden)
  cu12_t5$cv <- cv(cu12_t5)
}

# ------------------------ AGRO: CONTRATADOS POR OCUPACIÓN ------------------------- #
{
  cu12_t6_total <- svytotal(~contratadosu12, design_t6, na.rm = TRUE)
  cv(cu12_t6_total)
  
  # por ocupación
  cu12_t6 <- svyby(~contratadosu12, ~cargo, design_t6, svytotal, na.rm = TRUE) |> 
    mutate(orden = case_when(cargo == "Obrero agrícola de siembra, viveros"~1,
                             cargo == "Obrero agrícola de poda, raleo"~2,
                             cargo == "Obrero agrícola de riego, aplicación de agroquímicos"~3,
                             cargo == "Obrero agrícola de cosecha"~4,
                             cargo == "Obrero agrícola de packing frutícola, bodega, estabilización, embotellado"~5,
                             cargo == "Obrero agrícola de almácigos"~6,
                             cargo == "Obrero pecuario de ordeña"~7,
                             cargo == "Obrero pecuario de crianza, alimentación, pastoreo"~8,
                             cargo == "Obrero agrícola de manejo reproductivo y sanitario"~9,
                             cargo == "Obrero agroindustrial gestión de cría y engorda"~10,
                             cargo == "Obrero agroindustrial en matadero"~11,
                             cargo == "Obrero agroindustrial limpieza, control plagas y enfermedades"~12,
                             cargo == "Obrero forestal de siembra"~13,
                             cargo == "Obrero forestal de cosecha"~14,
                             cargo == "Obrero forestal en labores de aserrador"~15)) |> 
    arrange(orden)
  cu12_t6$cv <- cv(cu12_t6)
  }

# ********************************************************************************** #
# ---------------------------- VACANTES ÚLTIMOS 12 MESES --------------------------- #
# ********************************************************************************** #

# ----------------------- VACANTES NO LLENADAS POR OCUPACIÓN ----------------------- #
{
  # por ocupación
  odc_total <- svytotal(~vacantesu12, design_long, na.rm = TRUE)
  cv(odc_total)
  
  odc <- svyby(~vacantesu12, ~oficio4, design_long, svytotal, na.rm = TRUE) |> 
    arrange(oficio4)
  odc$cv <- cv(odc)
  
  unique_oficio <- unique(odc$oficio4)
  labels_oficio <- attr(odc$oficio4, "labels")
  matched_labels <- names(labels_oficio)[match(unique_oficio, labels_oficio)]
  odc$oficio_label <- matched_labels[match(odc$oficio, unique_oficio)]
}

# ---------------- CONSTRUCCIÓN: VACANTES NO LLENADAS POR OCUPACIÓN ---------------- #
{
  odc_t5_total <- svytotal(~vacantesu12, design_t5, na.rm = TRUE)
  cv(odc_t5_total)
  
  # por ocupación
  odc_t5 <- svyby(~vacantesu12, ~cargo, design_t5, svytotal, na.rm = TRUE) |>  
    mutate(orden = case_when(cargo == "Encargados de Obra"~1,
                             cargo == "Capataces"~2,
                             cargo == "Electrónicos, electromecánicos e instrumentistas"~3,
                             cargo == "Laboratoristas"~4,
                             cargo == "Electricistas (técnicos y/o maestros)"~5,
                             cargo == "Ingenieros, prevencionistas, arqueólogos, otros profesionales"~6,
                             cargo == "Operadores planta asfalto y áridos"~7,
                             cargo == "Operadores de maquinaria pesada"~8,
                             cargo == "Operadores de maquinaria liviana"~9,
                             cargo == "Trazadores"~10,
                             cargo == "Mecánicos"~11,
                             cargo == "Soldadores"~12,
                             cargo == "Enfierradores"~13,
                             cargo == "Albañiles"~14,
                             cargo == "Concreteros"~15,
                             cargo == "Carpinteros"~16,
                             cargo == "Pintores"~17,
                             cargo == "Baldoseros y ceramistas"~18,
                             cargo == "Tuberos y peradores de termofusión"~19,
                             cargo == "Sanitarios y gásfiteres"~20,
                             cargo == "Instaladores de gas"~21,
                             cargo == "Otros maestros de primera y segunda"~22,
                             cargo == "Buzos"~23,
                             cargo == "Bodegueros y cardcheckers"~24,
                             cargo == "Obreros y jornales"~25)) |> 
    arrange(orden)
  odc_t5$cv <- cv(odc_t5)
}

# -------------------- AGRO: VACANTES NO LLENADAS POR OCUPACIÓN -------------------- #
{
  odc_t6_total <- svytotal(~vacantesu12, design_t6, na.rm = TRUE)
  cv(odc_t6_total)
  
  # por ocupación
  odc_t6 <- svyby(~vacantesu12, ~cargo, design_t6, svytotal, na.rm = TRUE) |> 
    mutate(orden = case_when(cargo == "Obrero agrícola de siembra, viveros"~1,
                             cargo == "Obrero agrícola de poda, raleo"~2,
                             cargo == "Obrero agrícola de riego, aplicación de agroquímicos"~3,
                             cargo == "Obrero agrícola de cosecha"~4,
                             cargo == "Obrero agrícola de packing frutícola, bodega, estabilización, embotellado"~5,
                             cargo == "Obrero agrícola de almácigos"~6,
                             cargo == "Obrero pecuario de ordeña"~7,
                             cargo == "Obrero pecuario de crianza, alimentación, pastoreo"~8,
                             cargo == "Obrero agrícola de manejo reproductivo y sanitario"~9,
                             cargo == "Obrero agroindustrial gestión de cría y engorda"~10,
                             cargo == "Obrero agroindustrial en matadero"~11,
                             cargo == "Obrero agroindustrial limpieza, control plagas y enfermedades"~12,
                             cargo == "Obrero forestal de siembra"~13,
                             cargo == "Obrero forestal de cosecha"~14,
                             cargo == "Obrero forestal en labores de aserrador"~15)) |> 
    arrange(orden)
  
  odc_t6$cv <- cv(odc_t6)
}

# ------------------------- VACANTES NO LLENADAS POR SECTOR ------------------------ #
{
  # por ocupación
  odc <- svyby(~vacantesu12, ~oficio4+a2, design_long, svytotal, na.rm = TRUE) |> 
    arrange(oficio4)
  odc$cv <- cv(odc)
  
  unique_oficio <- unique(odc$oficio4)
  labels_oficio <- attr(odc$oficio4, "labels")
  matched_labels <- names(labels_oficio)[match(unique_oficio, labels_oficio)]
  odc$oficio_label <- matched_labels[match(odc$oficio, unique_oficio)]
  odc_sector <- odc |> group_by(a2) |> 
    arrange(a2, desc(vacantesu12)) |> 
    dplyr::mutate(orden = row_number()) |> 
    subset(orden == 1)
}

# ------------------------- VACANTES NO LLENADAS POR REGIÓN ------------------------ #
{
  # por ocupación
  odc <- svyby(~vacantesu12, ~oficio4+region_trab, design_long, svytotal, na.rm = TRUE) |> 
    arrange(oficio4)
  odc$cv <- cv(odc)
  
  unique_oficio <- unique(odc$oficio4)
  labels_oficio <- attr(odc$oficio4, "labels")
  matched_labels <- names(labels_oficio)[match(unique_oficio, labels_oficio)]
  odc$oficio_label <- matched_labels[match(odc$oficio, unique_oficio)]
  odc_region <- odc |> group_by(region_trab) |> 
    arrange(region_trab, desc(vacantesu12)) |> 
    dplyr::mutate(orden = row_number()) |> 
    subset(orden == 1)
}
# -------------------- VACANTES NO LLENADAS POR TAMAÑO EMPRESA -------------------- #
{
  # por ocupación
  odc <- svyby(~vacantesu12, ~oficio4+tam_val, design_long, svytotal, na.rm = TRUE) |> 
    arrange(oficio4)
  odc$cv <- cv(odc)
  
  unique_oficio <- unique(odc$oficio4)
  labels_oficio <- attr(odc$oficio4, "labels")
  matched_labels <- names(labels_oficio)[match(unique_oficio, labels_oficio)]
  odc$oficio_label <- matched_labels[match(odc$oficio, unique_oficio)]
  odc_tam <- odc |> group_by(tam_val) |> 
    arrange(tam_val, desc(vacantesu12)) |> 
    dplyr::mutate(orden = row_number()) |> 
    subset(orden == 1)
}

# -------------------- VACANTES NO LLENADAS POR TAMAÑO EMPRESA $$ -------------------- #
{
  # por ocupación
  odc <- svyby(~vacantesu12, ~oficio4+tam_vent, design_long, svytotal, na.rm = TRUE) |> 
    arrange(oficio4)
  odc$cv <- cv(odc)
  
  unique_oficio <- unique(odc$oficio4)
  labels_oficio <- attr(odc$oficio4, "labels")
  matched_labels <- names(labels_oficio)[match(unique_oficio, labels_oficio)]
  odc$oficio_label <- matched_labels[match(odc$oficio, unique_oficio)]
  odc_tamvent <- odc |> group_by(tam_vent) |> 
    arrange(tam_vent, desc(vacantesu12)) |> 
    dplyr::mutate(orden = row_number()) |> 
    subset(orden == 1)
}

# ********************************************************************************** #
# --------------------------- DIFICULTAD DE CONTRATACIÓN --------------------------- #
# ********************************************************************************** #

# ------------------------------ DIFICULTAD AGREGADO ------------------------------- #
{
  # DIFICULTAD 1
  dif_1 <- svyby(~oficios, ~dificultad1, design_long_tot, svytotal, na.rm = TRUE) |> 
    arrange(desc(oficios)) |> dplyr::rename(dif_1 = oficios, se_1 = se)
  dif_1$cv_1 <- cv(dif_1)
  
  unique_dif <- unique(dif_1$dificultad1)
  labels_dif <- attr(dif_1$dificultad1, "labels")
  matched_labels <- names(labels_dif)[match(unique_dif, labels_dif)]
  dif_1$dif_label <- matched_labels[match(dif_1$dificultad1, unique_dif)]
  
  # DIFICULTAD 2
  dif_2 <- svyby(~oficios, ~dificultad2, design_long_tot, svytotal, na.rm = TRUE) |> 
    arrange(desc(oficios)) |> dplyr::rename(dif_2 = oficios, se_2 = se)
  dif_2$cv_2 <- cv(dif_2)
  
  unique_dif <- unique(dif_2$dificultad2)
  labels_dif <- attr(dif_2$dificultad2, "labels")
  matched_labels <- names(labels_dif)[match(unique_dif, labels_dif)]
  dif_2$dif_label <- matched_labels[match(dif_2$dificultad2, unique_dif)]
  
  # DIFICULTAD 3 
  dif_3 <- svyby(~oficios, ~dificultad3, design_long_tot, svytotal, na.rm = TRUE) |> 
    arrange(desc(oficios)) |> dplyr::rename(dif_3 = oficios, se_3 = se)
  dif_3$cv_3 <- cv(dif_3)
  
  unique_dif <- unique(dif_3$dificultad3)
  labels_dif <- attr(dif_3$dificultad3, "labels")
  matched_labels <- names(labels_dif)[match(unique_dif, labels_dif)]
  dif_3$dif_label <- matched_labels[match(dif_3$dificultad3, unique_dif)]
  
  # join 
  dificultad_tot <- dif_1 |> left_join(dif_2, by = "dif_label") |> left_join(dif_3, by = "dif_label") |> 
    select(dif_label, ends_with("1"), ends_with("2"), ends_with("3")) |> 
    arrange(dificultad1)
}

# ---------------------------- DIFICULTAD: MÓDULOS 1-4 ----------------------------- #
{
  # DIFICULTAD 1
  dif_1 <- svyby(~oficios, ~dificultad1, design_long, svytotal, na.rm = TRUE) |> 
    arrange(desc(oficios)) |> dplyr::rename(dif_1 = oficios, se_1 = se)
  dif_1$cv_1 <- cv(dif_1)
                    
  unique_dif <- unique(dif_1$dificultad1)
  labels_dif <- attr(dif_1$dificultad1, "labels")
  matched_labels <- names(labels_dif)[match(unique_dif, labels_dif)]
  dif_1$dif_label <- matched_labels[match(dif_1$dificultad1, unique_dif)]
  
  # DIFICULTAD 2
  dif_2 <- svyby(~oficios, ~dificultad2, design_long, svytotal, na.rm = TRUE) |> 
     arrange(desc(oficios)) |> dplyr::rename(dif_2 = oficios, se_2 = se)
  dif_2$cv_2 <- cv(dif_2)
  
  unique_dif <- unique(dif_2$dificultad2)
  labels_dif <- attr(dif_2$dificultad2, "labels")
  matched_labels <- names(labels_dif)[match(unique_dif, labels_dif)]
  dif_2$dif_label <- matched_labels[match(dif_2$dificultad2, unique_dif)]
  
  # DIFICULTAD 3 
  dif_3 <- svyby(~oficios, ~dificultad3, design_long, svytotal, na.rm = TRUE) |> 
    arrange(desc(oficios)) |> dplyr::rename(dif_3 = oficios, se_3 = se)
  dif_3$cv_3 <- cv(dif_3)
  
  unique_dif <- unique(dif_3$dificultad3)
  labels_dif <- attr(dif_3$dificultad3, "labels")
  matched_labels <- names(labels_dif)[match(unique_dif, labels_dif)]
  dif_3$dif_label <- matched_labels[match(dif_3$dificultad3, unique_dif)]
  
  # join 
  dificultad <- dif_1 |> left_join(dif_2, by = "dif_label") |> left_join(dif_3, by = "dif_label") |> 
    select(dif_label, ends_with("1"), ends_with("2"), ends_with("3")) |> 
    arrange(dificultad1)
}
# ---------------------------- DIFICULTAD: CONSTRUCCIÓN ---------------------------- #
{
  # DIFICULTAD 1
  dif_1 <- svyby(~oficios, ~dificultad1, design_t5, svytotal, na.rm = TRUE) |> 
    arrange(desc(oficios)) |> dplyr::rename(dif_1 = oficios, se_1 = se)
  dif_1$cv_1 <- cv(dif_1)
  
  unique_dif <- unique(dif_1$dificultad1)
  labels_dif <- attr(dif_1$dificultad1, "labels")
  matched_labels <- names(labels_dif)[match(unique_dif, labels_dif)]
  dif_1$dif_label <- matched_labels[match(dif_1$dificultad1, unique_dif)]
  
  # DIFICULTAD 2
  dif_2 <- svyby(~oficios, ~dificultad2, design_t5, svytotal, na.rm = TRUE) |> 
    arrange(desc(oficios)) |> dplyr::rename(dif_2 = oficios, se_2 = se)
  dif_2$cv_2 <- cv(dif_2)
  
  unique_dif <- unique(dif_2$dificultad2)
  labels_dif <- attr(dif_2$dificultad2, "labels")
  matched_labels <- names(labels_dif)[match(unique_dif, labels_dif)]
  dif_2$dif_label <- matched_labels[match(dif_2$dificultad2, unique_dif)]
  
  # DIFICULTAD 3 
  dif_3 <- svyby(~oficios, ~dificultad3, design_t5, svytotal, na.rm = TRUE) |> 
    arrange(desc(oficios)) |> dplyr::rename(dif_3 = oficios, se_3 = se)
  dif_3$cv_3 <- cv(dif_3)
  
  unique_dif <- unique(dif_3$dificultad3)
  labels_dif <- attr(dif_3$dificultad3, "labels")
  matched_labels <- names(labels_dif)[match(unique_dif, labels_dif)]
  dif_3$dif_label <- matched_labels[match(dif_3$dificultad3, unique_dif)]
  
  # join 
  dificultad_t5 <- dif_1 |> left_join(dif_2, by = "dif_label") |> left_join(dif_3, by = "dif_label") |> 
    select(dif_label, ends_with("1"), ends_with("2"), ends_with("3")) |> 
    arrange(dificultad1)
}
# -------------------------------- DIFICULTAD: AGRO -------------------------------- #
{
  # DIFICULTAD 1
  dif_1 <- svyby(~oficios, ~dificultad1, design_t6, svytotal, na.rm = TRUE) |> 
    arrange(desc(oficios)) |> dplyr::rename(dif_1 = oficios, se_1 = se)
  dif_1$cv_1 <- cv(dif_1)
  
  unique_dif <- unique(dif_1$dificultad1)
  labels_dif <- attr(dif_1$dificultad1, "labels")
  matched_labels <- names(labels_dif)[match(unique_dif, labels_dif)]
  dif_1$dif_label <- matched_labels[match(dif_1$dificultad1, unique_dif)]
  
  # DIFICULTAD 2
  dif_2 <- svyby(~oficios, ~dificultad2, design_t6, svytotal, na.rm = TRUE) |> 
    arrange(desc(oficios)) |> dplyr::rename(dif_2 = oficios, se_2 = se)
  dif_2$cv_2 <- cv(dif_2)
  
  unique_dif <- unique(dif_2$dificultad2)
  labels_dif <- attr(dif_2$dificultad2, "labels")
  matched_labels <- names(labels_dif)[match(unique_dif, labels_dif)]
  dif_2$dif_label <- matched_labels[match(dif_2$dificultad2, unique_dif)]
  
  # DIFICULTAD 3 
  dif_3 <- svyby(~oficios, ~dificultad3, design_t6, svytotal, na.rm = TRUE) |> 
    arrange(desc(oficios)) |> dplyr::rename(dif_3 = oficios, se_3 = se)
  dif_3$cv_3 <- cv(dif_3)
  
  unique_dif <- unique(dif_3$dificultad3)
  labels_dif <- attr(dif_3$dificultad3, "labels")
  matched_labels <- names(labels_dif)[match(unique_dif, labels_dif)]
  dif_3$dif_label <- matched_labels[match(dif_3$dificultad3, unique_dif)]
  
  # join 
  dificultad_t6 <- dif_1 |> left_join(dif_2, by = "dif_label") |> left_join(dif_3, by = "dif_label") |> 
    select(dif_label, ends_with("1"), ends_with("2"), ends_with("3")) |> 
    arrange(dificultad1)
}

# ********************************************************************************** #
# ------------------------- REQUISITO/EXPERIENCIA/CANALES -------------------------- #
# ********************************************************************************** #

# ----------------------- LICENCIAS/CERTIFICADOS/REQUISITOS ------------------------ #
{
  licencia <- svyby(~oficios, ~licencia, design_long, svytotal, na.rm = TRUE) 
  requisito <- svyby(~oficios, ~requisito, design_long, svytotal, na.rm = TRUE) 
  certificado <- svyby(~oficios, ~certificado, design_long, svytotal, na.rm = TRUE) 
}
# --------------------------- AÑOS DE EXPERIENCIA MÍNIMO --------------------------- #
{
  exp_design <- subset(design_long, exp>0)
  
  exp_0 <- svyby(~oficios, ~exp, design_long, svytotal, na.rm = TRUE) |> 
    mutate(cv =se/oficios*100, 
           pc = oficios/sum(oficios)*100)
  
  exp_0_sector <- svyby(~oficios, ~exp_cat+a2, design_long, svytotal, na.rm = TRUE) |> 
    pivot_wider(names_from = exp_cat, values_from = c("oficios", "se"), names_glue = "{.value}_{exp_cat}")
  
  exp_mean <- svymean(~exp, exp_design, na.rm = TRUE)
  exp_mean$cv <- cv(exp_mean)
  

  exp_mean_sector <- svyby(~exp, ~a2, exp_design, svymean, na.rm = TRUE)
  exp_mean_sector$cv <- cv(exp_mean_sector)
  
  unique_dif <- unique(exp_mean_sector$a2)
  labels_dif <- attr(exp_mean_sector$a2, "labels")
  matched_labels <- names(labels_dif)[match(unique_dif, labels_dif)]
  exp_mean_sector$label <- matched_labels[match(exp_mean_sector$a2, unique_dif)]
  
  exp_mean_tam <- svyby(~exp, ~tam_val, exp_design, svymean, na.rm = TRUE)
  exp_mean_tam$cv <- cv(exp_mean_tam)
  
  unique_dif <- unique(exp_mean_sector$tam_val)
  labels_dif <- attr(exp_mean_sector$tam_val, "labels")
  matched_labels <- names(labels_dif)[match(unique_dif, labels_dif)]
  exp_mean_tam$label <- matched_labels[match(exp_mean_sector$tam_val, unique_dif)]
}
# ------------------------------------ EDUCACIÓN ----------------------------------- #
{
  educ_tot <- svyby(~oficios,~educ, design_long, svytotal, na.rm = TRUE)
  educ_tot$cv <- cv(educ_tot)
}
# ------------------------------------- CANALES ------------------------------------ #
{
  canal_1 <- svyby(~oficios, ~canal1, design_long, svytotal, na.rm = TRUE) |> 
    mutate(cv = se/oficios*100) |> arrange(desc(oficios)) |> 
    dplyr::rename(canal_1 = oficios,
                  se_1 = se,
                  cv_1 = cv)
  
  unique_canal <- unique(canal_1$canal1)
  labels_canal <- attr(canal_1$canal1, "labels")
  matched_labels <- names(labels_canal)[match(unique_canal, labels_canal)]
  canal_1$canal_label <- matched_labels[match(canal_1$canal1, unique_canal)]
  
  canal_2 <- svyby(~oficios, ~canal2, design_long, svytotal, na.rm = TRUE) |> 
    mutate(cv = se/oficios*100) |> arrange(desc(oficios)) |> 
    dplyr::rename(canal_2 = oficios,
                  se_2 = se,
                  cv_2 = cv)
  
  unique_canal <- unique(canal_2$canal2)
  labels_canal <- attr(canal_2$canal2, "labels")
  matched_labels <- names(labels_canal)[match(unique_canal, labels_canal)]
  canal_2$canal_label <- matched_labels[match(canal_2$canal2, unique_canal)]
  
  canal_3 <- svyby(~oficios, ~canal3, design_long, svytotal, na.rm = TRUE) |> 
    mutate(cv = se/oficios*100) |> arrange(desc(oficios)) |> 
    dplyr::rename(canal_3 = oficios,
                  se_3 = se,
                  cv_3 = cv)
  
  unique_canal <- unique(canal_3$canal3)
  labels_canal <- attr(canal_3$canal3, "labels")
  matched_labels <- names(labels_canal)[match(unique_canal, labels_canal)]
  canal_3$canal_label <- matched_labels[match(canal_3$canal3, unique_canal)]
  
  # join 
  canal <- canal_1 |> left_join(canal_2, by = "canal_label") |> left_join(canal_3, by = "canal_label") |> 
    select(canal_label, ends_with("1"), ends_with("2"), ends_with("3"))
}

# ********************************************************************************** #
# ---------------------- CONOCIMIENTO DE SUBSIDIOS/BENEFICIOS ---------------------- #
# ********************************************************************************** #
{
  # ----------------------------- INSTITUCIONES/SERVICIOS ---------------------------- #
  sercotec <- svyby(~empresas, ~d1r1, design_wide, svytotal, na.rm = TRUE) |> 
    mutate(pc = empresas/sum(empresas)) |> 
    dplyr::rename(respuesta = d1r1, emp_sercotec = empresas, se_sercotec = se, pc_sercotec = pc) 
  
  
  corfo <- svyby(~empresas, ~d1r2, design_wide, svytotal, na.rm = TRUE) |> 
    mutate(pc = empresas/sum(empresas)) |> 
    dplyr::rename(respuesta = d1r2, emp_corfo = empresas, se_corfo = se, pc_corfo = pc) 
  
  sence <- svyby(~empresas, ~d1r3, design_wide, svytotal, na.rm = TRUE) |> 
    mutate(pc = empresas/sum(empresas)) |> 
    dplyr::rename(respuesta = d1r3, emp_sence = empresas, se_sence = se, pc_sence = pc) 
  
  omil <- svyby(~empresas, ~d1r4, design_wide, svytotal, na.rm = TRUE) |> 
    mutate(pc = empresas/sum(empresas)) |> 
    dplyr::rename(respuesta = d1r4, emp_omil = empresas, se_omil = se, pc_omil = pc) 
  
  chileval <- svyby(~empresas, ~d1r5, design_wide, svytotal, na.rm = TRUE) |> 
    mutate(pc = empresas/sum(empresas)) |> 
    dplyr::rename(respuesta = d1r5, emp_chileval = empresas, se_chileval = se, pc_chileval = pc) 
  
  bne <- svyby(~empresas, ~d1r6, design_wide, svytotal, na.rm = TRUE) |> 
    mutate(pc = empresas/sum(empresas)) |> 
    dplyr::rename(respuesta = d1r6, emp_bne = empresas, se_bne = se, pc_bne = pc) 
  
  con_1 <- sercotec |> 
    left_join(corfo, by = c("respuesta")) |> 
    left_join(sence, by = c("respuesta")) |> 
    left_join(omil, by = c("respuesta")) |>
    left_join(chileval, by = c("respuesta")) |> 
    left_join(bne, by = c("respuesta")) |> 
    pivot_longer(cols = -respuesta, names_to = c(".value", "programa"), names_pattern = "(emp|se|pc)_(.*)")
    
  # ------------------------------- PROGRAMAS/SUBSIDIOS ------------------------------ #
  despega  <- svyby(~empresas, ~d2r1, design_wide, svytotal, na.rm = TRUE) |> 
    mutate(pc = empresas/sum(empresas)) |>
    dplyr::rename(respuesta = d2r1, emp_despega = empresas, se_despega = se, pc_despega = pc) 
  
  sej      <- svyby(~empresas, ~d2r2, design_wide, svytotal, na.rm = TRUE) |> 
    mutate(pc = empresas/sum(empresas)) |>
    dplyr::rename(respuesta = d2r2, emp_sej = empresas, se_sej = se, pc_sej = pc) 
  
  btm      <- svyby(~empresas, ~d2r3, design_wide, svytotal, na.rm = TRUE) |> 
    mutate(pc = empresas/sum(empresas)) |>
    dplyr::rename(respuesta = d2r3, emp_btm = empresas, se_btm = se, pc_btm = pc) 
  
  expmayor <- svyby(~empresas, ~d2r4, design_wide, svytotal, na.rm = TRUE) |> 
    mutate(pc = empresas/sum(empresas)) |>
    dplyr::rename(respuesta = d2r4, emp_expmayor = empresas, se_expmayor = se, pc_expmayor = pc) 
  
  aprendiz <- svyby(~empresas, ~d2r5, design_wide, svytotal, na.rm = TRUE) |> 
    mutate(pc = empresas/sum(empresas)) |>
    dplyr::rename(respuesta = d2r5, emp_aprendiz = empresas, se_aprendiz = se, pc_aprendiz = pc) 
  
  ft       <- svyby(~empresas, ~d2r6, design_wide, svytotal, na.rm = TRUE) |> 
    mutate(pc = empresas/sum(empresas)) |>
    dplyr::rename(respuesta = d2r6, emp_ft = empresas, se_ft = se, pc_ft = pc) 
  
  ferias   <- svyby(~empresas, ~d2r7, design_wide, svytotal, na.rm = TRUE) |> 
    mutate(pc = empresas/sum(empresas)) |>
    dplyr::rename(respuesta = d2r7, emp_ferias = empresas, se_ferias = se, pc_ferias = pc) 
  
  digpyme  <- svyby(~empresas, ~d2r8, design_wide, svytotal, na.rm = TRUE) |> 
    mutate(pc = empresas/sum(empresas)) |>
    dplyr::rename(respuesta = d2r8, emp_digpyme = empresas, se_digpyme = se, pc_digpyme = pc) 
  
  fosis    <- svyby(~empresas, ~d2r9, design_wide, svytotal, na.rm = TRUE) |> 
    mutate(pc = empresas/sum(empresas)) |>
    dplyr::rename(respuesta = d2r9, emp_fosis = empresas, se_fosis = se, pc_fosis = pc) 
  
  con_2 <- despega |> 
    left_join(sej, by = c("respuesta")) |> 
    left_join(btm, by = c("respuesta")) |> 
    left_join(expmayor, by = c("respuesta")) |> 
    left_join(aprendiz, by = c("respuesta")) |> 
    left_join(ft, by = c("respuesta")) |> 
    left_join(ferias, by = c("respuesta")) |> 
    left_join(digpyme, by = c("respuesta")) |> 
    left_join(fosis, by = c("respuesta")) |> 
    pivot_longer(cols = -respuesta, names_to = c(".value", "programa"), names_pattern = "(emp|se|pc)_(.*)")
}

# ********************************************************************************** #
# -------------------------------- GUARDAR EN EXCEL -------------------------------- #
# ********************************************************************************** #


write_xlsx(list("pc_acteco"= pc_acteco, #porcentaje por actividad económica
                "pc_region" = pc_region, #porcentaje por región
                "pc_tam"    = pc_tam, #porcentaje por tamaño (n° trabajadores)
                "pc_tamvent"= pc_tamvent), #porcentaje por tamaño (ventas)
           paste0("Tablas/porcentajes.xlsx"))


write_xlsx(list("contratos_totales" = contratos_sector,#contratos por sector
                "contratados_u12"        = cu12,#Contratado últimos 12 meses
                "contratados_u12_sector" = cu12_sector,#contratado últimos 12 meses por sector
                "contratados_u12_constr" = cu12_t5,#contratados últimos 12 meses construcción
                "contratados_u12_agro" = cu12_t6, #contratados últimos 12 meses agro
                "vacantes_totales_sector" = vnll_sector, #vacantes no llenadas por sector
                "vacantes_tot" = odc, 
                "vacantes_constr" = odc_t5,
                "vacantes_agro" = odc_t6,
                "vacantes_sector" = odc_sector,
                "vacantes_region" = odc_region,
                "vacantes_tam" = odc_tam,
                "vacantes_tamvent" = odc_tamvent,
                "dificultad" = dificultad,
                "dificultad_tot" = dificultad_tot,
                "dificultad_constr" = dificultad_t5,
                "dificultad_agro" = dificultad_t6,
                "exp_mean_sector" = exp_mean_sector,
                "exp_0_sector" = exp_0_sector,
                "exp_mean_tam" = exp_mean_tam,
                "educ_tot" = educ_tot,
                "canal" = canal,
                "conocimiento_1" = con_1,
                "conocimiento_2" = con_2),
           paste0("Tablas/contratos_vacantes_dificultad_canal_conocimiento.xlsx"))


#distribución del personal contratado b1-b12 

