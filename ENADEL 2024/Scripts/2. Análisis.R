############################Análisis##################
#packs
pacman::p_load("tidyverse", "dplyr", "plyr", "knitr", "haven", "readxl", "writexl", "Hmisc", "survey", "labelled", "archive")
#datos
enadel <- read_dta("Bases/enadel_2024.dta")
# Dejar -88 como perdido
enadel <- enadel %>%
  mutate_at(vars(starts_with("b")), ~ ifelse(is.numeric(.) & . == -88, NA, .))



#########################Diseño mnuestral######################################
#Revisar con las variables definitivas
#svy
options(survey.lonely.psu="adjust")
design_enadel_2024 <- svydesign(ids = ~id_emp, strata =~estrato ,  weights = ~fact_emp, data = enadel, nest = TRUE)

 #Cantidad de empresas
 tot_folios <- svytotal(~empresas, design_enadel_2024, na.rm = TRUE)[1]
 tot_folios_muestra <- length(unique(enadel$id_emp))

 # total trabajadores 
  tot_trab <- svytotal(~b6_1, design_enadel_2024, na.rm = TRUE)[1]
  tot_trab_muestra <- sum(enadel$b6_1)
  
  
  # empresas por sector
  folios_sector <- svyby(~empresas, ~a7, design_enadel_2024, svytotal, na.rm = TRUE)[1:2]
  folios_sector_muestra <- enadel |> group_by(a7) |> 
    dplyr::summarise(tot_folios = n())
  
  # trabajadores por sector
  trab_sector <- svyby(~b6_1, ~a7, design_enadel_2024, svytotal, na.rm = TRUE)[1:2]
  trab_sector_muestra <- enadel |> group_by(a7) |> 
    dplyr::summarise(tot_trab = sum(b6_1))
  
  cuadro_resumen <- data.frame(tipo = c(names(attr(folios_sector$a7, "labels")),"Total"),                                            
                               empresas_muestra = c(folios_sector_muestra$tot_folios, tot_trab),
                               empresas_expandidos = c(folios_sector$empresas, tot_folios),
                               trab_muestra = c(trab_sector_muestra$tot_trab, tot_trab_muestra),
                               trab_expandidos = c(trab_sector$b6_1, tot_trab))

  
  # --- % por region 
  # empresas por region
  folios_region <- svyby(~empresas, ~a6, design_enadel_2024, svytotal, na.rm = TRUE)[1:2]
  
  orden_regiones <- c(
    "Arica y Parinacota", "Tarapacá", "Antofagasta", "Atacama", 
    "Coquimbo", "Valparaíso", "Metropolitana", "O'Higgins", 
    "Maule", "Ñuble", "Biobío", "Araucanía", 
    "Los Ríos", "Los Lagos", "Aysén", "Magallanes"
  )

  # trabajadores por region 
  trab_region <- svyby(~b6_1, ~a6, design_enadel_2024, svytotal, na.rm = TRUE)[1:2]

  pc_region <- data.frame (region = c(orden_regiones),
                           empresas = folios_region$empresas,
                           trabajadores = trab_region$b6_1) |> 
    mutate(pc_empresas = empresas/sum(empresas),
           pc_trabajadores = trabajadores/sum(trabajadores),
           region= factor(region, levels = orden_regiones)
    ) |>
    arrange(region)
  
  # --- % por tamaño
  # empresas por tamaño
  tam_tot <- svyby(~empresas, ~tam_trab, design_enadel_2024, svytotal, na.rm = TRUE)[1:2]
  # trabajadores por tamaño
  tam_trab_tot <- svyby(~b6_1, ~tam_trab, design_enadel_2024, svytotal, na.rm = TRUE)[1:2]
  
  
  
  pc_tam <- data.frame (tam_trab = tam_tot$tam_trab,
                        empresas = tam_tot$empresas,
                        trabajadores = tam_trab_tot$b6_1) |> 
    mutate(pc_empresas = empresas/sum(empresas),
           pc_trabajadores = trabajadores/sum(trabajadores),
           tam_label = case_when(tam_trab == 2 ~ "Pequeña", 
                                 tam_trab == 3 ~ "Mediana", 
                                 tam_trab == 4 ~ "Grande")) |> 
    select(tam_label, empresas, trabajadores, pc_empresas, pc_trabajadores)
  

  # --- % por tamaño de ventas
  # empresas por tamaño
  tamvent_tot <- svyby(~empresas, ~a3, design_enadel_2024, svytotal, na.rm = TRUE)[1:2]
  # trabajadores por tamaño
  tamvent_trab_tot <- svyby(~b6_1, ~a3, design_enadel_2024, svytotal, na.rm = TRUE)[1:2]
  
  enadel$a3 <- as.factor(enadel$a3)
  
  enadel$a3 <- factor(enadel$a3, levels = c(1,2,3,4,5), labels = c("Sin ventas 2023","Micro","Pequeña","Mediana","Grande" ))
  
  
  pc_tamvent <- data.frame(a3 = tamvent_tot$a3,
                           empresas = tamvent_tot$empresas,
                           trabajadores = tamvent_trab_tot$b6_1) |>
    mutate(pc_empresas = empresas / sum(empresas),
           pc_trabajadores = trabajadores / sum(trabajadores)) |>
    select(a3, empresas, trabajadores, pc_empresas, pc_trabajadores)
  
  # --- % por sector #Aquí hay que cambiar a2!! por sector económico codificado------------------------------- aqui voy
  
  # empresas por sector
  acteco_tot <- svyby(~empresas, ~a2, design_enadel_2024, svytotal, na.rm = TRUE)[1:2]


  # trabajadores por sector
  acteco_trab_tot <- svyby(~b6_1, ~a2, design_enadel_2024, svytotal, na.rm = TRUE)[1:2]
  
  pc_acteco <- data.frame (act_eco = names(attr(acteco_tot$a2, "labels")[1:14]),
                           empresas = acteco_tot$empresas,
                           trabajadores = acteco_trab_tot$b6_1) |> 
    mutate(pc_empresas = empresas/sum(empresas),
           pc_trabajadores = trabajadores/sum(trabajadores)) |> 
    select(act_eco, empresas, trabajadores, pc_empresas, pc_trabajadores)

  
#Región Sucursales ----
  
  # Lista 
  region_vars <- paste0("a5_", 1:16)
  # Crear una fórmula con todas las variables de región
  formula_totales <- as.formula(paste("~", paste(region_vars, collapse = "+")))
  # Calcular los totales ponderados
  totales_regiones <- svytotal(formula_totales, design = design_enadel_2024, na.rm = TRUE)
  totales_regiones_df <- as.data.frame(totales_regiones)
  # Nombres de  regiones
  totales_regiones_df$Region <- rownames(totales_regiones_df)
  # Nombres de regiones
  nombres_regiones <- c("Arica y Parinacota", "Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", "Región Metropolitana de Santiago", "Libertador General Bernardo O'Higgins",
                        "Maule", "Ñuble", "Bío-Bío", "La Araucanía", "Los Ríos", "Los Lagos", "Aisén del General Carlos Ibáñez del Campo", "Magallanes y Antártica Chilena")
  totales_regiones_df$Region <- nombres_regiones
  totales_regiones_df <- totales_regiones_df |> mutate(total= round(total,0),
                                                       porc = total/ sum(total) )
  
  
  #subcontrato
  subcontratos <- svyby(~empresas, ~a8, design_enadel_2024, svytotal, na.rm = TRUE) [1:2]
  
  #subcontratos_sector
  subcontratos_sector <- svyby(~empresas, ~a8+a2, design_enadel_2024, svytotal, na.rm = TRUE)[1:3] |>
    pivot_wider(names_from = "a8", values_from = "empresas", names_glue = "empresas_{a8}") |> 
    mutate(tot_empresas = empresas_1+empresas_2) |> 
    dplyr::rename(subcontrato_1 = empresas_1) |> 
    dplyr::rename(subcontrato_2 = empresas_2)
  unique_subcontrato <- unique(subcontratos_sector$a2)
  labels_subcontrato <- attr(subcontratos_sector$a2, "labels")
  matched_labels <- names(labels_subcontrato)[match(unique_subcontrato, labels_subcontrato)]
  subcontratos_sector$sector_label <- matched_labels[match(subcontratos_sector$a2, unique_subcontrato)]
  
  #subcontratos_region
  subcontratos_region <- svyby(~empresas, ~a8+reg_muestra, design_enadel_2024, svytotal, na.rm = TRUE)[1:3] |>
    pivot_wider(names_from = "a8", values_from = "empresas", names_glue = "empresas_{a8}") |> 
    mutate(tot_empresas = empresas_1+empresas_2) |> 
    dplyr::rename(subcontrato_1 = empresas_1) |> 
    dplyr::rename(subcontrato_2 = empresas_2)
  unique_subcontrato <- unique(subcontratos_region$reg_muestra)
  labels_subcontrato <- attr(subcontratos_region$reg_muestra, "labels")
  matched_labels <- names(labels_subcontrato)[match(unique_subcontrato, labels_subcontrato)]
  subcontratos_region$region_label <- matched_labels[match(subcontratos_region$reg_muestra, unique_subcontrato)]
  
  
  #Conglomerado
  conglomerados <- svyby(~empresas, ~a9, design_enadel_2024, svytotal, na.rm = TRUE) [1:2]
  
  #Conglomerados_sector
  conglomerados_sector <- svyby(~empresas, ~a9+a2, design_enadel_2024, svytotal, na.rm = TRUE)[1:3] |>
    pivot_wider(names_from = "a9", values_from = "empresas", names_glue = "empresas_{a9}") |> 
    mutate(tot_empresas = empresas_1+empresas_2) |> 
    dplyr::rename(conglomerado_1 = empresas_1) |> 
    dplyr::rename(conglomerado_2 = empresas_2)
  unique_conglomerado <- unique(conglomerados_sector$a2)
  labels_conglomerado <- attr(conglomerados_sector$a2, "labels")
  matched_labels <- names(labels_conglomerado)[match(unique_conglomerado, labels_conglomerado)]
  conglomerados_sector$sector_label <- matched_labels[match(conglomerados_sector$a2, unique_conglomerado)]
  
  #conglomerados_region
  conglomerados_region <- svyby(~empresas, ~a9+reg_muestra, design_enadel_2024, svytotal, na.rm = TRUE)[1:3] |>
    pivot_wider(names_from = "a9", values_from = "empresas", names_glue = "empresas_{a9}") |> 
    mutate(tot_empresas = empresas_1+empresas_2) |> 
    dplyr::rename(conglomerado_1 = empresas_1) |> 
    dplyr::rename(conglomerado_2 = empresas_2)
  unique_conglomerado <- unique(conglomerados_region$reg_muestra)
  labels_conglomerado <- attr(conglomerados_region$reg_muestra, "labels")
  matched_labels <- names(labels_conglomerado)[match(unique_conglomerado, labels_conglomerado)]
  conglomerados_region$region_label <- matched_labels[match(conglomerados_region$reg_muestra, unique_conglomerado)]
  
  
  #Gremios
  gremios <- svyby(~empresas, ~a10, design_enadel_2024, svytotal, na.rm=TRUE) [1:2]
  
  #Gremios_sector
  gremios_sector <- svyby(~empresas, ~a10+a2, design_enadel_2024, svytotal, na.rm = TRUE)[1:3] |>
    pivot_wider(names_from = "a10", values_from = "empresas", names_glue = "empresas_{a10}") |> 
    mutate(tot_empresas = empresas_1+empresas_2) |> 
    dplyr::rename(gremios_1 = empresas_1) |> 
    dplyr::rename(gremios_2 = empresas_2)
  unique_gremio <- unique(gremios_sector$a2)
  labels_gremio <- attr(gremios_sector$a2, "labels")
  matched_labels <- names(labels_gremio)[match(unique_gremio, labels_gremio)]
  gremios_sector$sector_label <- matched_labels[match(gremios_sector$a2, unique_gremio)]
  gremios_sector<- gremios_sector |> select(sector=a2, gremios_1, gremios_2, tot_empresas, sector_label)
  
  #gremios_region
  gremios_region <- svyby(~empresas, ~a10+reg_muestra, design_enadel_2024, svytotal, na.rm = TRUE)[1:3] |>
    pivot_wider(names_from = "a10", values_from = "empresas", names_glue = "empresas_{a10}") |> 
    mutate(tot_empresas = empresas_1+empresas_2) |> 
    dplyr::rename(gremio_1 = empresas_1) |> 
    dplyr::rename(gremio_2 = empresas_2)
  unique_gremio <- unique(gremios_region$reg_muestra)
  labels_gremio <- attr(gremios_region$reg_muestra, "labels")
  matched_labels <- names(labels_gremio)[match(unique_gremio, labels_gremio)]
  gremios_region$region_label <- matched_labels[match(gremios_region$reg_muestra, unique_gremio)]

  
  # --------------Capítulo II: Demanda Laboral y Ocupaciones de Difícil Cobertura ----------------------#

  #Dotación (personal contratado)
  
  #Indefinido (b1)
  indefinido_total <- svytotal(~b1_1, design_enadel_2024, na.rm = TRUE)
  indefinido_mujer <- svytotal(~b1_2, design_enadel_2024, na.rm = TRUE)
  indefinido_hombre <- svytotal (~b1_3, design_enadel_2024, na.rm = TRUE)
  
  indefinido_total<- cbind(indefinido_total,indefinido_mujer,indefinido_hombre)# total = Mujer + hombre
  
  
 
  
  
  
  

  
  
  
  procesar_cargos_agrupados <- function(design) {
    # Extraer los datos originales del diseño
    data <- design$variables
    
    # Transformar las variables de nombres y contrataciones a formato largo
    cargos_largos <- data %>%
      select(id_emp, starts_with("c1_a_"), starts_with("c1_c_")) %>%
      pivot_longer(
        cols = -id_emp,  # Excluir id_emp de la transformación
        names_to = c(".value", "grupo"),
        names_pattern = "c1_(a|c)_(\\d+)"
      ) %>%
      filter(!is.na(a), c > 0) %>%  # Filtrar nombres no válidos y cantidades <= 0
      mutate(nombre_cargo = a, cantidad = c) %>%  # Crear columnas explícitamente
      select(-a, -c)  # Eliminar las columnas originales
    
    # Asociar con las variables necesarias del diseño
    cargos_completos <- cargos_largos %>%
      left_join(data %>% select(id_emp, estrato, fact_emp), by = "id_emp")  # Combinar por id_emp
    
    return(cargos_completos)  # Devolver cargos_completos para inspección
  }
  
  # Ejecutar y guardar el resultado
  cargos_completos <- procesar_cargos_agrupados(design_enadel_2024)
  
  
  # Filtrar valores extremos en cantidad
  cargos_completos <- cargos_completos %>%
    filter(cantidad <= quantile(cantidad, 0.99))
  
  # Crear un diseño muestral ajustado sin valores extremos
  design_cargos <- svydesign(
    ids = ~id_emp,
    strata = ~estrato,
    weights = ~fact_emp,
    data = cargos_completos,
    nest = TRUE
  )
  
  # Calcular el total ponderado por cargo
  resultado_ponderado <- svyby(
    ~cantidad,
    ~nombre_cargo,
    design = design_cargos,
    svytotal,
    na.rm = TRUE
  )
  
  # Mostrar los resultados
  print(as.data.frame(resultado_ponderado))
  
  
  
  
  #aHORA ES d2 CON d3 #######33
  
  enadel_2<-enadel |> filter(enadel$d2==2) 
  
  
 table(enadel_2$d3_a_1)
  
  
  
  
  

  
  
  # por ocupación
  indefinido <- svyby(~b1_1, ~c1, design_enadel_2024, svytotal, na.rm = TRUE) |> arrange(desc(b1_1))
  indefinido$cv <- cv(indefinido)
  # labels
  unique_oficio <- unique(indefinido$c1)
  labels_oficio <- attr(indefinido$c1, "labels")
  matched_labels <- names(labels_oficio)[match(unique_oficio, labels_oficio)]
  indefinido$oficio_label <- matched_labels[match(indefinido$c1, unique_oficio)]
  
  #por ocupación y sector
  indefinido_sector <- svyby(~b1_1, ~c1+a2, design_enadel_2024, svytotal, na.rm = TRUE) |> arrange(desc(b1_1))
  indefinido_sector$cv <- cv(indefinido_sector)
  # labels oficio
  unique_oficio <- unique(indefinido_sector$c1)
  labels_oficio <- attr(indefinido_sector$c1, "labels")
  matched_labels <- names(labels_oficio)[match(unique_oficio, labels_oficio)]
  indefinido_sector$oficio_label <- matched_labels[match(indefinido_sector$c1, unique_oficio)]
  
  # labels sector
  unique_sector <- unique(indefinido_sector$a2)
  labels_sector <- attr(indefinido_sector$a2, "labels")
  matched_labels <- names(labels_sector)[match(unique_sector, labels_sector)]
  indefinido_sector$sector_label <- matched_labels[match(indefinido_sector$a2, unique_sector)]
  
  indefinido_sector <- indefinido_sector |> arrange(sector_label, desc(b1_1)) |> 
    subset(b1_1>0) |> select(sector_label, a2, oficio_label, c1, b1_1, se, cv)
  
  
  
  # Plazo fijo, o por obra o faena (b2)
  plazofijo_total <- svytotal(~b2_1, design_enadel_2024, na.rm = TRUE)
  plazofijo_mujer <- svytotal (~b2_2,design_enadel_2024, na.rm = TRUE)
  plazofijo_hombre <-svytotal (~b2_3,design_enadel_2024, na.rm = TRUE)
  
  plazofijo_total<- cbind(plazofijo_total,plazofijo_mujer,plazofijo_hombre)# total = Mujer + hombre
  
  # por ocupación
  plazofijo <- svyby(~b2_1, ~c1, design_enadel_2024, svytotal, na.rm = TRUE) |> arrange(desc(b2_1))
  plazofijo$cv <- cv(plazofijo)
  # labels
  unique_oficio <- unique(plazofijo$c1)
  labels_oficio <- attr(plazofijo$c1, "labels")
  matched_labels <- names(labels_oficio)[match(unique_oficio, labels_oficio)]
  plazofijo$oficio_label <- matched_labels[match(plazofijo$c1, unique_oficio)]
  
  #por ocupación y sector
  plazofijo_sector <- svyby(~b2_1, ~c1+a2, design_enadel_2024, svytotal, na.rm = TRUE) |> arrange(desc(b2_1))
  plazofijo_sector$cv <- cv(plazofijo_sector)
  # labels oficio
  unique_oficio <- unique(plazofijo_sector$c1)
  labels_oficio <- attr(plazofijo_sector$c1, "labels")
  matched_labels <- names(labels_oficio)[match(unique_oficio, labels_oficio)]
  plazofijo_sector$oficio_label <- matched_labels[match(plazofijo_sector$c1, unique_oficio)]
  
  # labels sector
  unique_sector <- unique(plazofijo_sector$a2)
  labels_sector <- attr(plazofijo_sector$a2, "labels")
  matched_labels <- names(labels_sector)[match(unique_sector, labels_sector)]
  plazofijo_sector$sector_label <- matched_labels[match(plazofijo_sector$a2, unique_sector)]
  
  plazofijo_sector <- plazofijo_sector |> arrange(sector_label, desc(b2_1)) |> 
    subset(b2_1>0) |> select(sector_label, a2, oficio_label, c1, b2_1, se, cv)
  
  #Personal a Honorarios(b4)
  honorario_total <- svytotal(~b4_1, design_enadel_2024, na.rm = TRUE)
  honorario_mujer <- svytotal (~b4_2,design_enadel_2024, na.rm = TRUE)
  honorario_hombre <-svytotal (~b4_3,design_enadel_2024, na.rm = TRUE)
  
  honorario_total<- cbind(honorario_total,honorario_mujer,honorario_hombre)# total = Mujer + hombre
  # por ocupación
  honorario <- svyby(~b4_1, ~c1, design_enadel_2024, svytotal, na.rm = TRUE) |> arrange(desc(b4_1))
  honorario$cv <- cv(honorario)
  # labels
  unique_oficio <- unique(honorario$c1)
  labels_oficio <- attr(honorario$c1, "labels")
  matched_labels <- names(labels_oficio)[match(unique_oficio, labels_oficio)]
  honorario$oficio_label <- matched_labels[match(honorario$c1, unique_oficio)]
  
  #por ocupación y sector
  honorario_sector <- svyby(~b4_1, ~c1+a2, design_enadel_2024, svytotal, na.rm = TRUE) |> arrange(desc(b4_1))
  honorario_sector$cv <- cv(honorario_sector)
  # labels oficio
  unique_oficio <- unique(honorario_sector$c1)
  labels_oficio <- attr(honorario_sector$c1, "labels")
  matched_labels <- names(labels_oficio)[match(unique_oficio, labels_oficio)]
  honorario_sector$oficio_label <- matched_labels[match(honorario_sector$c1, unique_oficio)]
  
  # labels sector
  unique_sector <- unique(honorario_sector$a2)
  labels_sector <- attr(honorario_sector$a2, "labels")
  matched_labels <- names(labels_sector)[match(unique_sector, labels_sector)]
  honorario_sector$sector_label <- matched_labels[match(honorario_sector$a2, unique_sector)]
  
  honorario_sector <- honorario_sector |> arrange(sector_label, desc(b4_1)) |> 
    subset(b4_1>0) |> select(sector_label, a2, oficio_label, c1, b4_1, se, cv)

  #Dotación total (b6)
  dotacion_total <- svytotal(~b6_1, design_enadel_2024, na.rm = TRUE)
  dotacion_mujer <- svytotal (~b6_2,design_enadel_2024, na.rm = TRUE)
  dotacion_hombre <-svytotal (~b6_3,design_enadel_2024, na.rm = TRUE)
  
  dotacion_total<- cbind(dotacion_total,dotacion_mujer,dotacion_hombre)# total = Mujer + hombre
  # por ocupación
  dotacion <- svyby(~b6_1, ~c1, design_enadel_2024, svytotal, na.rm = TRUE) |> arrange(desc(b6_1))
  dotacion$cv <- cv(dotacion)
  # labels
  unique_oficio <- unique(dotacion$c1)
  labels_oficio <- attr(dotacion$c1, "labels")
  matched_labels <- names(labels_oficio)[match(unique_oficio, labels_oficio)]
  dotacion$oficio_label <- matched_labels[match(dotacion$c1, unique_oficio)]
  
  #por ocupación y sector
  dotacion_sector <- svyby(~b6_1, ~c1+a2, design_enadel_2024, svytotal, na.rm = TRUE) |> arrange(desc(b6_1))
  dotacion_sector$cv <- cv(dotacion_sector)
  # labels oficio
  unique_oficio <- unique(dotacion_sector$c1)
  labels_oficio <- attr(dotacion_sector$c1, "labels")
  matched_labels <- names(labels_oficio)[match(unique_oficio, labels_oficio)]
  dotacion_sector$oficio_label <- matched_labels[match(dotacion_sector$c1, unique_oficio)]
  
  # labels sector
  unique_sector <- unique(dotacion_sector$a2)
  labels_sector <- attr(dotacion_sector$a2, "labels")
  matched_labels <- names(labels_sector)[match(unique_sector, labels_sector)]
  dotacion_sector$sector_label <- matched_labels[match(dotacion_sector$a2, unique_sector)]
  
  dotacion_sector <- dotacion_sector |> arrange(sector_label, desc(b6_1)) |> 
    subset(b6_1>0) |> select(sector_label, a2, oficio_label, c1, b6_1, se, cv)
  
  
  #Cuadro resumen por hombre y mujer
  
  dotacion_resumen<- rbind(indefinido_total, plazofijo_total,honorario_total, dotacion_total)
  
  dotacion_resumen<- as.data.frame(dotacion_resumen) 
  
  dotacion_resumen <- dotacion_resumen |> 
    mutate(pc_mujer = (indefinido_mujer / indefinido_total) * 100,
           pc_hombre = (indefinido_hombre / indefinido_total) * 100)
  
  colnames(dotacion_resumen) <- c("Total", "Mujer", "Hombre", "% mujer", "% hombre")
  
  dotacion_resumen <- dotacion_resumen %>%
    mutate(Categoria = c("Indefinido", "Plazo fijo", "Honorario", "Dotación total")) %>%
    relocate(Categoria, .before = Total)
 

  #Demanda
  #Vacantes actuales
  vacantes_actuales_total <- svytotal(~b7_1, design_enadel_2024, na.rm = TRUE)
  vacantes_actuales_total<- as.data.frame(vacantes_actuales_total)
   #Vacantes futuras
  vacantes_futuras_actual <- svytotal(~b8_1, design_enadel_2024, na.rm = TRUE)#este año
  vacantes_futuras_actual <- as.data.frame(vacantes_futuras_actual)
  vacantes_futuras_prox_ano <- svytotal(~b9_1, design_enadel_2024, na.rm = TRUE)
  vacantes_futuras_prox_ano <- as.data.frame(vacantes_futuras_prox_ano)
  
  vacantes_actuales_futuras <- cbind(vacantes_actuales_total,vacantes_futuras_actual,vacantes_futuras_prox_ano)
  colnames(vacantes_actuales_futuras)<- c("actuales","b7_1", "futuras_actuales","b8_1", "futuras_prox_ano", "b9_1")
  vacantes_actuales_futuras<- vacantes_actuales_futuras |> select(-c(b7_1, b8_1, b9_1))
  
  #Salidas
  renuncia_total <- svytotal(~b10_1, design_enadel_2024, na.rm = TRUE)
  renuncia_mujer <- svytotal (~b10_2,design_enadel_2024, na.rm = TRUE)
  renuncia_hombre <-svytotal (~b10_3,design_enadel_2024, na.rm = TRUE)
  
  renuncia_total<- cbind(renuncia_total,renuncia_mujer,renuncia_hombre)# total = Mujer + hombre 
  renuncia_total<- as.data.frame(renuncia_total)
  #despidos y ceses (Ceses, terminaciones de empleados/as  permanentes, de corto plazo o estacionales.)
  despidos_total <- svytotal(~b11_1, design_enadel_2024, na.rm = TRUE)
  despidos_mujer <- svytotal (~b11_2,design_enadel_2024, na.rm = TRUE)
  despidos_hombre <-svytotal (~b11_3,design_enadel_2024, na.rm = TRUE)
  
  despidos_total<- cbind(despidos_total,despidos_mujer,despidos_hombre)# total = Mujer + hombre 
  despidos_total <- as.data.frame(despidos_total)
  #Contrataciones
  contrataciones_total <- svytotal(~b12_1, design_enadel_2024, na.rm = TRUE)
  contrataciones_mujer <- svytotal (~b12_2,design_enadel_2024, na.rm = TRUE)
  contrataciones_hombre <-svytotal (~b12_3,design_enadel_2024, na.rm = TRUE)
  
  contrataciones_total<- cbind(contrataciones_total,contrataciones_mujer,contrataciones_hombre)
  contrataciones_total <- as.data.frame(contrataciones_total)
  
  #vacantes que no pudo llenar
  vacantes_total <- svytotal(~b13_1, design_enadel_2024, na.rm = TRUE) #da un n° negativo
  vacantes_total <- as.data.frame(vacantes_total)
  
  #-----------------------Contrataciones últimos 12 meses------------------------#
  cu12_total <- svytotal(~c1_c_1, design_enadel_2024, na.rm = TRUE)
  cu12 <- svyby(~c1_c_1, ~ c1, design_enadel_2024, svytotal, na.rm = TRUE)|> arrange(desc(c1_c_1))
  cu12$cv <- cv(cu12)
  # labels
  unique_oficio <- unique(cu12$c1)
  labels_oficio <- attr(cu12$c1, "labels")
  matched_labels <- names(labels_oficio)[match(unique_oficio, labels_oficio)]
  cu12$oficio_label <- matched_labels[match(cu12$c1, unique_oficio)]
  
  
  # por ocupación y sector 
  cu12_sector <- svyby(~c1_c_1, ~c1+a2, design_enadel_2024, svytotal, na.rm = TRUE) |> arrange(desc(c1_c_1))
  cu12_sector$cv <- cv(cu12_sector)
  # labels oficio
  unique_oficio <- unique(cu12_sector$c1)
  labels_oficio <- attr(cu12_sector$c1, "labels")
  matched_labels <- names(labels_oficio)[match(unique_oficio, labels_oficio)]
  cu12_sector$oficio_label <- matched_labels[match(cu12_sector$c1, unique_oficio)]
  # labels sector
  unique_sector <- unique(cu12_sector$a2)
  labels_sector <- attr(cu12_sector$a2, "labels")
  matched_labels <- names(labels_sector)[match(unique_sector, labels_sector)]
  cu12_sector$sector_label <- matched_labels[match(cu12_sector$a2, unique_sector)]
  
  
  cu12_sector <- cu12_sector |> arrange(sector_label, desc(c1_c_1)) |> 
    subset(c1_c_1>0) |> select(sector_label, a2, oficio_label, c1, c1_c_1, se, cv)
  
  # Contratados ultimos 12 meses nivel educacional
  cu12_educ <- svyby(~c1_c_1, ~c1_d_1, design_enadel_2024, svytotal, na.rm = TRUE)  |> arrange(desc(c1_c_1))
  cu12_educ$cv <- cv(cu12_educ)
  unique_educ <- unique(cu12_educ$c1_d_1)
  labels_educ <- attr(cu12_educ$c1_d_1, "labels")
  matched_labels <- names(labels_educ)[match(unique_educ, labels_educ)]
  cu12_educ$educ_label <- matched_labels[match(cu12_educ$c1_d_1, unique_educ)]
  cu12_educ <- cu12_educ |> select(c1_c_1, educ_label,  se, cv) #contratados ultimos 12 meses por nivel de educación
  
  # ------------------------------------ EDUCACIÓN ----------------------------------- #
    educ_tot <- svyby(~oficios,~c1_d_1, design_enadel_2024, svytotal, na.rm = TRUE)
    educ_tot$cv <- cv(educ_tot)
  # ------------------------------------Licencias----------------------------------- #  
  
    certificaciones_tot <- svyby(~oficios,~c1_f_cert1_1, design_enadel_2024, svytotal, na.rm = TRUE)
    certificaciones_tot$cv <- cv(certificaciones_tot)

    
    cu12_educ <- svyby(~c1_c_1, ~c1_d_1, design_enadel_2024, svytotal, na.rm = TRUE)  |> arrange(desc(c1_c_1))
    cu12_educ$cv <- cv(cu12_educ)
    
    unique_cert <- unique(certificaciones_tot$c1_f_cert1_1     )
    labels_cert <- attr(certificaciones_tot$c1_f_cert1_1     , "labels")
    matched_labels <- names(labels_cert)[match(unique_cert, labels_cert)]
    certificaciones_tot$cert_label <- matched_labels[match(certificaciones_tot$c1_f_cert1_1     , unique_cert)]
    certificaciones_tot <- certificaciones_tot |> select(oficios, cert_label,  se, cv)

    
    # ------------------------------------- CANALES ------------------------------------ #
    {
      canal_1 <- svyby(~oficios, ~c2_1, design_enadel_2024, svytotal, na.rm = TRUE) |> 
        mutate(cv = se/oficios*100) |> arrange(desc(oficios)) |> 
        dplyr::rename(canal_1 = oficios,
                      se_1 = se,
                      cv_1 = cv)
      
      unique_canal <- unique(canal_1$c2_1)
      labels_canal <- attr(canal_1$c2_1, "labels")
      matched_labels <- names(labels_canal)[match(unique_canal, labels_canal)]
      canal_1$canal_label <- matched_labels[match(canal_1$c2_1, unique_canal)]
      
      canal_2 <- svyby(~oficios, ~c2_2, design_enadel_2024, svytotal, na.rm = TRUE) |> 
        mutate(cv = se/oficios*100) |> arrange(desc(oficios)) |> 
        dplyr::rename(canal_2 = oficios,
                      se_2 = se,
                      cv_2 = cv)
      
      unique_canal <- unique(canal_2$c2_2)
      labels_canal <- attr(canal_2$c2_2, "labels")
      matched_labels <- names(labels_canal)[match(unique_canal, labels_canal)]
      canal_2$canal_label <- matched_labels[match(canal_2$c2_2, unique_canal)]
      
      canal_3 <- svyby(~oficios, ~c2_3, design_enadel_2024, svytotal, na.rm = TRUE) |> 
        mutate(cv = se/oficios*100) |> arrange(desc(oficios)) |> 
        dplyr::rename(canal_3 = oficios,
                      se_3 = se,
                      cv_3 = cv)
      
      unique_canal <- unique(canal_3$c2_3)
      labels_canal <- attr(canal_3$c2_3, "labels")
      matched_labels <- names(labels_canal)[match(unique_canal, labels_canal)]
      canal_3$canal_label <- matched_labels[match(canal_3$c2_3, unique_canal)]
      
      # join 
      canal <- canal_1 |> left_join(canal_2, by = "canal_label") |> left_join(canal_3, by = "canal_label") |> 
        select(canal_label, ends_with("1"), ends_with("2"), ends_with("3"))
    }    
    
  #---------------------- OCUPACIONES DE DIFICIL COBERTURA -------------------------#
    #¿Tuvo ocupaciones que no pudo llenar?
    odc_dummy <- svyby(~empresas, ~d1, design_enadel_2024, svytotal, na.rm = TRUE)
    
    #odc_dummy_sector
    odc_dummy_sector <- svyby(~empresas, ~d1+a2, design_enadel_2024, svytotal, na.rm = TRUE)[1:3] |>
      pivot_wider(names_from = "d1", values_from = "empresas", names_glue = "empresas_{d1}") |> 
      mutate(tot_empresas = empresas_1+empresas_2) |> 
      dplyr::rename(odc_dummy_1 = empresas_1) |> 
      dplyr::rename(odc_dummy_2 = empresas_2)
    unique_odc_dummy <- unique(odc_dummy_sector$a2)
    labels_odc_dummy <- attr(odc_dummy_sector$a2, "labels")
    matched_labels <- names(labels_odc_dummy)[match(unique_odc_dummy, labels_odc_dummy)]
    odc_dummy_sector$sector_label <- matched_labels[match(odc_dummy_sector$a2, unique_odc_dummy)]
    odc_dummy_sector<- odc_dummy_sector |> select(sector=a2, odc_dummy_1, odc_dummy_2, tot_empresas, sector_label)

    #odc_dummy_region
    odc_dummy_region <- svyby(~empresas, ~d1+reg_muestra, design_enadel_2024, svytotal, na.rm = TRUE)[1:3] |>
      pivot_wider(names_from = "d1", values_from = "empresas", names_glue = "empresas_{d1}") |> 
      mutate(tot_empresas = empresas_1+empresas_2) |> 
      dplyr::rename(odc_dummy_1 = empresas_1) |> 
      dplyr::rename(odc_dummy_2 = empresas_2)
    unique_odc_dummy <- unique(odc_dummy_region$reg_muestra)
    labels_odc_dummy <- attr(odc_dummy_region$reg_muestra, "labels")
    matched_labels <- names(labels_odc_dummy)[match(unique_odc_dummy, labels_odc_dummy)]
    odc_dummy_region$region_label <- matched_labels[match(odc_dummy_region$reg_muestra, unique_odc_dummy)]
    odc_dummy_region<- odc_dummy_region |> select(region=reg_muestra, odc_dummy_1, odc_dummy_2, tot_empresas, region_label)
    
    #Total de ODC ¿Cuántos cargos/ocupaciones no pudo llenar?
    odc_total <- svytotal(~d2, design_enadel_2024, na.rm = TRUE)
    odc_total <- as.data.frame(odc_total)
    #(MÓDULO D) d3: ODC
    
    # por ocupación
    cuadro8_odc_nac <- svyby(~d2, ~d3, design_enadel_2024, svytotal, na.rm = TRUE) 
    cuadro8_odc_nac$cv <- cv(cuadro8_odc_nac)
    unique_oficio <- unique(cuadro8_odc_nac$d3)
    labels_oficio <- attr(cuadro8_odc_nac$d3, "labels")
    matched_labels <- names(labels_oficio)[match(unique_oficio, labels_oficio)]
    cuadro8_odc_nac$oficio_label <- matched_labels[match(cuadro8_odc_nac$d3, unique_oficio)]
    cuadro8_odc_nac <- cuadro8_odc_nac |>  mutate(n_region=0) %>% select(n_region, d3, oficio_label, d2, se, cv)
    
    cuadro8_odc_reg <- svyby(~d2, ~d3+~reg_muestra, design_enadel_2024, svytotal, na.rm = TRUE) 
    cuadro8_odc_reg$cv <- cv(cuadro8_odc_reg)
    unique_oficio <- unique(cuadro8_odc_reg$d3)
    labels_oficio <- attr(cuadro8_odc_reg$d3, "labels")
    matched_labels <- names(labels_oficio)[match(unique_oficio, labels_oficio)]
    cuadro8_odc_reg$oficio_label <- matched_labels[match(cuadro8_odc_reg$d3, unique_oficio)]
    cuadro8_odc_reg %<>% dplyr::rename(n_region = reg_muestra) %>% select(n_region, d3, oficio_label, d2, se, cv)
    
    cuadro8_odc <- rbind(cuadro8_odc_nac, cuadro8_odc_reg)
    
    # ------------------------- VACANTES NO LLENADAS POR SECTOR ------------------------ #
    {
      # por sector
      odc <- svyby(~d2, ~d3+a2, design_enadel_2024, svytotal, na.rm = TRUE) |> 
        arrange(d3)
      odc$cv <- cv(odc)
      
      unique_oficio <- unique(odc$d3)
      labels_oficio <- attr(odc$d3, "labels")
      matched_labels <- names(labels_oficio)[match(unique_oficio, labels_oficio)]
      odc$oficio_label <- matched_labels[match(odc$d3, unique_oficio)]
      odc_sector <- odc |> group_by(a2) |> 
        arrange(a2, desc(d2)) |> 
        dplyr::mutate(orden = row_number()) |> 
        subset(orden == 1)
    }
    
    # ------------------------- VACANTES NO LLENADAS POR REGIÓN ------------------------ #
    {
      # por ocupación
      odc <- svyby(~d2, ~d3+reg_muestra, design_enadel_2024, svytotal, na.rm = TRUE) |> 
        arrange(d3)
      odc$cv <- cv(odc)
      
      unique_oficio <- unique(odc$d3)
      labels_oficio <- attr(odc$d3, "labels")
      matched_labels <- names(labels_oficio)[match(unique_oficio, labels_oficio)]
      odc$oficio_label <- matched_labels[match(odc$d3, unique_oficio)]
      odc_region <- odc |> group_by(reg_muestra) |> 
        arrange(reg_muestra, desc(d2)) |> 
        dplyr::mutate(orden = row_number()) |> 
        subset(orden == 1)
      
      
      
    }
    # -------------------- VACANTES NO LLENADAS POR TAMAÑO EMPRESA -------------------- #
    {
      # por ocupación
      odc <- svyby(~d2, ~d3+tam_trab, design_enadel_2024, svytotal, na.rm = TRUE) |> 
        arrange(d3)
      odc$cv <- cv(odc)
      
      unique_oficio <- unique(odc$d3)
      labels_oficio <- attr(odc$d3, "labels")
      matched_labels <- names(labels_oficio)[match(unique_oficio, labels_oficio)]
      odc$oficio_label <- matched_labels[match(odc$d3, unique_oficio)]
      odc_tam <- odc |> group_by(tam_trab) |> 
        arrange(tam_trab, desc(d2)) |> 
        dplyr::mutate(orden = row_number()) |> 
        subset(orden == 1)
    }
    
    # -------------------- VACANTES NO LLENADAS POR TAMAÑO EMPRESA $$ -------------------- #
    {
      # por ocupación
      odc <- svyby(~d2, ~d3+a3, design_enadel_2024, svytotal, na.rm = TRUE) |> 
        arrange(d3)
      odc$cv <- cv(odc)
      
      unique_oficio <- unique(odc$d3)
      labels_oficio <- attr(odc$d3, "labels")
      matched_labels <- names(labels_oficio)[match(unique_oficio, labels_oficio)]
      odc$oficio_label <- matched_labels[match(odc$d3, unique_oficio)]
      odc_tamvent <- odc |> group_by(a3) |> 
        arrange(a3, desc(d2)) |> 
        dplyr::mutate(orden = row_number()) |> 
        subset(orden == 1)
    }
    
    
    
    #----------DIFICULTAD -------------------------#
    
    # DIFICULTAD 1
    dif_1 <- svyby(~oficios, ~d3_c_1, design_enadel_2024, svytotal, na.rm = TRUE) |> 
      arrange(desc(oficios)) 
    dif_1$cv <- cv(dif_1)
    
    
    unique_dif <- unique(dif_1$d3_c_1)
    labels_dif <- attr(dif_1$d3_c_1, "labels")
    matched_labels <- names(labels_dif)[match(unique_dif, labels_dif)]
    dif_1$dif_label <- matched_labels[match(dif_1$d3_c_1, unique_dif)]
    dif_1 <- dif_1  %>% select(dif_label, oficios, se, cv)
    
    
    # DIFICULTAD 2
    dif_2 <- svyby(~oficios, ~d3_c_2, design_enadel_2024, svytotal, na.rm = TRUE) |> 
      arrange(desc(oficios)) 
    dif_2$cv_2 <- cv(dif_2)
    
    unique_dif <- unique(dif_2$d3_c_2)
    labels_dif <- attr(dif_2$d3_c_2, "labels")
    matched_labels <- names(labels_dif)[match(unique_dif, labels_dif)]
    dif_2$dif_label <- matched_labels[match(dif_2$d3_c_2, unique_dif)]
    dif_2 <- dif_2  %>% select(dif_label, oficios, se, cv_2)
    
    
    # DIFICULTAD 3
    dif_3 <- svyby(~oficios, ~d3_c_3, design_enadel_2024, svytotal, na.rm = TRUE) |> 
      arrange(desc(oficios)) 
    dif_3$cv_3 <- cv(dif_3)
    
    unique_dif <- unique(dif_3$d3_c_3)
    labels_dif <- attr(dif_3$d3_c_3, "labels")
    matched_labels <- names(labels_dif)[match(unique_dif, labels_dif)]
    dif_3$dif_label <- matched_labels[match(dif_3$d3_c_3, unique_dif)]
    dif_3 <- dif_3  %>% select(dif_label, oficios, se, cv_3)
    
    
    # join 
    dificultad_tot <- dif_1 |> left_join(dif_2, by = "dif_label") |> left_join(dif_3, by = "dif_label") |> arrange(oficios)
    
    
    
    
    
    # --------------------------- AÑOS DE EXPERIENCIA MÍNIMO --------------------------- #
    
           exp <- svyby(~oficios, ~d3_e_1, design_enadel_2024, svytotal, na.rm = TRUE) |> #d3_a
        mutate(cv =se/oficios*100, 
               pc = oficios/sum(oficios)*100)
      
      exp_0_sector <- svyby(~oficios, ~exp_cat+a2, design_enadel_2024, svytotal, na.rm = TRUE) |> 
        pivot_wider(names_from = exp_cat, values_from = c("oficios", "se"), names_glue = "{.value}_{exp_cat}")
    
      exp_design <- subset(design_enadel_2024, d3_e_1>0)
      
      
      exp_mean <- svymean(~d3_e_1, exp_design, na.rm = TRUE)
      exp_mean <- as.data.frame(exp_mean)

      
      exp_mean_sector <- svyby(~d3_e_1, ~a2, exp_design, svymean, na.rm = TRUE)
      exp_mean_sector$cv <- cv(exp_mean_sector)
      
      unique_exp <- unique(exp_mean_sector$a2)
      labels_exp <- attr(exp_mean_sector$a2, "labels")
      matched_labels <- names(labels_exp)[match(unique_exp, labels_exp)]
      exp_mean_sector$label <- matched_labels[match(exp_mean_sector$a2, unique_exp)]
      
      exp_mean_tam <- svyby(~d3_e_1, ~tam_trab, exp_design, svymean, na.rm = TRUE)
      exp_mean_tam$cv <- cv(exp_mean_tam)
      
      unique_dif <- unique(exp_mean_sector$tam_trab)
      labels_dif <- attr(exp_mean_sector$tam_trab, "labels")
      matched_labels <- names(labels_dif)[match(unique_dif, labels_dif)]
      exp_mean_tam$label <- matched_labels[match(exp_mean_sector$tam_trab, unique_dif)]
    
    
  #Aquí queda pendiente ver posibles profundizaciones en algunos sectores
      
      # --------------Capítulo III: Capacitación y Habilidades----------------------#   
      #OTIC
      otic <- svyby(~empresas, ~e1, design_enadel_2024, svytotal, na.rm = TRUE)
      
      #OTIC_sector
      otic_sector <- svyby(~empresas, ~e1+a2, design_enadel_2024, svytotal, na.rm = TRUE)[1:3] |>
        pivot_wider(names_from = "e1", values_from = "empresas", names_glue = "empresas_{e1}") |> 
        mutate(tot_empresas = empresas_1+empresas_2) |> 
        dplyr::rename(otic_1 = empresas_1) |> 
        dplyr::rename(otic_2 = empresas_2)
      unique_otic <- unique(otic_sector$a2)
      labels_otic <- attr(otic_sector$a2, "labels")
      matched_labels <- names(labels_otic)[match(unique_otic, labels_otic)]
      otic_sector$sector_label <- matched_labels[match(otic_sector$a2, unique_otic)]
      
      #OTIC_region
      otic_region <- svyby(~empresas, ~e1+reg_muestra, design_enadel_2024, svytotal, na.rm = TRUE)[1:3] |>
        pivot_wider(names_from = "e1", values_from = "empresas", names_glue = "empresas_{e1}") |> 
        mutate(tot_empresas = empresas_1+empresas_2) |> 
        dplyr::rename(otic_1 = empresas_1) |> 
        dplyr::rename(otic_2 = empresas_2)
      unique_otic <- unique(otic_region$reg_muestra)
      labels_otic <- attr(otic_region$reg_muestra, "labels")
      matched_labels <- names(labels_otic)[match(unique_otic, labels_otic)]
      otic_region$region_label <- matched_labels[match(otic_region$reg_muestra, unique_otic)]
      
      
      #capacitacion
      capacitacion <- svyby(~empresas, ~e2, design_enadel_2024, svytotal, na.rm = TRUE)
      
      #capacitacion_sector
      capacitacion_sector <- svyby(~empresas, ~e2+a2, design_enadel_2024, svytotal, na.rm = TRUE)[1:3] |>
        pivot_wider(names_from = "e2", values_from = "empresas", names_glue = "empresas_{e2}") |> 
        mutate(tot_empresas = empresas_1+empresas_2) |> 
        dplyr::rename(capacitacion_1 = empresas_1) |> 
        dplyr::rename(capacitacion_2 = empresas_2)
      unique_capacitacion <- unique(capacitacion_sector$a2)
      labels_capacitacion <- attr(capacitacion_sector$a2, "labels")
      matched_labels <- names(labels_capacitacion)[match(unique_capacitacion, labels_capacitacion)]
      capacitacion_sector$sector_label <- matched_labels[match(capacitacion_sector$a2, unique_capacitacion)]
      
      #capacitacion_region
      capacitacion_region <- svyby(~empresas, ~e2+reg_muestra, design_enadel_2024, svytotal, na.rm = TRUE)[1:3] |>
        pivot_wider(names_from = "e2", values_from = "empresas", names_glue = "empresas_{e2}") |> 
        mutate(tot_empresas = empresas_1+empresas_2) |> 
        dplyr::rename(capacitacion_1 = empresas_1) |> 
        dplyr::rename(capacitacion_2 = empresas_2)
      unique_capacitacion <- unique(capacitacion_region$reg_muestra)
      labels_capacitacion <- attr(capacitacion_region$reg_muestra, "labels")
      matched_labels <- names(labels_capacitacion)[match(unique_capacitacion, labels_capacitacion)]
      capacitacion_region$region_label <- matched_labels[match(capacitacion_region$reg_muestra, unique_capacitacion)]
      
      
      
      #---------------------------Competencias---------------------------------#
      # Competencias básicas
      e3_a <- svyby(~empresas, ~e3_a, design_enadel_2024, svytotal, na.rm = TRUE)  
      e3_a$cv <- cv(e3_a)
      
      unique_com <- unique(e3_a$e3_a)
      labels_com <- attr(e3_a$e3_a, "labels")
      matched_labels <- names(labels_com)[match(unique_com, labels_com)]
      e3_a$com_label <- matched_labels[match(e3_a$e3_a, unique_com)]
      e3_a <- e3_a  %>% select(com_label, empresas, se, cv)
      
      
      #Competencias técnicas
      e3_b <- svyby(~empresas, ~e3_b, design_enadel_2024, svytotal, na.rm = TRUE)  
      e3_b$cv <- cv(e3_b)
      
      unique_com <- unique(e3_b$e3_b)
      labels_com <- attr(e3_b$e3_b, "labels")
      matched_labels <- names(labels_com)[match(unique_com, labels_com)]
      e3_b$com_label <- matched_labels[match(e3_b$e3_b, unique_com)]
      e3_b <- e3_b  %>% select(com_label, empresas, se, cv)
      
      #Competencias actitudinales, conductuales, emocionales y/o valóricas
      e3_c <- svyby(~empresas, ~e3_c, design_enadel_2024, svytotal, na.rm = TRUE)  
      e3_c$cv <- cv(e3_c)
      
      unique_com <- unique(e3_c$e3_c)
      labels_com <- attr(e3_c$e3_c, "labels")
      matched_labels <- names(labels_com)[match(unique_com, labels_com)]
      e3_c$com_label <- matched_labels[match(e3_c$e3_c, unique_com)]
      e3_c <- e3_c  %>% select(com_label, empresas, se, cv)
      
      #Competencias Competencias en idiomas extranjeros
      e3_d <- svyby(~empresas, ~e3_d, design_enadel_2024, svytotal, na.rm = TRUE)  
      e3_d$cv <- cv(e3_d)
      
      unique_com <- unique(e3_d$e3_d)
      labels_com <- attr(e3_d$e3_d, "labels")
      matched_labels <- names(labels_com)[match(unique_com, labels_com)]
      e3_d$com_label <- matched_labels[match(e3_d$e3_d, unique_com)]
      e3_d <- e3_d  %>% select(com_label, empresas, se, cv)
      
      #Competencias directivas o de gestión
      e3_e <- svyby(~empresas, ~e3_e, design_enadel_2024, svytotal, na.rm = TRUE)  
      e3_e$cv <- cv(e3_e)
      
      unique_com <- unique(e3_e$e3_e)
      labels_com <- attr(e3_e$e3_e, "labels")
      matched_labels <- names(labels_com)[match(unique_com, labels_com)]
      e3_e$com_label <- matched_labels[match(e3_e$e3_e, unique_com)]
      e3_e <- e3_e  %>% select(com_label, empresas, se, cv)
      
      #Competencias digitales básicas
      e3_f <- svyby(~empresas, ~e3_f, design_enadel_2024, svytotal, na.rm = TRUE)  
      e3_f$cv <- cv(e3_f)
      
      unique_com <- unique(e3_f$e3_f)
      labels_com <- attr(e3_f$e3_f, "labels")
      matched_labels <- names(labels_com)[match(unique_com, labels_com)]
      e3_f$com_label <- matched_labels[match(e3_f$e3_f, unique_com)]
      e3_f <- e3_f  %>% select(com_label, empresas, se, cv)
      
      
      #Competencias digitales avanzadas
      e3_g <- svyby(~empresas, ~e3_g, design_enadel_2024, svytotal, na.rm = TRUE)  
      e3_g$cv <- cv(e3_g)
      
      unique_com <- unique(e3_g$e3_g)
      labels_com <- attr(e3_g$e3_g, "labels")
      matched_labels <- names(labels_com)[match(unique_com, labels_com)]
      e3_g$com_label <- matched_labels[match(e3_g$e3_g, unique_com)]
      e3_g <- e3_g  %>% select(com_label, empresas, se, cv)
      
      #Competencias dde salud ocupacional y prevención
      e3_h <- svyby(~empresas, ~e3_h, design_enadel_2024, svytotal, na.rm = TRUE)  
      e3_h$cv <- cv(e3_h)
      
      unique_com <- unique(e3_h$e3_h)
      labels_com <- attr(e3_h$e3_h, "labels")
      matched_labels <- names(labels_com)[match(unique_com, labels_com)]
      e3_h$com_label <- matched_labels[match(e3_h$e3_h, unique_com)]
      e3_h <- e3_h  %>% select(com_label, empresas, se, cv)
      

      
      competencias_capacitadas<-rbind(e3_a[1,],
                                 e3_b[1,],
                                 e3_c[1,],
                                 e3_d[1,],
                                 e3_e[1,],
                                 e3_f[1,],
                                 e3_g[1,],
                                 e3_h[1,])
      
      competencias_capacitadas$com_label<- c("Competencias básicas", "Competencias técnicas","Competencias actitudinales, conductuales, emocionales y/o valóricas", 
                                        "Competencias en idiomas extranjeros", "Competencias directivas o de gestión", "Competencias digitales básicas",
                                        "Competencias digitales avanzadas", "Competencias de salud ocupacional y prevención")
      
      
      competencias_capacitadas<- as.data.frame(competencias_capacitadas) |> arrange(desc(empresas))
      
      
      #Fuentes de financiamiento
      capacitacion_finan <- svyby(~empresas, ~e5_1, design_enadel_2024, svytotal, na.rm = TRUE)
      
      
      # Necesidad de capacitar (¿Ha requerido la empresa capacitar a sus empleado/as en alguna de las siguientes competencias?)#
      
      # Competencias básicas
      e6_a <- svyby(~empresas, ~e6_a, design_enadel_2024, svytotal, na.rm = TRUE)  
      e6_a$cv <- cv(e6_a)
      
      unique_com <- unique(e6_a$e6_a)
      labels_com <- attr(e6_a$e6_a, "labels")
      matched_labels <- names(labels_com)[match(unique_com, labels_com)]
      e6_a$com_label <- matched_labels[match(e6_a$e6_a, unique_com)]
      e6_a <- e6_a  %>% select(com_label, empresas, se, cv)
      
      
      #Competencias técnicas
      e6_b <- svyby(~empresas, ~e6_b, design_enadel_2024, svytotal, na.rm = TRUE)  
      e6_b$cv <- cv(e6_b)
      
      unique_com <- unique(e6_b$e6_b)
      labels_com <- attr(e6_b$e6_b, "labels")
      matched_labels <- names(labels_com)[match(unique_com, labels_com)]
      e6_b$com_label <- matched_labels[match(e6_b$e6_b, unique_com)]
      e6_b <- e6_b  %>% select(com_label, empresas, se, cv)
      
      #Competencias actitudinales, conductuales, emocionales y/o valóricas
      e6_c <- svyby(~empresas, ~e6_c, design_enadel_2024, svytotal, na.rm = TRUE)  
      e6_c$cv <- cv(e6_c)
      
      unique_com <- unique(e6_c$e6_c)
      labels_com <- attr(e6_c$e6_c, "labels")
      matched_labels <- names(labels_com)[match(unique_com, labels_com)]
      e6_c$com_label <- matched_labels[match(e6_c$e6_c, unique_com)]
      e6_c <- e6_c  %>% select(com_label, empresas, se, cv)
      
      #Competencias Competencias en idiomas extranjeros
      e6_d <- svyby(~empresas, ~e6_d, design_enadel_2024, svytotal, na.rm = TRUE)  
      e6_d$cv <- cv(e6_d)
      
      unique_com <- unique(e6_d$e6_d)
      labels_com <- attr(e6_d$e6_d, "labels")
      matched_labels <- names(labels_com)[match(unique_com, labels_com)]
      e6_d$com_label <- matched_labels[match(e6_d$e6_d, unique_com)]
      e6_d <- e6_d  %>% select(com_label, empresas, se, cv)
      
      #Competencias directivas o de gestión
      e6_e <- svyby(~empresas, ~e6_e, design_enadel_2024, svytotal, na.rm = TRUE)  
      e6_e$cv <- cv(e6_e)
      
      unique_com <- unique(e6_e$e6_e)
      labels_com <- attr(e6_e$e6_e, "labels")
      matched_labels <- names(labels_com)[match(unique_com, labels_com)]
      e6_e$com_label <- matched_labels[match(e6_e$e6_e, unique_com)]
      e6_e <- e6_e  %>% select(com_label, empresas, se, cv)
      
      #Competencias digitales básicas
      e6_f <- svyby(~empresas, ~e6_f, design_enadel_2024, svytotal, na.rm = TRUE)  
      e6_f$cv <- cv(e6_f)
      
      unique_com <- unique(e6_f$e6_f)
      labels_com <- attr(e6_f$e6_f, "labels")
      matched_labels <- names(labels_com)[match(unique_com, labels_com)]
      e6_f$com_label <- matched_labels[match(e6_f$e6_f, unique_com)]
      e6_f <- e6_f  %>% select(com_label, empresas, se, cv)
      
      
      #Competencias digitales avanzadas
      e6_g <- svyby(~empresas, ~e6_g, design_enadel_2024, svytotal, na.rm = TRUE)  
      e6_g$cv <- cv(e6_g)
      
      unique_com <- unique(e6_g$e6_g)
      labels_com <- attr(e6_g$e6_g, "labels")
      matched_labels <- names(labels_com)[match(unique_com, labels_com)]
      e6_g$com_label <- matched_labels[match(e6_g$e6_g, unique_com)]
      e6_g <- e6_g  %>% select(com_label, empresas, se, cv)
      
      #Competencias de salud ocupacional y prevención
      e6_h <- svyby(~empresas, ~e6_h, design_enadel_2024, svytotal, na.rm = TRUE)  
      e6_h$cv <- cv(e6_h)
      
      unique_com <- unique(e6_h$e6_h)
      labels_com <- attr(e6_h$e6_h, "labels")
      matched_labels <- names(labels_com)[match(unique_com, labels_com)]
      e6_h$com_label <- matched_labels[match(e6_h$e6_h, unique_com)]
      e6_h <- e6_h  %>% select(com_label, empresas, se, cv)
      
      competencias_necesarias<-rbind(e6_a[1,],
            e6_b[1,],
            e6_c[1,],
            e6_d[1,],
            e6_e[1,],
            e6_f[1,],
            e6_g[1,],
            e6_h[1,])
      
      competencias_necesarias$com_label<- c("Competencias básicas", "Competencias técnicas","Competencias actitudinales, conductuales, emocionales y/o valóricas", 
                                        "Competencias en idiomas extranjeros", "Competencias directivas o de gestión", "Competencias digitales básicas",
                                        "Competencias digitales avanzadas", "Competencias de salud ocupacional y prevención")
      
      competencias_necesarias<-as.data.frame(competencias_necesarias) |> arrange(desc(empresas))
      
      
      
     #E7 Durante los próximos doce meses, ¿El personal de su empresa requerirá capacitación?
      #capacitacion
      capacitacion_futura <- svyby(~empresas, ~e7, design_enadel_2024, svytotal, na.rm = TRUE)
      
      #capacitacion_futura_sector
      capacitacion_futura_sector <- svyby(~empresas, ~e7+a2, design_enadel_2024, svytotal, na.rm = TRUE)[1:3] |>
        pivot_wider(names_from = "e7", values_from = "empresas", names_glue = "empresas_{e7}") |> 
        mutate(tot_empresas = empresas_1+empresas_2) |> 
        dplyr::rename(capacitacion_futura_1 = empresas_1) |> 
        dplyr::rename(capacitacion_futura_2 = empresas_2)
      unique_capacitacion_futura <- unique(capacitacion_futura_sector$a2)
      labels_capacitacion_futura <- attr(capacitacion_futura_sector$a2, "labels")
      matched_labels <- names(labels_capacitacion_futura)[match(unique_capacitacion_futura, labels_capacitacion_futura)]
      capacitacion_futura_sector$sector_label <- matched_labels[match(capacitacion_futura_sector$a2, unique_capacitacion_futura)]
      
      #capacitacion_futura_region
      capacitacion_futura_region <- svyby(~empresas, ~e7+reg_muestra, design_enadel_2024, svytotal, na.rm = TRUE)[1:3] |>
        pivot_wider(names_from = "e7", values_from = "empresas", names_glue = "empresas_{e7}") |> 
        mutate(tot_empresas = empresas_1+empresas_2) |> 
        dplyr::rename(capacitacion_futura_1 = empresas_1) |> 
        dplyr::rename(capacitacion_futura_2 = empresas_2)
      unique_capacitacion_futura <- unique(capacitacion_futura_region$reg_muestra)
      labels_capacitacion_futura <- attr(capacitacion_futura_region$reg_muestra, "labels")
      matched_labels <- names(labels_capacitacion_futura)[match(unique_capacitacion_futura, labels_capacitacion_futura)]
      capacitacion_futura_region$region_label <- matched_labels[match(capacitacion_futura_region$reg_muestra, unique_capacitacion_futura)]
      
      #E8 principales cargos/ocupaciones que requerirán capacitación
      
      orc <- svyby(~oficios, ~ e8, design_enadel_2024, svytotal, na.rm = TRUE)|> arrange(desc(oficios))
      orc$cv <- cv(orc)
      # labels
      unique_oficio <- unique(orc$e8)#Esta variable debería ser triple: e8_1, e8_2 y e8_3
      labels_oficio <- attr(orc$e8, "labels")
      matched_labels <- names(labels_oficio)[match(unique_oficio, labels_oficio)]
      orc$oficio_label <- matched_labels[match(orc$e8, unique_oficio)]
      
      
      #Competencias de la ocupacion n° 1 que requerirán capacitación (principales cargos/ocupaciones que requerirán capacitación)
      
      # 1. Competencias básicas
      e8_c1_1 <- svyby(~empresas, ~e8_c1_1, design_enadel_2024, svytotal, na.rm = TRUE) 
      e8_c1_1$cv <- cv(e8_c1_1)
      
      unique_com <- unique(e8_c1_1$e8_c1_1)
      labels_com <- attr(e8_c1_1$e8_c1_1, "labels")
      matched_labels <- names(labels_com)[match(unique_com, labels_com)]
      e8_c1_1$com_label <- matched_labels[match(e8_c1_1$e8_c1_1, unique_com)]
      e8_c1_1 <- e8_c1_1  %>% select(com_label, empresas, se, cv)
      
      # 2. Competencias técnicas
      e8_c2_1 <- svyby(~empresas, ~e8_c2_1, design_enadel_2024, svytotal, na.rm = TRUE) 
      e8_c2_1$cv <- cv(e8_c2_1)
      
      unique_com <- unique(e8_c2_1$e8_c2_1)
      labels_com <- attr(e8_c2_1$e8_c2_1, "labels")
      matched_labels <- names(labels_com)[match(unique_com, labels_com)]
      e8_c2_1$com_label <- matched_labels[match(e8_c2_1$e8_c2_1, unique_com)]
      e8_c2_1 <- e8_c2_1  %>% select(com_label, empresas, se, cv)
      
      # 3. Competencias actitudinales, conductuales, emocionales y/o valóricas
      e8_c3_1 <- svyby(~empresas, ~e8_c3_1, design_enadel_2024, svytotal, na.rm = TRUE) 
      e8_c3_1$cv <- cv(e8_c3_1)
      
      unique_com <- unique(e8_c3_1$e8_c3_1)
      labels_com <- attr(e8_c3_1$e8_c3_1, "labels")
      matched_labels <- names(labels_com)[match(unique_com, labels_com)]
      e8_c3_1$com_label <- matched_labels[match(e8_c3_1$e8_c3_1, unique_com)]
      e8_c3_1 <- e8_c3_1  %>% select(com_label, empresas, se, cv)
      
      # 4. Competencias en idiomas extranjeros
      e8_c4_1 <- svyby(~empresas, ~e8_c4_1, design_enadel_2024, svytotal, na.rm = TRUE) 
      e8_c4_1$cv <- cv(e8_c4_1)
      
      unique_com <- unique(e8_c4_1$e8_c4_1)
      labels_com <- attr(e8_c4_1$e8_c4_1, "labels")
      matched_labels <- names(labels_com)[match(unique_com, labels_com)]
      e8_c4_1$com_label <- matched_labels[match(e8_c4_1$e8_c4_1, unique_com)]
      e8_c4_1 <- e8_c4_1  %>% select(com_label, empresas, se, cv)
      
      # 5. Competencias directivas o de gestión
      e8_c5_1 <- svyby(~empresas, ~e8_c5_1, design_enadel_2024, svytotal, na.rm = TRUE) 
      e8_c5_1$cv <- cv(e8_c5_1)
      
      unique_com <- unique(e8_c5_1$e8_c5_1)
      labels_com <- attr(e8_c5_1$e8_c5_1, "labels")
      matched_labels <- names(labels_com)[match(unique_com, labels_com)]
      e8_c5_1$com_label <- matched_labels[match(e8_c5_1$e8_c5_1, unique_com)]
      e8_c5_1 <- e8_c5_1  %>% select(com_label, empresas, se, cv)
      
      # 6 Competencias digitales básicas
      e8_c6_1 <- svyby(~empresas, ~e8_c6_1, design_enadel_2024, svytotal, na.rm = TRUE) 
      e8_c6_1$cv <- cv(e8_c6_1)
      
      unique_com <- unique(e8_c6_1$e8_c6_1)
      labels_com <- attr(e8_c6_1$e8_c6_1, "labels")
      matched_labels <- names(labels_com)[match(unique_com, labels_com)]
      e8_c6_1$com_label <- matched_labels[match(e8_c6_1$e8_c6_1, unique_com)]
      e8_c6_1 <- e8_c6_1  %>% select(com_label, empresas, se, cv)
      
      # 7 Competencias digitales avanzadas
      e8_c7_1 <- svyby(~empresas, ~e8_c7_1, design_enadel_2024, svytotal, na.rm = TRUE) 
      e8_c7_1$cv <- cv(e8_c7_1)
      
      unique_com <- unique(e8_c7_1$e8_c7_1)
      labels_com <- attr(e8_c7_1$e8_c7_1, "labels")
      matched_labels <- names(labels_com)[match(unique_com, labels_com)]
      e8_c7_1$com_label <- matched_labels[match(e8_c7_1$e8_c7_1, unique_com)]
      e8_c7_1 <- e8_c7_1  %>% select(com_label, empresas, se, cv)
      
      # 8. Competencias de salud ocupacional y prevención
      e8_c8_1 <- svyby(~empresas, ~e8_c8_1, design_enadel_2024, svytotal, na.rm = TRUE) 
      e8_c8_1$cv <- cv(e8_c8_1)
      
      unique_com <- unique(e8_c8_1$e8_c8_1)
      labels_com <- attr(e8_c8_1$e8_c8_1, "labels")
      matched_labels <- names(labels_com)[match(unique_com, labels_com)]
      e8_c8_1$com_label <- matched_labels[match(e8_c8_1$e8_c8_1, unique_com)]
      e8_c8_1 <- e8_c8_1  %>% select(com_label, empresas, se, cv)
      
      
      competencias_futuras<-rbind(e8_c1_1[2,],
                                  e8_c2_1[2,],
                                  e8_c3_1[2,],
                                  e8_c4_1[2,],
                                  e8_c5_1[2,],
                                  e8_c6_1[2,],
                                  e8_c7_1[2,],
                                  e8_c8_1[2,])
      
      competencias_futuras$com_label<- c("Competencias básicas",
                                            "Competencias técnicas",
                                            "Competencias actitudinales, conductuales, emocionales y/o valóricas", 
                                            "Competencias en idiomas extranjeros",
                                            "Competencias directivas o de gestión",
                                            "Competencias digitales básicas",
                                            "Competencias digitales avanzadas",
                                            "Competencias de salud ocupacional y prevención")
      
      competencias_futuras<- as.data.frame(competencias_futuras) |> arrange(desc(empresas))
  
  # ********************************************************************************** #
  # -------------------------------- GUARDAR EN EXCEL -------------------------------- #
  # ********************************************************************************** #
  
  
  write_xlsx(list("contratados_u12_sector" = cu12_sector,#contratado últimos 12 meses por sector
                  "contratados_u12" = cu12, # Contratado últimos 12 meses
                  "contratados_u12_educ"=cu12_educ,
                  "certificaciones_tot"=certificaciones_tot,
                  "educ_tot" = educ_tot,
                  "dotacion_resumen" = dotacion_resumen,
                  "vacantes_actuales_futuras"=vacantes_actuales_futuras,
                  "renuncia_total"=renuncia_total,
                  "despidos_total"=despidos_total,
                  "contrataciones_total"=contrataciones_total,
                  "vacantes_total"=vacantes_total,
                  "indefinido_sector"=indefinido_sector,
                  "plazofijo_sector"=plazofijo_sector,
                  "honorario_sector"=honorario_sector,
                  "dotacion_sector"=dotacion_sector,
                  "canal"= canal,
                  "pc_acteco"= pc_acteco, 
                  "pc_region" = pc_region,
                  "pc_regiones_sucursal"=totales_regiones_df,
                  "pc_tam"    = pc_tam, 
                  "pc_tamvent" = pc_tamvent,
                  "Tipo_propiedad"=cuadro_resumen,
                  "cuadro8_odc" = cuadro8_odc,#vacantes por ocupación
                  "dificultad_tot"=dificultad_tot,
                  "vacantes_sector" = odc_sector,
                  "vacantes_region" = odc_region,
                  "vacantes_tam" = odc_tam,
                  "vacantes_tamvent" = odc_tamvent,
                  "odc_sector"=odc_sector,
                  "odc_region"=odc_region,
                  "odc_dummy_sector"=odc_dummy_sector,#Si es que tiene ODC por sector
                  "odc_dummy_region"= odc_dummy_region,
                  "odc_total"=odc_total,
                  "exp"=exp,
                  "exp_mean"=exp_mean,
                  "exp_mean_sector" = exp_mean_sector,
                  "exp_0_sector" = exp_0_sector,
                  "exp_mean_tam" = exp_mean_tam,
                  "subcontratos_sector"=subcontratos_sector,
                  "subcontratos_region"=subcontratos_region, 
                  "conglomerados_sector"=conglomerados_sector,
                  "conglomerados_region"=conglomerados_region,
                  "gremios_sector"=gremios_sector,
                  "gremios_region"=gremios_region,
                  "otic"= otic,
                  "otic_sector"=otic_sector,
                  "otic_region"= otic_region,
                  "capacitacion"=capacitacion,
                  "capacitacion_sector"=capacitacion_sector,
                  "capacitacion_region"=capacitacion_region,
                  "capacitacion_finan"=capacitacion_finan,
                  "competencias_capacitadas"=competencias_capacitadas,
                  "competencias_necesarias"=competencias_necesarias,
                  "capacitacion_futura"=capacitacion_futura,
                  "capacitacion_futura_sector"=capacitacion_futura_sector,
                  "capacitacion_futura_region"=capacitacion_futura_region,
                  "orc"=orc, #Ocupaciones que requerirán capacitación
                  "competencias_futuras"=competencias_futuras),
             path = "Tablas/porcentajes.xlsx" )
  
  
  
  
  
  
  
  
  

  