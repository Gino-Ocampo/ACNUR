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

    
    
    #Total de ODC ¿Cuántos cargos/ocupaciones no pudo llenar?
    odc_total <- svytotal(~d2, design_enadel_2024, na.rm = TRUE)
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
    
           exp <- svyby(~oficios, ~d3_e_1, design_enadel_2024, svytotal, na.rm = TRUE) |>
        mutate(cv =se/oficios*100, 
               pc = oficios/sum(oficios)*100)
      
      exp_0_sector <- svyby(~oficios, ~exp_cat+a2, design_enadel_2024, svytotal, na.rm = TRUE) |> 
        pivot_wider(names_from = exp_cat, values_from = c("oficios", "se"), names_glue = "{.value}_{exp_cat}")
    
      exp_design <- subset(design_enadel_2024, d3_e_1>0)
      
      
      exp_mean <- svymean(~d3_e_1, exp_design, na.rm = TRUE)
      exp_mean$cv <- cv(exp_mean)
      
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
                  "odc_dummy_sector"=odc_dummy_sector,#Si es que tiene ODC por sector
                  "exp_mean_sector" = exp_mean_sector,
                  "exp_0_sector" = exp_0_sector,
                  "exp_mean_tam" = exp_mean_tam,
                  "subcontratos_sector"=subcontratos_sector,
                  "subcontratos_region"=subcontratos_region, 
                  "conglomerados_sector"=conglomerados_sector,
                  "conglomerados_region"=conglomerados_region,
                  "gremios_sector"=gremios_sector,
                  "gremios_region"=gremios_region),
  path = "Tablas/porcentajes.xlsx" )
  
  
  
  
  
  
  
  
  

  