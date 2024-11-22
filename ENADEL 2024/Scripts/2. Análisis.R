############################Análisis##################
#packs
pacman::p_load("tidyverse", "dplyr", "plyr", "knitr", "haven", "readxl", "writexl", "Hmisc", "survey", "labelled", "archive")


#Datos
enadel <- readxl::read_excel("Datos/enadel_2024_rec.xlsx")

#Crear la variable de estratos concatenando región y rama de actividad económica

####Estos cálculos se deben eliminar cuando envíen la variable de estrato########

length(unique(enadel$reg_muestra))#15
length(unique(enadel$a2))#18

enadel <- enadel %>% mutate(a2= case_when(a2=="Agricultura, silvicultura y pesca" ~ "A",
                                          a2=="Industrias manufactureras" ~ "C",
                                          a2== "Suministro de electricidad y gas" | a2=="Suministro de agua y gestión de desechos" ~ "D-E",
                                          a2== "Construcción" ~ "F",
                                          a2== "Comercio" ~ "G",
                                          a2== "Transporte y almacenamiento" ~ "H",
                                          a2== "Alojamiento y de servicio de comida" ~ "I",
                                          a2== "Información y comunicaciones" ~ "J",
                                          a2== "Actividades financieras y de seguros" ~ "K",
                                          a2== "Actividades inmobiliarias" ~ "L",
                                          a2== "Actividades profesionales y técnicas" | a2==  ~ "Servicios administrativos y de apoyo" ~ "M-N",
                                          a2== "Actividades artísticas y recreativas/ Otras\nactividades de servicios" ~ "R-S",
                                          TRUE ~ NA))
                                          

enadel <- enadel[!is.na(enadel$a2), ]

enadel$estrato <- as.numeric(interaction(enadel$reg_muestra, enadel$a2, drop = TRUE))



length(unique(enadel$estrato))#125 estratos provisorios



#########################Diseño mnuestral######################################
#Revisar conn las variables definitivas

#svy
options(survey.lonely.psu="adjust")
design_enadel_2024 <- svydesign(ids = ~id_emp, strata =~estrato ,  weights = ~exp, data = enadel, nest = TRUE)

#Cantidad de empresas
tot_folios <- svytotal(~empresas, design_enadel_2024, na.rm = TRUE)[1]


tot_folios_muestra <- length(unique(enadel$id_emp))

 # total trabajadores 
  tot_trab <- svytotal(~b6_1, design_enadel_2024, na.rm = TRUE)[1]
  tot_trab_muestra <- sum(enadel$b6_1)
  
  
  # empresas por sector
  folios_sector <- svyby(~empresas, ~a2, design_enadel_2024, svytotal, na.rm = TRUE)[1:2]
  folios_sector_muestra <- enadel |> group_by(a2) |> 
    dplyr::summarise(tot_folios = n())
  
  # trabajadores por sector
  trab_sector <- svyby(~b6_1, ~a2, design_enadel_2024, svytotal, na.rm = TRUE)[1:2]
  trab_sector_muestra <- enadel |> group_by(a2) |> 
    dplyr::summarise(tot_trab = sum(b6_1))

  
  cuadro_resumen <- data.frame(tipo = c(folios_sector$a2, "Total"),
                               empresas_muestra = c(folios_sector_muestra$tot_folios, tot_trab),
                               empresas_expandidos = c(folios_sector$empresas, tot_folios),
                               trab_muestra = c(trab_sector_muestra$tot_trab, tot_trab_muestra),
                               trab_expandidos = c(trab_sector$trabval, tot_trab))
  
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
  base_regiones <- data.frame(region = orden_regiones)
  
  
  # Alinear folios_region con base_regiones
  folios_region_alineado <- merge(
    base_regiones, 
    folios_region, 
    by.x = "region", 
    by.y = "a6", 
    all.x = TRUE
  )
  
  # Alinear trab_region con base_regiones
  trab_region_alineado <- merge(
    base_regiones, 
    trab_region, 
    by.x = "region", 
    by.y = "a6", 
    all.x = TRUE
  )
  
  pc_region <- data.frame(
    region = base_regiones$region,
    empresas = folios_region_alineado$empresas,
    trabajadores = trab_region_alineado$b6_1
  ) |>
    mutate(
      pc_empresas = empresas / sum(empresas, na.rm = TRUE),
      pc_trabajadores = trabajadores / sum(trabajadores, na.rm = TRUE),
      region = factor(region, levels = orden_regiones) # Convertir a factor con niveles en el orden deseado
    ) |>
    arrange(region) # Ordenar según los niveles del factor
  
  
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
  
  # --- % por sector 
  # empresas por sector
  acteco_tot <- svyby(~empresas, ~a2, design_enadel_2024, svytotal, na.rm = TRUE)[1:2]
  # trabajadores por sector
  acteco_trab_tot <- svyby(~b6_1, ~a2, design_enadel_2024, svytotal, na.rm = TRUE)[1:2]
  

  act_eco_labels <- c("A", "C", "D-E", "F", "G", 
                      "H", "I", "J", "K", "L",
                      "M-N", "R-S") 
  
  pc_acteco <- data.frame (act_eco = act_eco_labels,
                           empresas = acteco_tot$empresas,
                           trabajadores = acteco_trab_tot$b6_1) |> 
    mutate(pc_empresas = empresas/sum(empresas),
           pc_trabajadores = trabajadores/sum(trabajadores)) |> 
    select(act_eco, empresas, trabajadores, pc_empresas, pc_trabajadores)
  
  
  
  respuestas_multiples_diseno <- function(design, columnas) {#Esta función sirve para calcular el total y no por columna de la variable de respuesta abierta
    # Crear una nueva columna uniendo las columnas especificadas, omitiendo NA
    respuesta_multiple <- apply(design$variables[, columnas], 1, function(x) {
      x <- x[!is.na(x)]  
      paste(x, collapse = "/ ")  # Unir respuestas válidas
    })
    
    # Separar las respuestas unidas por "/"
    respuestas_separadas <- strsplit(respuesta_multiple, "/ ")
    
    # Crear un vector de respuestas y un índice vinculado a las filas originales
    respuestas <- unlist(respuestas_separadas)
    filas_indices <- unlist(lapply(seq_along(respuestas_separadas), function(i) {
      rep(i, length(respuestas_separadas[[i]]))
    }))
    
    # Extraer las variables relevantes del diseño original
    datos_expandido <- design$variables[filas_indices, , drop = FALSE]
    datos_expandido$respuesta <- factor(respuestas)
    
    # Crear un nuevo diseño muestral para las respuestas múltiples
    design_expandido <- svydesign(
      ids = ~id_emp,
      strata = ~estrato,
      weights = ~exp,
      data = datos_expandido,
      nest = TRUE
    )
    
    # Calcular las frecuencias ponderadas
    tabla_frecuencias <- svytable(~respuesta, design = design_expandido)
    
    # Convertir la tabla a un data.frame para mayor legibilidad
    tabla_frecuencias_df <- as.data.frame(tabla_frecuencias)
    colnames(tabla_frecuencias_df) <- c("Respuesta", "Frecuencia Ponderada")
    
    return(tabla_frecuencias_df)
  }
  
#Región Sucursales ----
  
  # Lista 
  region_vars <- paste0("a5_", 1:16)
  # Crear una fórmula con todas las variables de región
  formula_totales <- as.formula(paste("~", paste(region_vars, collapse = "+")))
  # Calcular los totales ponderados
  totales_regiones <- svytotal(formula_totales, design = design_enadel_2024, na.rm = TRUE)
  totales_regiones_df <- as.data.frame(totales_regiones)
  # Nombres de  regiones
 # totales_regiones_df$Region <- rownames(totales_regiones_df)
  # Reordenar  columnas
  totales_regiones_df <- totales_regiones_df[, c("total", "SE")]

  
  # Calcular proporciones
  proporciones_regiones <- svymean(formula_totales, design = design_enadel_2024, na.rm = TRUE)
  # Convertir a data.frame
  proporciones_regiones_df <- as.data.frame(proporciones_regiones)
  # Reordenar columnas
  proporciones_regiones_df <- proporciones_regiones_df[, c("mean", "SE")]
  # Convertir proporciones a porcentajes
  proporciones_regiones_df$mean <- proporciones_regiones_df$mean * 100
  proporciones_regiones_df$SE <- proporciones_regiones_df$SE * 100

  # Nombres de regiones
  nombres_regiones <- c("Arica y Parinacota", "Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", "Metropolitana", "O’Higgins",
                        "Maule", "Ñuble", "Biobío", "La Araucanía", "Los Ríos", "Los Lagos", "Aysén", "Magallanes")
  totales_regiones_df$Region <- nombres_regiones
  proporciones_regiones_df$Region <- nombres_regiones
  
  #Fusionar en un sólo dataframe
  totales_regiones_sucursal <- totales_regiones_df %>% left_join(proporciones_regiones_df, by="Region")
  colnames(totales_regiones_sucursal) <- c("Región", "Total", "error_total", "%", "error_%")
  
  #Conglomerado
  conglomerados <- svyby(~empresas, ~a9, design_enadel_2024, svytotal, na.rm = TRUE) |> 
    mutate(pc=empresas/sum(empresas)) 
  
  conglomerados_sector <- svyby(~a9, ~ a2, design_enadel_2024, svytotal, na.rm= TRUE)
  
  #Gremios
  gremios <- svyby(~empresas, ~a10, design_enadel_2024, svytotal, na.rm=TRUE) |> 
    mutate(pc=empresas/sum(empresas))
  
  gremios_sector <- svyby(~a10, ~ a2, design_enadel_2024, svytotal, na.rm= TRUE)
  gremios_region <- svyby(~a10, ~ a6, design_enadel_2024, svytotal, na.rm= TRUE)
  
  #  

  
 
  
  
  #Gremio Empresarial
  gremios_sector <- svyby(~empresas, ~a10, design_enadel_2024, svytotal, na.rm= TRUE) [1:2]
  
  # ********************************************************************************** #
  # -------------------------------- GUARDAR EN EXCEL -------------------------------- #
  # ********************************************************************************** #
  
  
  write_xlsx(list("pc_acteco"= pc_acteco, 
                  "pc_region" = pc_region,
                  "pc_regiones_sucursal"=totales_regiones_sucursal,
                  "pc_tam"    = pc_tam, 
                  "pc_tamvent"= pc_tamvent),
  path = "Tablas/porcentajes.xlsx" )
  
  
  
  
  
  
  
  
  

  