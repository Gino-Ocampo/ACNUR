#librerías
pacman::p_load("tidyverse","dplyr", "plyr", "knitr", "haven", "readxl", "writexl", "Hmisc", "survey", "labelled", "archive", "purrr", "stringi")


#Datos
enadel <- read_dta("Bases/ENADEL_2024_(externa).dta")
long<- readRDS("Bases/enadel_long.rds") #Utilizar oficio4 de aquí


#VARIABLES PROVISORIAS: oficio4, actividad económica, estratos, empresas -------------------

#Crear la variable de estratos concatenando región y sector de actividad económica
enadel <- enadel[!is.na(enadel$a2), ]
enadel$estrato <- as.numeric(interaction(enadel$reg_muestra, enadel$a2, drop = TRUE))

#Crear factor de expansión aleatorio entre 10 y 100
enadel <- enadel %>% 
  mutate(fact_emp = runif(nrow(enadel), min = 1, max = 100))

#Crear variable empresas=1
enadel <- enadel %>% mutate(empresas=1)

#Crear variable oficios=1
enadel <- enadel |> mutate(oficios=1)

#c (cantidad), d3 (ODC), e8 (Requiere capacitación), f9 (disminución por avances tecnológicos), f12 (creación por avances tecnológicos)

enadel$c1 <- long$oficio4[1:5066] #Ocupación
enadel$d3 <- long$oficio4[4001:9066] #Ocupaciones de Difícil Cobertura
enadel$e8 <- long$oficio4[2001:7066] #Cargos que requiere capacitación
enadel$f9 <- long$oficio4[9001:14066] #Cargos que han disminuido dotación de personal por avances tecnológicos
enadel$f12 <- long$oficio4[7001:12066] #cargos/ocupaciones que se crearon por avances tecnológicos

#sector de actividad económica

enadel$act_eco <- iconv(enadel$act_eco, from = "latin1", to = "UTF-8")

# Corregir caracteres mal codificados en la variable act_eco
enadel$act_eco <- gsub("Ã­", "í", enadel$act_eco)
enadel$act_eco <- gsub("Ã©", "é", enadel$act_eco)
enadel$act_eco <- gsub("Ã±", "ñ", enadel$act_eco)
enadel$act_eco <- gsub("Ã³", "ó", enadel$act_eco)
enadel$act_eco <- gsub("Ãº", "ú", enadel$act_eco)
enadel$act_eco <- gsub("Ã", "í", enadel$act_eco)
enadel$act_eco <- gsub("ï¿½", "ñ", enadel$act_eco)
enadel$act_eco <- gsub("ï¿½", "í", enadel$act_eco)
enadel$act_eco <- gsub("\\\\u0083Â³", "", enadel$act_eco) 
enadel$act_eco <- gsub("ñn", "ñ", enadel$act_eco)
enadel$act_eco <- gsub("ña", "ña", enadel$act_eco)
enadel$act_eco <- gsub("í\\s+", "í", enadel$act_eco) # E

#Eliminar valores
valores_a_eliminar <- c("Construccií\u0083Â³n", "Agricultura, ganaderí\u0083Â­a, silvicultura y pesca")
enadel <- enadel[!(enadel$act_eco %in% valores_a_eliminar), ]

# Tabla de frecuencia para verificar los resultados
table(enadel$act_eco)

#Homologación

enadel <- enadel |> mutate(act_eco=case_when(act_eco=="Actividades artisticas, de entretenimiento y recreativas"|
                                                act_eco=="Actividades artísticas, de entretenimiento y recreativas"|
                                                act_eco=="Actividades artñsticas, de entretenimiento y recreativas" ~ "Actividades artísticas, de entretenimiento y recreativas",
                                             act_eco=="Actividades profesionales, cientificas y tecnicas" |
                                               act_eco=="Actividades profesionales, científicas y técnicas" |
                                               act_eco=="Actividades profesionales, cientñficas y tñcnicas" ~ "Actividades profesionales, científicas y técnicas",
                                             act_eco== "Agricultura, ganaderia, silvicultura y pesca"|
                                               act_eco=="Agricultura, ganadería, silvicultura y pesca"|
                                               act_eco=="Agricultura, ganaderña, silvicultura y pesca" | 
                                               act_eco=="Agricultura, ganaderiña, silvicultura y pesca" ~ "Agricultura, ganadería, silvicultura y pesca",
                                             act_eco=="Construcciñ" | act_eco=="Construccion" | act_eco== "Construcción" ~ "Construcción",
                                             act_eco=="Informaciñ y comunicaciones" |act_eco=="Información y comunicaciones" ~ "Información y comunicaciones",
                                             TRUE ~ act_eco))

enadel$act_eco <- as.factor(enadel$act_eco)

enadel$act_eco <- factor(enadel$act_eco, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), labels = c("Actividades artísticas, de entretenimiento y recreativas",
                                                                                                     "Actividades de alojamiento y de servicio de comidas",
                                                                                                     "Actividades de servicios administrativos y de apoyo",
                                                                                                     "Actividades financieras y de seguros",
                                                                                                     "Actividades inmobiliarias",
                                                                                                     "Actividades profesionales, científicas y técnicas",
                                                                                                     "Agricultura, ganadería, silvicultura y pesca",
                                                                                                     "Comercio al por mayor y al por menor",
                                                                                                     "Construcción",
                                                                                                     "Industria manufacturera",
                                                                                                     "Información y comunicaciones",
                                                                                                     "Otras actividades de servicios",
                                                                                                     "Suministro de agua",
                                                                                                     "Suministro de electricidad, gas, vapor y aire acondicionado",
                                                                                                     "Transporte y almacenamiento"))


# --------------------------------------------------------------------------------

#tamaño de empresa por cantidad de trabajadores

enadel$b6_1 <- as.numeric(enadel$b6_1)
enadel <- enadel %>% mutate(tam_trab= case_when(b6_1 <= 50 ~ 2,
                                                          b6_1 > 50 & b6_1 <=199 ~ 3,
                                                          b6_1 >= 200 ~ 4))


#Filtrar a2 
enadel <- enadel |>
  filter(!(a2 %in% c(2, 15, 16, 17)))

#Crear experiencia 0 dicotomica

enadel <- enadel |> mutate(exp_cat= case_when(d3_e_1==0~ 0,
                                              d3_e_1==88 ~NA,
                                              d3_e_1 > 0~ 1))

enadel <- enadel |> mutate(d3_e_1= case_when(d3_e_1==88 ~ NA,
                                             TRUE ~ d3_e_1))



#Guardar datos ENADEL 2024

write_dta(enadel, "Bases/enadel_2024.dta")

