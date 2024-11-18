 pacman::p_load("tidyverse","dplyr", "plyr", "knitr", "haven", "readxl", "writexl", "Hmisc", "survey", "labelled", "archive", "purrr")

 
enadel_2024 <- readxl::read_excel("base_ENADEL_2024.xlsx")

#Crear factor de expansión=1

enadel_2024 <- enadel_2024 %>% mutate(exp=1)


#Módulo A Identificación de la empresa ----
#Sector económico

unique(enadel_2024$a2)
enadel_2024 <- enadel_2024 %>% mutate(a2=case_when(a2==1 ~ "Agricultura, silvicultura y pesca",
                                                   a2==2 ~ "Minería",
                                                   a2==3 ~ "Industrias manufactureras",
                                                   a2==4 ~ "Suministro de electricidad y gas",
                                                   a2==5 ~ "Suministro de agua y gestión de desechos",
                                                   a2==6 ~ "Construcción",
                                                   a2==7 ~ "Comercio",
                                                   a2==8 ~ "Transporte y almacenamiento",
                                                   a2==9 ~ "Alojamiento y de servicio de comida",
                                                   a2==10 ~ "Información y comunicaciones",
                                                   a2==11 ~ "Actividades financieras y de seguros",
                                                   a2==12 ~ "Actividades inmobiliarias",
                                                   a2==13 ~ "Actividades profesionales y técnicas",
                                                   a2==14 ~ "Servicios administrativos y de apoyo",
                                                   a2==15 ~ "Administración pública",
                                                   a2==16 ~ "Enseñanza",
                                                   a2==17 ~ "Salud y asistencia social",
                                                   a2==18 ~ "Actividades artísticas y recreativas/ Otras
actividades de servicios"
                                                   ))
#Tramo de ventas
enadel_2024 <- enadel_2024 %>% mutate(a3=case_when(a3==1 ~ "Sin ventas 2023",
                                                   a3==2 ~ "Micro",
                                                   a3==3 ~ "Pequeña",
                                                   a3==4 ~ "Mediana",
                                                   a3==5 ~ "Grande",
                                                   TRUE ~ NA
                                                   ))
#Más de una sucursal
enadel_2024 <- enadel_2024 %>% mutate(a4=case_when(a4==1 ~"Sí",
                                                   a4==2 ~"No")) # No 444  Sí 56 

#regiones de las otras sucursales= múltiple a5_1,a5_2,a5_3,a5_4,a5_5,a5_6,a5_7,a5_8,a5_9,a5_10,a5_11,a5_12,a5_13,a5_14,a5_15,a5_16

#A6 Región en la que tiene más trabajadores contratados <<---------- Esta variable es la que manda en cuanto a región


table(enadel_2024$a6)#Hay que preguntar por esta codificación

#A7 ¿Cuál es la forma de propiedad de la empresa?

enadel_2024 <- enadel_2024 %>% mutate(a7= case_when(a7==1 ~ "Privada nacional",
                                                    a7==2 ~ "Privada extranjera",
                                                    a7==3 ~ "Estatal",
                                                    a7==4 ~ "Mixta"))

#A8 ¿Su empresa le ha prestado servicios a otras empresas a través de una subcontratación?
enadel_2024 <- enadel_2024 %>% mutate(a8= case_when(a8==1 ~ "Sí",
                                                    a8==2 ~ "No"))

#A9 ¿Pertenece la empresa a algún conglomerado o conjunto de empresas?
enadel_2024 <- enadel_2024 %>% mutate(a9= case_when(a9==1 ~ "Sí",
                                                    a9==2 ~ "No"))

#a10 ¿Su empresa pertenece a algún gremio empresarial?
enadel_2024 <- enadel_2024 %>% mutate(a10= case_when(a10==1 ~ "Sí",
                                                    a10==2 ~ "No"))

#Modulo B Demanda Laboral ----
# _2 = mujeres
# _3 = hombres
#---Dotación
#b1 Dotación de indefinidos 
#b2 Dotación de plazo fijo
#b3 Dotación de aprendizaje (practicantes)
#b4 Dotación de personal a honorarios
#b5 Dotación sin escrituración (personal contrato de palabra)
#b6 Total

#---Demanda
#b7 vacantes actuales
#b8 vacantes que se esperan abrir en lo que queda del año 2024
#b9 vacantes que se esparan abrir en el próximo año 2025


#---Salidas
#_2 = mujeres
#_3 = hombres
#b10 Renuncias
#b11 Despidos y Ceses


#b12 contrataciones (total, mujeres, hombres)

#b13 vacantes (Total de vacantes que no pudo llenar durante los últimos doce meses)

#Módulo C: Contrataciones ----

#c1 Podría indicar los 5 principales cargos/ocupaciones que contrató durante los últimos 12 meses  
# c1_a_1, c1_a_2, c1_a_3, c1_a_4, c1_a_5

#Esta variable no está codificada según CIUO 08!

#Cantidad de vacantes contratadas
#c1_c_1

#Nivel educacional
#c1_d_1, c1_d_2, c1_d_3, c1_d_4, c1_d_5



enadel_2024<- enadel_2024 %>% mutate(c1_d_1= case_when(c1_d_1== 0 ~ "Sin exigencia de educación formal",
                                                       c1_d_1== 1 ~ "Educación básica",
                                                       c1_d_1== 2 ~ "Educación media Científico humanista",
                                                       c1_d_1== 3 ~ "Educación media Técnico profesional",
                                                       c1_d_1== 4 ~ "Técnico superior",
                                                       c1_d_1== 5 ~ "Profesional",
                                                       c1_d_1== 6 ~ "Profesional con Magíster",
                                                       c1_d_1== 7 ~ "Profesional con doctorado"))

enadel_2024<- enadel_2024 %>% mutate(c1_d_2= case_when(c1_d_2== 0 ~ "Sin exigencia de educación formal",
                                                       c1_d_2== 1 ~ "Educación básica",
                                                       c1_d_2== 2 ~ "Educación media Científico humanista",
                                                       c1_d_2== 3 ~ "Educación media Técnico profesional",
                                                       c1_d_2== 4 ~ "Técnico superior",
                                                       c1_d_2== 5 ~ "Profesional",
                                                       c1_d_2== 6 ~ "Profesional con Magíster",
                                                       c1_d_2== 7 ~ "Profesional con doctorado"))

enadel_2024<- enadel_2024 %>% mutate(c1_d_3= case_when(c1_d_3== 0 ~ "Sin exigencia de educación formal",
                                                       c1_d_3== 1 ~ "Educación básica",
                                                       c1_d_3== 2 ~ "Educación media Científico humanista",
                                                       c1_d_3== 3 ~ "Educación media Técnico profesional",
                                                       c1_d_3== 4 ~ "Técnico superior",
                                                       c1_d_3== 5 ~ "Profesional",
                                                       c1_d_3== 6 ~ "Profesional con Magíster",
                                                       c1_d_3== 7 ~ "Profesional con doctorado"))

enadel_2024<- enadel_2024 %>% mutate(c1_d_4= case_when(c1_d_4== 0 ~ "Sin exigencia de educación formal",
                                                       c1_d_4== 1 ~ "Educación básica",
                                                       c1_d_4== 2 ~ "Educación media Científico humanista",
                                                       c1_d_4== 3 ~ "Educación media Técnico profesional",
                                                       c1_d_4== 4 ~ "Técnico superior",
                                                       c1_d_4== 5 ~ "Profesional",
                                                       c1_d_4== 6 ~ "Profesional con Magíster",
                                                       c1_d_4== 7 ~ "Profesional con doctorado"))

enadel_2024<- enadel_2024 %>% mutate(c1_d_5= case_when(c1_d_5== 0 ~ "Sin exigencia de educación formal",
                                                       c1_d_5== 1 ~ "Educación básica",
                                                       c1_d_5== 2 ~ "Educación media Científico humanista",
                                                       c1_d_5== 3 ~ "Educación media Técnico profesional",
                                                       c1_d_5== 4 ~ "Técnico superior",
                                                       c1_d_5== 5 ~ "Profesional",
                                                       c1_d_5== 6 ~ "Profesional con Magíster",
                                                       c1_d_5== 7 ~ "Profesional con doctorado"))



#Licencia certificación/requisito (múltiple máximo 3)
#c1_f_cert1_x, c1_f_cert2_x, c1_f_cert3_x. Con x e {1,5}


# Diccionario con las equivalencias
certificado_dict <- c(
  "1" = "Certificados de Antecedentes",
  "2" = "Certificado de Experiencia Laboral",
  "3" = "Certificado de Estudios",
  "4" = "Certificado de competencias laborales (ChileValora)",
  "5" = "Licencia de conducir Clase A",
  "6" = "Licencia de conducir Clase B",
  "7" = "Licencia de conducir Clase C",
  "8" = "Licencia de conducir Clase D",
  "9" = "Licencia de conducir Clase E",
  "10" = "Certificados de acreditación de cursos",
  "11" = "Capacitaciones relacionadas al rubro y ocupación",
  "12" = "Certificados de Salud y/o Afiliación",
  "13" = "Hoja de vida del conductor",
  "14" = "Otra"
)

# Función para transformar una columna según el diccionario
transformar_certificado <- function(df, columna, dict) {
  df[[columna]] <- as.character(dict[as.character(df[[columna]])])
  return(df)
}

# Aplicar la transformación a todas las columnas deseadas
columnas_a_transformar <- c("c1_f_cert1_1", "c1_f_cert2_1", "c1_f_cert3_1",
                            "c1_f_cert1_2", "c1_f_cert2_2", "c1_f_cert3_2",
                            "c1_f_cert1_3", "c1_f_cert2_3", "c1_f_cert3_3",
                            "c1_f_cert1_4", "c1_f_cert2_4", "c1_f_cert3_4",
                            "c1_f_cert1_5", "c1_f_cert2_5", "c1_f_cert3_5")
for (columna in columnas_a_transformar) {
  enadel_2024 <- transformar_certificado(enadel_2024, columna, certificado_dict)
}


# Canales o medios de contratación (múltiple con orden de importancia)

# Diccionario con las equivalencias
canales_contrato <- c(
  "1" = "Diario o radio",
  "2" = "Plataforma web (trabajando.com, laborum, linkedin, Yapo u otra) (excluyendo redes sociales)",
  "3" = "Redes sociales (facebook, twitter, instagram, linkedin gratuito etc.)",
  "4" = "Bolsa Nacional de Empleo (BNE)",
  "5" = "Oficina Municipal de Información Laboral (OMIL)",
  "6" = "Redes de profesionales, egresados (mailing list) o contacto con universidades",
  "7" = "Plataforma web de la empresa",
  "8" = "Recomendaciones de trabajadores de la empresa u otros actores (boca a boca)",
  "9" = "Contratación de empresas de reclutamiento, intermediadores o enganchadores",
  "10" = "Avisos en las inmediaciones de la empresa o banco de Curriculums",
  "11" = "Redes personales del empleador",
  "12" = "Otro"
)

# Función para transformar una columna según el diccionario
transformar_canales <- function(df, columna, dict) {
  df[[columna]] <- as.character(dict[as.character(df[[columna]])])
  return(df)
}

columnas_canales <- c("c2_1", "c2_2", "c2_3")

for (columna in columnas_canales) {
  enadel_2024 <- transformar_canales(enadel_2024, columna, canales_contrato)
}

# Módulo D: Difícil Cobertura ----

#D1. Durante los últimos doce meses, ¿La empresa tuvo vacantes que no pudo llenar?

#D2. ¿cuántos cargos no pudo llenar?

#D3. Por favor indicar los 5 principales cargos/ocupaciones que no pudo llenar

# Variable de Cargo d3_a_1,d3_a_2,d3_a_3,d3_a_4,d3_a_5 estas variables no están codificadas en CIUO 08!

#Dificultad de contratación
# d3_c_1, d3_c_2, d3_c_3, d3_c_4, d3_c_5 (múltiple, máx 5, min 1)


enadel_2024<- enadel_2024 %>% mutate(d3_c_1=case_when(d3_c_1==1 ~ "Candidatos sin competencias o habilidades técnicas requeridas",
                                                      d3_c_1==2 ~ "Candidatos sin habilidades blandas o socioemocionales requeridas",
                                                      d3_c_1==3 ~ "Candidatos sin nivel educacional requerido",
                                                      d3_c_1==4 ~ "Candidatos sin licencias, certificaciones o requisitos legales",
                                                      d3_c_1==5 ~ "Candidatos sin la experiencia laboral mínima requerida",
                                                      d3_c_1==6 ~ "Remuneración ofrecida no aceptada",
                                                      d3_c_1==7 ~ "Condiciones laborales no aceptadas",
                                                      d3_c_1==8 ~ "Falta de postulantes",
                                                      d3_c_1==9 ~ "Otra dificultad"))


enadel_2024<- enadel_2024 %>% mutate(d3_c_2=case_when(d3_c_2==1 ~ "Candidatos sin competencias o habilidades técnicas requeridas",
                                                      d3_c_2==2 ~ "Candidatos sin habilidades blandas o socioemocionales requeridas",
                                                      d3_c_2==3 ~ "Candidatos sin nivel educacional requerido",
                                                      d3_c_2==4 ~ "Candidatos sin licencias, certificaciones o requisitos legales",
                                                      d3_c_2==5 ~ "Candidatos sin la experiencia laboral mínima requerida",
                                                      d3_c_2==6 ~ "Remuneración ofrecida no aceptada",
                                                      d3_c_2==7 ~ "Condiciones laborales no aceptadas",
                                                      d3_c_2==8 ~ "Falta de postulantes",
                                                      d3_c_2==9 ~ "Otra dificultad"))


enadel_2024<- enadel_2024 %>% mutate(d3_c_3=case_when(d3_c_3==1 ~ "Candidatos sin competencias o habilidades técnicas requeridas",
                                                      d3_c_3==2 ~ "Candidatos sin habilidades blandas o socioemocionales requeridas",
                                                      d3_c_3==3 ~ "Candidatos sin nivel educacional requerido",
                                                      d3_c_3==4 ~ "Candidatos sin licencias, certificaciones o requisitos legales",
                                                      d3_c_3==5 ~ "Candidatos sin la experiencia laboral mínima requerida",
                                                      d3_c_3==6 ~ "Remuneración ofrecida no aceptada",
                                                      d3_c_3==7 ~ "Condiciones laborales no aceptadas",
                                                      d3_c_3==8 ~ "Falta de postulantes",
                                                      d3_c_3==9 ~ "Otra dificultad"))

enadel_2024<- enadel_2024 %>% mutate(d3_c_4=case_when(d3_c_4==1 ~ "Candidatos sin competencias o habilidades técnicas requeridas",
                                                      d3_c_4==2 ~ "Candidatos sin habilidades blandas o socioemocionales requeridas",
                                                      d3_c_4==3 ~ "Candidatos sin nivel educacional requerido",
                                                      d3_c_4==4 ~ "Candidatos sin licencias, certificaciones o requisitos legales",
                                                      d3_c_4==5 ~ "Candidatos sin la experiencia laboral mínima requerida",
                                                      d3_c_4==6 ~ "Remuneración ofrecida no aceptada",
                                                      d3_c_4==7 ~ "Condiciones laborales no aceptadas",
                                                      d3_c_4==8 ~ "Falta de postulantes",
                                                      d3_c_4==9 ~ "Otra dificultad"))

enadel_2024<- enadel_2024 %>% mutate(d3_c_5=case_when(d3_c_5==1 ~ "Candidatos sin competencias o habilidades técnicas requeridas",
                                                      d3_c_5==2 ~ "Candidatos sin habilidades blandas o socioemocionales requeridas",
                                                      d3_c_5==3 ~ "Candidatos sin nivel educacional requerido",
                                                      d3_c_5==4 ~ "Candidatos sin licencias, certificaciones o requisitos legales",
                                                      d3_c_5==5 ~ "Candidatos sin la experiencia laboral mínima requerida",
                                                      d3_c_5==6 ~ "Remuneración ofrecida no aceptada",
                                                      d3_c_5==7 ~ "Condiciones laborales no aceptadas",
                                                      d3_c_5==8 ~ "Falta de postulantes",
                                                      d3_c_5==9 ~ "Otra dificultad"))


#Módulo E: Capacitación y Habilidades

#E1. ¿La empresa se encuentra adherida a algún Organismo Técnico Intermedio para Capacitación (OTIC)?

enadel_2024<- enadel_2024 %>% mutate(e1=case_when(e1==1 ~"Sí",
                                                  e1==2 ~"No",
                                                  TRUE~ NA))


#E2. Durante los últimos doce meses, ¿La empresa ha proporcionado o realizado capacitaciones laborales para su personal?

enadel_2024<- enadel_2024 %>% mutate(e2=case_when(e2==1 ~"Sí",
                                                  e2==2 ~"No",
                                                  TRUE~ NA))

#E3 (sólo si E2=Sí) En los últimos doce meses, ¿En cuál de las siguientes competencias capacitó a sus trabajadores(as)?
#Se cuentan sólo las que son iguales a "Sí" ya que es múltiple

#Competencias básicas
enadel_2024 <- enadel_2024 %>% mutate(e3_a=case_when(e3_a=="1"~ "Sí",
                                                     e3_a=="2"~ "No",
                                                     TRUE~NA))



#Competencias técnicas
enadel_2024 <- enadel_2024 %>% mutate(e3_b=case_when(e3_b=="1"~ "Sí",
                                                     e3_b=="2"~ "No",
                                                     TRUE~NA))

#Competencias actitudinales, conductuales, emocionales y/o valóricas
enadel_2024 <- enadel_2024 %>% mutate(e3_c=case_when(e3_c=="1"~ "Sí",
                                                     e3_c=="2"~ "No",
                                                     TRUE~NA))
#Competencias en idiomas extranjeros
enadel_2024 <- enadel_2024 %>% mutate(e3_d=case_when(e3_d=="1"~ "Sí",
                                                     e3_d=="2"~ "No",
                                                     TRUE~NA))
#Competencias directivas o de gestión
enadel_2024 <- enadel_2024 %>% mutate(e3_e=case_when(e3_e=="1"~ "Sí",
                                                     e3_e=="2"~ "No",
                                                     TRUE~NA))

#Competencias digitales básicas
enadel_2024 <- enadel_2024 %>% mutate(e3_f=case_when(e3_f=="1"~ "Sí",
                                                     e3_f=="2"~ "No",
                                                     TRUE~NA))
#Competencias digitales avanzadas
enadel_2024 <- enadel_2024 %>% mutate(e3_g=case_when(e3_g=="1"~ "Sí",
                                                     e3_g=="2"~ "No",
                                                     TRUE~NA))

#Competencias de salud ocupacional y prevención
enadel_2024 <- enadel_2024 %>% mutate(e3_h=case_when(e3_h=="1"~ "Sí",
                                                     e3_h=="2"~ "No",
                                                     TRUE~NA))

#Otra
enadel_2024 <- enadel_2024 %>% mutate(e3_i=case_when(e3_i=="1"~ "Sí",
                                                     e3_i=="2"~ "No",
                                                     TRUE~NA))



#E4 (sólo si E2=Sí) Durante los últimos doce meses, ¿las capacitaciones realizadas fueron ejecutadas
#principalmente por? (Múltiple)

#e4_1 La misma empresa 
#e4_2 Un Organismo Técnico de Capacitación (OTEC) 
#e4_3 Una mutual o el Instituto de Seguridad Laboral (ISL)
#e4_4 Una institución educativa (instituto, universidades, CFT, IP, etc.).
#e4_5 Otra institución (fundación, empresa, etc.)

#E5 (sólo si E2=Sí) Durante los últimos doce meses, ¿Cuáles fueron las principales fuentes de financiamiento
#de la capacitación?

#e5_1 Financiamiento vía franquicia tributaria (SENCE) 
#e5_2 Programa público de capacitación (FOSIS, Mineduc, SENCE-FONCAP u otro) 
#e5_3 Recursos propios de la empresa
#e5_4 Recursos propios de los/as trabajadores(as).
#e5_5 Beca de institución privada
#e5_6 Una mutual / Instituto de Seguridad Laboral (ISL)
#e5_7 Otra

#E6 (Sólo si e2=No) Aunque no capacitó, durante los últimos doce meses, ¿Ha requerido la empresa capacitar a
#sus empleado/as en alguna de las siguientes competencias?

#Competencias básicas
enadel_2024 <- enadel_2024 %>% mutate(e6_a=case_when(e6_a=="1"~ "Sí",
                                                     e6_a=="2"~ "No",
                                                     TRUE~NA))

#Competencias técnicas
enadel_2024 <- enadel_2024 %>% mutate(e6_b=case_when(e6_b=="1"~ "Sí",
                                                     e6_b=="2"~ "No",
                                                     TRUE~NA))

#Competencias actitudinales, conductuales, emocionales y/o valóricas
enadel_2024 <- enadel_2024 %>% mutate(e6_c=case_when(e6_c=="1"~ "Sí",
                                                     e6_c=="2"~ "No",
                                                     TRUE~NA))
#Competencias en idiomas extranjeros
enadel_2024 <- enadel_2024 %>% mutate(e6_d=case_when(e6_d=="1"~ "Sí",
                                                     e6_d=="2"~ "No",
                                                     TRUE~NA))
#Competencias directivas o de gestión
enadel_2024 <- enadel_2024 %>% mutate(e6_e=case_when(e6_e=="1"~ "Sí",
                                                     e6_e=="2"~ "No",
                                                     TRUE~NA))

#Competencias digitales básicas
enadel_2024 <- enadel_2024 %>% mutate(e6_f=case_when(e6_f=="1"~ "Sí",
                                                     e6_f=="2"~ "No",
                                                     TRUE~NA))
#Competencias digitales avanzadas
enadel_2024 <- enadel_2024 %>% mutate(e6_g=case_when(e6_g=="1"~ "Sí",
                                                     e6_g=="2"~ "No",
                                                     TRUE~NA))

#Competencias de salud ocupacional y prevención
enadel_2024 <- enadel_2024 %>% mutate(e6_h=case_when(e6_h=="1"~ "Sí",
                                                     e6_h=="2"~ "No",
                                                     TRUE~NA))

#Otra
enadel_2024 <- enadel_2024 %>% mutate(e6_i=case_when(e6_i=="1"~ "Sí",
                                                     e6_i=="2"~ "No",
                                                     TRUE~NA))

#E7 Durante los próximos doce meses, ¿El personal de su empresa requerirá capacitación?
enadel_2024 <- enadel_2024 %>% mutate(e7=case_when(e7==1~"Sí",
                                                   e7==1~"No",
                                                   TRUE ~ NA))

#E8. Podría indicar los principales cargos/ocupaciones que requerirán capacitación? Indique las
#principales tareas asociadas al puesto y competencias asociadas al puesto. (múltipe máximo 3)

#Esta variable no está codificada según CIUO 08!

#Competencias (tarjeta 8)

#Competencias básicas
enadel_2024 <- enadel_2024 %>% mutate(e8_c1_1=case_when(e8_c1_1=="1"~ "Sí",
                                                        e8_c1_1=="2"~ "No",
                                                     TRUE~NA))

enadel_2024 <- enadel_2024 %>% mutate(e8_c1_2=case_when(e8_c1_2=="1"~ "Sí",
                                                        e8_c1_2=="2"~ "No",
                                                        TRUE~NA))

enadel_2024 <- enadel_2024 %>% mutate(e8_c1_3=case_when(e8_c1_3=="1"~ "Sí",
                                                        e8_c1_3=="2"~ "No",
                                                        TRUE~NA))

#Competencias técnicas
enadel_2024 <- enadel_2024 %>% mutate(e8_c2_1=case_when(e8_c2_1=="1"~ "Sí",
                                                        e8_c2_1=="2"~ "No",
                                                        TRUE~NA))

enadel_2024 <- enadel_2024 %>% mutate(e8_c2_2=case_when(e8_c2_2=="1"~ "Sí",
                                                        e8_c2_2=="2"~ "No",
                                                        TRUE~NA))

enadel_2024 <- enadel_2024 %>% mutate(e8_c2_3=case_when(e8_c2_3=="1"~ "Sí",
                                                        e8_c2_3=="2"~ "No",
                                                        TRUE~NA))

#Competencias actitudinales, conductuales, emocionales y/o valóricas
enadel_2024 <- enadel_2024 %>% mutate(e8_c3_1=case_when(e8_c3_1=="1"~ "Sí",
                                                        e8_c3_1=="2"~ "No",
                                                        TRUE~NA))

enadel_2024 <- enadel_2024 %>% mutate(e8_c3_2=case_when(e8_c3_2=="1"~ "Sí",
                                                        e8_c3_2=="2"~ "No",
                                                        TRUE~NA))

enadel_2024 <- enadel_2024 %>% mutate(e8_c3_3=case_when(e8_c3_3=="1"~ "Sí",
                                                        e8_c3_3=="2"~ "No",
                                                        TRUE~NA))

#Competencias en idiomas extranjeros
enadel_2024 <- enadel_2024 %>% mutate(e8_c4_1=case_when(e8_c4_1=="1"~ "Sí",
                                                        e8_c4_1=="2"~ "No",
                                                        TRUE~NA))

enadel_2024 <- enadel_2024 %>% mutate(e8_c4_2=case_when(e8_c4_2=="1"~ "Sí",
                                                        e8_c4_2=="2"~ "No",
                                                        TRUE~NA))

enadel_2024 <- enadel_2024 %>% mutate(e8_c4_3=case_when(e8_c4_3=="1"~ "Sí",
                                                        e8_c4_3=="2"~ "No",
                                                        TRUE~NA))

#Competencias directivas o de gestión
enadel_2024 <- enadel_2024 %>% mutate(e8_c5_1=case_when(e8_c5_1=="1"~ "Sí",
                                                        e8_c5_1=="2"~ "No",
                                                        TRUE~NA))

enadel_2024 <- enadel_2024 %>% mutate(e8_c5_2=case_when(e8_c5_2=="1"~ "Sí",
                                                        e8_c5_2=="2"~ "No",
                                                        TRUE~NA))

enadel_2024 <- enadel_2024 %>% mutate(e8_c5_3=case_when(e8_c5_3=="1"~ "Sí",
                                                        e8_c5_3=="2"~ "No",
                                                        TRUE~NA))

#Competencias digitales básicas
enadel_2024 <- enadel_2024 %>% mutate(e8_c6_1=case_when(e8_c6_1=="1"~ "Sí",
                                                        e8_c6_1=="2"~ "No",
                                                        TRUE~NA))

enadel_2024 <- enadel_2024 %>% mutate(e8_c6_2=case_when(e8_c6_2=="1"~ "Sí",
                                                        e8_c6_2=="2"~ "No",
                                                        TRUE~NA))

enadel_2024 <- enadel_2024 %>% mutate(e8_c6_3=case_when(e8_c6_3=="1"~ "Sí",
                                                        e8_c6_3=="2"~ "No",
                                                        TRUE~NA))
#Competencias digitales avanzadas
enadel_2024 <- enadel_2024 %>% mutate(e8_c7_1=case_when(e8_c7_1=="1"~ "Sí",
                                                        e8_c7_1=="2"~ "No",
                                                        TRUE~NA))

enadel_2024 <- enadel_2024 %>% mutate(e8_c7_2=case_when(e8_c7_2=="1"~ "Sí",
                                                        e8_c7_2=="2"~ "No",
                                                        TRUE~NA))

enadel_2024 <- enadel_2024 %>% mutate(e8_c7_3=case_when(e8_c7_3=="1"~ "Sí",
                                                        e8_c7_3=="2"~ "No",
                                                        TRUE~NA))

#Competencias de salud ocupacional y prevención
enadel_2024 <- enadel_2024 %>% mutate(e8_c8_1=case_when(e8_c8_1=="1"~ "Sí",
                                                        e8_c8_1=="2"~ "No",
                                                        TRUE~NA))

enadel_2024 <- enadel_2024 %>% mutate(e8_c8_2=case_when(e8_c8_2=="1"~ "Sí",
                                                        e8_c8_2=="2"~ "No",
                                                        TRUE~NA))

enadel_2024 <- enadel_2024 %>% mutate(e8_c8_3=case_when(e8_c8_3=="1"~ "Sí",
                                                        e8_c8_3=="2"~ "No",
                                                        TRUE~NA))

#Otra
enadel_2024 <- enadel_2024 %>% mutate(e8_c9_1=case_when(e8_c9_1=="1"~ "Sí",
                                                        e8_c9_1=="2"~ "No",
                                                        TRUE~NA))

enadel_2024 <- enadel_2024 %>% mutate(e8_c9_2=case_when(e8_c9_2=="1"~ "Sí",
                                                        e8_c9_2=="2"~ "No",
                                                        TRUE~NA))

enadel_2024 <- enadel_2024 %>% mutate(e8_c9_3=case_when(e8_c9_3=="1"~ "Sí",
                                                        e8_c9_3=="2"~ "No",
                                                        TRUE~NA))


#Módulo F: Transiciones

#F1. Durante los últimos doce meses, ¿Qué tan afectada ha estado su empresa por 
#las consecuencias de eventos climáticos extremos como olas de calor, heladas,
# inundaciones, marejadas, sequía, incendios forestales entre otros?

enadel_2024<- enadel_2024 %>% mutate(f1= case_when(f1=="1" ~ "Muy Afectada",
                                                   f1=="2" ~ "Medianamente afectada",
                                                   f1=="3" ~ "Poco afectada",
                                                   f1=="4" ~ "La empresa no ha sido afectada",
                                                   TRUE ~ NA))

#F2a. ¿Qué tan afectada cree que estará...en los proximos 12 meses?

enadel_2024<- enadel_2024 %>% mutate(f2_a= case_when(f2_a=="1" ~ "Muy Afectada",
                                                   f2_a=="2" ~ "Medianamente afectada",
                                                   f2_a=="3" ~ "Poco afectada",
                                                   f2_a=="4" ~ "La empresa no ha sido afectada",
                                                   TRUE ~ NA))

#F2b. ¿Qué tan afectada cree que estará...en 5 años más?
enadel_2024<- enadel_2024 %>% mutate(f2_b= case_when(f2_b=="1" ~ "Muy Afectada",
                                                   f2_b=="2" ~ "Medianamente afectada",
                                                   f2_b=="3" ~ "Poco afectada",
                                                   f2_b=="4" ~ "La empresa no ha sido afectada",
                                                   TRUE ~ NA))


#f3. Durante los últimos 12 meses, ¿Hubo cambios en la dotación del personal de la empresa
#como consecuencias de eventos climáticos extremos como olas de calor, heladas,
#inundaciones, marejadas, sequía, incendios forestales entre otros?
enadel_2024<- enadel_2024 %>% mutate(f3=case_when(f3=="1"~"Disminuyó la dotación de personal",
                                                  f3=="2"~"No ha cambiado la dotación de personal",
                                                  f3=="3"~"Aumentó la dotación de personal",
                                                  TRUE ~ NA))

#f4. De los siguientes eventos climáticos extremos ¿Cuáles considera que son o serán un
#problema importante para su empresa dentro de los próximos 5 años? Opción múltiple (DUMMYS)

#Sequía: f4_1

#Inundaciones: f4_2

#Olas de calor:f4_3

#Heladas:f4_4

#Incendios Forestales:f4_5

#Marejadas:f4_6

#Aumento del nivel del mar:f4_7

#Avalanchas o aluviones:f4_8

#Otra: f4_9

#Ninguna: f4_10

#f5. ¿Cuál de las siguientes medidas ha adoptado su empresa para el cuidado del
# medio ambiente? Opción múltiple (señale hasta 5)

enadel_2024 <- enadel_2024 %>% mutate(f5_1= case_when(f5_1==1 ~ "Reciclaje de los desechos producidos por su empresa (más allá de la Ley REP)",
                                                      f5_1==2 ~ "Planes ahorros de energía y eficiencia energética",
                                                      f5_1==3 ~ "Reconversión de procesos productivos a energías limpias",
                                                      f5_1==4 ~ "Utilización de insumos sustentables o biodegradables",
                                                      f5_1==5 ~ "Obtención de certificaciones ambientales nacionales o internacionales",
                                                      f5_1==6 ~ "Medición de gases de efecto invernadero/huella de carbono",
                                                      f5_1==7 ~ "Transacción de bonos de carbono/emisiones",
                                                      f5_1==8 ~ "Capacitación, educación o investigación en temáticas relacionada con el cambio climático, sostenibilidad y sus impactos",
                                                      f5_1==9 ~ "Proyectos sociales verdes y de ayuda a la comunidad local",
                                                      f5_1==10 ~ "Ninguna",
                                                      f5_1==11 ~ "Otra"))

enadel_2024 <- enadel_2024 %>% mutate(f5_2= case_when(f5_2==1 ~ "Reciclaje de los desechos producidos por su empresa (más allá de la Ley REP)",
                                                      f5_2==2 ~ "Planes ahorros de energía y eficiencia energética",
                                                      f5_2==3 ~ "Reconversión de procesos productivos a energías limpias",
                                                      f5_2==4 ~ "Utilización de insumos sustentables o biodegradables",
                                                      f5_2==5 ~ "Obtención de certificaciones ambientales nacionales o internacionales",
                                                      f5_2==6 ~ "Medición de gases de efecto invernadero/huella de carbono",
                                                      f5_2==7 ~ "Transacción de bonos de carbono/emisiones",
                                                      f5_2==8 ~ "Capacitación, educación o investigación en temáticas relacionada con el cambio climático, sostenibilidad y sus impactos",
                                                      f5_2==9 ~ "Proyectos sociales verdes y de ayuda a la comunidad local",
                                                      f5_2==10 ~ "Ninguna",
                                                      f5_2==11 ~ "Otra" ))

enadel_2024 <- enadel_2024 %>% mutate(f5_3= case_when(f5_3==1 ~ "Reciclaje de los desechos producidos por su empresa (más allá de la Ley REP)",
                                                      f5_3==2 ~ "Planes ahorros de energía y eficiencia energética",
                                                      f5_3==3 ~ "Reconversión de procesos productivos a energías limpias",
                                                      f5_3==4 ~ "Utilización de insumos sustentables o biodegradables",
                                                      f5_3==5 ~ "Obtención de certificaciones ambientales nacionales o internacionales",
                                                      f5_3==6 ~ "Medición de gases de efecto invernadero/huella de carbono",
                                                      f5_3==7 ~ "Transacción de bonos de carbono/emisiones",
                                                      f5_3==8 ~ "Capacitación, educación o investigación en temáticas relacionada con el cambio climático, sostenibilidad y sus impactos",
                                                      f5_3==9 ~ "Proyectos sociales verdes y de ayuda a la comunidad local",
                                                      f5_3==10 ~ "Ninguna",
                                                      f5_3==11 ~ "Otra" ))

enadel_2024 <- enadel_2024 %>% mutate(f5_4= case_when(f5_4==1 ~ "Reciclaje de los desechos producidos por su empresa (más allá de la Ley REP)",
                                                      f5_4==2 ~ "Planes ahorros de energía y eficiencia energética",
                                                      f5_4==3 ~ "Reconversión de procesos productivos a energías limpias",
                                                      f5_4==4 ~ "Utilización de insumos sustentables o biodegradables",
                                                      f5_4==5 ~ "Obtención de certificaciones ambientales nacionales o internacionales",
                                                      f5_4==6 ~ "Medición de gases de efecto invernadero/huella de carbono",
                                                      f5_4==7 ~ "Transacción de bonos de carbono/emisiones",
                                                      f5_4==8 ~ "Capacitación, educación o investigación en temáticas relacionada con el cambio climático, sostenibilidad y sus impactos",
                                                      f5_4==9 ~ "Proyectos sociales verdes y de ayuda a la comunidad local",
                                                      f5_4==10 ~ "Ninguna",
                                                      f5_4==11 ~ "Otra" ))

enadel_2024 <- enadel_2024 %>% mutate(f5_5= case_when(f5_5==1 ~ "Reciclaje de los desechos producidos por su empresa (más allá de la Ley REP)",
                                                      f5_5==2 ~ "Planes ahorros de energía y eficiencia energética",
                                                      f5_5==3 ~ "Reconversión de procesos productivos a energías limpias",
                                                      f5_5==4 ~ "Utilización de insumos sustentables o biodegradables",
                                                      f5_5==5 ~ "Obtención de certificaciones ambientales nacionales o internacionales",
                                                      f5_5==6 ~ "Medición de gases de efecto invernadero/huella de carbono",
                                                      f5_5==7 ~ "Transacción de bonos de carbono/emisiones",
                                                      f5_5==8 ~ "Capacitación, educación o investigación en temáticas relacionada con el cambio climático, sostenibilidad y sus impactos",
                                                      f5_5==9 ~ "Proyectos sociales verdes y de ayuda a la comunidad local",
                                                      f5_5==10 ~ "Ninguna",
                                                      f5_5==11 ~ "Otra" ))

unique(enadel_2024$f5_1)
