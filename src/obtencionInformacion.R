##########################################################################################
# Limpieza de los datos descargados del Portal de Datos Abiertos del Ayuntamiento 
# de Madrid. 
# La idea ha sido fabricar una especie de base de datos, en la que tengamos una tabla con 
# los avisos recibidos, otra con los resueltos, otra con los que quedan pendientes de 
# resolver y otras con el significado de los cÃ³digos ID de las anomalÃ­as, barrios, 
# secciones...
#########################################################################################

library(dplyr)
setwd("~/Documentos/Experto Data Science/TrabajoFinal")

#Carga de los datos. AVISOS RECIBIDOS
recibidos2014 <- read.csv('raw/recibidos_2014.csv',sep = ";", stringsAsFactors = TRUE, 
                          header = TRUE, encoding = 'latin1')
recibidos2015 <- read.csv('raw/recibidos_201511.csv', sep = ";", stringsAsFactors = TRUE, 
                          header = TRUE, encoding = 'latin1')
recibidos2016 <- read.csv('raw/recibidos_201612.csv', sep = ";", stringsAsFactors = TRUE, 
                          header = TRUE, encoding = 'latin1')
recibidos2017 <- read.csv('raw/recibidos_201707.csv', sep = ";", stringsAsFactors = TRUE, 
                          header = TRUE, encoding = 'latin1')

# Los uno en un mismo data.frame
recibidos <- rbind(recibidos2014, recibidos2015, recibidos2016, recibidos2017)
# creo una nueva columna para la direccion
recibidos$direccion <- paste(recibidos$NOMBRE_DE_VIAL, recibidos$NUMERO, sep = ' ')

recibidos$CANAL_DE_ENTRADA_ID[is.na(recibidos$CANAL_DE_ENTRADA_ID)] <- -1 
#los NA en realidad son presencial/telefÃ³nico

# paso a formato date porque lo necesitarÃ© mÃ¡s adelante
recibidos <- recibidos %>% 
  mutate(FECHA_DE_RECEPCION = as.Date(FECHA_DE_RECEPCION, format= '%d/%m/%Y')) 
#Para saber en quÃ© dÃ­a de la semana se recibiÃ³ el aviso
recibidos$weekday <- weekdays(recibidos$FECHA_DE_RECEPCION) 

# me quedo con la informaciÃ³n relevante, el significado de las IDs las clasifico en otras tablas a modo de BBDD
recibidos02 <- recibidos[, c("TIPO_INCIDENCIA_ID", "CANAL_DE_ENTRADA_ID", "FECHA_DE_RECEPCION",
                           "HORA_DE_RECEPCION", 'SECCION_ID', 'ANOMALIA_ID', "DISTRITO_ID",
                           "BARRIO_ID", 'direccion', "COORDENADA_OFICIAL_X", "COORDENADA_OFICIAL_Y", "weekday")]
recibidos02 <- as.data.frame(sapply(recibidos02, gsub, pattern = 'Ã', replacement = 'Ñ'))

saveRDS(recibidos02, 'res/recibidos.rds')
rm(recibidos2014, recibidos2015, recibidos2016, recibidos2017)

#Carga de los datos. AVISOS RESUELTOS
resueltos2014 <- read.csv('raw/resueltos_2014.csv', sep = ";", stringsAsFactors = TRUE, 
                          header = TRUE, encoding = 'latin1')
resueltos2015 <- read.csv('raw/resueltos_201512.csv', sep = ";", stringsAsFactors = TRUE, 
                          header = TRUE, encoding = 'latin1')
resueltos2016 <- read.csv('raw/resueltos_201612.csv', sep = ";", stringsAsFactors = TRUE, 
                          header = TRUE, encoding = 'latin1')
resueltos2017 <- read.csv('raw/resueltos_201707.csv', sep = ";", stringsAsFactors = TRUE, 
                          header = TRUE, encoding = 'latin1')

resueltos <- rbind(resueltos2014, resueltos2015, resueltos2016, resueltos2017)

resueltos$direccion <- paste(resueltos$NOMBRE_DE_VIAL, resueltos$NUMERO, sep = ' ')

resueltos$CANAL_DE_ENTRADA_ID[is.na(resueltos$CANAL_DE_ENTRADA_ID)] <- -1 
#los NA en realidad son presencial/telefÃ³nico

# para calcular el tiempo de resoluciÃ³n (en dÃ­as)
resueltos <- resueltos %>% 
  mutate(FECHA_DE_RECEPCION = as.Date(FECHA_DE_RECEPCION, format = '%d/%m/%Y'),
         FECHA_DE_RESOLUCION = as.Date(FECHA_DE_RESOLUCION, format = '%d/%m/%Y'))
resueltos$tiempo_de_resolucion <- (resueltos$FECHA_DE_RESOLUCION - resueltos$FECHA_DE_RECEPCION)

#para saber en quÃ© dÃ­a de la semana se abriÃ³ la incidencia y en quÃ© dÃ­a de la semana se solucionÃ³
resueltos$weekday <- weekdays(resueltos$FECHA_DE_RECEPCION)
resueltos$weekdayResolucion <- weekdays(resueltos$FECHA_DE_RESOLUCION)

# me quedo con las variables necesarias
resueltos02 <- resueltos[, c("TIPO_INCIDENCIA_ID", "CANAL_DE_ENTRADA_ID", "FECHA_DE_RECEPCION",
                           "HORA_DE_RECEPCION", "FECHA_DE_RESOLUCION", "HORA_DE_RESOLUCION",
                           'SECCION_ID', 'ANOMALIA_ID',"DISTRITO_ID", "BARRIO_ID", "direccion",
                           "COORDENADA_OFICIAL_X", "COORDENADA_OFICIAL_Y", "tiempo_de_resolucion", 
                           "weekday", "weekdayResolucion", "ORGANISMO_TRASLADO_NIVEL_1_ID", 
                           "ORGANISMO_TRASLADO_NIVEL_2_ID", "ORGANISMO_TRASLADO_NIVEL_3_ID")]
resueltos02 <- as.data.frame(sapply(resueltos02, gsub, pattern = 'Ã', replacement = 'Ñ'))

saveRDS(resueltos02, 'res/resueltos.rds')

## EXTRAIGO LOS QUE HAN QUEDADO SIN RESOLVER. serÃ¡n los avisos que estÃ¡n en recibidos, 
## pero no en resueltos (automÃ¡ticamente filtra por recepciÃ³n posterior al '2014-01-01', 
## ya que en recibidos sÃ³lo se recogen a partir de esa fecha)
sinresolver <- anti_join(recibidos, resueltos) 
#133187 avisos, de un total de 1731444 (el 7,69%)

sinresolver$weekday <- weekdays(sinresolver$FECHA_DE_RECEPCION)
sinresolver$weekday <- weekdays(sinresolver$FECHA_DE_RECEPCION)

sinresolver <- as.data.frame(sapply(sinresolver, gsub, pattern = 'Ã', replacement = 'Ñ'))

sinresolver <- sinresolver %>% 
  mutate(FECHA_DE_RECEPCION = as.Date(FECHA_DE_RECEPCION, format= '%Y-%m-%d'))
sinresolver$tiempo_sin_resolver <- Sys.Date() - sinresolver$FECHA_DE_RECEPCION
sinresolver <- sinresolver %>% 
  select("TIPO_INCIDENCIA_ID", "CANAL_DE_ENTRADA_ID", 'FECHA_DE_RECEPCION', 
         'HORA_DE_RECEPCION', 'SECCION_ID', 'ANOMALIA_ID', 'direccion', 
         'COORDENADA_OFICIAL_X', 'COORDENADA_OFICIAL_Y', 'tiempo_sin_resolver', 
         'DISTRITO_ID', "BARRIO_ID", "weekday")
sinresolver$ORGANISMO_TRASLADO_NIVEL_1_ID <- sinresolver$ORGANISMO_TRASLADO_NIVEL_2_ID <- sinresolver$ORGANISMO_TRASLADO_NIVEL_3_ID <- NA
saveRDS(sinresolver, 'res/sinresolver.rds')

# Me fabrico tablas con las ID del tipo de incidencia...
# los cÃ³digos de la ID de la anomalÃ­a no se proporciona, asÃ­ que tengo que sacarlo yo
IDs <- rbind(resueltos2014, resueltos2015, resueltos2016, resueltos2017)
IDs <- as.data.frame(sapply(IDs, gsub, pattern = 'Ã', replacement = 'Ñ'))

ID_tipoincidencia <- IDs %>% 
  select(TIPO_INCIDENCIA_ID, TIPO_INCIDENCIA) %>% 
  distinct() %>% 
  arrange(TIPO_INCIDENCIA_ID)
write.csv(ID_tipoincidencia, 'res/ID_tipoincidencia.csv', row.names = FALSE, fileEncoding = 'latin1')

ID_Canal <- IDs %>% 
  select(CANAL_DE_ENTRADA_ID, CANAL_DE_ENTRADA) %>% 
  distinct() %>% 
  arrange(CANAL_DE_ENTRADA_ID)
write.csv(ID_Canal, 'res/ID_canal.csv', row.names = FALSE, fileEncoding = 'latin1')

ID_Seccion <- IDs %>%
  select(SECCION_ID, SECCION) %>% 
  distinct() %>% 
  arrange(SECCION_ID)
which(duplicated(ID_Seccion$SECCION_ID))
ID_Seccion <- ID_Seccion[-c(38),]
write.csv(ID_Seccion, 'res/ID_seccion.csv', row.names = FALSE, fileEncoding = 'latin1')

ID_Anomalia <- IDs %>% 
  select(ANOMALIA_ID, ANOMALIA) %>% 
  distinct() %>% 
  arrange(ANOMALIA_ID)
which(duplicated(ID_Anomalia$ANOMALIA_ID))
ID_Anomalia <- ID_Anomalia[-c(1, 68, 139, 180, 202),]
write.csv(ID_Anomalia, 'res/ID_anomalia.csv', row.names = FALSE, fileEncoding = 'latin1')

ID_Distrito <- IDs %>% 
  select(DISTRITO_ID, DISTRITO) %>% 
  distinct() %>% 
  arrange(DISTRITO_ID)
write.csv(ID_Distrito, 'res/ID_distrito.csv', row.names = FALSE, fileEncoding = 'latin1')

ID_Barrio <- IDs %>% 
  select(BARRIO_ID, BARRIO) %>% 
  distinct() %>% 
  arrange(BARRIO_ID)
ID_Barrio <- ID_Barrio[-c(20, 32, 49, 71),]
write.csv(ID_Barrio, 'res/ID_barrio.csv', row.names = FALSE, fileEncoding = 'latin1')

ID_OrganismoNivel1 <- IDs %>% 
  select(ORGANISMO_TRASLADO_NIVEL_1_ID, ORGANISMO_TRASLADO_NIVEL_1) %>% 
  distinct() %>% 
  arrange(ORGANISMO_TRASLADO_NIVEL_1_ID)
ID_OrganismoNivel1 <- ID_OrganismoNivel1[-c(36),]
write.csv(ID_OrganismoNivel1, 'res/ID_Organismo1.csv', row.names = FALSE, fileEncoding = 'latin1')

ID_OrganismoNivel2 <- IDs %>% 
  select(ORGANISMO_TRASLADO_NIVEL_2_ID, ORGANISMO_TRASLADO_NIVEL_2) %>% 
  distinct() %>% 
  arrange(ORGANISMO_TRASLADO_NIVEL_2_ID)
ID_OrganismoNivel2 <- ID_OrganismoNivel2[-c(65),]
write.csv(ID_OrganismoNivel2, 'res/ID_Organismo2.csv', row.names = FALSE, fileEncoding = 'latin1')

ID_OrganismoNivel3 <- IDs %>% 
  select(ORGANISMO_TRASLADO_NIVEL_3_ID, ORGANISMO_TRASLADO_NIVEL_3) %>% 
  distinct() %>% 
  arrange(ORGANISMO_TRASLADO_NIVEL_3_ID)
ID_OrganismoNivel3 <- ID_OrganismoNivel3[-c(70),]
write.csv(ID_OrganismoNivel3, 'res/ID_Organismo3.csv', row.names = FALSE, fileEncoding = 'latin1')
