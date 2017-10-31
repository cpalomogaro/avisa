##########################################################################################
# Geolocalización de las bicicletas robadas
##########################################################################################

bici_robada <- recibidos %>% 
  filter(SECCION_ID== 57 & ANOMALIA_ID == 3561) %>% # los robos de bicimad corresponden a la seccion 57, 
  group_by(direccion) %>%                           # anomalía 3561
  count() %>% 
  filter(n > 2 & n < 280) #elimino C/Alcalá 1, debe de ser un artefacto, 
# la localización por defecto cuando se elige 'Madrid'

#Para poder pintar las localizaciones con ggmap necesito la longitud y latitud
#el dataset proporciona las coordenadas UTM pero no sirven.

bici_robada$address <- paste0(bici_robada$direccion, ', Madrid, Spain')
latlon <- geocode(bici_robada$address, output = 'latlona', source = 'google')
colnames(latlon)[3] <- 'direccion'
bici_robada_latlon <- cbind(latlon, bici_robada$n)
colnames(bici_robada_latlon)[4] <- 'num_veces'
write.csv(bici_robada_latlon, 'res/latlon_bicirobada.csv', row.names = FALSE, fileEncoding = 'latin1')

## a pesar de haber añadido "Madrid, Spain" a la dirección, la función geocode se equivoca al situar 
# nombres comunes de calles como los de las calles madrid y plaza madrid. Aquí los sitúa en Jaén, Valladolid y 
# Roquetas de Mar cuando también existen en Getafe y Mejorada. Podría darse el caso de que una de estas bicis 
# apareciese allí, pero he decidido obviarlo en la representación
# se podría afinar más incluyendo en la dirección el tipo de vía y el código postal

#Actualización Octubre 2017: El portal de datos abiertos incuye un datset con las coordenadas (longitud y latitud)
#de cada calle de Madrid
