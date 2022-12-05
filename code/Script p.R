#### Laura Morales Vernaza - 201821520
#### Problem set 3
#### R version 4.2.1


### Configuración inicial
rm(list=ls())

## llamar pacman 
require(pacman)

### Cargar librerías que voy a usar
p_load(tidyverse,rio,skimr,
       sf, ## datos espaciales
       leaflet, ## visualizaciones
       tmaptools, ## geocodificar
       ggsn, ## map scale bar 
       osmdata, ## packages with census data
       rvest,  ## web scrapping 
       modelsummary, # Coefplot with modelplot
       stargazer, # export tables to latex 
       wordcloud, # nube de palabras
       RColorBrewer) 


### 1. Regresiones

data_regresiones <- readRDS("~/GitHub/pset-3/input/data_regresiones.rds") # cargar base de datos en un objeto


## 1.1 Estimaciones

# Estimación 1
price.lm1<-lm(price ~ rooms + bathrooms + surface_total, data = data_regresiones)# regresión lineal multiple para estimar el efecto de la cantidad de cuartos, baños y superficie de la vivienda sobre el precio de la vivienda.
summary(price.lm1) # visualizar los resultados de la regresión

# Estimación 2
price.lm2<-lm(price ~ dist_cbd + dist_cole + dist_park, data = data_regresiones)# regresión lineal multiple para estimar el efecto de las distancias a parques, colegios y al centro de negocios de la ciudad sobre el precio de la vivienda.
summary(price.lm2) # visualizar los resultados de la regresión

# Estimación 3
price.lm3<-lm(price ~ bathrooms + dist_cole + surface_total, data = data_regresiones)# regresión lineal multiple para estimar el efecto de la distancias a colegios, la cantidad de baños y la superficie total de la vivienda sobre el precio de la vivienda.
summary(price.lm3) # visualizar los resultados de la regresión


## 1.2 Presentar resultados

msummary(list(price.lm1, price.lm1 , price.lm3)) # tabla con coeficientes 


modelos = list('reg1' = price.lm1 , 'reg2' = price.lm2 , "reg3" = price.lm3)

modelplot(modelos) + coord_flip() + 
  labs(title = "Precios de las viviendas" , subtitle = "Comparación de variables")


## 1.3 Exportar resultados

stargazer(price.lm1, price.lm2, price.lm3, 
          type= 'text',
          dep.var.labels = c('','price',''), 
          df = FALSE,
          digits = 3, 
          out = paste0('output/resultados_regresiones.xlsx')) # exportar tabla



### 2. Datos espaciales

## 2.1 Descargar datos
opq(bbox = getbb("Cali Colombia")) # descargar datos de Cali.

parques <- opq(bbox = getbb("Cali Colombia")) %>% # descargar datos de parques en Cali.
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf() %>% .$osm_polygons %>% select(osm_id,name)


osm = opq(bbox = getbb("Cali Colombia")) %>%
  add_osm_feature(key="amenity" , value="restaurant") # 
class(osm)

osm_sf = osm %>% osmdata_sf() # extraer colección de Simple Features
osm_sf

restaurantes = osm_sf$osm_points %>% select(osm_id,amenity) # 
restaurantes

#restaurantes <- opq ("Cali Colombia") %>%
 # add_osm_feature (key = "amenity", value = "restaurant") # descargar datos de restaurantes en Cali.


## 2.2 Visualizaciones

leaflet() %>% addTiles() %>% addPolygons(data=parques) # visualizar parques

leaflet() %>% addTiles() %>% addCircleMarkers(data=restaurantes, col="red")


## 2.3

geocode_OSM("Museo La Tertulia, Cali") # usar geocode para encontrar una dirección (la tertulia)

## basic plot
ggplot() + geom_sf(data= parques)

## plot variable
parques$normal <- rnorm(nrow(parques),100,10)
ggplot() + geom_sf(data=parques , aes(fill=normal))

## plot variable + scale
map <- ggplot() + geom_sf(data=parques , aes(fill=normal)) +
  scale_fill_viridis(option = "A" , name = "Variable")
map 

## add scale_bar
map <- map +
  scalebar(data = parques , dist = 5 , transform = T , dist_unit = "km") +
  north(data = parques , location = "topleft")
map 

## add theme
map <- map + theme_linedraw() + labs(x="" , y="")
map

## add osm layer
osm_layer <- get_stamenmap(bbox = as.vector(st_bbox(bog)), 
                           maptype="toner", source="osm", zoom=13) 

map2 <- ggmap(osm_layer) + 
  geom_sf(data=bog , aes(fill=normal) , alpha=0.3 , inherit.aes=F) +
  scale_fill_viridis(option = "D" , name = "Variable") +
  scalebar(data = bog , dist = 5 , transform = T , dist_unit = "km") +
  north(data = bog , location = "topleft") + theme_linedraw() + labs(x="" , y="")
map2


### 3. Web-scrapping y procesamiento de texto

## 3.1 leer url

url = "https://es.wikipedia.org/wiki/Departamentos_de_Colombia" # leer url

xml_document = read_html(url) # guardar html en un objeto


## 3.2 Xpath

xml_document %>% html_nodes(xpath = '//*[@id="firstHeading"]/span') %>% html_text() # extraer el título de la página


## 3.3  Extraer la tabla del html

tabla = xml_document %>% html_table() # extraer todas las tablas
length(tabla) # ver la cantidad de tablas
tabla_departamento <- tabla[[4]] # guardar en un objeto la tabla de departamentos


stargazer(tabla_departamento, 
          type= 'text',
          dep.var.labels = c('','price',''), 
          df = FALSE,
          digits = 3, 
          out = paste0('output/tabla_departamento.xlsx')) # exportar tabla


## 3.4 Nube de palabras

parrafos = xml_document %>% html_nodes("p")


