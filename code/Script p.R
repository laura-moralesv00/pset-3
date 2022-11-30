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
       rvest) ## web scrapping 


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


### 2. Datos espaciales

## 2.1 Descargar datos
opq(bbox = getbb("Cali Colombia")) # descargar datos de Cali.

parques <- opq(bbox = getbb("Cali Colombia")) %>% # descargar datos de parques en Cali.
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf() %>% .$osm_polygons %>% select(osm_id,name)

restaurantes <- opq(bbox = getbb("Cali Colombia")) %>% # descargar datos de restaurantes en Cali.
  add_osm_feature(key = "leisure", value = "restaurant") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)




## 2.2 Visualizaciones
leaflet() %>% addTiles() %>% addPolygons(data=parques) # visualizar parques

leaflet() %>% addTiles() %>% addCircleMarkers(data=restaurant , col="red") # visualizar restaurantes


## 2.3

geocode_OSM("Casa de Nariño, Bogotá") # usar geocode para encontrar una dirección
