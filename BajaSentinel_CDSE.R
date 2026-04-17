# Proyecto CBS828
# Obtencion de imágenes Sentinel con CDSE

## Instalación del paquete CDSE
# install.packages("CDSE", dep=T)
# #  Alternativa desde github
library(pak)
# # pak("zivankaraman/CDSE")


# Librerías
library(CDSE)
library(sf)
library(terra)
id <- "sh-ce2e1cf4-8ccc-40ee-8db6-ad545e147160"
secret <-  "75jnZY496FPiwP07zo1iHtR1ijhJpeEh"

token <- GetOAuthToken(id = id, secret = secret)
OAuthClient <- GetOAuthClient(id = id, secret = secret)

# # Librerías
# library(CDSE)
# library(sf)
# library(terra)
# # Aqui poner sus propias credenciales
# id <- "sh-ce2e1cf4-8cac-40ee-8db6-ad545e147161"
# secret <-  "79jnZY496FPiwP07zo1iHtR1jjhJpeEh"
# 
# token <- GetOAuthToken(id = id, secret = secret)
# OAuthClient <- GetOAuthClient(id = id, secret = secret)

## Area de interés ROI (Presa Solís)
caja <- st_bbox(c(xmin = -100.68, xmax = -100.483, ymin = 20.023, ymax = 20.13),
                crs = st_crs(4326))
aoi <- st_sf(st_as_sfc(caja))

colecs <- GetCollections()
head(colecs)

images <- SearchCatalog(aoi = aoi, from = "2024-01-01", 
                        to = "2024-2-28",
                        collection = "sentinel-2-l2a",
                        with_geometry = TRUE, 
                        client = OAuthClient)

head(images)
nrow(images) #son 12 imágenes

############ Filtro por cobertura de nube
buenas <- images$acquisitionDate[images$tileCloudCover < 70]
imagesbuenas <- images[images$tileCloudCover < 70,]
length(buenas) # 10
as.Date(buenas) #fechas de las imágenes

### Scripts (ver Moodle)
script_file_bands <- "/home/jf/pCloudDrive/trabajos/PresaSolis/Bands-SR_1-8A.js"
script_file_clouds <- "/home/jf/pCloudDrive/trabajos/PresaSolis/scriptCLD.js"


# #fechas con poca nube
# ### Primero la máscara de nubes
# fecha <- buenas[1]
# for (fecha in buenas){
# fecha <-  as.Date(fecha)
# filename <- paste0("imagenes/cl",fecha,".tif")
# if(!file.exists(filename)){
# GetImage(aoi = aoi, time_range = fecha,
#          script = script_file_clouds,
#          collection = "sentinel-2-l2a",
#          format = "image/tiff",
#          mosaicking_order = "leastCC",
#          resolution = 20,
#          client = OAuthClient, file= paste0("imagenes/cl",fecha,".tif"))
# }#if
# }#loop fechas

##################################################################3
##### Preprocesamiento de imágenes (Sentinel)
library(terra)
library(sf)
library(dplyr)
library(tmap)

## Funciones
# Function to reclassify CLS layer

# De 0 a 11
# “No Data”, “Saturated or Defective”, “Dark area pixels”, “Cloud Shadows”, “Vegetation”, 
# “non-Vegetated”, “Water”, “Unclassified”, “Cloud medium”, “Cloud high”, “Thin cirrus” and “Snow”.

## medium y high cloud, cirrus, cloud shadow, saturated
cloud_ext <- function(SCL){
  SCL == 1 | SCL == 3 | SCL == 8 | SCL == 9 | SCL == 10
}

water <- function(SCL){
  SCL == 6
}

coverpath <- "/home/jf/pCloudDrive/trabajos/PresaSolis/mapas/Cuadro_Solis.gpkg"
Solis <- st_read(coverpath)
AOI <-  st_transform(Solis, crs = 32614)

### Definir las rutas a carpetas
# rutazip carpeta donde están los zip de las imágenes
rutazip <- "/home/jf/pCloudDrive/BaseDatosEspaciales/SentinelGto/Sentinel-2/S2MSI2A/"
# ruta carpeta donde salvar recortes
rutaout <- "/home/jf/Downloads/"
# ruta de archivos temporales
rutaTmp <- "/home/jf/Documents/tmp"

file_zip <- "S2A_MSIL2A_20241109T171531_N0511_R112_T14QLH_20241109T215152.SAFE.zip"
unzip(zipfile = paste0(rutazip,file_zip),exdir =rutaTmp)


ruta20m <- "/S2A_MSIL2A_20241109T171531_N0511_R112_T14QLH_20241109T215152.SAFE/GRANULE/L2A_T14QLH_A049015_20241109T172641/IMG_DATA/R20m/"

ruta10m <- "/S2A_MSIL2A_20241109T171531_N0511_R112_T14QLH_20241109T215152.SAFE/GRANULE/L2A_T14QLH_A049015_20241109T172641/IMG_DATA/R10m/"

## En el caso de Sentinel, se pasa de ND a SR  
#  dividiendo por 10000
gain <- 10000 # poner gain <- 1 para quedar con ND
# Imágenes a 10m
b2 <- crop(rast(paste0(rutaTmp,ruta10m, "T14QLH_20241109T171531_B02_10m.jp2")),AOI)/gain
b3 <- crop(rast(paste0(rutaTmp,ruta10m, "T14QLH_20241109T171531_B03_10m.jp2")),AOI)/gain
b4 <- crop(rast(paste0(rutaTmp,ruta10m, "T14QLH_20241109T171531_B04_10m.jp2")),AOI)/gain
b8 <- crop(rast(paste0(rutaTmp,ruta10m, "T14QLH_20241109T171531_B08_10m.jp2")),AOI)/gain

# bandas a 20m: 1, 5, 6, 8A, 11 y 12
b1 <- crop(rast(paste0(rutaTmp,ruta20m,"T14QLH_20241109T171531_B01_20m.jp2")),AOI)/gain
b1 <- project(b1,b2) # b1 está a 20m
plot(b1)

b5 <- crop(rast(paste0(rutaTmp,ruta20m,"T14QLH_20241109T171531_B05_20m.jp2")),AOI)/gain
b5 <- project(b5,b2) # b5 está a 20m

b6 <- crop(rast(paste0(rutaTmp,ruta20m,"T14QLH_20241109T171531_B06_20m.jp2")),AOI)/gain
b6 <- project(b6,b2) # b6 está a 20m
b7 <- crop(rast(paste0(rutaTmp,ruta20m,"T14QLH_20241109T171531_B07_20m.jp2")),AOI)/gain
b7 <- project(b7,b2) # b6 está a 20m

b8A <- crop(rast(paste0(rutaTmp,ruta20m,"T14QLH_20241109T171531_B8A_20m.jp2")),AOI)/gain
b8A <- project(b8A,b2) # b8A está a 20m

b11 <- crop(rast(paste0(rutaTmp,ruta20m,"T14QLH_20241109T171531_B11_20m.jp2")),AOI)/gain
b11 <- project(b11,b2) # b11 está a 20m

b12 <- crop(rast(paste0(rutaTmp,ruta20m,"T14QLH_20241109T171531_B12_20m.jp2")),AOI)/gain
b12 <- project(b12,b2) # b12 está a 20m

rutaMSK_CLDPRB <- "/S2A_MSIL2A_20241109T171531_N0511_R112_T14QLH_20241109T215152.SAFE/GRANULE/L2A_T14QLH_A049015_20241109T172641/QI_DATA/" 

probacoud <- crop(rast(paste0(rutaTmp,rutaMSK_CLDPRB, "MSK_CLDPRB_20m.jp2")),AOI)
probacoud <- project(probacoud,b2)

scl<- crop(rast(paste0(rutaTmp,ruta20m,"T14QLH_20241109T171531_SCL_20m.jp2")),AOI)
scl <- project(scl,b2) # slc está a 20m
# SLC clasif de 0 a 11
# “No Data”, “Saturated or Defective”, “Dark area pixels”, “Cloud Shadows”, “Vegetation”, 
# “non-Vegetated”, “Water”, “Unclassified”, “Cloud medium”, “Cloud high”, “Thin cirrus” and “Snow”.

# ------------------- Máscaras")
mask_water <- water(scl)
plot(mask_water) 

mask_nube <- cloud_ext(scl)
plot(mask_nube)


mask_probanube <- probacoud > 30 # enmarcara cuando proba > 0.3 (30%)
plot(mask_probanube)

## Aplica las mascaras de agua, nube y sombra de nube
# ojo aqui solo mask agua
b1[mask_water == 0 | mask_nube == 1] <- NA
b2[mask_water == 0 | mask_nube == 1] <- NA
b3[mask_water == 0 | mask_nube == 1] <- NA
b4[mask_water == 0 | mask_nube == 1] <- NA
b5[mask_water == 0 | mask_nube == 1] <- NA
b6[mask_water == 0 | mask_nube == 1] <- NA
b7[mask_water == 0 | mask_nube == 1] <- NA
b8[mask_water == 0 | mask_nube == 1] <- NA
b8A[mask_water == 0 | mask_nube == 1] <- NA
b11[mask_water == 0 | mask_nube == 1] <- NA
b12[mask_water == 0 | mask_nube == 1] <- NA

# ## Dar nombre a gusto para salvar recortes
# name <- "cortes_S-20241109.tif"
# writeRaster(c(b1,b2,b3,b4,b5,b6,b7,b8,b8A,b11,b12), file=name, overwrite=TRUE,
# wopt= list(gdal=c("COMPRESS=NONE"), datatype='FLT4S'))
# 
# ## si necesidad reproyectar
# #r <- project(r, "EPSG:32614") # proyecta de UTM z13 a z14

# ###### AUTOMATIZACION #############################################################
# ## Automatizando sobre un gran número de datos (proceso iterativo)
# # usando list.files() y for(){}
# 
# ## Hace la lista de todos los zip en la carpeta rutazip
# #  (ojo todos tienen que ser imágenes Sentinel de la zona)
# lista.zip <- list.files(path=rutazip,pattern='.zip$',full.names=F)
# 
# for (i in 1:length(lista.zip)){
#   print(i)
#   file_zip <- lista.zip[i]
#   fecha <- substr(file_zip,12,19) # extrae fecha imagen
#   codigo <- substr(file_zip,54,60) # extrae clave única imagen
#   unzip(zipfile = paste0(rutazip,file_zip),exdir =rutaTmp)
#   lista.b1 <- list.files(path=rutaTmp,pattern='B01_20m.jp2',full.names=T,
#                          recursive=T) # blue
#   lista.b2 <- list.files(path=rutaTmp,pattern='B02_10m.jp2',full.names=T,
#                          recursive=T) # blue
#   lista.b3 <- list.files(path=rutaTmp,pattern='B03_10m.jp2',full.names=T,
#                          recursive=T) # green
#   lista.b4 <- list.files(path=rutaTmp,pattern='B04_10m.jp2',full.names=T,
#                          recursive=T) # red
#   lista.b8 <- list.files(path=rutaTmp,pattern='B08_10m.jp2',full.names=T,
#                          recursive=T) # nir
#   # Proba presencia de nube 0 a 100
#   lista.cloud <- list.files(path=rutaTmp,pattern="MSK_CLDPRB_20m.jp2",
#                             full.names=T, recursive=T)
# # SLC clasif de 0 a 11
# lista.slc <- list.files(path=rutaTmp,pattern="SCL_20m.jp2.jp2",
#                         full.names=T, recursive=T)
# 
#   ## En el caso de Sentinel, se pasa de ND a SR
#   # dividiendo por 10000
#   gain <- 10000 # poner gain <- 1 para quedar con ND
#   b2 <- crop(rast(lista.b2),AOI)/gain
#   b1 <- crop(rast(lista.b1),AOI)/gain
#   b1 <- project(b1,b2) # b1 está a 20m
#   b3 <- crop(rast(lista.b3),AOI)/gain
#   b4 <- crop(rast(lista.b4),AOI)/gain
#   b8 <- crop(rast(lista.b8),AOI)/gain
#   probacoud <- crop(rast(lista.cloud),AOI)
#   probacoud <- project(probacoud,b3)
#   slc <- crop(rast(lista.slc),AOI)
#   slc <- project(slc,b2) # slc está a 20m
# 
# plot(b2)
# plot(SCL)
# 
# print("------------------- Máscaras")
# mask_water <- water(SCL)
# mask_nube <- cloud_ext(SCL)
# mask_probanube <- probacoud > 50 # enmarcara cuando proba > 0.5 (50%)
# #plot(mask_water)
# 
# ## Aplica las mascaras de agua, nube y sombra de nube
# # ojo aqui solo mask agua
# b1[mask_water ==0] <- NA
# b2[mask_water ==0] <- NA
# b3[mask_water ==0] <- NA
# b4[mask_water ==0] <- NA
# b8[mask_water ==0] <- NA
# 
# ## Dar nombre a gusto para salvar recortes
# name <- paste0("cortes_S/",prefix,sitio,".tif")
# writeRaster(c(b1,b2,b3,b4,b8), file=name, overwrite=TRUE,
#             wopt= list(gdal=c("COMPRESS=NONE"), datatype='FLT4S'))
# 
# ## borra la imagen completa en tmp
# print("borra tmp")
# unlink(paste0(rutaTmp,"*"), recursive = T, force = FALSE)
# } # for (loop sobre todos los zip)
# 
# ## opción opción para borrar en tmp
# # print("borra los tif")
# # listatifdel <- list.files(path="/tmp",include.dirs = T, recursive=T,
# #                           pattern=".TIF",full.names = T)
# # file.remove(listatifdel)