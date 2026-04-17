# Proyecto CBS828
# Obtencion de productos MODIS con modisfast

# 2025-09-17 : IMPORTANT NOTIFICATION : Due to major recent migrations of NASA’s servers (details here), 
# modisfast will be temporarily unavailable in the coming months. Work is in progress to connect modisfast 
# to the new NASA OPeNDAP servers.   ----> esperar y buscar alternativas
# Explorar alternativa MODISTools
system("git status")
usethis::use_git_config(user.name = "jfmas", user.email = "jfmas@ciga.unam.mx")
usethis::use_github()
###########################################################
### MODISFAST (imagenes and productos MODIS)
# Instalación de modisfast
library(pak)
pak("ptaconet/modisfast")

library(modisfast)
library(tmap)
library(terra)
library(sf)

# Colecciones de productos MODIS
mf_list_collections()$collection

# Carpetas
temporalpath <- "/home/jf/Downloads/tmpMODIS"

# log <- mf_login(credentials = c("usuario","contraseña"))
# # poner su propio username y password

## Login to Earthdata servers with your EOSDIS credentials.
# To create an account (free) go to : https://urs.earthdata.nasa.gov/.

log <- mf_login(credentials = c("jfmas","EElaneta68##"))  # set your own EOSDIS username and password

## Coordinadas extremas de Presa Solis

roi <- st_as_sf(data.frame(id = "roi_id", 
                           geom = "POLYGON ((  -100.84 20.021, -100.473 
                                                20.021, -100.473 20.224, 
                                              -100.84 20.224, -100.84 20.021))"),
                wkt="geom", crs = 4326)

## Indicar el EPSG del roi
EPSG <- 4326  # 4326: Geograficas  # 6372 Lambert
plot(roi)

# #### Vistazo al AOI
tmap_mode("view")
tm_shape(roi) +
  tm_polygons(fill = NA, col = "magenta", lwd = 2, fill_alpha = 0) +
  tm_basemap(server = "Esri.WorldImagery")

# MODIS collections and variables (bands) of interest
collection <- "MOD11A2.061"
mf_list_variables("MOD11A2.061")
variables <- c("LST_Day_1km","QC_Day")

# Periodo de tiempo
time_range <- as.Date(c("2024-01-01","2024-01-31"))

## Get the URLs of the data
urls <- mf_get_url(
  collection = collection,
  variables = variables,
  roi = roi,
  time_range = time_range
)

res_dl <- mf_download_data(urls, parallel = T, 
                           path = temporalpath)
r <- mf_import_data(
  path = dirname(res_dl$destfile[1]),
  collection = collection,
  roi_mask = roi,
  proj_epsg = EPSG
)
print(r)
class(r)
names(r)
plot(r)