# rsat - Modern alternative with unified interface
install.packages("rsat")
library(rsat)


library(rsat)

# replace with your own "username" and "password"
set_credentials("jfmas", "EElaneta68##")

# region and time of interest: rtoi
roi <- ex.navarre
toi <- as.Date("2020-01-11")
rtp <- tempdir()

set_database(file.path(tempdir(), "DATABASE"))

navarre <- new_rtoi("Navarre", roi, rtp)

# search, acquire, customize, and process
rsat_search(region = navarre, product = "mod09ga", dates = toi)
rsat_download(navarre)
rsat_mosaic(navarre, overwrite = TRUE)

rsat_derive(navarre, 
            product = "mod09ga", 
            variable = "NDVI")

# plot the results
plot(navarre, "view" , 
     product = "mod09ga", 
     variable = "NDVI", 
     breaks = seq(0, 1, 0.1))

plot(navarre,"dates")










# Initialize rsat (sets up access to multiple satellite data sources)
rsat::rsat_initialize()

# Define your area of interest
aoi <- extent(-100.84, -100.473, 20.021, 20.224)

# Search for MODIS data
modis_data <- rsat_search(
  satellite = "MODIS",
  source = "LPDAAC",
  product = "MOD11A2",
  aoi = aoi,
  date = c("2024-01-01", "2024-01-31")
)

# Download and process
download_data <- rsat_download(modis_data)