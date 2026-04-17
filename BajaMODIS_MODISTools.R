# Proyecto CBS828
# Obtencion de productos MODIS con MODISTools
# https://cran.r-project.org/web/packages/MODISTools/vignettes/modistools-vignette.html
# https://cran.r-project.org/web/packages/MODISTools/MODISTools.pdf

###########################################################
### MODISTools (imagenes y productos MODIS)
# Instalación de MODISTools
install.packages("MODISTools", dep = T)
library(MODISTools)
library(tmap)
library(terra)
library(sf)
library(dplyr)
library(tools)

install.packages("earthdatalogin")
library(earthdatalogin)
login_earthdata(username = "jfmas", password = "EElaneta68##")
tools::R_user_dir("earthdatalogin")
edl_netrc(
  username = "jfmas",
  password = "EElaneta68##",
  netrc_path = "/home/jf/.local/share/R/earthdatalogin",
  cookie_path = "/home/jf/.local/share/R/earthdatalogin",
  cloud_config = TRUE
)


# Method 1: Set your token as an environment variable (RECOMMENDED)
# First, manually create a token at:
# https://urs.earthdata.nasa.gov/profile -> "My Tokens" -> "Create Token"
# Then set it in your R environment:
Sys.setenv(earthdata_token = "eyJ0eXAiOiJKV1QiLCJvcmlnaW4iOiJFYXJ0aGRhdGEgTG9naW4iLCJzaWciOiJlZGxqd3RwdWJrZXlfb3BzIiwiYWxnIjoiUlMyNTYifQ.eyJ0eXBlIjoiVXNlciIsInVpZCI6ImpmbWFzIiwiZXhwIjoxNzgxNjM4Njk1LCJpYXQiOjE3NzY0NTQ2OTUsImlzcyI6Imh0dHBzOi8vdXJzLmVhcnRoZGF0YS5uYXNhLmdvdiIsImlkZW50aXR5X3Byb3ZpZGVyIjoiZWRsX29wcyIsImFjciI6ImVkbCIsImFzc3VyYW5jZV9sZXZlbCI6M30.us7lHZo3KXA9teb7EHnFFyBvvq2V2V53wMKcqYR7ZjfOnp2w30wAu-RcoCl2rkh3NfckDbwmMbheJijgKIpX6tw_e-i3rlOLHllwG1EQ0hnhBbDPRrKV0vmHLG8LORedjMac7HdutYXUf6Zbe7rJOENPrXXbayJZVF-x2ZtkAKbvJ9aNrUPremRJ01oPICY8LxcM-kvfoymTUpGw1JQ0X6N7yWx5VHAyqgK7NKKL3xvl7dXLBG6OsYqjoCK7Snjq3bAHo1lzZoyhDchyzS3BQTvL6kqfqNKwI2k8OUiCqQlhsGK5_fooE6o6PSSZnf5EJobtZLmNJbmiZVGdm25xGQ")
options(MODISTools_token = Sys.getenv("earthdata_token"))



# Carpetas
temporalpath <- "/home/jf/Downloads/tmpMODIS"

## Coordinadas extremas de Presa Solis
roi <- st_as_sf(data.frame(id = "roi_id", 
                           geom = "POLYGON ((  -100.84 20.021, -100.473 
                                                20.021, -100.473 20.224, 
                                              -100.84 20.224, -100.84 20.021))"),
                wkt="geom", crs = 4326)

## Indicar el EPSG del roi
EPSG <- 4326  # 4326: Geograficas  # 6372 Lambert
plot(roi)

# Vistazo al AOI
tmap_mode("view")
tm_shape(roi) +
  tm_polygons(fill = NA, col = "magenta", lwd = 2, fill_alpha = 0) +
  tm_basemap(server = "Esri.WorldImagery")

# List available products (equivalent to mf_list_collections)
# Correct way to list products with MODISTools
products <- mt_products()
head(products)

# Filter for MOD11 products
mod11_products <- products[grepl("MOD11", products$product), ]
print(mod11_products)


# MODIS collections and variables (bands) of interest
collection <- "MOD11A2"  # Note: MODISTools doesn't include version in product name

# Get bands for the product
bands <- mt_bands(product = collection)
print(bands)

# Get the bands (variables) we need
# For MOD11A2, look for LST_Day_1km and QC_Day
variables <- c("LST_Day_1km", "QC_Day")
# Verify these bands exist
variables_exist <- variables[variables %in% bands$band]
print(paste("Available bands:", paste(variables_exist, collapse = ", ")))

# Periodo de tiempo
time_range <- as.Date(c("2024-01-01","2024-01-31"))

# Convert time range to Julian dates for MODIS
start_date <- format(time_range[1], "%Y-%m-%d")
end_date <- format(time_range[2], "%Y-%m-%d")

# Set your Earthdata login credentials
# Create account at: https://urs.earthdata.nasa.gov/
# Option 1: Set as environment variables (recommended)
Sys.setenv(earthdata_usr = "jfmas66")
Sys.setenv(earthdata_pwd = "Bpited@@__@@")

# Option 2: Use mt_auth
# mt_auth(username = "jfmas", password = "EElaneta68##")

# Download the data using mt_subset

lst_data <- mt_subset(product = collection,
                          lat = 20.1,  # min and max latitude
                          lon = -100.6,  # min and max longitude
                          band = "LST_Day_1km",
                          start = "2004-01-01",
                          end = "2004-12-30",
                          km_lr = 20,
                          km_ab = 20,
                          site_name = "Presa_Solis",
                          internal = TRUE,
                          progress = FALSE
)
class(lst_data)

qc_data <- mt_subset(product = collection,
                      lat = 20.1,  # min and max latitude
                      lon = -100.6,  # min and max longitude
                      band = "QC_Day",
                      start = "2004-01-01",
                      end = "2004-12-30",
                      km_lr = 20,
                      km_ab = 20,
                      site_name = "Presa_Solis",
                      internal = TRUE,
                      progress = FALSE
)
                      

  # Check the structure of downloaded data
  str(modis_data)
  head(modis_data)
  
  # Convert the subset data to a raster format
  # MODISTools returns data in long format, we need to reshape it
  if(!is.null(modis_data) && nrow(modis_data) > 0) {
    
    # For LST data, we can create time series plots
    library(ggplot2)
    
    # Plot LST time series
    lst_data <- modis_data[modis_data$band == "LST_Day_1km", ]
    
    if(nrow(lst_data) > 0) {
      ggplot(lst_data, aes(x = as.Date(calendar_date), y = value)) +
        geom_line() +
        geom_point() +
        facet_wrap(~site) +
        labs(title = "LST Day Time Series",
             x = "Date",
             y = "LST (K)") +
        theme_minimal()
    }
    
    # Combine the data if needed
    # Add a band identifier column
    lst_data$band <- "LST_Day_1km"
    qc_data$band <- "QC_Day"
    combined_data <- rbind(lst_data, qc_data)
    
    # convert to raster
    lst_data_r <- mt_to_terra(df = lst_data)
plot(lst_data_r)    
    