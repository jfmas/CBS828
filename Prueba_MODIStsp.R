
library("pak")
library(devtools)
install_github("ropensci/MODIStsp")
pak("ropensci/MODIStsp")
library(MODIStsp)

MODIStsp(
  gui = FALSE,
  prod_bandsel = c("LST_Day_1km", "QC_Day"),  # This accepts multiple bands
  prod_version = "061",
  prod_name = "MOD11A2",
  start_date = "2024-01-01",
  end_date = "2024-01-31",
  bbox = c(-100.84, 20.021, -100.473, 20.224),
  out_folder = temporalpath,
  out_format = "GTiff"
)

