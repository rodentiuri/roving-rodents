#0. BEGINNINGS -------------------------------------------------------------

# Load (and install if necessary) the required packages
if(!require(tidyverse)) install.packages("tidyverse") # data processing
if(!require(geodata)) install.packages("geodata")
if(!require(rnaturalearth)) install.packages("rnaturalearth") # boundaries
if(!require(rnaturalearthdata)) install.packages("rnaturalearthdata") # boundaries
if(!require(nngeo)) install.packages("nngeo") # boundaries
if(!require(sf)) install.packages("sf") # simple features
if(!require(tmap)) install.packages("tmap") # maps
if(!require(spocc)) install.packages("spocc") # fetch occurrences
if(!require(CoordinateCleaner)) install.packages("CoordinateCleaner") # coordinate cleaning
if(!require(spThin)) install.packages("spThin") # data filtering
if(!require(mapview)) install.packages("mapview") # map visualization
if(!require(mapedit)) install.packages("mapedit") # graphical map editor
if(!require(raster)) install.packages("raster") # raster manipulation
if(!require(viridis)) install.packages("viridis") # colors
if(!require(usdm)) install.packages("usdm")  # modeling tools
if(!require(ENMTools)) install.packages("ENMTools") # If not available on CRAN for the current R version, search for installation on GitHub (danlwarren/ENMTools)

# Options
options(timeout = 1e5) # increases the timeout to prevent heavy operations from being interrupted before completion
options(scipen = 50) # threshold for scientific notation
library(beepr) # sound alert for the end of long operations

# Directories - run only in 1st time
#dir.create("data")
#dir.create("data/occs")
#dir.create("data/vars")
#dir.create("data/shapes")
#dir.create("scripts")
#dir.create("output")

# Import Atlantic Forest boundaries ----
ma <- read_sf(dsn = "data/shapes/morrone_atlantic_florest.shp", layer = "morrone_atlantic_florest")
ma
tm_shape(ma) +
  tm_polygons()
