#2. ENVIRONMENTAL VARIABLES ----

# Download
env <- geodata::worldclim_global(var = "bio", res = 10, path = "data/vars/00_raw")

# Rename
names(env) <-  c("bio01", paste0("bio", 10:19), paste0("bio0", 2:9))
env

# Plot variable bio01 to check if it is ok
plot(env$bio01)

# Size and resolution ----
# Adjusting size and resolution
env_ma <- env %>%
  raster::crop(ma) %>%
  raster::mask(ma) #%>%
#  raster::aggregate(fact = .5/res(env)[1]) # reducing resolution for visualization
plot(env_ma)

# Saving the raster cropped to MA
#writeRaster(env_ma, "env_ma.tif", format='GTiff', overwrite=T) # fix later

# Importing the already cropped raster
env_ma <- dir(path = "data/vars/ma_cut", pattern = ".tif$", full.names = TRUE) %>%
  raster::stack() %>%
  raster::brick()

# plot a variable to check if the crop was well done
tm_shape(env_ma$env_ma_1) +
  tm_raster(palette = "-RdBu", n = 10) +
  tm_shape(ma) +
  tm_borders(col = "black") +
  tm_layout(legend.position = c("right", "bottom"))

# Collinearity ----
# correlation
ENMTools::raster.cor.matrix(env_ma, method = "pearson")
ENMTools::raster.cor.plot(env_ma)

# Raster as a data frame
env_ma_df <- as.data.frame(env_ma)

# vif
env_ma_vif <- usdm::vif(env_ma_df)
env_ma_vif

# vifstep
env_ma_vifstep <- usdm::vifstep(env_ma_df, th = 2)
env_ma_vifstep

# vifcor
env_ma_vifcor <- usdm::vifcor(env_ma_df, th = .7)
env_ma_vifcor

# select
env_ma_vif <- usdm::exclude(env_ma, env_ma_vifstep)
env_ma_vif

env_ma_cor <- usdm::exclude(env_ma, env_ma_vifcor)
env_ma_cor

# scale ----
env_ma_vif_scale <- raster::scale(env_ma_vif)
env_ma_vif_scale

# plot
plot(env_ma_vif, col = viridis::viridis(100))
plot(env_ma_vif_scale, col = viridis::viridis(100))

# export ----
raster::writeRaster(x = env_ma_vif_scale,
                    filename = paste0("data/vars/", names(env_ma_vif_scale)),
                    bylayer = TRUE,
                    format = "GTiff",
                    overwrite = TRUE)
