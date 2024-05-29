#2. Variaveis ----

# PASSOS JA CONCLUIDOS ----

# Download
download.file(url = "https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_bio.zip",
              destfile = "data/vars/wc2.1_30s_bio.zip", mode = "wb")
# Extracao
unzip(zipfile = "data/vars/wc2.1_30s_bio.zip",
      exdir = "data/vars/00_raw")

# Importacao
env <- dir(path = "data/vars/00_raw", pattern = ".tif$", full.names = TRUE) %>%
  raster::stack() %>%
  raster::brick()

# Conferir as variaveis importadas
env

# Renomear
names(env)
names(env) <- c("bio01", paste0("bio", 10:19), paste0("bio0", 2:9))
names(env)
env

# Plot da variavel bio01 para conferir se estah ok
plot(env$bio01)

# Tamanho e resolucao ----
# Ajuste de tamanho e resolucao
env_ma <- env %>%
  raster::crop(ma) %>%
  raster::mask(ma) #%>%
#  raster::aggregate(fact = .5/res(env)[1]) # dimuindo a resolucao para visualizar
env_ma

# Salvando o raster cortado para a MA
writeRaster(env_ma, "env_ma.tif", format='GTiff', overwrite=T) # corrigir depois

###### COMECAR AQUI ----
# Importar o raster jah recortado

env_ma <- dir(path = "data/vars/ma_cut", pattern = ".tif$", full.names = TRUE) %>%
  raster::stack() %>%
  raster::brick()

# plot de uma variavel para conferir se o corte foi bem feito
tm_shape(env_ma$env_ma_1) +
  tm_raster(palette = "-RdBu", n = 10) +
  tm_shape(ma) +
  tm_borders(col = "black") +
  tm_layout(legend.position = c("right", "bottom"))

# Colinearidade ----
# correlacao
ENMTools::raster.cor.matrix(env_ma, method = "pearson")
ENMTools::raster.cor.plot(env_ma)

# Raster como data frame
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
