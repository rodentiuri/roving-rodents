#0. PREPARACAO -------------------------------------------------------------
# Chamar (e obter se necessario) os pacotes utilizados
if(!require(tidyverse)) install.packages("tidyverse") # tratamento de dados
if(!require(geodata)) install.packages("geodata")
if(!require(rnaturalearth)) install.packages("rnaturalearth") # limites
if(!require(rnaturalearthdata)) install.packages("rnaturalearthdata") # limites
if(!require(nngeo)) install.packages("nngeo") # limites
if(!require(sf)) install.packages("sf") # simple features
if(!require(tmap)) install.packages("tmap") # mapas
if(!require(spocc)) install.packages("spocc") # buscar ocorrencias
if(!require(CoordinateCleaner)) install.packages("CoordinateCleaner") # limpeza de coords
if(!require(spThin)) install.packages("spThin") # filtragem de dados
if(!require(mapview)) install.packages("mapview") # visualizacao de mapas
if(!require(mapedit)) install.packages("mapedit") # editor grafico de mapas
if(!require(raster)) install.packages("raster") # manipulacao de raster
if(!require(viridis)) install.packages("viridis") # cores
if(!require(usdm)) install.packages("usdm")  # ferramentas de modelagem
if(!require(ENMTools)) install.packages("ENMTools") # Caso nao esteja disponivel no CRAN para a versao atual do R, procurar instalacao no github (danlwarren/ENMTools)

# Opcoes
options(timeout = 1e5) # aumenta o timeout para impedir que operacoes pesadas sejam interrompidas antes do fim
options(scipen = 50) # threshold para notacao cientifica
library(beepr) # aviso sonoro para o fim de operacoes longas

# Pastas
dir.create("data")
dir.create("data/occs")
dir.create("data/vars")
dir.create("data/shapes")
dir.create("scripts")
dir.create("output")

# Importar limites da Mata Atlantica ----
ma <- read_sf(dsn = "data/shapes/morrone_atlantic_florest.shp", layer = "morrone_atlantic_florest")
ma
tm_shape(ma) +
  tm_polygons()
