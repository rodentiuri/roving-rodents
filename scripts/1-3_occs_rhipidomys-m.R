#2. Dados de ocorrencia: Rhipidomys mastacalis ----

# Importando dados da GBIF ----
rhipi_occ_spocc <- spocc::occ(query = "Rhipidomys mastacalis",
                              from = c("gbif"),
                              has_coords = TRUE,
                              limit = 1e5)
rhipi_occ_spocc

# tratamento e selecao dos dados de localizacao
rhipi_occ_spocc_data <- spocc::occ2df(rhipi_occ_spocc) %>%
  dplyr::mutate(species = "Rhipidomys mastacalis",
                longitude = as.numeric(longitude),
                latitude = as.numeric(latitude),
                year = date %>% lubridate::year(),
                base = prov %>% stringr::str_to_lower()) %>%
  dplyr::select(name, species, longitude, latitude, year, base)
rhipi_occ_spocc_data

# Importando dados tratados manualmente ----
rhipi_occ_manual <- read.csv("data/occs/pre_R/rhipi_asm.csv") %>% 
  dplyr::mutate(longitude = as.numeric(longitude),
                latitude = as.numeric(latitude),
                year = as.numeric(year)) %>%
  dplyr::select(species, longitude, latitude, year, base) %>% 
  as_tibble()
rhipi_occ_manual

# Combinando datasets ----
rhipi_occ_data <- dplyr::bind_rows(rhipi_occ_spocc_data, 
                                   rhipi_occ_manual)
rhipi_occ_data

# Visualizando no mapa ----
rhipi_occ_data_vector <- rhipi_occ_data %>%
  tidyr::drop_na(longitude, latitude) %>%
  dplyr::mutate(lon = longitude, lat = latitude) %>%
  dplyr::filter(lon >= -180, lon <= 180, lat >= -90, lat <= 90) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
rhipi_occ_data_vector

tm_shape(ma, bbox = rhipi_occ_data_vector) +
  tm_polygons() +
  tm_shape(rhipi_occ_data_vector) +
  tm_dots(size = .1, shape = 21, col = "gold3") +
  tm_graticules(lines = FALSE)

# Filtro espacial ----

# Importar limites de distribuicao atual
rm_shape <- read_sf(dsn = "data/shapes/Rhipidomys-mastacalis/data/Rhipidomys_mastacalis.shp", layer = "Rhipidomys_mastacalis")
rm_shape
tm_shape(rm_shape) +
  tm_polygons()

# Recortar ocorrencias de acordo com a Mata Atlantica e com a distribuicao atual
## Este comando adiciona uma nova coluna indicando se a coordenada esta dentro ou fora do limite
rhipi_occ_data_sptlim <- rhipi_occ_data_vector %>%
  dplyr::mutate(sptlim_filter = as.logical(sf::st_intersects(rhipi_occ_data_vector, ma, sparse = FALSE)),
                distlim_filter = as.logical(sf::st_intersects(rhipi_occ_data_vector, rm_shape, sparse = FALSE)))
rhipi_occ_data_sptlim

# Visualizando no mapa
tm_shape(ma) +
  tm_polygons() +
  tm_shape(rhipi_occ_data_sptlim %>%
             filter(sptlim_filter == TRUE,
                    distlim_filter == TRUE)) +
  tm_dots(size = .15, shape = 21, col = "gold3")

# Filtrando vieses ----
rhipi_occ_data_sptlim_bias <- CoordinateCleaner::clean_coordinates(
  x = sf::st_drop_geometry(rhipi_occ_data_sptlim),
  species = "species",
  lon = "longitude",
  lat = "latitude",
  outliers_mtp = 2,
  tests = c("duplicates", 
            "equal", 
            "institutions",
            "seas", 
            "validity", 
            "zeros" 
  )) %>%
  tibble::as_tibble() %>%
  dplyr::mutate(lon = longitude, lat = latitude) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
rhipi_occ_data_sptlim_bias # tibble com os status de cada coordenada de acordo com os limites considerados

# Visualizando no mapa
## mapa bem parecido com o anterior pq pontos estao aglomerados.
## dica: diminuir o tamanho dos pontos e dar zoom no mapa.
## eh possivel ver a diferenca de pontos entre esse e o mapa anterior.
tm_shape(ma) +
  tm_polygons() +
  tm_shape(rhipi_occ_data_sptlim_bias %>%
             filter(sptlim_filter == TRUE,
                    distlim_filter == TRUE,
                    .summary == TRUE)) +
  tm_dots(size = .1, shape = 21, col = "gold3")

# Distancia minima entre os pontos ----
filter_thin <- spThin::thin(loc.data = rhipi_occ_data_sptlim_bias,
                            lat.col = "latitude",
                            long.col = "longitude",
                            spec.col = "species",
                            thin.par = 5, # tem que justificar - unidade = km
                            reps = 1,
                            write.files = FALSE,
                            write.log.file = FALSE,
                            locs.thinned.list.return = TRUE,
                            verbose = TRUE) %>%
  .[[1]] %>%
  tibble::as_tibble() %>%
  dplyr::rename_with(tolower) %>%
  dplyr::mutate(sptdist_filter = TRUE)
filter_thin

# Juntar os filtros
rhipi_occ_data_sptlim_bias_sptdist <- dplyr::left_join(
  x = rhipi_occ_data_sptlim_bias,
  y = filter_thin,
  by = c("longitude", "latitude")) %>%
  dplyr::mutate(sptdist_filter = replace_na(sptdist_filter, FALSE)) #%>%
# dplyr::relocate(sptdist_filter, .after = distlim_filter)
rhipi_occ_data_sptlim_bias_sptdist

tm_shape(ma) +
  tm_polygons() +
  tm_shape(rhipi_occ_data_sptlim_bias_sptdist %>%
             filter(sptlim_filter == TRUE,
                    distlim_filter == TRUE,
                    sptdist_filter == TRUE,
                    .summary == TRUE)) +
  tm_dots(size = .1, shape = 21, col = "gold3")

# Aplicar todos os filtros ----
## aqui e criada uma nova tabela, sem as occs que caem em algum filtro
rhipi_occ_data_filter <- rhipi_occ_data_sptlim_bias_sptdist %>%
  filter(sptlim_filter == TRUE,
         distlim_filter == TRUE,
         sptdist_filter == TRUE,
         .summary == TRUE) %>%
  dplyr::select(species, longitude, latitude) #essa linha seleciona quais cols ficam
rhipi_occ_data_filter

mapview::mapview(rhipi_occ_data_filter)

# manual editing ----
#rhipi_occ_data_filter_edit <- mapedit::editFeatures(rhipi_occ_data_filter) # atencao para o Done!
rhipi_occ_data_filter_edit <- rhipi_occ_data_filter

# verificar
mapview::mapview(rhipi_occ_data_filter_edit)

# export ----
# vetor
rhipi_occ_data_filter_edit %>%
  sf::st_write("data/occs/rhipi.shp", append=F)

# tabela
rhipi_occ_data_filter_edit %>%
  sf::st_drop_geometry() %>%
  readr::write_csv("data/occs/rhipi_filtrado.csv")
