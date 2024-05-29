#1-1. Dados de ocorrencia: AKODON CURSOR ----

# Importando dados da GBIF ----
akod_occ_spocc <- spocc::occ(query = "Akodon cursor",
                             from = c("gbif"),
                             has_coords = TRUE,
                             limit = 1e5)
akod_occ_spocc

# Tratamento e selecao dos dados de localizacao
akod_occ_spocc_data <- spocc::occ2df(akod_occ_spocc) %>%
  dplyr::mutate(species = "Akodon cursor",
                longitude = as.numeric(longitude),
                latitude = as.numeric(latitude),
                year = date %>% lubridate::year(),
                base = prov %>% stringr::str_to_lower()) %>%
  dplyr::select(species, longitude, latitude, year, base, key)
akod_occ_spocc_data

# Importando dados tratados manualmente ----
akod_occ_manual <- read.csv("data/occs/pre_R/akod.csv") %>% 
  dplyr::mutate(longitude = as.numeric(longitude),
                latitude = as.numeric(latitude),
                year = as.numeric(year)) %>%
  dplyr::select(species, longitude, latitude, year, base, id_og) %>% 
  as_tibble()
akod_occ_manual

# Combinando datasets ----
akod_occ_data <- dplyr::bind_rows(akod_occ_spocc_data, 
                                  akod_occ_manual)
akod_occ_data

# Visualizando no mapa ----
akod_occ_data_vector <- akod_occ_data %>%
  tidyr::drop_na(longitude, latitude) %>%
  dplyr::mutate(lon = longitude, lat = latitude) %>%
  dplyr::filter(lon >= -180, lon <= 180, lat >= -90, lat <= 90) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
akod_occ_data_vector

tm_shape(ma, bbox = akod_occ_data_vector) +
  tm_polygons() +
  tm_shape(akod_occ_data_vector) +
  tm_dots(size = .2, shape = 21, col = "lightsalmon") +
  tm_graticules(lines = FALSE)

# Filtro espacial ----

# Importar limites de distribuicao atual
ac_shape <- read_sf(dsn = "data/shapes/Akodon-cursor/data/Akodon_cursor.shp", layer = "Akodon_cursor")
ac_shape
tm_shape(ac_shape) +
  tm_polygons()

# Recortar ocorrencias de acordo com a Mata Atlantica e coma  distribuicao atual de Akodon
## Este comando adiciona uma nova coluna indicando se a coordenada esta dentro ou fora do limite
akod_occ_data_sptlim <- akod_occ_data_vector %>%
  dplyr::mutate(sptlim_filter = as.logical(sf::st_intersects(akod_occ_data_vector, ma, sparse = FALSE)), 
                distlim_filter = as.logical(sf::st_intersects(akod_occ_data_vector, ac_shape, sparse = FALSE)))

# Visualizando no mapa
tm_shape(ma) +
  tm_polygons() +
  tm_shape(akod_occ_data_sptlim %>%
             filter(sptlim_filter == TRUE,
                    distlim_filter == TRUE)) +
  tm_dots(size = .2, shape = 21, col = "lightsalmon")

# Filtrando vieses ----
akod_occ_data_sptlim_bias <- CoordinateCleaner::clean_coordinates(
  x = sf::st_drop_geometry(akod_occ_data_sptlim),
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
akod_occ_data_sptlim_bias # tibble com os status de cada coordenada de acordo com os testes aplicados

# Visualizando no mapa
## mapa bem parecido com o anterior pq pontos estao algomerados
## dica: diminuir o tamanho dos pontos e dar zoom no mapa.
### e possivel ver a diferenca de pontos entre esse e o mapa anterior.
tm_shape(ma) +
  tm_polygons() +
  tm_shape(akod_occ_data_sptlim_bias %>%
             filter(sptlim_filter == TRUE,
                    distlim_filter == TRUE,
                    .summary == TRUE)) +
  tm_dots(size = .1, shape = 21, col = "lightsalmon")

# Distancia minima entre os pontos ----
filter_thin <- spThin::thin(loc.data = akod_occ_data_sptlim_bias,
                            lat.col = "latitude",
                            long.col = "longitude",
                            spec.col = "species",
                            thin.par = 5, # tem que justificar - Unidade: km
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
akod_occ_data_sptlim_bias_sptdist <- dplyr::left_join(
  x = akod_occ_data_sptlim_bias,
  y = filter_thin,
  by = c("longitude", "latitude")) %>%
  dplyr::mutate(sptdist_filter = replace_na(sptdist_filter, FALSE)) #%>%
#dplyr::relocate(sptdist_filter, .after = date_filter)
akod_occ_data_sptlim_bias_sptdist

tm_shape(ma) +
  tm_polygons() +
  tm_shape(akod_occ_data_sptlim_bias_sptdist %>%
             filter(sptlim_filter == TRUE,
                    distlim_filter == TRUE,
                    sptdist_filter == TRUE,
                    .summary == TRUE)) +
  tm_dots(size = .1, shape = 21, col = "lightsalmon")

# Aplicar todos os filtros ----
## aqui e criada uma nova tabela, sem as occs que caem em algum filtro
akod_occ_data_filter <- akod_occ_data_sptlim_bias_sptdist %>%
  filter(sptlim_filter == TRUE,
         distlim_filter == TRUE,
         sptdist_filter == TRUE,
         .summary == TRUE) %>%
  dplyr::select(species, longitude, latitude, year, base, id_og)
akod_occ_data_filter

mapview::mapview(akod_occ_data_filter)

# manual editing ----
#akod_occ_data_filter_edit <- mapedit::editFeatures(akod_occ_data_filter) # atencao para o Done!
akod_occ_data_filter_edit <- akod_occ_data_filter

# verificar
mapview::mapview(akod_occ_data_filter_edit)

# export ----
# vetor
akod_occ_data_filter_edit %>%
  sf::st_write("data/occs/akod.shp", append=F)

# tabela
akod_occ_data_filter_edit %>%
  sf::st_drop_geometry() %>%
  readr::write_csv("data/occs/akod_filtered.csv")
