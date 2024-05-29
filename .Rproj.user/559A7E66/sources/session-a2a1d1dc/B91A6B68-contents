#1-2. Occurrence data: NECTOMYS SQUAMIPES ----

# Importing data from GBIF ----
necto_occ_spocc <- spocc::occ(query = "Nectomys squamipes",
                              from = c("gbif"),
                              has_coords = TRUE,
                              limit = 1e5)
necto_occ_spocc

# Processing and selecting location data
necto_occ_spocc_data <- spocc::occ2df(necto_occ_spocc) %>%
  dplyr::mutate(species = "Nectomys squamipes",
                longitude = as.numeric(longitude),
                latitude = as.numeric(latitude),
                year = date %>% lubridate::year(),
                base = prov %>% stringr::str_to_lower()) %>%
  dplyr::select(name, species, longitude, latitude, year, base)
necto_occ_spocc_data

# Importing manually processed data ----
necto_occ_manual <- read.csv("data/occs/pre_R/necto_asm.csv") %>% 
  dplyr::mutate(longitude = as.numeric(longitude),
                latitude = as.numeric(latitude),
                year = as.numeric(year)) %>%
  dplyr::select(species, longitude, latitude, year, base) %>% 
  as_tibble()
necto_occ_manual

# Combining datasets ----
necto_occ_data <- dplyr::bind_rows(necto_occ_spocc_data, 
                                   necto_occ_manual)
necto_occ_data

# Visualizing on the map ----
necto_occ_data_vector <- necto_occ_data %>%
  tidyr::drop_na(longitude, latitude) %>%
  dplyr::mutate(lon = longitude, lat = latitude) %>%
  dplyr::filter(lon >= -180, lon <= 180, lat >= -90, lat <= 90) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
necto_occ_data_vector

tm_shape(ma, bbox = necto_occ_data_vector) +
  tm_polygons() +
  tm_shape(necto_occ_data_vector) +
  tm_dots(size = .15, shape = 21, col = "steelblue") +
  tm_graticules(lines = FALSE)

# Spatial filter ----

# Import current distribution boundaries
ns_shape <- read_sf(dsn = "data/shapes/Nectomys-squamipes/data/Nectomys_squamipes.shp", layer = "Nectomys_squamipes")
ns_shape
tm_shape(ns_shape) +
  tm_polygons()

# Clip occurrences according to the Atlantic Forest and the current distribution
## This command adds a new column indicating whether the coordinate is inside or outside the boundary
necto_occ_data_sptlim <- necto_occ_data_vector %>%
  dplyr::mutate(sptlim_filter = as.logical(sf::st_intersects(necto_occ_data_vector, ma, sparse = FALSE)),
                distlim_filter = as.logical(sf::st_intersects(necto_occ_data_vector, ns_shape, sparse = FALSE)))
necto_occ_data_sptlim

# Visualizing on the map
tm_shape(ma) +
  tm_polygons() +
  tm_shape(necto_occ_data_sptlim %>%
             filter(sptlim_filter == TRUE,
                    distlim_filter == TRUE)) +
  tm_dots(size = .05, shape = 21, col = "turquoise")

# Filtering biases ----
necto_occ_data_sptlim_bias <- CoordinateCleaner::clean_coordinates(
  x = sf::st_drop_geometry(necto_occ_data_sptlim),
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
necto_occ_data_sptlim_bias # tibble with the status of each coordinate according to the limits considered

# Visualizing on the map
## map very similar to the previous one because points are clustered.
## tip: reduce the size of the points and zoom in on the map.
### it is possible to see the difference in points between this and the previous map.
tm_shape(ma) +
  tm_polygons() +
  tm_shape(necto_occ_data_sptlim_bias %>%
             filter(sptlim_filter == TRUE,
                    distlim_filter == TRUE,
                    .summary == TRUE)) +
  tm_dots(size = .1, shape = 21, col = "turquoise")

# Minimum distance between points ----
filter_thin <- spThin::thin(loc.data = necto_occ_data_sptlim_bias,
                            lat.col = "latitude",
                            long.col = "longitude",
                            spec.col = "species",
                            thin.par = 5, # must be justified - unit = km
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

# Combining filters
necto_occ_data_sptlim_bias_sptdist <- dplyr::left_join(
  x = necto_occ_data_sptlim_bias,
  y = filter_thin,
  by = c("longitude", "latitude")) %>%
  dplyr::mutate(sptdist_filter = replace_na(sptdist_filter, FALSE)) #%>%
# dplyr::relocate(sptdist_filter, .after = date_filter)
necto_occ_data_sptlim_bias_sptdist

tm_shape(ma) +
  tm_polygons() +
  tm_shape(necto_occ_data_sptlim_bias_sptdist %>%
             filter(sptlim_filter == TRUE,
                    distlim_filter == TRUE,
                    sptdist_filter == TRUE,
                    .summary == TRUE)) +
  tm_dots(size = .1, shape = 21, col = "turquoise")

# Applying all filters ----
## here a new table is created, without the occurrences that fall into any filter
necto_occ_data_filter <- necto_occ_data_sptlim_bias_sptdist %>%
  filter(sptlim_filter == TRUE,
         distlim_filter == TRUE,
         sptdist_filter == TRUE,
         .summary == TRUE) %>%
  dplyr::select(species, longitude, latitude) #this line selects which columns remain
necto_occ_data_filter

mapview::mapview(necto_occ_data_filter)

# manual editing ----
#necto_occ_data_filter_edit <- mapedit::editFeatures(necto_occ_data_filter) # attention to the Done!
necto_occ_data_filter_edit <- necto_occ_data_filter

# check
mapview::mapview(necto_occ_data_filter_edit)

# export ----
# vector
necto_occ_data_filter_edit %>%
  sf::st_write("data/occs/necto.shp", append=F)

# table
necto_occ_data_filter_edit %>%
  sf::st_drop_geometry() %>%
  readr::write_csv("data/occs/necto_filtrado.csv")
