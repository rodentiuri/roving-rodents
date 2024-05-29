#1-3. Occurrence data: RHIPIDOMYS MASTACALIS ----

# Importing data from GBIF ----
rhipi_occ_spocc <- spocc::occ(query = "Rhipidomys mastacalis",
                              from = c("gbif"),
                              has_coords = TRUE,
                              limit = 1e5)
rhipi_occ_spocc

# Processing and selecting location data
rhipi_occ_spocc_data <- spocc::occ2df(rhipi_occ_spocc) %>%
  dplyr::mutate(species = "Rhipidomys mastacalis",
                longitude = as.numeric(longitude),
                latitude = as.numeric(latitude),
                year = date %>% lubridate::year(),
                base = prov %>% stringr::str_to_lower()) %>%
  dplyr::select(name, species, longitude, latitude, year, base)
rhipi_occ_spocc_data

# Importing manually processed data ----
rhipi_occ_manual <- read.csv("data/occs/pre_R/rhipi_asm.csv") %>% 
  dplyr::mutate(longitude = as.numeric(longitude),
                latitude = as.numeric(latitude),
                year = as.numeric(year)) %>%
  dplyr::select(species, longitude, latitude, year, base) %>% 
  as_tibble()
rhipi_occ_manual

# Combining datasets ----
rhipi_occ_data <- dplyr::bind_rows(rhipi_occ_spocc_data, 
                                   rhipi_occ_manual)
rhipi_occ_data

# Visualizing on the map ----
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

# Spatial filter ----

# Import current distribution boundaries
rm_shape <- read_sf(dsn = "data/shapes/Rhipidomys-mastacalis/data/Rhipidomys_mastacalis.shp", layer = "Rhipidomys_mastacalis")
rm_shape
tm_shape(rm_shape) +
  tm_polygons()

# Clip occurrences according to the Atlantic Forest and the current distribution
## This command adds a new column indicating whether the coordinate is inside or outside the boundary
rhipi_occ_data_sptlim <- rhipi_occ_data_vector %>%
  dplyr::mutate(sptlim_filter = as.logical(sf::st_intersects(rhipi_occ_data_vector, ma, sparse = FALSE)),
                distlim_filter = as.logical(sf::st_intersects(rhipi_occ_data_vector, rm_shape, sparse = FALSE)))
rhipi_occ_data_sptlim

# Visualizing on the map
tm_shape(ma) +
  tm_polygons() +
  tm_shape(rhipi_occ_data_sptlim %>%
             filter(sptlim_filter == TRUE,
                    distlim_filter == TRUE)) +
  tm_dots(size = .15, shape = 21, col = "gold3")

# Filtering biases ----
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
rhipi_occ_data_sptlim_bias # tibble with the status of each coordinate according to the limits considered

# Visualizing on the map
## map very similar to the previous one because points are clustered.
## tip: reduce the size of the points and zoom in on the map.
## it is possible to see the difference in points between this and the previous map.
tm_shape(ma) +
  tm_polygons() +
  tm_shape(rhipi_occ_data_sptlim_bias %>%
             filter(sptlim_filter == TRUE,
                    distlim_filter == TRUE,
                    .summary == TRUE)) +
  tm_dots(size = .1, shape = 21, col = "gold3")

# Minimum distance between points ----
filter_thin <- spThin::thin(loc.data = rhipi_occ_data_sptlim_bias,
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

# Applying all filters ----
## here a new table is created, without the occurrences that fall into any filter
rhipi_occ_data_filter <- rhipi_occ_data_sptlim_bias_sptdist %>%
  filter(sptlim_filter == TRUE,
         distlim_filter == TRUE,
         sptdist_filter == TRUE,
         .summary == TRUE) %>%
  dplyr::select(species, longitude, latitude) #this line selects which columns remain
rhipi_occ_data_filter

mapview::mapview(rhipi_occ_data_filter)

# manual editing ----
#rhipi_occ_data_filter_edit <- mapedit::editFeatures(rhipi_occ_data_filter) # attention to the Done!
rhipi_occ_data_filter_edit <- rhipi_occ_data_filter

# check
mapview::mapview(rhipi_occ_data_filter_edit)

# export ----
# vector
rhipi_occ_data_filter_edit %>%
  sf::st_write("data/occs/rhipi.shp", append=F)

# table
rhipi_occ_data_filter_edit %>%
  sf::st_drop_geometry() %>%
  readr::write_csv("data/occs/rhipi_filtrado.csv")
