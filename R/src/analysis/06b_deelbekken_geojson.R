
# ====================================================
# Scriptnaam: 06_export_geojson_per_soort.R
# Project: Craywatch
# Datum: 11-03-2026
# Beschrijving:
# - Exporteert per soort een aparte GeoJSON voor webpublicatie.
# - Elke GeoJSON bevat:
#     * alle deelbekkens als polygonen
#     * alle punten van de geselecteerde soort
# - De logica is analoog aan 06_deelbekken_leaflet.R:
#     * actieve deelbekkens = minstens 1 VHAG-waarneming
#     * rode punten = VHAG/open systeem
#     * oranje punten = WVLC/niet gekoppeld
# - Schrijft optioneel ook een species_index.json weg.
# ====================================================

# --- 0. Instellingen laden ---
source("./src/analysis/config.R")

# --- 1. Data inlezen ---
if (!file.exists(file_analyse_dataset_rapport)) {
  stop("Analyse dataset niet gevonden. Draai eerst script 03.")
}
df_analyse <- readr::read_csv(file_analyse_dataset_rapport, show_col_types = FALSE)

# --- AUTO-FIX: Check of VHAG bestaat (Script 04) ---
if (!"VHAG" %in% names(df_analyse)) {
  message("⚠️ Kolom 'VHAG' ontbreekt. Script 04 wordt uitgevoerd...")
  script_candidates <- c("./src/04_link_vhag-wvlc.R", "./04_link_vhag-wvlc.R")
  script_04 <- script_candidates[file.exists(script_candidates)][1]
  
  if (!is.na(script_04)) {
    source(script_04)
    df_analyse <- readr::read_csv(file_analyse_dataset_rapport, show_col_types = FALSE)
  } else {
    stop("Kan script 04 niet vinden.")
  }
}

required_cols <- c("Longitude", "Latitude", "date", "VHAG")
missing_required <- setdiff(required_cols, names(df_analyse))
if (length(missing_required) > 0) {
  stop("Ontbrekende kolommen in analyse_dataset.csv: ", paste(missing_required, collapse = ", "))
}

# --- 2. Deelbekkens ophalen ---
message("Deelbekken ophalen via API...")
subbekkens_sf <- sf::st_read(url_wfs_deelbekken, quiet = TRUE) %>%
  sf::st_make_valid() %>%
  sf::st_transform(crs_wgs84)

poly_id_col <- "DEELBID"
if (!poly_id_col %in% names(subbekkens_sf)) poly_id_col <- names(subbekkens_sf)[1]

poly_name_col <- if ("DEELBEKNM" %in% names(subbekkens_sf)) "DEELBEKNM" else NA_character_

# --- 3. Outputmap ---
out_dir <- file.path(dir_maps_deelbekken, "geojson_per_soort")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# --- 4. Helperfuncties ---
make_slug <- function(x) {
  x %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_replace_all("^_|_$", "")
}

# schrijf json index zonder extra packages buiten config-setup te veronderstellen
write_species_index <- function(index_df, path) {
  jsonlite::write_json(
    x = split(index_df, seq_len(nrow(index_df))),
    path = path,
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
  )
}

species_index <- list()

# --- 5. Loop over soorten ---
for (species_name in gbif_species) {
  
  sp_col <- tolower(species_name)
  
  if (!sp_col %in% names(df_analyse)) {
    message("Kolom ontbreekt voor soort, overslaan: ", species_name)
    next
  }
  
  message("Verwerk soort: ", species_name)
  
  # Zelfde logica als in de leafletkaart
  df_sp <- df_analyse %>%
    dplyr::filter(.data[[sp_col]] == 1) %>%
    dplyr::mutate(
      species = species_name,
      species_slug = make_slug(species_name),
      has_vhag = !is.na(VHAG),
      point_type = dplyr::if_else(has_vhag, "VHAG (Open)", "WVLC/Niet gekoppeld"),
      point_color = dplyr::if_else(has_vhag, "#cb181d", "orange")
    ) %>%
    dplyr::arrange(has_vhag)
  
  if (nrow(df_sp) == 0) {
    message("Geen waarnemingen voor: ", species_name)
    next
  }
  
  # --- Punten naar sf ---
  points_sf <- sf::st_as_sf(
    df_sp,
    coords = c("Longitude", "Latitude"),
    crs = crs_wgs84,
    remove = FALSE
  )
  
  # --- Join om actieve deelbekkens te bepalen ---
  points_joined <- sf::st_join(points_sf, subbekkens_sf, join = sf::st_intersects)
  
  counts_per_poly <- points_joined %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(has_vhag == TRUE) %>%
    dplyr::filter(!is.na(.data[[poly_id_col]])) %>%
    dplyr::count(.data[[poly_id_col]], name = "n_obs_open")
  
  # --- Alle deelbekkens exporteren, met actieve status ---
  map_polygons_all <- subbekkens_sf %>%
    dplyr::left_join(counts_per_poly, by = poly_id_col) %>%
    dplyr::mutate(
      n_obs_open = dplyr::coalesce(n_obs_open, 0L),
      species = species_name,
      species_slug = make_slug(species_name),
      feature_type = "deelbekken",
      active = n_obs_open > 0,
      fillColor = dplyr::if_else(active, "#fb6a4a", NA_character_),
      fillOpacity = dplyr::if_else(active, 0.6, 0),
      color = dplyr::if_else(active, "#fb6a4a", "#999999"),
      weight = dplyr::if_else(active, 2, 1),
      point_color = NA_character_,
      point_type = NA_character_,
      has_vhag = NA,
      popup_text = paste0(
        "<b>Deelbekken:</b> ",
        if (!is.na(poly_name_col)) .data[[poly_name_col]] else "Onbekend",
        "<br><b>Open Systeem waarnemingen:</b> ", n_obs_open
      )
    )
  
  # --- Punten attribuuttabel harmoniseren ---
  points_sf <- points_sf %>%
    dplyr::mutate(
      feature_type = "observation",
      active = NA,
      n_obs_open = NA_integer_,
      fillColor = NA_character_,
      fillOpacity = NA_real_,
      color = point_color,
      weight = 1,
      popup_text = paste0(
        "<b>Soort:</b> <i>", species_name, "</i><br>",
        "<b>Status:</b> ", point_type, "<br>",
        "<b>Datum:</b> ", date
      )
    )
  
  # --- Zelfde kolommen afdwingen ---
  target_cols <- union(names(map_polygons_all), names(points_sf))
  
  for (cc in setdiff(target_cols, names(map_polygons_all))) map_polygons_all[[cc]] <- NA
  for (cc in setdiff(target_cols, names(points_sf))) points_sf[[cc]] <- NA
  
  map_polygons_all <- map_polygons_all[, target_cols]
  points_sf <- points_sf[, target_cols]
  
  combined_sf <- dplyr::bind_rows(map_polygons_all, points_sf)
  
  # GeoJSON naar EPSG:4326 forceren
  combined_sf <- sf::st_transform(combined_sf, crs_wgs84)
  
  out_file <- file.path(out_dir, paste0(make_slug(species_name), ".geojson"))
  if (file.exists(out_file)) file.remove(out_file)
  
  sf::st_write(combined_sf, out_file, driver = "GeoJSON", quiet = TRUE)
  
  species_index[[length(species_index) + 1]] <- data.frame(
    species = species_name,
    slug = make_slug(species_name),
    file = paste0(make_slug(species_name), ".geojson"),
    label_nl = dplyr::coalesce(unname(species_labels_dutch[tolower(species_name)]), species_name),
    n_points = nrow(points_sf),
    n_active_deelbekkens = sum(map_polygons_all$active, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

# --- 6. Indexbestand wegschrijven ---
if (length(species_index) > 0) {
  species_index_df <- dplyr::bind_rows(species_index) %>%
    dplyr::arrange(label_nl)
  
  index_file <- file.path(out_dir, "species_index.json")
  write_species_index(species_index_df, index_file)
  
  message("species_index.json opgeslagen in: ", index_file)
} else {
  message("Geen GeoJSON-bestanden aangemaakt; species_index.json niet geschreven.")
}

message("Klaar. GeoJSON-bestanden opgeslagen in: ", out_dir)
EOF