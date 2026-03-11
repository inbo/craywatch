# ====================================================
# Scriptnaam: 18_cpue_maps_trends.R
# Project: Craywatch
# Datum: 01-12-2025
# Beschrijving: 
# - Maakt verspreidingskaarten met CPUE, absences (Craywatch) 
# en presences (GBIF)
# - Berekent trends in bezette km-hokken (pre & post Craywatch)
# ====================================================

# --- 0. Instellingen laden ---
source("./src/analysis/config.R")

# --- 1. Data inlezen ---
if (!file.exists(file_analyse_dataset_rapport)) stop("Run eerst script 03!")

# Lees de gecombineerde dataset (CW + GBIF)
occurrences <- read_csv(file_analyse_dataset_rapport, show_col_types = FALSE)

# Shapefiles zijn al beschikbaar via get_baseplot() in config, 
# maar voor de grid-analyse (Deel 2) hebben we de polygoon van Vlaanderen nodig.
vlaanderen_sf <- st_read(file_vlaanderen_grenzen, quiet = TRUE) %>% 
  st_transform(crs_lambert)

# ==============================================================================
# DEEL 1: CPUE KAARTEN GENEREREN
# ==============================================================================
message("Starten met genereren van CPUE kaarten...")

# Loop over elke soort die in de config staat
for (species_col in tolower(gbif_species)) {
  
  # Naam voor plot titels/bestanden
  species_name_nl <- species_labels_dutch[species_col]
  
  # 1. Data filteren voor deze soort
  
  # GBIF waarnemingen (alleen presences)
  gbif_points <- occurrences %>%
    filter(dat.source == "gbif_data", !!sym(species_col) == 1) %>%
    filter(!is.na(Longitude), !is.na(Latitude)) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = crs_wgs84) %>%
    st_transform(crs_lambert)
  
  # Craywatch: presences (met CPUE waarde)
  cpue_col_name <- paste0("CPUE_", species_col)
  
  cw_presence <- occurrences %>%
    filter(dat.source == "craywatch_data", !!sym(species_col) == 1) %>%
    filter(!is.na(Longitude), !is.na(Latitude)) %>%
    # sorteren van hoge CPUE, dus bovenop in de kaart
    arrange(!!sym(cpue_col_name)) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = crs_wgs84) %>%
    st_transform(crs_lambert)
  
  # Craywatch: nul-vangsten
  cw_absence <- occurrences %>%
    filter(dat.source == "craywatch_data", !!sym(species_col) == 0) %>%
    filter(!is.na(Longitude), !is.na(Latitude)) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = crs_wgs84) %>%
    st_transform(crs_lambert)
  
  # Kaart genereren
  p <- get_baseplot() +
    geom_sf(data = cw_absence, shape = 4, size = 2, color = "grey60", alpha = 0.8, stroke = 0.8) +
    geom_sf(data = gbif_points, shape = 16, size = 2, color = "#404040", alpha = 0.6) +
    geom_sf(data = cw_presence, aes(color = !!sym(cpue_col_name)), size = 3) +
    scale_color_gradient(
      low = "yellow", 
      high = "red",
      limits = c(0, 5),         
      oob = scales::squish,     
      name = "CPUE"
    ) +
    labs(
      title = species_name_nl,
      subtitle = NULL
    ) +
      theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0),
      title.position = "left",
      legend.position = "right",
      legend.title = element_text(size = 12),
      legend.key.height = unit(1, "cm")
    )

  filename <- file.path(dir_maps_cpue, paste0("map_cpue_", gsub(" ", "_", species_col), ".png"))
  ggsave(filename, plot = p, width = 10, height = 6, bg = "white")
  
  print(paste("Kaart opgeslagen:", filename))
}


