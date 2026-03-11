# ====================================================
# Scriptnaam: 05_deelbekken_maps.R
# Project: Craywatch 
# Datum: 27-11-2025
# Beschrijving:
# - genereert jpg kaarten per soort en hun status in de deelbekkens
# ====================================================

# --- 0. Instellingen laden ---
source("./src/analysis/config.R")

# --- 1. Data inlezen ---

# A. Analyse dataset laden
# A. Analyse dataset laden
if (!file.exists(file_analyse_dataset_rapport)) {
  stop("Analyse dataset niet gevonden. Draai eerst script 03.")
}
df_analyse <- read_csv(file_analyse_dataset_rapport, show_col_types = FALSE)

# --- AUTO-FIX: Check of VHAG bestaat (Script 04) ---
if (!"VHAG" %in% names(df_analyse)) {
  message("Kolom 'VHAG' ontbreekt (Script 04 is nog niet gedraaid).")
  message("Script 04 wordt nu uitgevoerd om de ruimtelijke koppeling te maken...")
  
  script_04 <- "./04_link_vhag-wvlc.R" 
  
  if(file.exists(script_04)) {
    source(script_04)
    df_analyse <- read_csv(file_analyse_dataset_rapport, show_col_types = FALSE)
    message("✅ Data ververst. Analyse gaat verder...")
    
  } else {
    stop(paste("Kan script 04 niet vinden op:", script_04))
  }
}

# B. Kaartlagen voorbereiden
message("Kaartlagen laden...")

# 1. Deelbekken via API
deelbekkens_sf <- st_read(url_wfs_deelbekken, quiet = TRUE) %>%
  st_make_valid() 

# 2. Baseplot genereren
message("Baseplot genereren...")
base_map <- get_baseplot() 

# 3. Grenzen ophalen uit de base_map
# In config.R is 'vlaanderen' de eerste laag (layers[[1]]) van basemap
vlaanderen_sf <- base_map$layers[[1]]$data

# 4. Transformatie deelbekkens naar lambert 72 
target_crs <- 31370
deelbekkens_sf <- st_transform(deelbekkens_sf, target_crs)

# --- 2. Loop over elke soort ---
message("Start genereren van kaarten...")

for (species_name in gbif_species) {
  
  # kolomnaam bepalen
  sp_col <- tolower(species_name)
  
  if (!sp_col %in% names(df_analyse)) {
    message(paste("LET OP: Kolom", sp_col, "niet gevonden. Sla over."))
    next
  }
  
  # --- 3. Data prepareren ---
  df_sp <- df_analyse %>%
    filter(.data[[sp_col]] == 1) %>%
    mutate(
      has_vhag   = !is.na(VHAG),
      point_type = if_else(has_vhag, "open systeem", "gesloten systeem - niet gekoppeld")
    ) %>%
  arrange(has_vhag) #rode bollen vanboven
  
  n_obs <- nrow(df_sp)
  
  if (n_obs == 0) {
    message(paste("  - Geen waarnemingen voor", species_name))
    next
  }
  
  message(paste("Bezig met:", species_name, "(", n_obs, "waarnemingen )"))
  
  # --- 4. Spatial join ---
  points_sf <- st_as_sf(df_sp, coords = c("Longitude", "Latitude"), crs = 4326) %>%
    st_transform(target_crs)
  
  points_joined <- st_join(points_sf, deelbekkens_sf, join = st_intersects)
  
  poly_id_col <- "DEELBID" 
  if (!poly_id_col %in% names(deelbekkens_sf)) poly_id_col <- names(deelbekkens_sf)[1]
  
  counts_per_poly <- points_joined %>%
    st_drop_geometry() %>%
    filter(has_vhag == TRUE) %>%
    filter(!is.na(.data[[poly_id_col]])) %>%
    count(.data[[poly_id_col]], name = "n_obs_open")
  
  map_polygons_colored <- deelbekkens_sf %>%
    inner_join(counts_per_poly, by = poly_id_col) %>% 
    filter(n_obs_open > 0)
  
  # --- 5. plotten ---
  
  p <- base_map +
    
    # laag 1: gekoloniseerde deelbekkens ingekleurd
    geom_sf(
      data = map_polygons_colored, 
      aes(fill = "aanwezigheid in open systeem"), # Let op: exacte match met scale_fill
      color = NA,       
      linewidth = 0,
      alpha = 0.5 
    ) +
    
    # laag 2: Alle deelbekken contouren
    geom_sf(data = deelbekkens_sf, fill = NA, color = "grey60", linewidth = 0.2) +
    
    # Laag 3: punten
    geom_sf(data = points_sf, aes(color = point_type), size = 1.5, alpha = 0.8) +
    
    # kleurschalen
    scale_fill_manual(
      values = c("aanwezigheid in open systeem" = "#3182bd"), 
      name = "Status van het deelbekken"
    ) +
    
    scale_color_manual(
      values = c("open systeem" = "#cb181d", "gesloten systeem - niet gekoppeld" = "orange"), 
      name = "Waarneming in:"
    ) +
    
    # legende voor status deelbekken
    guides(
      fill = guide_legend(
        override.aes = list(color = "black", linewidth = 0.2, alpha = 1)
      )
    ) +
    
    # focus op Vlaanderen
    coord_sf(
      xlim = st_bbox(vlaanderen_sf)[c(1,3)], 
      ylim = st_bbox(vlaanderen_sf)[c(2,4)],
      expand = FALSE
    ) +
    
    # layout
    labs(
      title = paste0("Waarnemingen van ", species_name, " op deelbekkenniveau"),
      subtitle = paste("Aantal waarnemingen:", n_obs),
      x = NULL, y = NULL
    ) +
    
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold.italic", size = 14, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 10, t=10)),
      legend.title = element_text(face="bold", size=10),
      legend.text = element_text(size=9)
    )
  
  # --- 6. opslaan ---
  file_name <- paste0("map_", gsub(" ", "_", species_name), "_deelbekken.jpg")
  output_path <- file.path(dir_maps_deelbekken, file_name)
  
  ggsave(output_path, plot = p, width = 10, height = 8, dpi = 300)
}

message("Klaar! JPG kaarten opgeslagen in: ", dir_maps_deelbekken)