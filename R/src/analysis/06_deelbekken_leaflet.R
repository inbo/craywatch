# ====================================================
# Scriptnaam: 06_deelbekken_leaflet.R
# Project: Craywatch
# Datum: 24-12-2025
# Beschrijving:
# - Genereert één interactieve kaart met ALLE soorten.
# - Soorten zijn selecteerbaar via een menu (mutueel exclusief).
# - Alle deelbekkens worden getoond als grijze contouren (achtergrond).
# - Gekoloniseerde deelbekkens worden rood ingekleurd.
# ====================================================

# --- 0. Instellingen laden ---
source("./src/analysis/config.R")

# --- 1. Data inlezen ---

# A. Analyse dataset laden
if (!file.exists(file_analyse_dataset_rapport)) {
  stop("Analyse dataset niet gevonden. Draai eerst script 03.")
}
df_analyse <- read_csv(file_analyse_dataset_rapport, show_col_types = FALSE)

# --- AUTO-FIX: Check of VHAG bestaat (Script 04) ---
if (!"VHAG" %in% names(df_analyse)) {
  message("⚠️ Kolom 'VHAG' ontbreekt. Script 04 wordt uitgevoerd...")
  script_04 <- "./04_link_vhag-wvlc.R" 
  if(file.exists(script_04)) {
    source(script_04)
    df_analyse <- read_csv(file_analyse_dataset_rapport, show_col_types = FALSE)
  } else { stop("Kan script 04 niet vinden.") }
}

# B. Shapefiles (deelbekkens) - Via API
message("Deelbekken ophalen via API...")
subbekkens_sf <- st_read(url_wfs_deelbekken, quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(crs_wgs84) 

# ID kolom bepalen
poly_id_col <- "DEELBID" 
if (!poly_id_col %in% names(subbekkens_sf)) poly_id_col <- names(subbekkens_sf)[1]

# --- 2. Initialiseer de Kaart ---
message("Start genereren van de verzamelkaart...")

# Maak de basiskaart
# We voegen een laag "Grenzen" toe die altijd zichtbaar is.
# AANGEPAST: Kleur donkerder gemaakt (#999999) zodat contouren duidelijk zijn.
m <- leaflet(options = leafletOptions(minZoom = 7)) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    data = subbekkens_sf,
    fill = FALSE, 
    color = "#999999",  # Donkerder grijs voor duidelijke contouren
    weight = 1, 
    group = "Grenzen"
  )

# Lijst om de groepsnamen bij te houden voor het menu
species_groups <- c()

# --- 3. Loop over elke soort en voeg lagen toe ---

for (species_name in gbif_species) {
  
  sp_col <- tolower(species_name)
  group_name <- paste0("<i>", str_to_sentence(species_name), "</i>") 
  
  if (!sp_col %in% names(df_analyse)) next
  
  # --- Data prepareren ---
  df_sp <- df_analyse %>%
    filter(.data[[sp_col]] == 1) %>%
    mutate(
      has_vhag   = !is.na(VHAG),
      point_type = if_else(has_vhag, "VHAG (Open)", "WVLC/Niet gekoppeld"),
      point_color = if_else(has_vhag, "#cb181d", "orange") 
    ) %>%
    arrange(has_vhag)
  
  if (nrow(df_sp) == 0) next
  
  species_groups <- c(species_groups, group_name)
  
  # --- Spatial join voor Polygonen ---
  points_sf <- st_as_sf(df_sp, coords = c("Longitude", "Latitude"), crs = crs_wgs84, remove = FALSE)
  points_joined <- st_join(points_sf, subbekkens_sf, join = st_intersects)
  
  counts_per_poly <- points_joined %>%
    st_drop_geometry() %>%
    filter(has_vhag == TRUE) %>%
    filter(!is.na(.data[[poly_id_col]])) %>%
    count(.data[[poly_id_col]], name = "n_obs_open")
  
  # Filter de deelbekken-shapefile voor DEZE soort (alleen de actieve)
  map_polygons_sp <- subbekkens_sf %>%
    inner_join(counts_per_poly, by = poly_id_col) %>% 
    filter(n_obs_open > 0)
  
  # --- Polygoon laag toevoegen (indien aanwezig) ---
  if (nrow(map_polygons_sp) > 0) {
    
    map_polygons_sp$poly_popup <- paste0(
      "<b>Deelbekken:</b> ", if("DEELBEKNM" %in% names(map_polygons_sp)) map_polygons_sp$DEELBEKNM else "Onbekend", "<br>",
      "<b>Open Systeem waarnemingen:</b> ", map_polygons_sp$n_obs_open
    )
    
    m <- m %>%
      addPolygons(
        data = map_polygons_sp,
        fillColor = "#fb6a4a", 
        fillOpacity = 0.6,
        color = "#fb6a4a", # Rood randje voor actieve bekkens (ligt bovenop grijs)
        weight = 2,        # Iets dikker dan de grijze lijnen
        popup = ~poly_popup, 
        group = group_name 
      )
  }
  
  # --- Punten toevoegen ---
  points_sf$popup_text <- paste0(
    "<b>Soort:</b> <i>", species_name, "</i><br>",
    "<b>Status:</b> ", points_sf$point_type, "<br>",
    "<b>Datum:</b> ", points_sf$date
  )
  
  m <- m %>%
    addCircleMarkers(
      data = points_sf, 
      lng = ~Longitude, 
      lat = ~Latitude,
      color = ~point_color, 
      fillColor = ~point_color, 
      fillOpacity = 0.8,
      radius = 4, 
      weight = 1, 
      popup = ~popup_text,
      group = group_name 
    )
}

# --- 4. Layer Control & Legende ---

if (length(species_groups) > 0) {
  m <- m %>%
    addLayersControl(
      baseGroups = species_groups, 
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    
    # Aangepaste legende met extra uitleg over de contouren
    addControl(
      html = paste0(
        "<div style='background:white; padding:8px; border:1px solid #ccc; font-size:12px; width:220px; color:black;'>",
        "<b>Legende</b><br>",
        "<hr style='margin: 4px 0;'>",
        
        # Polygoon Actief
        "<i style='background:#fb6a4a; width:12px; height:12px; display:inline-block; opacity:0.6; margin-right:5px; border:1px solid red;'></i>",
        "Aanwezig (Open systeem)<br>",
        
        # Polygoon Leeg 
        "<i style='background:transparent; width:12px; height:12px; display:inline-block; margin-right:5px; border:1px solid #999999;'></i>",
        "Deelbekken (niet in open systeem)<br>",
        
        # Punten uitleg
        "<div style='margin-top:4px;'><b>Type Waarneming:</b></div>",
        "<div style='margin-top:2px;'>",
        "<i style='background:#cb181d; width:10px; height:10px; display:inline-block; border-radius:50%; margin-right:5px;'></i> Open Systeem (VHAG)",
        "</div>",
        "<div style='margin-top:2px;'>",
        "<i style='background:orange; width:10px; height:10px; display:inline-block; border-radius:50%; margin-right:5px;'></i> Gesloten/Niet gekoppeld",
        "</div>",
        "</div>"
      ),
      position = "bottomleft"
    )
} else {
  message("Geen soorten gevonden om te plotten.")
}

# --- 5. Opslaan ---
if(!dir.exists(dir_maps_deelbekken)) dir.create(dir_maps_deelbekken, recursive = TRUE)

file_name <- "kaart_totaal_overzicht_deelbekken.html"
output_path <- file.path(dir_maps_deelbekken, file_name)

saveWidget(m, file = output_path, selfcontained = TRUE)
message(paste("Verzamelkaart opgeslagen:", output_path))