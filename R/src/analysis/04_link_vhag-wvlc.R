# ==============================================================================
# Script: 04_link_vhag-wclc.R
# Project: Craywatch
# Doel:   Koppeling kreeftendata aan VHAG/WVLC 
# ==============================================================================

# --- 0. Setup ---
source("./src/analysis/config.R")

# --- 1. Data laden ---
message("Laden kreeftendata en referentielagen...")

if (!file.exists(file_analyse_dataset_rapport)) stop("Input file (stap 03) niet gevonden.")

# Masterdataset omzetten naar SF (Lambert72)
crayfish_sf <- read_csv(file_analyse_dataset_rapport) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(31370) %>%
  select(-any_of(c("VHAG", "CATC", "WVLC", "distances"))) 

# Shapefiles laden
# Gebruik de gedetailleerde segmenten voor de koppeling!
waterloopsegmenten <- read_sf(file_waterloopsegmenten, quiet = TRUE) %>% st_transform(31370)
vha_waterloop      <- read_sf(file_vha_catc, quiet = TRUE) %>% st_transform(31370)
watervlakken       <- read_sf(file_watervlakken, quiet = TRUE) %>% st_transform(31370)
watergang          <- read_sf(file_watergang, quiet = TRUE) %>% st_transform(31370)

message(paste("Analysedataset geladen:", nrow(crayfish_sf), "rijen."))

# --- 2. Initiële koppeling (Nearest feature) ---
message("Uitvoeren koppeling met WVLC/VHAS...")

# We zoeken in de segmenten
idx_riv <- st_nearest_feature(crayfish_sf, waterloopsegmenten)
idx_wat <- st_nearest_feature(crayfish_sf, watervlakken)

crayfish_raw_spatial_calc <- crayfish_sf %>%
  mutate(
    # Kandidaat open (uit waterloopsegmenten)
    VHAG_cand = waterloopsegmenten$VHAG[idx_riv],
    VHAS_cand = waterloopsegmenten$VHAS[idx_riv],
    # Check of CATC aanwezig is in segmenten file, anders later joinen
    CATC_cand = if("CATC" %in% names(waterloopsegmenten)) waterloopsegmenten$CATC[idx_riv] else NA,
    dist_riv  = as.numeric(st_distance(geometry, waterloopsegmenten[idx_riv, ], by_element = TRUE)),
    
    # Kandidaat gesloten
    WVLC_cand = watervlakken$WVLC[idx_wat],
    dist_wat  = as.numeric(st_distance(geometry, watervlakken[idx_wat, ], by_element = TRUE)),
    
    # Bepaal type en afstand
    type_water  = if_else(dist_riv <= dist_wat, "open", "gesloten"),
    dist_actual = pmin(dist_riv, dist_wat)
  )

# --- 3. Validatie & filtering ---
message("Toepassen validatie logica...")

buffer <- max_link_distance_m

# A. VHAS/WVLC binnen afstand buffer
crayfish_valid_strict <- crayfish_raw_spatial_calc %>%
  filter(dist_actual <= buffer) %>%
  mutate(
    valid_link = TRUE, # assumptie dat waterlichaam binnen bufferafstand gekoppeld is
    link_method = "<buffer"
  )

# B. VHAS buiten afstand buffer -> GRB -logica
crayfish_candidates_distant <- crayfish_raw_spatial_calc %>%
  filter(dist_actual > buffer & type_water == "open")

# C. WVLC niet binnen afstand buffer 
crayfish_notlinked <- crayfish_raw_spatial_calc %>%
  filter(dist_actual > buffer & type_water == "gesloten") %>%
  mutate(valid_link = FALSE, link_method = "not linked")

# Verwerking GRB kandidaten
if(nrow(crayfish_candidates_distant) > 0) {
  
  crayfish_valid_widt_corrected <- crayfish_candidates_distant %>%
    st_join(watergang, join = st_nearest_feature, suffix = c("", "_wg")) %>%
    mutate(
      vhag_from_grb = VHAG, 
      is_same_vhag = if_else(!is.na(vhag_from_grb), 
                             as.character(VHAG_cand) == as.character(vhag_from_grb), 
                             TRUE), 
      # Buffer berekening
      breedteschatting = OPPERVL / LENGTE,
      max_buffer       = (breedteschatting / 2) + buffer,
      # Dubbele validatie: afstand binnen buffer
      valid_link  = (dist_actual <= max_buffer) & is_same_vhag,
      link_method = if_else(valid_link, "GRB/2 + buffer", "not linked")
    ) %>%
    # Ruim GRB kolommen op (incl de tijdelijke vhag_from_grb)
    select(any_of(colnames(crayfish_raw_spatial_calc)), valid_link, link_method)
  
} else {
  crayfish_valid_widt_corrected <- crayfish_candidates_distant %>% mutate(valid_link = FALSE, link_method = NA)
}

# --- 4. Finaliseren ---
message("Samenvoegen en opschonen...")

crayfish_linked_dataset <- bind_rows(crayfish_valid_strict, crayfish_valid_widt_corrected, crayfish_notlinked) %>%
  arrange(locID, date) %>%
  mutate(
    VHAG = if_else(valid_link & type_water == "open", as.character(VHAG_cand), NA_character_),
    VHAS = if_else(valid_link & type_water == "open", as.character(VHAS_cand), NA_character_),
    CATC = if_else(valid_link & type_water == "open", as.character(CATC_cand), NA_character_),
    WVLC = if_else(valid_link & type_water == "gesloten", as.character(WVLC_cand), NA_character_),
    distances = if_else(valid_link, dist_actual, NA_real_)
  ) %>%
  select(-ends_with("_cand"), -dist_riv, -dist_wat, -dist_actual, -type_water)%>%
  mutate( vangstID = row_number())

# --- 5. Export ---
# stats printen
cat(paste0("Totaal records: ", nrow(crayfish_linked_dataset), "\n"))
cat(paste0("Succesvol gekoppeld: ", sum(crayfish_linked_dataset$valid_link), "\n"))
print(table(crayfish_linked_dataset$link_method[crayfish_linked_dataset$valid_link]))

# check via histogram
p <- ggplot(crayfish_linked_dataset %>% filter(valid_link), aes(x = distances)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  geom_vline(xintercept = buffer, color = "red", linetype = "dashed") +
  labs(title = "Afstanden gevalideerde koppelingen", x = "Afstand (m)")
ggsave(file.path(dir_data_intermediate, "check_spatial_link_distances.png"), p, width = 8, height = 6)

# opslaan
write.csv(crayfish_linked_dataset %>% st_drop_geometry(), 
          file = file_analyse_dataset_rapport, 
          quote = TRUE, 
          row.names = FALSE)

message(paste("Dataset opgeslagen in rapport map:", file_analyse_dataset_rapport))

map_sf <- crayfish_linked_dataset %>%
  # soorten samenvoegen tot één kolom voor weergave
  pivot_longer(
    cols = c("procambarus clarkii", "procambarus virginalis", "procambarus acutus", 
             "faxonius limosus", "pacifastacus leniusculus", "faxonius virilis", 
             "pontastacus leptodactylus"),
    names_to = "Soort",
    values_to = "Aanwezig"
  ) %>%
  filter(Aanwezig == 1) %>%
  
  mutate(
    # status bepalen
    Status = case_when(
      !is.na(VHAG) ~ "VHAG (Waterloop)",
      !is.na(WVLC) ~ "WVLC (Watervlak)",
      TRUE ~ "Niet gekoppeld"
    ),
    # afstand afronden op 1 decimaal voor popup
    Afstand_m = round(distances, 1)
  ) %>%
  
  # omzetten naar sf
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  
  # kolommen voor de popup
  select(Soort, date, Status, VHAG, WVLC, Afstand_m)

# kaart 
mapview(
  map_sf, 
  zcol = "Status", 
  col.regions = c("gray", "red", "orange"), 
  layer.name = "Kreeften",
  legend = TRUE
)
