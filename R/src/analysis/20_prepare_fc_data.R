# ====================================================
# Scriptnaam: 20_Prepare_fc_data.R
# Project: Craywatch
# Datum: 04-12-2025
# Beschrijving: 
# - Ruimtelijke validatie van de fysicochemische data (koppelen aan VHAG/WVLC)
# ====================================================

# --- 0. Instellingen & libraries ---
source("./src/analysis/config.R")

library(mapview)
library(units)

# --- 1. Data & shapefiles laden ---
message("Laden FC data en referentielagen...")

file_fc_breed_input <- file.path(dir_data_input, "fc_data", "fc_fc_data_breed.Rdata")
file_fc_long_input  <- file.path(dir_data_input, "fc_data", "fc_data.Rdata")

if(!file.exists(file_fc_breed_input)) stop("FC breed data niet gevonden!")
if(!file.exists(file_fc_long_input)) stop("FC data niet gevonden!")

load(file_fc_breed_input) # Laadt object 'fc_breed'
load(file_fc_long_input)  # Laadt object 'fc_data'

# GIS Referentielagen transformeren naar Lambert (31370)
waterloopsegmenten <- read_sf(file_waterloopsegmenten) %>% st_transform(crs_lambert)
watervlakken       <- read_sf(file_watervlakken) %>% st_transform(crs_lambert)
watergang          <- read_sf(file_watergang) %>% st_transform(crs_lambert) 

# ==============================================================================
# FC meetpunten aan WVLC en VHAG koppelen (valideren)
# ==============================================================================

# --- 2. Ruimtelijke koppeling (berekeningen) ---
message("--- Start Ruimtelijke Koppeling ---")

columns_to_keep <-c("sample_point", "sample_datum_monstername", "sample_tijdstip_monstername",
                    "sample_point_omschrijving", "lambert_x", "lambert_y", "waterloop",  
                    "bekken", "gemeente", "geometry")

# 1. fc_raw_spatial_calc:
#    Bevat ALLE punten met de berekende afstand tot dichtstbijzijnde water (nog geen filtering)
fc_raw_spatial_calc <- fc_breed %>%
  select(any_of(columns_to_keep)) %>%
  distinct(sample_point, .keep_all = TRUE) %>%
  mutate(
    # Bepaal het dichtst waterloopsegment (VHAG)
    nearest_river_index = st_nearest_feature(., waterloopsegmenten),
    VHAS = waterloopsegmenten$VHAS[nearest_river_index],
    VHAG = waterloopsegmenten$VHAG[nearest_river_index],
    distances_vhag = st_distance(geometry, waterloopsegmenten[nearest_river_index, ], by_element = TRUE),
    
    # bepaal het dichtste watervlak (WVLC)
    nearest_waterbody_index = st_nearest_feature(., watervlakken),
    WVLC = watervlakken$WVLC[nearest_waterbody_index],
    distances_wv = st_distance(geometry, watervlakken[nearest_waterbody_index, ], by_element = TRUE)
  ) %>%
  # vergelijk VHAG vs WVLC en kies de dichtste
  mutate(
    VHASFinal = if_else(distances_vhag <= distances_wv, VHAS, NA),
    VHAGFinal = if_else(distances_vhag <= distances_wv, VHAG, NA),
    WVLCFinal = if_else(distances_wv < distances_vhag, WVLC, NA),
    
    distances = pmin(distances_vhag, distances_wv),
    distances = set_units(distances, "m"), 
    is_river  = distances_vhag <= distances_wv 
  )

# check afstanden
summary(fc_raw_spatial_calc$distances)


# --- 3. Koppeling op basis van afstandscriterium ---
message("--- Filteren op afstand en breedte ---")

# 2. fc_valid_strict:
#    Punten op minder dan 10m afstand van VHAG of WVLC object
fc_valid_strict <- fc_raw_spatial_calc %>%
  filter(distances <= set_units(max_link_distance_m, "m"))

# 3. fc_candidates_distant:
#    Punten die te ver liggen (> 10m) en nader onderzoek nodig hebben.
fc_candidates_distant <- fc_raw_spatial_calc %>%
  filter(distances > set_units(max_link_distance_m, "m"))

# 4. fc_valid_width_corrected:
#    Punten die ver liggen, maar wel binnen 10 m van de rivierbreedte vallen (via GRB)
fc_valid_width_corrected <- fc_candidates_distant %>%
  
  # Enkel rivieren kunnen we checken op breedte (meren hebben geen lineaire breedte)
  filter(is_river == TRUE) %>%
  
  # Koppel aan GRB watergang
  st_join(watergang, join = st_nearest_feature, suffix = c("", "wg")) %>%
  
  # Validatie: Zit de GRB polygoon op dezelfde VHAG?
  filter(VHAGFinal == VHAGwg) %>% 
  
  # Bereken buffer
  mutate(
    breedteschatting = OPPERVL / LENGTE,
    max_allowed_dist = set_units(breedteschatting / 2 + max_link_distance_m, "m")
  ) %>%
  
  # Check of punt binnen buffer valt
  filter(distances <= max_allowed_dist) %>%
  
  # Zorg dat kolommen gelijk zijn aan de 'strict' set voor samenvoegen
  select(any_of(colnames(fc_valid_strict)))

# 5. fc_locations_validated:
#    finale set van unieke locaties (strict + width corrected)
fc_locations_validated <- bind_rows(fc_valid_strict, fc_valid_width_corrected) %>%
  dplyr::select(-VHAS, -VHAG, -WVLC, -sample_datum_monstername, -sample_tijdstip_monstername) %>%
  dplyr::rename(VHAS = VHASFinal, VHAG = VHAGFinal, WVLC = WVLCFinal) 

message(paste0("Totaal unieke FC-locaties na validatie: ", nrow(fc_locations_validated)))

# --- 4. check ---
message("--- Visuele Controle ---")

# Identificeer wat is weggegooid
ids_validated <- fc_locations_validated$sample_point
fc_removed <- fc_breed %>% 
  dplyr::filter(!sample_point %in% ids_validated) %>%
  distinct(sample_point, .keep_all = TRUE)

message(paste("Aantal verwijderde punten:", nrow(fc_removed)))

if (interactive()) {
  map_totaal <- mapview(waterloopsegmenten, layer.name = "Waterlopen", color = "blue") +
    mapview(watervlakken, layer.name = "Watervlakken", col.regions = "lightblue") +
    mapview(fc_locations_validated, col.regions = "green", layer.name = "Behouden (Valid)") +
    mapview(fc_removed, col.regions = "red", cex = 4, layer.name = "Verwijderd (Invalid)")
  
  print(map_totaal)
}

# exporteer de gekoppelde fc locaties
if(!dir.exists(dirname(file_fc_locations_validated))) dir.create(dirname(file_fc_locations_validated), recursive = TRUE)
save(fc_locations_validated, file = file_fc_locations_validated) 

# ==============================================================================
# Enkel FC waarden van gekoppelde FC meetlocaties houden
# ==============================================================================
message("--- Start constructie FC dataset ---")

# --- 5. fc data aan gekoppelde fc locaties koppelen ---
fc_locations_validated <- st_drop_geometry(fc_locations_validated)

fc_locations_data <- fc_breed %>%
  # 1. koppelen (geen inner_join? alle waarden behouden?)
  left_join(fc_locations_validated, by = "sample_point") %>%
  
  # 2. variabelen aanmaken
  mutate(
    date    = as.Date(sample_datum_monstername),
    Year    = as.numeric(format(date, "%Y")),
    MaandNr = as.numeric(format(date, "%m")),
    entryID = row_number() 
  ) %>%
  
  # 3. selecteren & rangschikken
  select(
    entryID,                                
    sample_point, 
    sample_datum_monstername,
    sample_tijdstip_monstername,
    Year,                                   
    MaandNr,                                
    geometry,
    VHAG, VHAS, WVLC,
    any_of(fc_parameter_map)                
  ) %>%

# chlorofyl toevoegen
  full_join(
    fc_data %>%
      dplyr::filter(parameter_symbool == 'Clfyl a') %>%
      select(-c(parameter_omschrijving, teken, eenheid,
                resultaat, sample_point_omschrijving:gemeente)) %>%
      pivot_wider(names_from = parameter_symbool,
                  values_from = resultaat_detectielimiet),
    by = c("sample_point", 
           "sample_datum_monstername", 
           "sample_tijdstip_monstername")
  )%>%
# ongekoppelde verwijderen
  filter(!is.na(VHAG) | !is.na(WVLC))

# Export fc_locations_data
if(!dir.exists(dirname(file_fc_locations_data))) dir.create(dirname(file_fc_locations_data), recursive = TRUE)
save(fc_locations_data, file = file_fc_locations_data) 

message("Aantal rijen in fc_locations_data: ", nrow(fc_locations_data))
message(paste("Klaar! Dataset opgeslagen met", nrow(fc_locations_data), "rijen."))