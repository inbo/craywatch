# ====================================================
# Scriptnaam: 03_merge_craywatch-gbif.R
# Project: Craywatch
# Datum: 26-11-2025
# Beschrijving:
# - Voegt verwerkte Craywatch en GBIF data samen
# - Verwijdert dubbele waarnemingen (GBIF records die al in Craywatch zitten)
# - Koppelt ruimtelijke info (Bekkens, waterlopen, watervlakken)
# - Exporteert de finale dataset voor analyse
# # Gebaseerd op: Concepten uit script [04_analyze_validated_data] van M. Vermeylen
# ====================================================

# --- 0. Instellingen laden ---
source("./src/analysis/config.R")
required_species <- tolower(gbif_species)

# --- 1. Inputs Controleren & Laden ---

# Check: Craywatch processed data (uit script 01)
if (!file.exists(file_inter_craywatch_clean)) {
  message("Bewerkte craywatch data niet gevonden. Script 01 wordt uitgevoerd...")
  source("./01_prepare_craywatch_data.R")
}
craywatch_wide <- readRDS(file_inter_craywatch_clean)

# Check: GBIF processed data (uit script 02)
if (!file.exists(file_inter_gbif_processed)) {
  message("Bewerkte GBIF data niet gevonden. Script 02 wordt uitgevoerd...")
  source("./02_prepare_gbif_data.R")
}
gbif_clean <- readRDS(file_inter_gbif_processed)

# Check: Raw craywatch data (nodig voor ontdubbeling op ID niveau)
if (!file.exists(file_craywatch_validated)) stop("Craywatch bronbestand niet gevonden!")
craywatch_raw <- read_csv(file_craywatch_validated, show_col_types = FALSE)

# --- 2. Shapefiles laden ---
bekken       <- st_read(file_bekken, quiet = TRUE) %>% st_transform(crs_lambert)

# --- 3. Data ontdubbelen ---
# Verwijder GBIF records afkomstig van 'Natuurpunt:Waarnemingen' 
# én waarvan het ID al voorkomt in de craywatch dataset.

cray_ids <- as.character(craywatch_raw$id)

ids_to_discard <- gbif_clean %>%
  select(occurrenceID) %>% 
  filter(str_detect(occurrenceID, "^Natuurpunt:Waarnemingen")) %>% 
  mutate(extracted_ids = str_remove(occurrenceID, "^Natuurpunt:Waarnemingen:")) %>%
  separate_rows(extracted_ids, sep = ":") %>%
  filter(extracted_ids %in% cray_ids) %>%
  pull(occurrenceID) %>%
  unique()

gbif_clean <- gbif_clean %>%
  filter(!occurrenceID %in% ids_to_discard)

print(paste("GBIF records verwijderd (al in craywatch):", length(ids_to_discard)))

# --- 4. GBIF naar wide ---
# Maak structuur compatibel met craywatch (presence=1, absences = NA, CPUE=NA)
gbif_wide <- gbif_clean %>%
  transmute( 
    locID      = NA_character_,
    session_nr = NA_character_,
    vrijwillID = NA_character_,
    dat.source = "gbif_data",
    year       = year,
    date       = make_date(year, month, day),
    Latitude   = decimalLatitude,
    Longitude  = decimalLongitude,
    trapdays   = NA_real_,
    found_species = tolower(species),
    coordinateUncertaintyInMeters = coordinateUncertaintyInMeters
  )

# Vul kolommen aan voor elke soort
for (sp in required_species) {
  gbif_wide[[sp]] <- if_else(gbif_wide$found_species == sp, 1, NA_real_)
  gbif_wide[[paste0("CPUE_", sp)]] <- NA_real_
}
gbif_wide <- gbif_wide %>% select(-found_species)

# --- 4b. Add coordinateuncertainty-column for VHAG/WVLC coupling in 04 (Aangepast) ---
# De waarde (2m) is een aanname voor de hoge precisie van velddata
craywatch_wide$coordinateUncertaintyInMeters <- 2

# --- 5. Samenvoegen craywatch & GBIF ---
full_dataset <- bind_rows(craywatch_wide, gbif_wide)
message(paste("Totaal aantal records in merged dataset:", nrow(full_dataset)))

# Maak het SF object aan
full_dataset_sf <- st_as_sf(
  full_dataset,
  coords = c("Longitude", "Latitude"), # De kolomnamen voor X en Y
  crs = 4326,                          # Input is WGS84 (GPS coördinaten)
  remove = FALSE                       # Behoud de kolommen in de dataframe (handig voor CSV export)
) %>%
  st_transform(crs_lambert)


dataset_analyse <- full_dataset_sf %>%
  # Koppel bekkens (punt in polygoon)
  st_join(
    bekken %>% select(BEKNR, BEKNAAM), 
    join = st_intersects, 
    left = TRUE) %>%
  
  # Terug naar dataframe
  st_drop_geometry() %>%
  
  # Selecteer de uiteindelijke kolommen
  select(
    dat.source, locID, session_nr, vrijwillID, year, date, Latitude, Longitude, 
    BEKNR, BEKNAAM, coordinateUncertaintyInMeters,
    all_of(required_species), starts_with("CPUE_")
  )

# --- 8. Export CSV ---
if (!dir.exists(dirname(file_analyse_dataset_rapport))) dir.create(dirname(file_analyse_dataset_rapport), recursive = TRUE)
write.csv(dataset_analyse, file = file_analyse_dataset_rapport, quote = TRUE, row.names = FALSE)
message(paste("Dataset opgeslagen in rapport map:", file_analyse_dataset_rapport))


