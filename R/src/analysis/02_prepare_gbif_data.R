# ====================================================
# Scriptnaam: 02_prepare_gbif_data.R
# Project: Craywatch
# Datum: 26-11-2025
# Beschrijving:
# - Downloadt recente GBIF data (automatisch indien niet aanwezig)
# - Filtert op taxon, kwaliteit en locatie (ruimtelijke grens Vlaanderen)
# - Slaat verwerkte data op voor gebruik in Script 03
# # Gebaseerd op: Concepten uit script [04_analyze_validated_data] van M. Vermeylen
# ====================================================

# --- 0. Instellingen laden ---
source("./src/analysis/config.R")

# --- 1. Download Logica Bepalen ---
# Check of het bestand al bestaat in de folderstructuur
if (file.exists(file_gbif_occurrences)) {
  message(paste("Bestaand GBIF bestand gevonden:", file_gbif_occurrences))
  message("Download wordt overgeslagen. Zet 'FORCE_DOWNLOAD <- TRUE' om toch te vernieuwen.")
  RUN_NEW_DOWNLOAD <- FALSE
} else {
  message("Geen lokaal GBIF bestand gevonden.")
  message("Nieuwe download wordt automatisch gestart...")
  RUN_NEW_DOWNLOAD <- TRUE
}

# Optioneel: Forceer download door dit op TRUE te zetten (overschrijft bovenstaande logica)
FORCE_DOWNLOAD <- FALSE 
if (FORCE_DOWNLOAD) {
  message("Download geforceerd door gebruiker (FORCE_DOWNLOAD = TRUE).")
  RUN_NEW_DOWNLOAD <- TRUE
}

if (RUN_NEW_DOWNLOAD) {
  
  message("Starten procedure GBIF download...")
  
  # Haal taxon keys op voor de soorten uit config.R
  taxon_data <- name_backbone_checklist(name = gbif_species)
  
  taxonkeys <- taxon_data %>% 
    filter(matchType == "EXACT") %>% 
    pull(usageKey)
  
  message(paste(length(taxonkeys), "taxa gevonden voor download."))
  
  # Start de download request bij GBIF
  download_key <- occ_download(
    pred_in("taxonKey", taxonkeys),
    pred("country", gbif_country),
    pred("hasCoordinate", TRUE),
    pred("hasGeospatialIssue", FALSE),
    pred_gte("year", gbif_min_year),
    pred("occurrenceStatus", gbif_occurrence_state),
    user  = gbif_user,
    pwd   = gbif_pwd,
    email = gbif_email,
    curlopts = list(http_version = 2)
  )
  
  # Wachtloop
  print(paste("Download aangevraagd. ID:", download_key))
  occ_download_wait(download_key) 
  
  # Download ophalen
  occ_download_get(download_key, path = dir_gbif_download, overwrite = TRUE)
  
  # Unzippen en inlezen
  gbif_raw_import <- occ_download_import(
    x = occ_download_get(download_key, path = dir_gbif_download)
  )
  
  # Sla ruwe CSV op als archief
  if (!dir.exists(dirname(file_gbif_occurrences))) dir.create(dirname(file_gbif_occurrences), recursive = TRUE)
  write_csv(gbif_raw_import, file_gbif_occurrences)
  
  # Sla DOI op
  meta <- occ_download_meta(download_key)
  writeLines(meta$doi, con = file.path(dir_gbif_input, "gbif_download_doi.txt"))
  
  print("Nieuwe data gedownload en opgeslagen.")
  
} else {
  message("Download stap overgeslagen. Overgaan op filterlogica")
}


# --- 3. Filteren ---

# Lees data (vers gedownload of bestaand)
if (!file.exists(file_gbif_occurrences)) stop("Geen input bestand gevonden na download stap.")
gbif_raw <- read_csv(file_gbif_occurrences, show_col_types = FALSE)

print(paste("Totaal aantal ruwe records:", nrow(gbif_raw)))

# Filteren 
# Filters (issue, status, onzekerheid) komen uit config.R
gbif_clean <- gbif_raw %>%
  distinct(occurrenceID, .keep_all = TRUE) %>%
  filter(
    !issue %in% gbif_issues_to_discard,
    !identificationVerificationStatus %in% gbif_id_status_to_discard,
    coordinateUncertaintyInMeters <= gbif_max_coordinate_uncertainty_m | is.na(coordinateUncertaintyInMeters)
  ) 

print(paste("Aantal na kwaliteitsfilters:", nrow(gbif_clean)))

# B. Ruimtelijke filter voor Vlaanderen
if (!file.exists(file_vlaanderen_grenzen)) stop("Shapefile Vlaanderen niet gevonden!")
vlaanderen <- st_read(file_vlaanderen_grenzen, quiet = TRUE)

gbif_clean <- gbif_clean %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), 
           crs = crs_wgs84, 
           remove = FALSE) %>% # Kolommen decimalLatitude/Longitude behouden!
  st_transform(st_crs(vlaanderen)) %>%
  st_filter(vlaanderen) %>%
  st_drop_geometry()

print(paste("Aantal GBIF records na ruimtelijke filter:", nrow(gbif_clean)))

# --- 4. Opslaan ---
saveRDS(gbif_clean, file_inter_gbif_processed)
message(paste("GBIF data succesvol opgeslagen in:", file_inter_gbif_processed))




