# ====================================================
# Scriptnaam: 08_load_aq_sbz.R
# Project: Craywatch
# Beschrijving: 
# - Laadt alle ruimtelijke beschermingslagen (N2000, SBP, HBTRL)
# - Laadt en verwerkt de kreeftendata (CF_presence)
# - Transformeert alles naar crs lambert
# ====================================================
source("./src/analysis/config.R")

# We gaan ervan uit dat config.R al geladen is door het moederscript
if (!exists("file_n2000_habitats")) {
  stop("Configuratie niet geladen. Run eerst source('./src/config.R')")
}

message("--- Start laden ruimtelijke data (Lagen & Soorten) ---")

# 1. Natura 2000 (enkel als laag voor leaflet, niet voor statische kaart)
# ----------------------------------------------------
if (!exists("natura_2000_aq")) { 
  message("Laden Natura 2000...")
  natura_2000 <- st_read(file_n2000_habitats, quiet = TRUE) %>%
    st_transform(crs_lambert)
  
  natura_2000_aq <- natura_2000 %>%
    filter(str_detect(HAB1, paste0("\\b(", paste(aqua_habcodes, collapse = "|"), ")\\b"))) %>%
    mutate(habitat_id = row_number()) 
}

# 2. Habitatrichtlijngebieden (HBTRL)
# ----------------------------------------------------
if (!exists("hbtrl_aq")) {
  message("Laden HBTRL...")
  hbtrl <- st_read(url_hbrl, quiet = TRUE)
  
  if (is.na(st_crs(hbtrl))) st_crs(hbtrl) <- crs_lambert
  hbtrl <- st_transform(hbtrl, crs_lambert)
}

# 3. Soortenbeschermingsprogramma's (SBP)
# ----------------------------------------------------
if (!exists("sbp_pgs_aq")) {
  message("Laden SBP (PGS)...")
  sbp_pgs <- st_read(url_sbp_pgs, quiet = TRUE)
  if (is.na(st_crs(sbp_pgs))) st_crs(sbp_pgs) <- crs_lambert
  
  sbp_pgs_aq <- sbp_pgs %>%
    st_transform(crs_lambert) %>%
    filter(sbp %in% aquatische_sbp) %>%
    mutate(habitat_id = row_number())
}

# B. SBP Vissen (Lijnen) + DEBUG Naamkoppeling
# B. SBP Vissen (Lijnen) + Naamkoppeling
if (!exists("sbp_vissen")) {
  message("Laden SBP (Vissen)...")
  sbp_vissen <- st_read(url_sbp_pls, quiet = TRUE)
  
  if (is.na(st_crs(sbp_vissen))) st_crs(sbp_vissen) <- crs_lambert
  sbp_vissen <- st_transform(sbp_vissen, crs_lambert) %>%
    mutate(habitat_id = row_number())
  
  message("  > Start koppeling waterloopnamen...")
  
  if (file.exists(file_waterloopsegmenten)) {
    vha_ref <- st_read(file_waterloopsegmenten, quiet = TRUE) %>%
      st_transform(crs_lambert) %>%
      filter(!is.na(NAAM) & NAAM != "") %>%
      select(NAAM) 
    
    if (nrow(vha_ref) > 0 && nrow(sbp_vissen) > 0) {
      
      # Nearest Feature matching
      nearest_idx <- st_nearest_feature(sbp_vissen, vha_ref)
      matched_vha <- vha_ref[nearest_idx, ]
      dists <- st_distance(sbp_vissen, matched_vha, by_element = TRUE)
      
      sbp_vissen <- sbp_vissen %>%
        mutate(
          temp_naam = matched_vha$NAAM,
          match_afstand_m = as.numeric(dists) 
        )
      
      # Debug output naar console
      message("\n  --- MATCHING STATISTIEKEN ---")
      message(paste("  Aantal lijnen:", nrow(sbp_vissen)))
      message("  Afstand tot waterloop (min / mediaan / max):")
      print(summary(sbp_vissen$match_afstand_m))
      
      bad_matches <- sbp_vissen %>% 
        st_drop_geometry() %>%
        arrange(desc(match_afstand_m)) %>%
        select(gebied, temp_naam, match_afstand_m) %>%
        head(5)
      
      message("\n  Top 5 grootste afstanden:")
      print(bad_matches)
      message("  ---------------------------------\n")
      
      # Definitieve toewijzing (zonder haakjes)
      sbp_vissen <- sbp_vissen %>%
        mutate(
          waterloop_naam = temp_naam,
          gebied = case_when(
            !is.na(waterloop_naam) ~ paste0(gebied, " ", waterloop_naam),
            TRUE ~ gebied
          )
        ) %>%
        select(-temp_naam) 
      
      message(paste("  > Klaar. Alle gebieden voorzien van waterloopnaam."))
      
    } else {
      message("  ! Waarschuwing: Referentielaag of SBP laag is leeg.")
    }
  } else {
    message("  ! Fout: Bestand niet gevonden: ", file_waterloopsegmenten)
  }
}

# 4. Kreeftendata (analysedataset)
# ----------------------------------------------------
if (!exists("CF_presence")) {
  message("Laden Kreeftendata & Transformatie...")
  
  # Lees CSV
  CF_data_raw <- read_csv(file_analyse_dataset_rapport, show_col_types = FALSE)
  
  # Forceer alle kolomnamen naar kleine letters
  names(CF_data_raw) <- tolower(names(CF_data_raw))
  
  # Check of 'date' kolom bestaat (eventueel hernoemen van 'datum')
  if(!"date" %in% names(CF_data_raw) && "datum" %in% names(CF_data_raw)) {
    CF_data_raw <- CF_data_raw %>% rename(date = datum)
  }
  
  if(!"date" %in% names(CF_data_raw)) {
    stop("Fout: Geen kolom 'date' of 'datum' gevonden in de dataset!")
  }
  
  # Zorg dat kolomnamen matchen met config
  species_columns <- tolower(gbif_species)
  
  # Van Wide naar Long format
  CF_long <- CF_data_raw %>%
    # Selecteer kolommen (nu zeker met kleine letters)
    select(
      dat.source,
      date,
      longitude, latitude,
      all_of(species_columns)
    ) %>%
    # Datum conversie (veilig)
    mutate(
      # Als read_csv het al als Date herkende, doe niets. Anders parse.
      date = if(inherits(date, "Date")) date else ymd(date)
    ) %>%
    pivot_longer(
      cols      = all_of(species_columns),
      names_to  = "species",
      values_to = "presence_raw"
    ) %>%
    mutate(
      presence = case_when(
        presence_raw == 1 ~ 1,
        presence_raw == 0 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(-presence_raw)
  
  # Omzetten naar SF object (Punten) en transformeren naar Lambert
  CF_presence <- CF_long %>%
    filter(presence == 1) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = crs_wgs84) %>%
    st_transform(crs_lambert)
}

message("--- Alle data succesvol geladen ---")