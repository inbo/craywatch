# ====================================================
# Scriptnaam: 16_citizenscience_tables.R
# Beschrijving: 
# - Metadata analyse van burgerwetenschapsdata
# - Focus op bruikbare sessies
# - CATC tabel met aan/afwezigheid
# ====================================================

# --- 0. Instellingen laden ---
source("./src/analysis/config.R")

library(tidyverse)
library(officer)
library(flextable)

if(!exists("dir_citizen_stats_output")) {
  dir_citizen_stats_output <- file.path(dir_data_output, "citizen_stats")
}
if(!dir.exists(dir_citizen_stats_output)) dir.create(dir_citizen_stats_output, recursive = TRUE)

file_provincies <- file.path(dir_shapefiles, "provincies.shp")

# --- 1. Data Inlezen ---
if (!file.exists(file_inter_craywatch_clean)) stop("Run script 01 eerst!")
cray_clean_wide <- readRDS(file_inter_craywatch_clean)

if (!file.exists(file_analyse_dataset_rapport)) stop("Run script 03 eerst!")
spatial_links <- read_csv(file_analyse_dataset_rapport, show_col_types = FALSE) %>%
  filter(dat.source == "craywatch_data") %>%
  select(locID, session_nr, VHAG, WVLC, CATC) %>%
  distinct(session_nr, .keep_all = TRUE)

# --- 2. Data Preparatie ---
cray_meta <- cray_clean_wide %>%
  left_join(spatial_links, by = c("locID", "session_nr")) %>%
  pivot_longer(
    cols = starts_with("CPUE_"),
    names_to = "species_col",
    values_to = "cpue_val"
  ) %>%
  mutate(
    species = str_remove(species_col, "CPUE_"),
    individuals_caught = round(cpue_val * trapdays)
  ) %>%
  select(
    locID, session_nr, year, date, trapdays, vrijwillID, 
    Latitude, Longitude, VHAG, WVLC, CATC,
    species, individuals_caught
  )

# --- 3. Protocol & Bruikbaarheid ---
session_summary <- cray_meta %>%
  group_by(locID, session_nr, year, vrijwillID, Latitude, Longitude, VHAG, WVLC, CATC) %>%
  summarise(
    total_catch = sum(individuals_caught, na.rm = TRUE),
    trapdays    = first(trapdays),
    .groups = "drop"
  ) %>%
  mutate(
    by_protocol = case_when(
      total_catch > 0 ~ TRUE,
      trapdays >= 12  ~ TRUE,
      TRUE            ~ FALSE
    )
  )

bruikbaar      <- session_summary %>% filter(by_protocol == TRUE)
niet_bruikbaar <- session_summary %>% filter(by_protocol == FALSE)

# ====================================================
# START RAPPORTAGE (WORD)
# ====================================================
message("--- Start genereren Word Rapport ---")

# --- STIJL DEFINITIES ---
# We maken een specifieke stijl aan voor gewone tekst (Calibri 11)
style_calibri_11 <- fp_text(font.size = 11, font.family = "Calibri")

# Helper functie voor tabellen
style_flextable <- function(ft) {
  ft %>%
    theme_vanilla() %>%
    autofit() %>%
    font(fontname = "Calibri", part = "all") %>%
    fontsize(size = 11, part = "all")
}

doc_report <- read_docx() %>%
  body_add_par("Citizen Science Rapportage Craywatch", style = "heading 1") %>%
  body_add_fpar(fpar(ftext(paste("Gegenereerd op:", Sys.Date()), prop = style_calibri_11))) %>%
  body_add_par("", style = "Normal") 

# --- DEEL A: Algemene Statistieken ---
message("... Algemene statistieken")

stats_text <- paste0(
  "Totaal aantal sessies: ", nrow(session_summary), "\n",
  "Totaal unieke locaties: ", n_distinct(session_summary$locID), "\n\n",
  "Aantal BRUIKBARE SESSIES: ", nrow(bruikbaar), "\n",
  "Aantal NIET-BRUIKBARE SESSIES: ", nrow(niet_bruikbaar), "\n",
  "Percentage bruikbaar: ", round(nrow(bruikbaar)/nrow(session_summary)*100, 1), "%"
)

doc_report <- doc_report %>%
  body_add_par("1. Algemene Statistieken", style = "heading 2") %>%
  body_add_fpar(fpar(ftext(stats_text, prop = style_calibri_11))) %>%
  body_add_par("", style = "Normal")

# --- DEEL B: Vrijwilligers ---
message("... Vrijwilligers tabel")

df_vrijwilligers <- bruikbaar %>% 
  group_by(year) %>% 
  summarise(
    `Aantal Vrijwilligers` = n_distinct(vrijwillID),
    `Aantal Sessies` = n(),
    `Unieke Locaties` = n_distinct(locID)
  )

ft_vrijwilligers <- flextable(df_vrijwilligers) %>% 
  style_flextable()

doc_report <- doc_report %>%
  body_add_par("2. Deelname per jaar (Bruikbare data)", style = "heading 2") %>%
  body_add_flextable(ft_vrijwilligers) %>%
  body_add_par("", style = "Normal")

# --- DEEL C: Ruimtelijke Analyse ---
message("... Ruimtelijke analyse")

if (file.exists(file_provincies)) {
  provincies <- st_read(file_provincies, quiet = TRUE) %>% st_transform(4326)
  
  loc_sf <- bruikbaar %>%
    distinct(locID, .keep_all = TRUE) %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  
  loc_prov <- st_join(loc_sf, provincies, join = st_within)
  
  missing <- which(is.na(loc_prov$PROVNAAM))
  if(length(missing) > 0) {
    nearest <- st_nearest_feature(loc_prov[missing,], provincies)
    loc_prov$PROVNAAM[missing] <- provincies$PROVNAAM[nearest]
  }
  
  bruikbaar_prov <- bruikbaar %>% 
    left_join(st_drop_geometry(loc_prov) %>% select(locID, PROVNAAM), by = "locID")
  
  # C1. Tabel Provincies (Sessies)
  df_prov <- bruikbaar_prov %>% 
    group_by(PROVNAAM) %>% 
    summarise(
      `Aantal Sessies` = n(),
      `Unieke Locaties` = n_distinct(locID)
    ) %>% 
    arrange(desc(`Aantal Sessies`))
  
  ft_prov <- flextable(df_prov) %>% 
    style_flextable() %>%
    set_header_labels(PROVNAAM = "Provincie")
  
  doc_report <- doc_report %>%
    body_add_break() %>%
    body_add_par("3. Verdeling per Provincie", style = "heading 2") %>%
    body_add_flextable(ft_prov) %>%
    body_add_par("", style = "Normal")
  
  # C2. Tabel Watertypes
  bruikbaar_water <- bruikbaar_prov %>%
    mutate(
      Watertype = case_when(
        !is.na(WVLC) ~ "Gesloten",
        !is.na(VHAG) ~ "Open",
        TRUE         ~ "Niet gekoppeld"
      )
    )
  
  df_water_pivot <- bruikbaar_water %>% 
    group_by(PROVNAAM, Watertype) %>% 
    summarise(n_sessies = n(), .groups = "drop") %>%
    pivot_wider(names_from = Watertype, values_from = n_sessies, values_fill = 0) %>%
    mutate(`Niet gekoppeld` = if("Niet gekoppeld" %in% names(.)) `Niet gekoppeld` else 0) %>%
    mutate(Totaal = Gesloten + Open + `Niet gekoppeld`) %>%
    select(PROVNAAM, any_of(c("Gesloten", "Open", "Niet gekoppeld", "Totaal")))
  
  ft_water <- flextable(df_water_pivot) %>% 
    style_flextable() %>%
    set_header_labels(PROVNAAM = "Provincie") %>%
    bold(j = "Totaal", part = "all")
  
  noot_watertype <- "Noot: 'Niet gekoppeld' betreft waarnemingen die niet automatisch gekoppeld konden worden aan een waterloop (afstand > 50m)."
  
  doc_report <- doc_report %>%
    body_add_par("4. Sessies per Provincie en Watertype", style = "heading 2") %>%
    body_add_flextable(ft_water) %>%
    body_add_fpar(fpar(ftext(noot_watertype, prop = style_calibri_11))) %>%
    body_add_par("", style = "Normal")
  
  # C3. Detail Open Systemen (CATC)
  message("... Tabel CATC genereren")
  
  df_catc <- bruikbaar_water %>%
    filter(!is.na(CATC)) %>%
    mutate(Resultaat = if_else(total_catch > 0, "Aanwezig", "Nulvangst")) %>%
    group_by(CATC, Resultaat) %>%
    summarise(n_sessies = n(), .groups = "drop") %>%
    pivot_wider(names_from = Resultaat, values_from = n_sessies, values_fill = 0) %>%
    mutate(
      Aanwezig  = if("Aanwezig" %in% names(.)) Aanwezig else 0,
      Nulvangst = if("Nulvangst" %in% names(.)) Nulvangst else 0,
      Totaal    = Aanwezig + Nulvangst
    ) %>%
    select(CATC, Aanwezig, Nulvangst, Totaal) %>%
    arrange(CATC)
  
  ft_catc <- flextable(df_catc) %>%
    style_flextable() %>%
    set_header_labels(CATC = "VHA Categorie") %>%
    bold(j = "Totaal", part = "all")
  
  noot_catc <- "Cat. 0 = Onbevaarbaar cat. 0; Cat. 1 = Onbevaarbaar cat. 1, etc. Aantallen zijn sessies."
  
  doc_report <- doc_report %>%
    body_add_par("5. Detail Open Systemen: Categorie Waterloop (CATC)", style = "heading 2") %>%
    body_add_flextable(ft_catc) %>%
    body_add_fpar(fpar(ftext(noot_catc, prop = style_calibri_11)))
  
}

# --- DEEL D: Tabel Aantallen per Soort ---
message("... Soortentabel genereren (Individuen & Locaties)")

cray_bruikbaar_soorten <- cray_meta %>%
  semi_join(bruikbaar, by = "session_nr")

# 1. Totalen per jaar
stats_jaar <- cray_bruikbaar_soorten %>%
  group_by(species, year) %>%
  summarise(
    I = sum(individuals_caught, na.rm = TRUE),
    L = n_distinct(locID[individuals_caught > 0]), 
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = c(I, L),
    names_glue = "{year}_{.value}", 
    values_fill = 0
  )

# 2. Totaal generaal
stats_totaal <- cray_bruikbaar_soorten %>%
  group_by(species) %>%
  summarise(
    Totaal_I = sum(individuals_caught, na.rm = TRUE),
    Totaal_L = n_distinct(locID[individuals_caught > 0]),
    .groups = "drop"
  )

# 3. Zorg dat de kolommen bestaan
required_cols <- c("2024_I", "2024_L", "2025_I", "2025_L")
for(col in required_cols) {
  if(!col %in% names(stats_jaar)) stats_jaar[[col]] <- 0
}

# 4. Samenvoegen
tabel_data <- stats_jaar %>%
  left_join(stats_totaal, by = "species") %>%
  mutate(
    Soort = species_labels_dutch[species]
  ) %>%
  mutate(Soort = coalesce(Soort, str_to_sentence(species))) %>%
  select(
    Soort, 
    `2024_I`, `2024_L`, 
    `2025_I`, `2025_L`, 
    Totaal_I, Totaal_L
  ) %>%
  arrange(desc(Totaal_I))

# 5. Flextable opmaak
ft_tabel <- flextable(tabel_data) %>%
  set_header_labels(
    Soort = "Soort",
    `2024_I` = "I", `2024_L` = "L",
    `2025_I` = "I", `2025_L` = "L",
    Totaal_I = "I", Totaal_L = "L"
  ) %>%
  add_header_row(
    values = c("", "2024", "2025", "Totaal"),
    colwidths = c(1, 2, 2, 2)
  ) %>%
  style_flextable() %>% # Past Calibri 11 toe
  align(j = 2:7, align = "center", part = "all") %>%
  bold(part = "header") %>%
  border_remove() %>%
  border_inner_h(part = "body", border = fp_border(color = "gray", width = 0.5)) %>%
  border_outer(part = "all", border = fp_border(color = "black", width = 1.5)) %>%
  border_inner_h(part = "header", border = fp_border(color = "black", width = 1))

noot_soorten <- "Legende: I = Aantal gevangen Individuen, L = Aantal unieke Locaties waar de soort daadwerkelijk gevangen is."

doc_report <- doc_report %>%
  body_add_break() %>%
  body_add_par("6. Overzicht vangsten per soort", style = "heading 2") %>%
  body_add_flextable(ft_tabel) %>%
  body_add_fpar(fpar(ftext(noot_soorten, prop = style_calibri_11)))

# --- 4. Opslaan ---
outfile_rapport <- file.path(dir_citizen_stats_output, "craywatch_citizenscience_rapport.docx")

print(doc_report, target = outfile_rapport)
message(paste("Klaar! Rapport opgeslagen in:", outfile_rapport))