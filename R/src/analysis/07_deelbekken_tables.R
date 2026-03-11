# ====================================================
# Scriptnaam: 07_deelbekken_tables.R
# Project: Craywatch
# Datum: 15-12-2025
# Beschrijving:
# - Genereert matrix-tabel (Deelbekken x Soort).
# ====================================================

# --- 0. Instellingen laden ---
source("./src/analysis/config.R")

# --- 1. Data inlezen ---
if (!file.exists(file_analyse_dataset_rapport)) {
  stop("Analyse dataset niet gevonden. Draai eerst script 03.")
}
df_analyse <- read_csv(file_analyse_dataset_rapport, show_col_types = FALSE)

# Deelbekkens ophalen
subbekkens_sf <- st_read(url_wfs_deelbekken, quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(crs_lambert)

name_col <- "DEELBEKNM"
if (!name_col %in% names(subbekkens_sf)) name_col <- names(subbekkens_sf)[1]

# --- 2. Ruimtelijke Analyse ---
message("Start ruimtelijke koppeling...")

all_species_data <- list()

for (species_name in gbif_species) {
  
  sp_col <- tolower(species_name)
  if (!sp_col %in% names(df_analyse)) next
  
  df_sp <- df_analyse %>%
    filter(.data[[sp_col]] == 1) %>%
    mutate(
      has_vhag = !is.na(VHAG),
      # Veilige namen zonder spaties
      Soort_Safe = gsub(" ", "_", str_to_sentence(species_name)) 
    )
  
  if (nrow(df_sp) > 0) {
    points_sf <- st_as_sf(df_sp, coords = c("Longitude", "Latitude"), crs = crs_wgs84) %>%
      st_transform(crs_lambert)
    
    joined <- st_join(points_sf, subbekkens_sf, join = st_intersects)
    
    joined_clean <- joined %>%
      st_drop_geometry() %>%
      filter(!is.na(.data[[name_col]])) %>%
      select(Deelbekken = all_of(name_col), has_vhag, Soort_Safe)
    
    all_species_data[[species_name]] <- joined_clean
  }
}

full_data <- bind_rows(all_species_data)

# --- 3. Matrix opbouwen ---
message("Matrix opbouwen...")

matrix_wide <- full_data %>%
  group_by(Deelbekken, Soort_Safe) %>%
  summarise(
    n_open = sum(has_vhag, na.rm = TRUE),
    n_total = n(),
    .groups = "drop"
  ) %>%
  mutate(
    status_code = case_when(
      n_open > 0 ~ "OPEN",
      n_total > 0 ~ "CLOSED",
      TRUE ~ "ABSENT"
    )
  ) %>%
  select(Deelbekken, Soort_Safe, status_code) %>%
  pivot_wider(
    names_from = Soort_Safe,
    values_from = status_code,
    values_fill = "ABSENT"
  ) %>%
  arrange(Deelbekken)

# --- 4. Tabel opmaken (Strakke celmaten) ---
message("Flextable opmaken...")

safe_species_cols <- setdiff(names(matrix_wide), "Deelbekken")

# Header labels herstellen 
header_labels <- as.list(gsub("_", " ", safe_species_cols))
names(header_labels) <- safe_species_cols
header_labels[["Deelbekken"]] <- "Deelbekken"

ft <- flextable(matrix_wide) %>%
  set_header_labels(values = header_labels) %>%
  
  # Basisstijl
  theme_vanilla() %>%
  font(fontname = "Calibri", part = "all") %>%
  
  # 1. Opmaak header
  # Headers mogen groter zijn en geroteerd
  fontsize(size = 9, part = "header") %>%
  italic(part = "header", j = safe_species_cols) %>%
  rotate(part = "header", j = safe_species_cols, rotation = "btlr", align = "bottom") %>%
  
  # 2. Opmaak body 
  # Zet fontgrootte op 1 pt voor de soorten (zodat tekst de cel niet open duwt)
  fontsize(j = safe_species_cols, size = 1, part = "body") %>%
  fontsize(j = "Deelbekken", size = 9, part = "body") %>% # Deelbekken naam wel leesbaar houden
  
  # Verwijder witruimte in de body
  padding(part = "body", padding = 0) %>%
  
  # 3. Afmetingen
  # autofit voor de "Deelbekken" kolom breedte en headers hoogte
  autofit() %>%
  
  # Forceer daarna de exacte maten voor de cellen
  width(j = safe_species_cols, width = 0.5, unit = "cm") %>%
  
  # Forceer rijhoogte naar exact 0.5 cm
  height(part = "body", height = 0.5, unit = "cm") %>%
  hrule(rule = "exact", part = "body") %>%
  
  # 4. uitlijning
  # Verticaal centreren zorgt dat deelbekken-tekst  in het midden van de 0.5cm staat
  valign(valign = "center", part = "body") %>%
  align(j = safe_species_cols, align = "center", part = "all") %>%
  
  # randen
  border_remove() %>%
  border_outer(part = "all", border = fp_border(color = "black", width = 1.5)) %>%
  border_inner(part = "all", border = fp_border(color = "gray80", width = 0.5))

# --- 5. Kleuren ---
col_open   <- "#cb181d" 
col_closed <- "orange" 

for(col in safe_species_cols) {
  f_open   <- as.formula(paste0("~ ", col, " == 'OPEN'"))
  f_closed <- as.formula(paste0("~ ", col, " == 'CLOSED'"))
  f_absent <- as.formula(paste0("~ ", col, " == 'ABSENT'"))
  
  ft <- ft %>%
    bg(j = col, i = f_open, bg = col_open) %>%
    bg(j = col, i = f_closed, bg = col_closed) %>%
    
    # Kleur gelijk aan achtergrond
    color(j = col, i = f_open, color = col_open) %>%
    color(j = col, i = f_closed, color = col_closed) %>%
    color(j = col, i = f_absent, color = "white")
}

# --- 6. Opslaan ---
outfile_doc <- file.path(dir_maps_output, "deelbekken", "deelbekken_table_matrix.docx")

doc <- read_docx() %>%
  body_add_fpar(fpar(ftext("Tabel: Aanwezigheid per deelbekken", prop = fp_text(bold = TRUE, font.size = 12)))) %>%
  
  body_add_fpar(fpar(
    ftext("Legende: ", prop = fp_text(bold = TRUE)),
    ftext("Donkerrood ", prop = fp_text(color = col_open, shading.color = col_open)),
    ftext(" = Aanwezig in open systeem (VHAG);  ", prop = fp_text()),
    ftext("Lichtrood ", prop = fp_text(color = col_closed, shading.color = col_closed)),
    ftext(" = Aanwezig in gesloten of niet-gekoppeld systeem.", prop = fp_text())
  )) %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_flextable(ft) %>%
  body_end_section_continuous()

print(doc, target = outfile_doc)

message(paste0("Klaar! Tabel opgeslagen in: ", outfile_doc))