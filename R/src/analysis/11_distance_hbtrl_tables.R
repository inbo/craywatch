# ==============================================================================
# Scriptnaam:   11_distance_hbtrl_tables.R
# Project: Craywatch
# Beschrijving: 
#   1. Analyseert ruimtelijke relaties tussen kreeftenpopulaties en SBZ-H gebieden.
#   2. Bepaalt de status per gebied: In gebied, <100m of <1000m.
#   3. Aggregeert resultaten: kiest per gebied de 'dichtste' status en de 'oudste' datum.
#   4. Exporteert een A4-geformatteerde tabel naar Word (met Flextable).
#
# Afhankelijkheden:
#   - src/analysis/config.R
#   - src/analysis/08_load_aq_sbz.R
#
# Output:
#   - CSV: data/output/.../tabel_afstanden_hbtrl_volledig.csv
#   - Word: data/output/.../tabel_afstanden_hbtrl_A4.docx
# ==============================================================================

# --- 0. Instellingen & Data laden ---
source("./src/analysis/config.R")
library(tidyverse)
library(flextable)
library(officer)

# Laad de basisdata (zorgt dat 'hbtrl' en 'CF_presence' beschikbaar zijn)
source("./src/analysis/08_load_aq_sbz.R")

message("--- Start Ruimtelijke Analyse (Volledige HBTRL) ---")

# --- 1. Voorbereiding Referentielaag (HBTRL) ---
# Selecteer naam en geometrie. We voegen een area_id toe voor unieke identificatie.
# We gebruiken 'geom' omdat hbtrl hierin zit, maar select(naam) pakt geometry auto mee.
hbtrl_base <- hbtrl %>% 
  select(naam, geom) %>%   
  mutate(area_id = row_number()) 

message("Buffers berekenen voor referentielagen (dit kan even duren)...")
# Virtuele lagen aanmaken voor de afstandsanalyses
hbtrl_buf_100  <- st_buffer(hbtrl_base, dist = 100)
hbtrl_buf_1000 <- st_buffer(hbtrl_base, dist = 1000)


# --- 2. Wolken vormen (Populatieclusters) ---
message("Populatie-wolken genereren...")

create_clouds <- function(data_sf) {
  unique_soorten <- unique(data_sf$species)
  result_list <- list()
  
  for (soort in unique_soorten) {
    # Selecteer punten van 1 soort
    sp_points <- data_sf %>% filter(species == soort)
    
    if (nrow(sp_points) == 0) next
    
    # Buffer 100m rond punten en samenvoegen
    clouds_geom <- sp_points %>% 
      st_buffer(100) %>% 
      st_union() %>% 
      st_cast("POLYGON") %>% 
      st_sf() 
    
    if (nrow(clouds_geom) > 0) {
      clouds_geom$species <- soort
      # Unieke ID per wolk (seq_len is veiliger dan row_number buiten dplyr verbs)
      clouds_geom$wolk_id <- paste0(substr(soort, 1, 3), "_", seq_len(nrow(clouds_geom)))
      
      # Datum fix: sapply retourneert integers, we moeten terug naar Date converteren
      intersects <- st_intersects(clouds_geom, sp_points)
      raw_dates <- sapply(intersects, function(idx) {
        if(length(idx) > 0) min(sp_points$date[idx], na.rm = TRUE) else NA
      })
      clouds_geom$first_date <- as.Date(raw_dates, origin = "1970-01-01")
      
      result_list[[soort]] <- clouds_geom
    }
  }
  
  if(length(result_list) == 0) return(NULL)
  do.call(rbind, result_list)
}

clouds_sf <- create_clouds(CF_presence)

if(is.null(clouds_sf)) stop("Geen wolken gegenereerd. Check input data.")


# --- 3. Ruimtelijke Koppeling (Spatial Joins) ---
message("Intersecties berekenen...")

# A. IN GEBIED
join_in <- st_join(clouds_sf, hbtrl_base, left = FALSE) %>%
  mutate(distance_cat = "1_in_gebied", label = "In gebied")

# B. BUFFER 100m
join_100 <- st_join(clouds_sf, hbtrl_buf_100, left = FALSE) %>%
  mutate(distance_cat = "2_buffer_100", label = "< 100m")

# C. BUFFER 1000m
join_1000 <- st_join(clouds_sf, hbtrl_buf_1000, left = FALSE) %>%
  mutate(distance_cat = "3_buffer_1000", label = "< 1000m")


# --- 4. Resultaten Aggregeren per Gebied ---
message("Resultaten samenvatten per gebied...")

all_matches <- bind_rows(join_in, join_100, join_1000)

final_table <- all_matches %>%
  st_drop_geometry() %>%
  # Groepeer per Soort en Gebied
  group_by(species, naam) %>%
  # Stap 1: Houd alleen de regels over met de 'beste' (kleinste) afstandscategorie
  filter(distance_cat == min(distance_cat)) %>%
  # Stap 2: Binnen die beste categorie, pak de vroegste datum
  summarise(
    Locatie_Status = first(label),      
    Datum_Eerste_Wnm = min(first_date, na.rm = TRUE),
    .groups = "drop"
  )


# --- 5. Sortering en CSV Output ---
output_df <- final_table %>%
  select(
    Soort = species,
    Locatie_Status,
    Gebiedsnaam = naam,
    Datum_Eerste_Wnm
  ) %>%
  arrange(
    Soort, 
    factor(Locatie_Status, levels = c("In gebied", "< 100m", "< 1000m")), 
    Gebiedsnaam
  )

# CSV Opslaan
outfile_csv <- file.path(dir_bescherming_output, "tabel_afstanden_hbtrl_volledig.csv")
if(!dir.exists(dirname(outfile_csv))) dir.create(dirname(outfile_csv), recursive=TRUE)
write_csv(output_df, outfile_csv)
message(paste("CSV tabel opgeslagen:", outfile_csv))


# --- 6. Formatteren voor Word (Flextable) ---
message("Start opmaak tabel voor A4 formaat...")

# 6a. Definieer sectie-eigenschappen (A4 Portrait)
sect_properties <- prop_section(
  page_size = page_size(orient = "portrait", width = 21, height = 29.7),
  type = "continuous",
  page_margins = page_mar(bottom = 2, top = 2, right = 2, left = 2, gutter = 0)
)

# 6b. Flextable opbouw
ft <- flextable(output_df) %>%
  
  # A. Algemene stijl & Compactheid
  theme_vanilla() %>% 
  font(fontname = "Calibri", part = "all") %>%
  fontsize(size = 9, part = "body") %>%
  fontsize(size = 10, part = "header") %>%
  bold(part = "header") %>%
  
  # Rijhoogte verkleinen: Padding minimaliseren
  padding(padding.top = 1.5, padding.bottom = 1.5, part = "body") %>%
  height(height = 0.4, part = "body") %>% 
  
  # B. Verticale 'Soort' kolom (Samenvoegen & Draaien)
  merge_v(j = "Soort") %>%
  rotate(j = "Soort", rotation = "btlr", align = "center") %>% # btlr = bottom-to-top
  valign(j = "Soort", valign = "center", part = "body") %>%
  
  # C. Uitlijning
  align(j = c("Locatie_Status", "Datum_Eerste_Wnm"), align = "center", part = "all") %>%
  align(j = "Gebiedsnaam", align = "left", part = "all") %>%
  
  # D. Kolombreedtes (Totaal ~16cm voor A4 Portrait)
  width(j = "Soort", width = 0.8, unit = "cm") %>%            
  width(j = "Locatie_Status", width = 2.7, unit = "cm") %>%
  width(j = "Datum_Eerste_Wnm", width = 2.5, unit = "cm") %>%
  width(j = "Gebiedsnaam", width = 10.0, unit = "cm") %>%     
  
  # E. Randen 
  border_remove() %>%
  # Buitenkader
  border_outer(part = "all", border = fp_border(color = "black", width = 1.5)) %>%
  # Header lijn
  border_inner_h(part = "header", border = fp_border(color = "black", width = 1.5)) %>%
  # Verticale scheidingslijnen
  border_inner_v(part = "all", border = fp_border(color = "gray80", width = 0.5)) %>%
  # Horizontale lijnen: Eerst overal aanzetten, fix_border_issues haalt ze weg in de gemerged cells
  border_inner_h(part = "body", border = fp_border(color = "black", width = 1.0)) %>%
  fix_border_issues()


# --- 7. Opslaan naar Word ---
outfile_doc <- file.path(dir_bescherming_output, "tabel_afstanden_hbtrl_A4.docx")

doc <- read_docx() %>%
  body_add_fpar(fpar(ftext("Tabel: Overzicht Kreeften in HBTRL-gebieden", prop = fp_text(bold = TRUE, font.size = 12)))) %>%
  body_add_flextable(ft) %>%
  body_end_section_continuous() 

print(doc, target = outfile_doc)

message(paste0("Klaar! De tabel is opgeslagen in:\n", outfile_doc))