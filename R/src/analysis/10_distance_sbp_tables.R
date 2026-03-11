# ==============================================================================
# Scriptnaam:   src/10_sbp_analyse_afstanden.R
# Project: Craywatch
# Beschrijving: 
#   1. Analyseert ruimtelijke relaties tussen kreeften en SBP (Vissen + Pgs).
#   2. Aggregeert gebieden tot één lijst per status-categorie om de tabel compact te houden.
#   3. Formaat: "Gebiedsnaam (Jaartal); Gebiedsnaam (Jaartal)"
#
# Afhankelijkheden:
#   - src/config.R
#   - src/07_load_aq_sbz.R
#
# Output:
#   - Word: data/output/.../tabel_afstanden_sbp_compact_A4.docx
# ==============================================================================

# --- 0. Instellingen & Data laden ---
source("./src/analysis/config.R")

# Laad de basisdata 
source("./src/analysis/08_load_aq_sbz.R")

message("--- Start Ruimtelijke Analyse (SBP Compact) ---")

# --- 1. Voorbereiding referentielaag (SBP) ---
message("SBP lagen voorbereiden en samenvoegen...")

# A. Verwerk sbp_vissen (Lijnen -> polygonen via 0.4m buffer)
sbp_vis_poly <- sbp_vissen %>%
  select(gebied, Doelsoort = soort, geom) %>%
  st_buffer(dist = 0.4) 

# B. Verwerk sbp_pgs_aq (polygonen)
sbp_pgs_poly <- sbp_pgs_aq %>%
  select(gebied, Doelsoort = soort, geom)

# C. Samenvoegen
sbp_base <- bind_rows(sbp_vis_poly, sbp_pgs_poly) %>%
  mutate(area_id = row_number())

# D. Buffers
message("Buffers berekenen...")
sbp_buf_100  <- st_buffer(sbp_base, dist = 100)
sbp_buf_1000 <- st_buffer(sbp_base, dist = 1000)


# --- 2. Wolken vormen ---
message("Populatie-wolken genereren...")

create_clouds <- function(data_sf) {
  unique_soorten <- unique(data_sf$species)
  result_list <- list()
  
  for (soort in unique_soorten) {
    sp_points <- data_sf %>% filter(species == soort)
    if (nrow(sp_points) == 0) next
    
    clouds_geom <- sp_points %>% 
      st_buffer(100) %>% 
      st_union() %>% 
      st_cast("POLYGON") %>% 
      st_sf() 
    
    if (nrow(clouds_geom) > 0) {
      clouds_geom$species <- soort
      # Unieke ID
      clouds_geom$wolk_id <- paste0(substr(soort, 1, 3), "_", seq_len(nrow(clouds_geom)))
      
      # Datum bepalen
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
if(is.null(clouds_sf)) stop("Geen wolken.")


# --- 3. Ruimtelijke koppeling ---
message("Intersecties berekenen...")

# Status categorieën toevoegen
join_in <- st_join(clouds_sf, sbp_base, left = FALSE) %>%
  mutate(distance_cat = 1, label = "In gebied")

join_100 <- st_join(clouds_sf, sbp_buf_100, left = FALSE) %>%
  mutate(distance_cat = 2, label = "< 100m")

join_1000 <- st_join(clouds_sf, sbp_buf_1000, left = FALSE) %>%
  mutate(distance_cat = 3, label = "< 1000m")

all_matches <- bind_rows(join_in, join_100, join_1000)


# --- 4. Resultaten aggregeren  ---
message("Resultaten aggregeren naar compact formaat...")

# Stap 4a: Eerst per gebied de 'beste' (kortste) afstand bepalen en jaartal ophalen
base_stats <- all_matches %>%
  st_drop_geometry() %>%
  group_by(species, Doelsoort, gebied) %>%
  filter(distance_cat == min(distance_cat)) %>%
  summarise(
    Locatie_Status = first(label),
    Locatie_Cat_Order = first(distance_cat),
    Jaartal = year(min(first_date, na.rm = TRUE)), # Alleen jaartal
    .groups = "drop"
  )

# Stap 4b: aggregeren: gebieden samenvoegen in één string
final_compact <- base_stats %>%
  # maak de string per gebied: "gebied (jaar)"
  mutate(gebied_str = paste0(gebied, " (", Jaartal, ")")) %>%
  
  # groepeer op kreeft, soort en status
  group_by(species, Doelsoort, Locatie_Status, Locatie_Cat_Order) %>%
  
  # gebieden aan elkaar plakken gescheiden met een puntkomma
  summarise(
    Gebieden_Lijst = paste(sort(gebied_str), collapse = "; "),
    .groups = "drop"
  ) %>%
  
  # genus hoofdletter
  mutate(
    species = str_to_sentence(species),
    Doelsoort = tolower(Doelsoort)
  ) %>%
  
  # sorteren voor de tabel
  arrange(species, Doelsoort, Locatie_Cat_Order) %>%
  
  # kolomnamen aanpassen
  select(
    Soort = species, 
    Doelsoort, 
    Status = Locatie_Status, 
    `Gebied (jaartal vroegste waarneming)` = Gebieden_Lijst
  )


# CSV export 
write_csv(final_compact, file.path(dir_bescherming_output, "tabel_afstanden_sbp_compact.csv"))


# --- 5. Formatteren voor word  ---
message("Start opmaak compacte tabel...")

# Sectie instellingen 
sect_properties <- prop_section(
  page_size = page_size(orient = "portrait", width = 21, height = 29.7),
  type = "continuous",
  page_margins = page_mar(bottom = 2, top = 2, right = 2, left = 2, gutter = 0)
)

ft <- flextable(final_compact) %>%
  
  # A. Algemene opmaak
  theme_vanilla() %>% 
  font(fontname = "Calibri", part = "all") %>%
  fontsize(size = 9, part = "body") %>%
  fontsize(size = 10, part = "header") %>%
  bold(part = "header") %>%
  italic(j = "Soort", part = "body") %>%
  # rijhoogte verkleinen 
  padding(padding.top = 3, padding.bottom = 3, part = "body") %>%
  
  # B. Cel samenvoegingen 
  merge_v(j = c("Soort", "Doelsoort")) %>%
  
  # C. Uitlijning
  rotate(j = "Soort", rotation = "btlr", align = "center") %>%
  valign(j = c("Soort", "Doelsoort"), valign = "center", part = "body") %>%
  align(j = "Status", align = "center", part = "all") %>%
  align(j = "Gebied (jaartal vroegste waarneming)", align = "left", part = "all") %>%
  
  # D. Kolombreedtes instellen 
  width(j = "Soort", width = 0.8, unit = "cm") %>%
  width(j = "Doelsoort", width = 3.0, unit = "cm") %>%
  width(j = "Status", width = 2.5, unit = "cm") %>%
  width(j = "Gebied (jaartal vroegste waarneming)", width = 10.7, unit = "cm") %>%
  
  # E. Randen
  border_remove() %>%
  border_outer(part = "all", border = fp_border(color = "black", width = 1.5)) %>%
  border_inner_h(part = "header", border = fp_border(color = "black", width = 1.5)) %>%
  border_inner_v(part = "all", border = fp_border(color = "gray80", width = 0.5)) %>%
  border_inner_h(part = "body", border = fp_border(color = "gray", width = 0.5)) %>%
  fix_border_issues()


# --- 6. Opslaan naar word ---
outfile_doc <- file.path(dir_bescherming_output, "tabel_afstanden_sbp_compact_A4.docx")

doc <- read_docx() %>%
  body_add_fpar(fpar(ftext("Tabel: Overzicht Kreeften & SBP Doelsoorten", prop = fp_text(bold = TRUE, font.size = 12)))) %>%
  body_add_flextable(ft) %>%
  body_end_section_continuous() 

print(doc, target = outfile_doc)

message(paste0("Klaar! De compacte tabel is opgeslagen in:\n", outfile_doc))