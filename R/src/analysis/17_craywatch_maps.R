# ====================================================
# Scriptnaam: 17_craywatch_maps.R
# Auteur: Frédérique Steen 
# Beschrijving: 
# - Genereert statische kaarten voor Craywatch
# - Volgorde correctie: Aanwezigheden liggen bovenop afwezigheden
# - Kaart 1: Totaal overzicht (craywatch_map)
# - Kaart 2: Focus op waterlopen CATC 0/1 (catc_map)
# Project: Craywatch
# ====================================================

# --- 0. Instellingen laden ---
source("./src/analysis/config.R")

library(ggspatial) 

required_species <- tolower(gbif_species)

# --- 1. Data Inlezen ---
if (!file.exists(file_analyse_dataset_rapport)) stop("Run eerst script 03!")
df_analyse <- read_csv(file_analyse_dataset_rapport, show_col_types = FALSE)
cw_data <- df_analyse %>% filter(dat.source == "craywatch_data")

# --- 2. Shapefiles laden ---
message("Shapefiles laden en voorbereiden...")

# Vlaanderen
vlaanderen <- st_read(file_vlaanderen_grenzen, quiet = TRUE) %>% 
  st_transform(crs_lambert)

# Waterlopen 
vha_raw <- st_read(file_vha_catc, quiet = TRUE) %>% st_transform(crs_lambert)
cat0_lines <- vha_raw %>% filter(CATC == 0) %>% st_intersection(vlaanderen)
cat1_lines <- vha_raw %>% filter(CATC == 1) %>% st_intersection(vlaanderen)

# --- 3. Data Preparatie ---
cw_long <- cw_data %>%
  select(locID, Latitude, Longitude, CATC, any_of(required_species)) %>%
  pivot_longer(cols = any_of(required_species), names_to = "species", values_to = "present")

# A. Aanwezigheden
sf_aanwezig <- cw_long %>% filter(present == 1) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% st_transform(crs_lambert)

# B. Afwezigheden
locs_met_vangst <- unique(sf_aanwezig$locID)
sf_afwezig <- cw_data %>% filter(!locID %in% locs_met_vangst) %>%
  mutate(species = "Afwezigheid") %>%
  select(locID, species, CATC, Latitude, Longitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% st_transform(crs_lambert)

# Styling parameters
point_size_abs  <- 1.0
point_size_pres <- 1.0 

# --- Overall craywatch map ---
message("Genereren craywatch_map...")

p_totaal <- get_baseplot() +
  # eerst afwezigheden
  geom_sf(data = sf_afwezig, aes(color = species), size = point_size_abs) +
  # dan aanwezigheden
  geom_sf(data = sf_aanwezig, aes(color = species), size = point_size_pres) +
  
  scale_color_manual(values = species_colors, labels = species_labels_dutch) +
  guides(color = guide_legend(override.aes = list(size = 3), ncol = 2))

ggsave(file.path(dir_maps_craywatch, "craywatch_map.png"), p_totaal, width = 15, height = 8, units = "cm", dpi = 400)
message("Klaar! Kaarten opgeslagen in: ", dir_maps_craywatch)

# --- Map navigatable and cat1 non navigatable watercourses ---
message("Genereren catc_map...")

# Kleuren specifiek voor deze kaart
col_cat0_detail <- "#004C99" # Donkerblauw 
col_cat1_detail <- "#99CCFF" # Lichtblauw

# Filter data op CATC 0 en 1
sf_afwezig_cat <- sf_afwezig %>% filter(CATC %in% c(0, 1))
sf_aanwezig_cat <- sf_aanwezig %>% filter(CATC %in% c(0, 1))

p_catc <- ggplot() +
  # Achtergrond
  geom_sf(data = vlaanderen, fill = "#EEEEEE", size = 0.2, colour = "black") +

  # Specifieke waterlopen
  geom_sf(data = cat1_lines, size = 0.4, colour = col_cat1_detail) + 
  geom_sf(data = cat0_lines, size = 0.3, colour = col_cat0_detail) + 
  
  # Punten: eerst afwezigheid, dan aanwezigheid
  geom_sf(data = sf_afwezig_cat, aes(color = species), size = point_size_abs, alpha = 0.8) +
  geom_sf(data = sf_aanwezig_cat, aes(color = species), size = point_size_pres) +
  
  scale_color_manual(values = species_colors, labels = species_labels_dutch) +
  guides(color = guide_legend(override.aes = list(size = 3), ncol = 2)) +
  theme_void() +
  theme(legend.title = element_blank(), 
        legend.text = element_text(size = 8, face = "italic"), 
        legend.key.size = unit(0.4, "cm"), 
        legend.position = "bottom",
        plot.title = element_text(face = "italic", hjust = 0.5))

ggsave(file.path(dir_maps_catc, "catc_map.png"), p_catc, width = 15, height = 8, units = "cm", dpi = 400)

message("Klaar! Kaarten opgeslagen in: ", dir_maps_catc)