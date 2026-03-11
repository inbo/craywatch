# ====================================================
# Scriptnaam: 19_gridcell_trend_plot.R
# Beschrijving: 
# - Expansie-analyse (toename bezette km-hokken)
# - Filtert data < 2010 weg (start_pre)
# - Filtert soorten zonder historie of expansie
# - Sorteert op totaal aantal (meeste links)
# - FIX: Robuuste arcering
# Project: Craywatch
# ====================================================

source("./src/analysis/config.R")

# --- 1. Data Inlezen & Filteren ---
if (!file.exists(file_analyse_dataset_rapport)) stop("Run eerst script 03!")

# Dataset
occurrences <- read_csv(file_analyse_dataset_rapport, show_col_types = FALSE) %>%
  filter(!is.na(Longitude), !is.na(Latitude)) %>%
  # Filter op watergebonden waarnemingen (toch niet nodig)
  # filter(!is.na(VHAG) | !is.na(WVLC)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = crs_wgs84) %>%
  st_transform(crs_lambert)

# Shapefile Vlaanderen
vlaanderen <- st_read(file_vlaanderen_grenzen, quiet = TRUE) %>% 
  st_transform(crs_lambert)

# --- 2. Grid & Koppeling ---
message("Starten met grid berekening...")

grid <- st_make_grid(vlaanderen, cellsize = 1000, square = TRUE) %>% 
  st_sf() %>% mutate(grid_id = 1:n())

occ_grid <- st_join(occurrences, grid, left = FALSE) %>% 
  st_drop_geometry()

# Datums instellen
occ_grid$date <- as.Date(occ_grid$date)
start_pre     <- as.Date("2010-01-01") 
end_pre       <- as.Date("2024-05-31") 

# --- 3. Berekening ---

# Stap A: Aggregatie
grid_summary <- occ_grid %>%
  filter(date >= start_pre) %>% 
  pivot_longer(cols = all_of(tolower(gbif_species)), names_to = "species", values_to = "present") %>%
  filter(present == 1) %>%
  group_by(species, grid_id) %>%
  summarise(
    is_pre = any(date <= end_pre),                
    is_cw  = any(dat.source == "craywatch_data"), 
    .groups = "drop"
  )

# Stap B: Classificeren
df_trends <- grid_summary %>%
  mutate(Categorie = case_when(
    is_pre ~ "n_basis",                           
    is_cw  ~ "n_cw_winst",                        
    TRUE   ~ "n_rest_winst"                       
  )) %>%
  count(species, Categorie, name = "Aantal") %>%
  mutate(dutch_name = species_labels_dutch[species])

# Stap C: Totalen, Filters & Factoren
df_plot_ready <- df_trends %>%
  group_by(dutch_name) %>%
  mutate(
    n_basis = sum(Aantal[Categorie == "n_basis"]),
    total_winst = sum(Aantal[Categorie != "n_basis"]),
    n_totaal = sum(Aantal),
    toename_pct = if_else(n_basis > 0, (total_winst / n_basis), Inf)
  ) %>%
  ungroup() %>%
  
  # Filters
  filter(n_basis > 0) %>%
  filter(total_winst > 0) %>%
  
  mutate(
    # Factors (Volgorde van stapelen: Basis onderaan)
    Component = factor(Categorie, 
                       levels = c("n_cw_winst", "n_rest_winst", "n_basis"),
                       labels = c("Door Craywatch", "Na Craywatch (GBIF)", "Voor Craywatch")),
    
    # Label
    label_pct = paste0("+", round(toename_pct * 100, 0), "%")
  )

if(nrow(df_plot_ready) == 0) stop("Geen data om te plotten na filtering.")

# --- SORTEREN ---
order_sp <- df_plot_ready %>%
  select(dutch_name, n_totaal) %>% 
  distinct() %>%
  arrange(desc(n_totaal)) %>% 
  pull(dutch_name)

df_plot_ready$dutch_name <- factor(df_plot_ready$dutch_name, levels = order_sp)

# --- 4. PLOTTEN (MAXIMALE LEESBAARHEID + GEFIXTE MARGES) ---

p_trend <- ggplot(df_plot_ready, aes(x = dutch_name, y = Aantal)) +
  
  geom_col_pattern(
    aes(fill = Component, pattern = Component),
    pattern_fill = "black", 
    pattern_color = "black", 
    pattern_alpha = 0.5,     
    pattern_density = 0.1,   
    pattern_spacing = 0.05,  
    pattern_angle = 45,
    color = "black",         
    linewidth = 0.6          
  ) +
  
  scale_fill_manual(
    values = c(
      "Voor Craywatch"      = "#6BA1D3",
      "Na Craywatch (GBIF)" = "#33A02C",
      "Door Craywatch"      = "#33A02C"
    )
  ) +
  
  scale_pattern_manual(
    values = c(
      "Door Craywatch"      = "stripe", 
      "Na Craywatch (GBIF)" = "none", 
      "Voor Craywatch"      = "none"
    )
  ) +
  
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  
  # Y-AS FIX: expansion zorgt voor extra ruimte boven de hoogste balk
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  
  geom_text(
    data = unique(df_plot_ready[, c("dutch_name", "n_totaal", "label_pct")]),
    aes(x = dutch_name, y = n_totaal, label = label_pct),
    vjust = -0.7, size = 11, fontface = "bold" # Iets grotere tekst voor percentages
  ) +
  
  labs(
    title = "Aantal bezette kilometerhokken per soort",
    subtitle = "Situatie voor en na de start van het Craywatch project",
    y = "Aantal bezette 1x1 km grid cellen", 
    x = ""
  ) +
  
  theme_minimal(base_size = 30) + 
  theme(
    legend.position = "bottom", 
    legend.title = element_blank(), 
    legend.text = element_text(size = 28),
    legend.key.size = unit(1.8, "cm"), 
    
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, lineheight = 0.8, size = 26),
    axis.text.y = element_text(size = 26),
    axis.title.y = element_text(size = 30, margin = margin(r = 20)),
    
    plot.title = element_text(size = 40, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 30, margin = margin(b = 30)),
    
    panel.grid.major.x = element_blank(),
    plot.margin = margin(40, 40, 40, 40) # Ruime marges rondom de hele plot
  ) +
  
  guides(
    pattern = "none",
    fill = guide_legend(
      override.aes = list(
        pattern = c("stripe", "none", "none"), 
        pattern_density = 0.4, 
        pattern_spacing = 0.02,
        pattern_alpha = 1
      )
    )
  )

# Opslaan op groot formaat
file_plot <- file.path(dir_gridcell_output, "gridcell_plot.png")
ggsave(file_plot, p_trend, width = 20, height = 14, bg = "white", dpi = 300)

