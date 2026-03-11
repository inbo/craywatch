# ====================================================
# Scriptnaam: 13_urbanisation_plot.R
# Project: Craywatch
# Datum: 01-12-2025
# Beschrijving: 
# - Koppelt waarnemingen aan verstedelijkingsgraad
# - Plot frequentie per type verstedelijking 
# - Plot soorten per type 
# - Plot vangstsucces per type verstedelijking 
# ====================================================

# --- 0. Instellingen laden ---
source("./src/analysis/config.R")

# --- 1. Data Inlezen ---

# A. laad analyse dataset
if (!file.exists(file_analyse_dataset_rapport)) stop("Run eerst script 03!")

df_analyse <- read_csv(file_analyse_dataset_rapport, show_col_types = FALSE)

# B. Verstedelijking shapefile
file_urban_map <- file.path(dir_shapefiles, "verstedelijking.gpkg") 
if (!file.exists(file_urban_map)) stop(paste("Bestand niet gevonden:", file_urban_map))

layers <- st_layers(file_urban_map)
urban_sf <- st_read(file_urban_map, layer = layers$name[1], quiet = TRUE) %>%
  st_transform(crs_lambert) %>%
  select(geom, type_urban = type)

# --- 2. Ruimtelijke koppeling ---
cray_sf <- df_analyse %>%
  filter(!is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = crs_wgs84) %>%
  st_transform(crs_lambert)

# spatial join met statistische sectoren
cray_urban <- st_join(cray_sf, urban_sf, join = st_intersects) %>%
  filter(!is.na(type_urban)) %>% 
  st_drop_geometry()

print("Aantal punten per omgevingstype:")
print(table(cray_urban$type_urban))

# --- 3. Data voorbereiding ---

cray_urban$type_urban <- factor(
  cray_urban$type_urban, 
  levels = urban_levels
)

# Omzetten naar long format
cray_long <- cray_urban %>%
  pivot_longer(
    cols = all_of(tolower(gbif_species)), 
    names_to = "species",
    values_to = "presence"
  ) %>%
  filter(!is.na(presence))

# ==============================================================================
# 4. VISUALISATIES (GEOPTIMALISEERD VOOR RAPPORTAGE)
# ==============================================================================

# Instellingen voor tekstgroottes (uniform met andere scripts)
font_base <- 30
font_axis_labels <- 26
font_axis_titles <- 30
font_legend <- 28
font_geom_text <- 11

# ------------------------------------------------------------------------------
# Plot 1: Frequentie per type verstedelijking
# ------------------------------------------------------------------------------

data_distributie <- cray_long %>%
  filter(presence == 1) %>% 
  group_by(species, type_urban) %>%
  summarise(n = n(), .groups = "drop") %>% 
  complete(species, type_urban, fill = list(n = 0)) %>%
  group_by(species) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(dutch_name = factor(species_labels_dutch[species], levels = species_labels_dutch))

p_distributie <- ggplot(data_distributie, aes(x = dutch_name, y = percentage, fill = type_urban)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7, color = "black", linewidth = 0.6) +
  geom_text(
    aes(label = ifelse(n > 0, n, "")), 
    position = position_stack(vjust = 0.5), 
    size = font_geom_text, color = "white", fontface = "bold"
  ) +
  scale_fill_manual(values = urban_colors) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  scale_y_continuous(limits = c(0, 101), expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "Relatieve frequentie van de waarnemingen",
    subtitle = "per type verstedelijking en soort",
    y = "Percentage van de waarnemingen (%)",
    x = "", fill = ""
  ) +
  theme_minimal(base_size = font_base) + 
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = font_legend),
    legend.key.size = unit(1.8, "cm"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, lineheight = 0.8, size = font_axis_labels),
    axis.text.y = element_text(size = font_axis_labels),
    axis.title.y = element_text(size = font_axis_titles, margin = margin(r = 20)),
    plot.title = element_text(size = 40, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 30, margin = margin(b = 30)),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(40, 40, 40, 40)
  )

ggsave(file.path(dir_urbanisation_output, "plot_urban_distributie.png"), p_distributie, width = 20, height = 14, bg = "white", dpi = 300)

# ------------------------------------------------------------------------------
# Plot 2: Soorten per type verstedelijking
# ------------------------------------------------------------------------------

data_community <- cray_long %>%
  filter(presence == 1) %>%
  group_by(type_urban, species) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(dutch_name = factor(species_labels_dutch[species], levels = species_labels_dutch))

p_community <- ggplot(data_community, aes(x = type_urban, y = percentage, fill = species)) + 
  geom_bar(stat = "identity", position = "stack", width = 0.7, color = "black", linewidth = 0.6) + 
  scale_fill_manual(values = species_colors, labels = species_labels_dutch) +      
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "Soortensamenstelling per type verstedelijking",
    subtitle = "Aandeel van de verschillende soorten binnen de omgeving",
    y = "Aandeel in de gemeenschap (%)",
    x = "", fill = ""
  ) +
  theme_minimal(base_size = font_base) + 
  theme(
    legend.position = "right",
    legend.text = element_text(size = 22),
    axis.text.x = element_text(size = font_axis_labels, face = "bold"),
    axis.text.y = element_text(size = font_axis_labels),
    axis.title.y = element_text(size = font_axis_titles, margin = margin(r = 20)),
    plot.title = element_text(size = 40, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 30, margin = margin(b = 30)),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(40, 40, 40, 40)
  )

ggsave(file.path(dir_urbanisation_output, "plot_urban_community.png"), p_community, width = 22, height = 14, bg = "white", dpi = 300)


# ------------------------------------------------------------------------------
# Plot 3: Vangstsucces per type verstedelijking
# ------------------------------------------------------------------------------

data_vangstsucces <- cray_long %>%
  filter(dat.source == "craywatch_data") %>% 
  filter(!is.na(presence)) %>%
  group_by(species, type_urban) %>%
  summarise(
    n_traps = n(),
    percentage = mean(presence) * 100,
    .groups = "drop"
  ) %>%
  group_by(species) %>%
  filter(sum(percentage) > 0) %>% 
  ungroup() %>%
  complete(species, type_urban, fill = list(percentage = 0, n_traps = 0)) %>%
  mutate(dutch_name = factor(species_labels_dutch[species], levels = species_labels_dutch))

max_y <- max(data_vangstsucces$percentage, na.rm = TRUE)

p_vangstsucces <- ggplot(data_vangstsucces, aes(x = dutch_name, y = percentage, fill = type_urban)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, color = "black", linewidth = 0.6) +
  geom_text(
    aes(label = ifelse(percentage > 0, paste0(round(percentage, 1), "%"), "")), 
    position = position_dodge(width = 0.8), 
    vjust = -0.7, 
    size = 9,
    fontface = "bold"
  ) +
  scale_fill_manual(values = urban_colors) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(limits = c(0, max_y * 1.3), expand = expansion(mult = c(0, 0))) +
  labs(
    title = "Vangstkans per type verstedelijking",
    subtitle = "op basis van de gestandaardiseerde monitoring (Craywatch)",
    y = "Vangstkans (%)",
    x = "", fill = ""
  ) +
  theme_minimal(base_size = font_base) + 
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = font_legend),
    legend.key.size = unit(1.8, "cm"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, lineheight = 0.8, size = font_axis_labels),
    axis.text.y = element_text(size = font_axis_labels),
    axis.title.y = element_text(size = font_axis_titles, margin = margin(r = 20)),
    plot.title = element_text(size = 40, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 30, margin = margin(b = 30)),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(40, 40, 40, 40)
  )

ggsave(file.path(dir_urbanisation_output, "plot_urban_vangstsucces.png"), p_vangstsucces, width = 20, height = 14, bg = "white", dpi = 300)