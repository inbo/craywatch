# ====================================================
# Scriptnaam: 14_watertype_plot_glmm.R
# Project: Craywatch
# Datum: 01-12-2025
# Beschrijving: 
# - Classificeert waarnemingen als gesloten of open
# - Plot frequentie per watertype  
# - Plot soorten per watertype 
# - Plot vangstsucces per watertype  
# - Statistiek: GLMM
# ====================================================

# --- 0. Instellingen laden ---
source("./src/analysis/config.R")

# --- 1. Data Inlezen ---
if (!file.exists(file_analyse_dataset_rapport)) stop("Run eerst script 03!")

df_analyse <- read_csv(file_analyse_dataset_rapport, show_col_types = FALSE)

# 2. Check op Ruimtelijke Koppeling (Script 04)
if (!"valid_link" %in% names(df_analyse)) {
  stop(
    "\n\n======================================================================",
    "\nFOUT: De dataset mist de ruimtelijke koppeling (VHAG/WVLC).",
    "\n\nDe kolommen 'valid_link' en 'link_method' ontbreken.",
    "\n-> Voer eerst script '04_spatial_link_core.R' uit om dit toe te voegen.",
    "\n======================================================================\n"
  )
} else {
  message("Ruimtelijke koppeling geverifieerd. Analyse start...")
}

# --- 2. Data voorbereiden ---

cray_water <- df_analyse %>%
  mutate(
    # logica: WVLC (gesloten), anders (open)
    water_type = if_else(!is.na(WVLC), "gesloten", "open"),
    water_type = factor(water_type, levels = water_levels)
  )

print("Aantal waarnemingen per watertype:")
print(table(cray_water$water_type))

# Omzetten naar long format
cray_long <- cray_water %>%
  pivot_longer(
    cols = all_of(tolower(gbif_species)), 
    names_to = "species",
    values_to = "presence"
  ) %>%
  filter(!is.na(presence)) 

# ==============================================================================
# 3. VISUALISATIES
# ==============================================================================

# Instellingen voor tekstgroottes (gebaseerd op gridcell_plot stijl)
font_base <- 30
font_axis_labels <- 26
font_axis_titles <- 30
font_legend <- 28
font_geom_text <- 11

# ------------------------------------------------------------------------------
# Plot frequentie per watertype
# ------------------------------------------------------------------------------

data_distributie <- cray_long %>%
  filter(presence == 1) %>% 
  group_by(species, water_type) %>%
  summarise(n = n(), .groups = "drop") %>% 
  complete(species, water_type, fill = list(n = 0)) %>%
  group_by(species) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(dutch_name = factor(species_labels_dutch[species], levels = species_labels_dutch))

p_distributie <- ggplot(data_distributie, aes(x = dutch_name, y = percentage, fill = water_type)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7, color = "black", linewidth = 0.6) +
  geom_text(
    aes(label = ifelse(n > 0, n, "")), 
    position = position_stack(vjust = 0.5), 
    size = font_geom_text, color = "white", fontface = "bold"
  ) +
  scale_fill_manual(values = water_colors) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  scale_y_continuous(limits = c(0, 101), expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "Relatieve frequentie van de waarnemingen",
    subtitle = "per type systeem en soort",
    y = "Percentage van de populatie (%)",
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

print(p_distributie)
ggsave(file_plot_water_distributie, p_distributie, width = 20, height = 14, bg = "white", dpi = 300)


# ------------------------------------------------------------------------------
# Plot soorten per type
# ------------------------------------------------------------------------------

data_community <- cray_long %>%
  filter(presence == 1) %>%
  group_by(water_type, species) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(species = factor(species, levels = names(species_labels_dutch)))

p_community <- ggplot(data_community, aes(x = water_type, y = percentage, fill = species)) + 
  geom_bar(stat = "identity", position = "