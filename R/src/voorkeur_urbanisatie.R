############################################################
# Script:    Voorkeur_urbanisatie.R
# Doel:      Nagaan of de verschillende soorten invasieve rivierkreeften voorkomen in meer stedelijke of natuurlijkere omgevingen
# Auteur:    Margot Vermeylen
# Datum:     21-08-2025
############################################################

library(sf)
library(dplyr)

# Data en lagen inlezen
verstedelijking <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/verstedelijking.gpkg")
verstedelijking <- st_transform(verstedelijking, 31370)
verstedelijking <- verstedelijking[, c("geom", "type")]

craywatch_2024 <- read.csv("~/GitHub/craywatch/R/data/output/final_craywatch_data_2024.csv", header = TRUE)
craywatch_2024 <- st_as_sf(craywatch_2024, coords = c("Longitude", "Latitude"), crs = 4326)
craywatch_2024 <- st_transform(craywatch_2024, 31370)

# maak een intersect tussen de craywatch data en verstedelijking
craywatch_verstedelijking <- st_join(craywatch_2024, verstedelijking, join = st_intersects)

# Tel het aantal punten per species en per type verstedelijking
species_counts <- craywatch_verstedelijking %>%
  group_by(species, type) %>%
  count()

# Toon het resultaat
print(species_counts)

# Creëer een binaire kolom 'aanwezigheid'
# Deze kolom is 1 als de soort aanwezig is, en 0 als deze afwezig is
craywatch_verstedelijking <- craywatch_verstedelijking %>%
  mutate(aanwezigheid = if_else(species == "absent", 0, 1))

# Controleer of het gelukt is
table(craywatch_verstedelijking$aanwezigheid)

unieke_soorten <- unique(craywatch_verstedelijking$species[craywatch_verstedelijking$species != "absent"])

# 2. Voer de analyse uit in een loop voor elke soort
for (soort in unieke_soorten) {
  
  # Creëer een tijdelijke binaire kolom voor deze specifieke soort
  temp_data <- craywatch_verstedelijking %>%
    mutate(aanwezigheid_soort = if_else(tolower(trimws(species)) == tolower(trimws(soort)), 1, 0))
  
  # Voer de logistische regressie uit
  model <- glm(aanwezigheid_soort ~ type, data = temp_data, family = "binomial")
  
  # Druk de resultaten af
  cat("--------------------------------------------------\n")
  cat("Resultaten voor soort:", soort, "\n")
  print(summary(model))
  
  # Druk de odds ratio's af voor een directe interpretatie
  cat("\nOdds Ratio's:\n")
  print(exp(coef(model)))
  cat("\n")
}