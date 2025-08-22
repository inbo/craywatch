############################################################
# Script:    Voorkeur_lentisch_lotisch.R
# Doel:      Nagaan of de verschillende soorten invasieve rivierkreeften een habitatvoorkeur hebben 
#             voor stromende versus niet-stromende waterlichamen in Vlaanderen op basis van Craywatch waarnemingen
# Auteur:    Margot Vermeylen
# Datum:     21-08-2025
############################################################

library(sf)
library(ggplot2)
library(dplyr)
library(rgbif)
library(readr)

# Lees shapefiles
stilstaand_water <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/watervlakken.shp")
stromend_water <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/Wtz.shp")

stilstaand_water <- st_transform(stilstaand_water, 31370)
stromend_water <- st_transform(stromend_water, 31370)

# Lagen samenvoegen
stilstaand_water$type <- "stilstaand"
stromend_water$type <- "stromend"

stilstaand_water <- stilstaand_water[, c("geometry", "type")]
stromend_water <- stromend_water[, c("geometry", "type")]

water <- rbind(stilstaand_water, stromend_water)

# Upload craywatch data
craywatch_2024 <- read.csv("~/GitHub/craywatch/R/data/output/final_craywatch_data_2024.csv", header = TRUE)
craywatch_2024 <- st_as_sf(craywatch_2024, coords = c("Longitude", "Latitude"), crs = 4326)
craywatch_2024 <- st_transform(craywatch_2024, 31370)

# Vind de index van het dichtstbijzijnde waterlichaam voor elk punt
nearest_water_index <- st_nearest_feature(craywatch_2024, water)

# Bereken de afstand tot de dichtstbijzijnde waterlichaam
distance_to_water <- st_distance(craywatch_2024, water[nearest_water_index,], by_element = TRUE)

# Voeg de afstanden en de types van de dichtstbijzijnde waterlichamen toe
craywatch_2024$type <- water$type[nearest_water_index]
craywatch_2024$distance <- distance_to_water

# Filter de punten die binnen 20 meter van hun dichtstbijzijnde waterlichaam liggen
craywatch_in_water <- craywatch_2024 %>%
  filter(distance <= units::set_units(20, "m"))

# Filter de punten die verder dan 20 meter van hun dichtstbijzijnde waterlichaam liggen
craywatch_outside_water <- craywatch_2024 %>%
  filter(distance > units::set_units(20, "m"))

# Toon de LocID's van de punten buiten de 100m-grens
print(craywatch_outside_water$locID)

# Handmatig ontbrekende locIDs aan een type toekennen
stromend_locids <- c("I_1653_1", "V_3000_4", "V_3290_5", "V_8300_17", "V_8340_20", "V_8340_23", "V_8340_24")
stilstaand_locids <- c("V_3010_11", "V_3010_14")

craywatch_2024 <- craywatch_2024 %>%
  mutate(type = case_when(
    # Als de LocID in de "stromend"-lijst staat, wijs "stromend" toe.
    locID %in% stromend_locids ~ "stromend",
    # Als de LocID in de "stilstaand"-lijst staat, wijs "stilstaand" toe.
    locID %in% stilstaand_locids ~ "stilstaand",
    # Behoud voor alle andere LocID's de bestaande waarde.
    TRUE ~ type
  ))

# Hoeveel van elk type water bemonsterd
type_bemonsterd <- craywatch_2024 %>%
  group_by(type) %>%
  count()
print(type_bemonsterd)

# Tel het aantal punten per species en per type water
species_counts <- craywatch_2024 %>%
  group_by(species, type) %>%
  count()

# Toon het resultaat
print(species_counts)

#########################Nagaan voor volledige gbif data#############################################

# Lijst van soorten
species <- c("Procambarus clarkii",
             "Procambarus virginalis",
             "Procambarus acutus",
             "Faxonius limosus",
             "Pacifastacus leniusculus",
             "Faxonius virilis",
             "Faxonius immunis",
             "Faxonius juvenilis",
             "Faxonius rusticus",
             "Pontastacus leptodactylus")

# GBIF authenticatiegegevens
gbif_user <- "margotvermeylen"
gbif_pwd <- "Pezootje1"
gbif_email <- "margotvermeylen1@gmail.com"

# Zoek de taxonkeys op
taxonkeys <- species %>%
  name_backbone_checklist() %>%
  filter(!matchType == "NONE") %>%
  pull(usageKey)

# Start de download met de opgegeven criteria
# Let op: de download vindt plaats op de GBIF servers en kan enige tijd duren
occ_download_request <- occ_download(
  pred_in("taxonKey", taxonkeys),
  pred_in("country", c("BE")),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  pred_gte("year", 2000),
  user = gbif_user,
  pwd = gbif_pwd,
  email = gbif_email
)

repeat {
  Sys.sleep(10) # Wacht 10 seconden
  download_status <- occ_download_meta(occ_download_request)
  if (download_status$status == "SUCCEEDED") {
    print("Download voltooid. Nu worden de gegevens verwerkt...")
    break
  }
  print(paste("Download status:", download_status$status))
}

# Download het bestand en lees het direct in R
occ_data <- occ_download_get(key = occ_download_request, overwrite = TRUE) %>%
  occ_download_import()

# Selecteer de kolommen 'species' en 'decimalLatitude', 'decimalLongitude'
# En filter op 'occurrenceStatus' == "PRESENT" om alleen aanwezige waarnemingen te behouden
selected_data <- occ_data %>%
  filter(occurrenceStatus == "PRESENT") %>%
  select(species, decimalLatitude, decimalLongitude)

# Schrijf de geselecteerde data naar een CSV-bestand
write_csv(selected_data, "crayfish_occurrences.csv")

print("De data is succesvol opgeslagen in crayfish_occurrences.csv")

# Voer dezelfde analyse uit
gbif_data <- read.csv("crayfish_occurrences.csv")
gbif_data <- st_as_sf(gbif_data, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
gbif_data <- st_transform(gbif_data, 31370)

# Vind de index van het dichtstbijzijnde waterlichaam voor elk punt
nearest_water_index_gbif <- st_nearest_feature(gbif_data, water)

# Bereken de afstand tot de dichtstbijzijnde waterlichaam
distance_to_water_gbif <- st_distance(gbif_data, water[nearest_water_index_gbif,], by_element = TRUE)

# Voeg de afstanden en de types van de dichtstbijzijnde waterlichamen toe
gbif_data$type <- water$type[nearest_water_index_gbif]
gbif_data$distance <- distance_to_water_gbif

# Filter de punten die binnen 100 meter van hun dichtstbijzijnde waterlichaam liggen
crayfish_in_water <- gbif_data %>%
  filter(distance <= units::set_units(100, "m"))

# Tel het aantal punten per species en per type water
species_counts_gbif <- crayfish_in_water %>%
  group_by(species, type) %>%
  count()

# Toon het resultaat
print(species_counts)

