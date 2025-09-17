# ====================================================
# Scriptnaam: 04_analyze_validated_data.R
# Auteur: Margot Vermeylen
# Datum: 09-01-2025 (update 28-08-2025)
# Beschrijving: 
# Dit script haalt de bruikbare Craywatch data uit de gevalideerde datas met waarnemingen
# en creërt de finale dataset
# ====================================================

# Laad libraries
library(ggspatial)
library(sf)
library(tidyverse)
library(dplyr)
library(scales)
library(osmdata)
library(ggplot2)
library(tidyr)
library(lubridate)

# Lees data
craywatch_data <- read.csv("~/GitHub/craywatch/R/data/observations/data_validation/all_validated_data.csv")
map_data <- read.csv("~/GitHub/craywatch/assets/localities.csv")

# Zorg dat de data kolom in het juiste formaat staat
craywatch_data$date <- dmy(craywatch_data$date) # Convert to Date using dmy format

# 1: Creeer een unieke sessie ID per locatie
craywatch_data <- craywatch_data %>%
  arrange(locID, date) %>%
  group_by(locID) %>%
  mutate(
    date_diff = c(0, diff(date)),
    session_id = cumsum(date_diff > 7)
  ) %>%
  ungroup()

# 2: som van vallen en individuen per dag en per locatie per soort en per sessie
daily_data <- craywatch_data %>%
  group_by(locID, session_id, date, soort) %>%
  summarize(
    individuals_daily = sum(number.of.individuals, na.rm = TRUE),
    traps_daily = sum(number.of.traps, na.rm = TRUE),
    vrijwillID = first(vrijwillID),
    .groups = 'drop'
  )
         
# 3: som van de dagelijkse data per locatie en per soort
grouped_craywatch_data <- daily_data %>%
  group_by(locID, session_id, soort) %>%  # Groepeer per locatie, sessie EN soort
  summarize(
    individuals.caught = sum(individuals_daily, na.rm = TRUE), 
    days_sampled = n_distinct(date), 
    start_date = min(date, na.rm = TRUE),
    end_date = max(date, na.rm = TRUE),
    consecutive = {
      date_diff <- diff(sort(unique(date)))
      all(date_diff == 1)
    },
    traps_used = sum(traps_daily, na.rm = TRUE),
    CPUE = ifelse(traps_used > 0, individuals.caught / traps_used, 0),
    .groups = 'drop',
    vrijwillID = first(vrijwillID),
  ) %>%
  select(-session_id) # Verberg de session_id kolom

# Selecteer de kolommen 'locID', 'Latitude', and 'Longitude' van localities
localities_selected <- map_data %>%
  dplyr::select(locID, Latitude, Longitude)

# voeg data samen met localities om Latitude en Longitude toe te voegen obv locID
grouped_craywatch_data <- left_join(grouped_craywatch_data, localities_selected, by = "locID")

########## vanaf hier aanpassen ###########
# Nagaan hoeveel mensen exact het protocol volgden
protocol_followers <- grouped_craywatch_data %>%
  filter(days_sampled == 4 & consecutive == "TRUE") # locaties waar er exact 4 na elkaar dagen gecontroleerd is

n_distinct(protocol_followers$vrijwillID)

# Filter the data die we willen gebruiken
craywatch_data_usable <- grouped_craywatch_data %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  mutate(
    Longitude = as.numeric(Longitude),
    Latitude = as.numeric(Latitude)) %>%
  filter(!(species == "absent" & days_sampled < 4))  # Exclude rows where species is "absent" and number_of_days < 4

# Sla finale Craywatch data op als een CSV-bestand
write.csv(craywatch_data_usable, "~/GitHub/craywatch/R/data/output/final_craywatch_data_all.csv", row.names = FALSE)

# Maak GIS-laag
craywatch_sf <- st_as_sf(craywatch_data_usable, coords = c("Longitude", "Latitude"), crs = 4326)
st_write(craywatch_sf, "~/GitHub/craywatch/R/data/output/GIS-laag/craywatch_results.shp", append=FALSE)
st_write(craywatch_sf, "~/GitHub/craywatch/R/data/output/GIS-laag/craywatch_results.gpkg", layer = "craywatch_observations", append=FALSE)


