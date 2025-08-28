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
craywatch_data <- read.csv("~/GitHub/craywatch/R/data/observations/data_validation/craywatch_validation.csv")
map_data <- read.csv("~/GitHub/craywatch/assets/localities.csv")

# Zorg dat de data kolom in het juiste formaat staat
craywatch_data$date <- dmy(craywatch_data$date) # Convert to Date using dmy format

# groepeer per locID and verwerk data
grouped_craywatch_data <- craywatch_data %>%
  group_by(locID) %>%
  summarize(
    individuals = sum(number.of.individuals, na.rm = TRUE), 
    days_sampled = n(), 
    start_date = min(date, na.rm = TRUE), # schrijf de eerste datum
    end_date = max(date, na.rm = TRUE), # schrijf de laatste datum
    species = list(
      if (sum(number.of.individuals, na.rm = TRUE) > 0) unique(soort[number.of.individuals > 0]) else "absent"
    ), # Zet 'absent' als er geen kreeften gevangen zijn
    consecutive = {
      # kijk of data aaneengesloten zijn per locID
      date_diff <- diff(sort(unique(date)))
      all(date_diff == 1) # aaneengesloten als verschil tussen alle datums 1 is
    },
    vrijwillID = first(vrijwillID)  # Neem de eerste vrijwilligersID per locID
  ) %>%
  unnest(cols = c(species)) # verdeel elke soort in een kolom


# Selecteer de kolommen 'locID', 'Latitude', and 'Longitude' van localities
localities_selected <- map_data %>%
  dplyr::select(locID, Latitude, Longitude)

# voeg data samen met localities om Latitude en Longitude toe te voegen obv locID
grouped_craywatch_data <- left_join(grouped_craywatch_data, localities_selected, by = "locID")

# bekijk data
head(grouped_craywatch_data) 
str(grouped_craywatch_data)

# Nagaan hoeveel mensen exact het protocol volgden
protocol_followers <- grouped_craywatch_data %>%
  filter(days_sampled == 4 & consecutive == "TRUE") # locaties waar er exact 4 na elkaar dagen gecontroleerd is

n_distinct(protocol_followers$vrijwillID)

# Filter the data die we willen gebruiken
craywatch_data_usable <- grouped_craywatch_data %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  filter(!(species == "absent" & days_sampled < 4))  # Exclude rows where species is "absent" and number_of_days < 4

# Sla finale Craywatch data op als een CSV-bestand
write.csv(craywatch_data_usable, "~/GitHub/craywatch/R/data/output/final_craywatch_data_2024.csv", row.names = FALSE)

# Maak GIS-laag
craywatch_sf <- st_as_sf(craywatch_data_usable, coords = c("Longitude", "Latitude"), crs = 4326)
st_write(craywatch_sf, "~/GitHub/craywatch/R/data/output/GIS-laag/craywatch_results_2024.shp", append=FALSE)
st_write(craywatch_sf, "~/GitHub/craywatch/R/data/output/GIS-laag/craywatch_results_2024.gpkg", layer = "craywatch_observations", append=FALSE)


