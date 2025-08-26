# ====================================================
# Scriptnaam:   Process_observation_data.R
# Auteur:       Frédérique Steen
# Datum:        01-07-2024
#
# Beschrijving:
# Dit script combineert gevalideerde en te controleren observatiedata, koppelt 
# deze aan de locaties, en construeert een uitgebreide dataset met:
# - Aantallen per soort per locatie
# - Het totaal aantal vangstdagen
# - Een controle of de vangstdagen opeenvolgend zijn
# - Een aanduiding of de data als 'clean' beschouwd kan worden (volledige 4 opeenvolgende vangstdagen)
# De verrijkte dataset wordt opgeslagen als CSV voor verdere analyses.
#
# Input:
# - ./data/observations/output/cleandata.csv
# - ./data/observations/output/datacheck.csv
# - ../assets/localities.csv
#
# Output:
# - ./data/observations/output/first_data.csv
#
# Benodigde packages:
# dplyr, lubridate
# ====================================================


library(dplyr)


### LOAD DATA ##################################################################
cleandata <- read.csv("./data/observations/output/cleandata.csv")
datacheck <- read.csv("./data/observations/output/datacheck.csv")

alldata <- rbind(cleandata,datacheck)

localities <- read.csv("../assets/localities.csv")


### CONSTRUCT NEW DATAFRAME ####################################################
# Select the specified columns from the localities dataframe for sampled locID
localities_data <- localities %>%
  filter(locID %in% alldata$locID) %>%
  select(NAAM, OMSCHR, vrijwillID, locID, SystemType, provincie, gemeente, CATC, Latitude, Longitude)

# Add column names according to found species
column_names <- unique(alldata$soort)

for (col_name in column_names) {
  localities_data[[col_name]] <- NA  # Initialize the new column with NA
}


### VOEG AANTALLEN PER SOORT TOE ###############################################

#Voeg de aantallen gevangen kreeften toe per soort
aggregated_data <- alldata %>%
  group_by(locID, soort) %>%
  summarise(total_individuals = sum(number.of.individuals)) %>%
  ungroup()

# Loop door elke soort om de totalen in de juiste kolommen van localities_data te plaatsen
for (species in unique(aggregated_data$soort)) {
  # Filter de geaggregeerde data voor de huidige soort
  species_data <- aggregated_data %>%
    filter(soort == species) %>%
    select(locID, total_individuals)
  
  # Voeg de data toe aan de juiste kolom in localities_data
  localities_data <- localities_data %>%
    left_join(species_data, by = "locID") %>%
    mutate(!!species := coalesce(total_individuals, get(species))) %>%
    select(-total_individuals) # Verwijder tijdelijke kolom
}


### VOEG HET TOTAAL AANTAL VANGSTDAGEN VOOR DE LOCID TOE #######################

vangstdagen_data <- alldata %>%
  group_by(locID) %>%
  summarise(vangstdagen = n_distinct(date)) %>%
  ungroup()

# Samenvoegen met localities_data
localities_data <- localities_data %>%
  left_join(vangstdagen_data, by = "locID")

print(localities_data)

### ZIJN DE DAGEN OPEENVOLGEND? ################################################

# Stap 1: Controle op opeenvolgende dagen
consecutive_data <- alldata %>%
  arrange(locID, date) %>%  # Sorteren op locID en datum
  group_by(locID) %>%
  summarise(consecutive = all(diff(as.Date(date)) == 1)) %>%
  ungroup()

# Stap 2: Samenvoegen met localities_data
localities_data <- localities_data %>%
  left_join(consecutive_data, by = "locID")


### ZIJN DE DATA CLEAN ? 4 VANGSTDAGEN + OPEENVOLGEND ##########################
# Stap 1: Toevoegen van de clean-status kolom
localities_data <- localities_data %>%
  mutate(clean_data = locID %in% cleandata$locID)

# Bekijk de bijgewerkte tabel
print(localities_data)


# Find locID values that are in combined_locID but not in localities$locID
erratic_locID <- setdiff(alldata$locID, localities$locID)

write.csv(localities_data, "./data/observations/output/first_data.csv", row.names = FALSE)

