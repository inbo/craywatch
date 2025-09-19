# ====================================================
# Scriptnaam: 04_analyze_validated_data.R
# Auteur: Margot Vermeylen
# Datum: 09-01-2025 (update 18-09-2025)
# Beschrijving: 
# Dit script maakt een dataset voor analyse waarin de gefilterde craywatch data en gbif data sinds 2000 gecombineerd worden.
# ====================================================

# Laad libraries
library(ggspatial)
library(sf)
library(dplyr)
library(scales)
library(osmdata)
library(tidyr)
library(lubridate)
library(rgbif)
library(readr)
library(glue)

# ############### 1. Haal gbif data ###################
# species <- c("Procambarus clarkii",
#              "Procambarus virginalis",
#              "Procambarus acutus",
#              "Faxonius limosus",
#              "Pacifastacus leniusculus",
#              "Faxonius virilis",
#              "Faxonius immunis",
#              "Faxonius juvenilis",
#              "Faxonius rusticus",
#              "Pontastacus leptodactylus")
# 
# 
# taxonkeys <-species %>% name_backbone_checklist()  %>% # match to backbone
#   filter(!matchType == "NONE") %>% # get matched names
#   pull(usageKey) # get the gbif taxonkeys
# 
# gbif_user <- Sys.getenv("gbif_user")
# gbif_pwd <- Sys.getenv("gbif_pwd")
# gbif_email <- Sys.getenv("gbif_email")
# 
# set <-occ_download(
#   pred_in("taxonKey", taxonkeys),
#   pred_in("country", c("BE")),
#   pred("hasCoordinate", TRUE),
#   pred("hasGeospatialIssue", FALSE),
#   pred_gte("year", 2000),
#   pred("occurrenceStatus", "PRESENT"),
#   user=gbif_user,pwd=gbif_pwd,email=gbif_email,
#   curlopts=list(http_version=2)
# )
# 
# repeat{
#   Sys.sleep(time = 5*length(taxonkeys))
#   test_set <- occ_download_meta(set)
#   if(test_set$status == "SUCCEEDED"){
#     download_doi <- test_set$doi
#     print(paste("De DOI van je download is:", download_doi))
#     rawdata_set_imported <- occ_download_get(set,
#                                              path = "~/GitHub/Craywatch-Rapport/R/data/intermediate/",
#                                              overwrite = TRUE,
#                                              curlopts=list(http_version=2),
#                                              return = NULL,
#                                              verbatim = NULL) %>% 
#       occ_download_import()
#     break
#   }
#   print(test_set$status)
# }
# 
# unzip(paste0("~/GitHub/Craywatch-Rapport/R/data/intermediate/",test_set$key,".zip"), 
#       exdir= paste0("~/GitHub/Craywatch-Rapport/R/data/intermediate/", test_set$key))
# path=paste0("~/GitHub/Craywatch-Rapport/R/data/intermediate/", test_set$key,"/occurrence.txt")
# 
# CF_occ<-read.delim(path, header=TRUE)
# 
# years<-unique(CF_occ$year)
# species_names<-unique(CF_occ$species)
# 
# occ_gbif <- st_as_sf(CF_occ, coords = c("decimalLongitude", "decimalLatitude"),
#                         crs = "+proj=longlat +datum=WGS84")
# 
# occ_gbif <- occ_gbif %>%
#   mutate(decimalLongitude = sf::st_coordinates(.)[,1],
#          decimalLatitude = sf::st_coordinates(.)[,2]) %>%
#   st_drop_geometry() 
# 
# write_csv(occ_gbif, "~/GitHub/Craywatch-Rapport/R/data/input/gbif/gbif_occ_CF.csv")
# writeLines(download_doi, con = "~/GitHub/Craywatch-Rapport/R/data/input/gbif/gbif_download_doi.txt")


############ 2. Filter de bruikbare craywatch data er uit ###############
# Lees craywatch data, localities file en gbif_occ in 
craywatch_data <- read.csv("~/GitHub/craywatch/R/data/output/craywatch_data.csv")
map_data <- read.csv("~/GitHub/craywatch/assets/localities.csv")
gbif_data <- read.csv("~/GitHub/Craywatch-Rapport/R/data/input/gbif/gbif_occ_CF.csv")

# Creeer een unieke sessie ID per locatie (voor locaties die meer dan 1 keer bemonsterd zijn)
craywatch_data$date <- dmy(craywatch_data$date) # Converteer naar datum
craywatch_data <- craywatch_data %>%
  arrange(locID, date) %>%
  group_by(locID) %>%
  mutate(
    date_diff = c(0, diff(date)),
    session_id = cumsum(date_diff > 7)
  ) %>%
  ungroup()

# som van vallen en individuen per dag, locatie, soort en sessie
daily_data <- craywatch_data %>%
  group_by(locID, session_id, date, soort) %>%
  summarize(
    individuals_daily = sum(number.of.individuals, na.rm = TRUE),
    traps_daily = sum(number.of.traps, na.rm = TRUE),
    vrijwillID = first(vrijwillID),
    .groups = 'drop'
  )
         
# groepeer de data en filter de bruikbare data er uit
grouped_craywatch_data <- daily_data %>%
  group_by(locID, session_id, soort) %>%  # Groepeer per locatie, sessie en soort
  summarize(
    individuals_caught = sum(individuals_daily, na.rm = TRUE),
    days_sampled = n_distinct(date),
    start_date = min(date, na.rm = TRUE),
    end_date = max(date, na.rm = TRUE),
    consecutive = {
      date_diff <- diff(sort(unique(date)))
      all(date_diff == 1)
    },
    traps_used = sum(traps_daily, na.rm = TRUE),
    CPUE = ifelse(traps_used > 0, individuals_caught / traps_used, 0),
    .groups = 'drop',
    vrijwillID = first(vrijwillID),
  ) %>%
  filter((soort == "crayfish indet" & traps_used >= 12) | (soort != "crayfish indet")) %>% # Filter rijen die aan het protocol voldoen
  select(-session_id) # Verberg de session_id kolom

# Selecteer de kolommen 'locID', 'Latitude', and 'Longitude' van localities
localities_selected <- map_data %>%
  dplyr::select(locID, Latitude, Longitude)

# voeg data samen met localities om Latitude en Longitude toe te voegen obv locID
grouped_craywatch_data <- left_join(grouped_craywatch_data, localities_selected, by = "locID")

# Ontbrekende locIDs en coördinaten er uit halen
grouped_craywatch_data <- grouped_craywatch_data %>%
  mutate(
    longitude = as.numeric(Longitude),
    latitude = as.numeric(Latitude)) %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  select(-Latitude, -Longitude)

# # Maak GIS-laag
# craywatch_sf <- st_as_sf(grouped_craywatch_data, coords = c("longitude", "latitude"), crs = 4326)
# st_write(craywatch_sf, "~/GitHub/craywatch/R/data/output/GIS-laag/craywatch_results.shp", append=FALSE)
# st_write(craywatch_sf, "~/GitHub/craywatch/R/data/output/GIS-laag/craywatch_results.gpkg", layer = "craywatch_observations", append=FALSE)


######## 3. combineer tot analyze dataset #########
# verwerk absences apart
crayfish_absence <- grouped_craywatch_data %>%
  filter(soort == "crayfish indet") %>%
  mutate(
    dat.source = "craywatch_data",
    year = year(end_date) # Gebruik end_date voor het jaar
  ) %>%
  select(year, latitude, longitude, traps_used, dat.source, days_sampled, consecutive, CPUE, individuals_caught, date = end_date)

# Verwerk de soorten-waarnemingen
species_data <- grouped_craywatch_data %>%
  filter(soort != "crayfish indet") %>%
  mutate(
    dat.source = "craywatch_data",
    year = year(end_date) # Gebruik end_date voor het jaar
  ) %>%
  rename(species = soort) %>%
  separate_rows(species, sep = ", ") %>%
  mutate(species_present = 1) %>%
  group_by(year, latitude, longitude, species, traps_used, dat.source, days_sampled, consecutive, CPUE, individuals_caught, date = end_date) %>%
  summarise(
    species_present = max(species_present),
    .groups = 'drop'
  ) %>%
  pivot_wider(names_from = species, values_from = species_present, values_fill = 0)


# Filter de te gerbuiken gbif data er uit
issues_to_discard <- c(
  "ZERO_COORDINATE",
  "COORDINATE_OUT_OF_RANGE", 
  "COORDINATE_INVALID",
  "COUNTRY_COORDINATE_MISMATCH"
)

identificationVerificationStatus_to_discard <- c(
  "unverified",
  "not validated", 
  "under validation"
)

occ_filtered <- gbif_data %>%
  distinct(occurrenceID, .keep_all = TRUE) %>%
  filter(!issue %in% issues_to_discard) %>%
  filter(!identificationVerificationStatus %in% identificationVerificationStatus_to_discard) %>%
  filter(coordinateUncertaintyInMeters <= 100 | is.na(coordinateUncertaintyInMeters)) %>%
  filter(level1Name == "Vlaanderen") %>%
  mutate(
    dat.source = "gbif_data",
    species_present = 1,
    species = tolower(species)
  ) %>%
  rename(
    latitude = decimalLatitude,
    longitude = decimalLongitude
  ) %>%
  # Voeg month en day toe aan de groep
  group_by(year, month, day, latitude, longitude, species, dat.source) %>%
  summarise(species_present = max(species_present), .groups = 'drop') %>%
  pivot_wider(names_from = species, values_from = species_present) %>%
  # Voeg de ontbrekende kolommen toe met NA's
  mutate(
    traps_used = NA_real_,
    days_sampled = NA_real_,
    consecutive = NA,
    CPUE = NA_real_,
    individuals.caught = NA_real_
  ) %>%
  # Maak de date-kolom aan uit year, month, en day
  mutate(date = ymd(paste(year, month, day, sep = "-"))) %>%
  # Verwijder de originele year, month en day kolommen
  select(-month, -day)

# Voeg de drie datasets samen
final_dataset <- bind_rows(species_data, crayfish_absence, occ_filtered)

# 4. Vervang de NA's in de Craywatch-rijen door 0, tenzij traps_used < 12
final_dataset <- final_dataset %>%
  mutate(has_presence = rowSums(across(
    .cols = c("faxonius limosus", "procambarus virginalis", "procambarus acutus", "faxonius virilis", "procambarus clarkii", "pontastacus leptodactylus", "pacifastacus leniusculus")
  )) > 0) %>%
  mutate(across(
    .cols = c("faxonius limosus", "procambarus virginalis", "procambarus acutus", "faxonius virilis", "procambarus clarkii", "pontastacus leptodactylus", "pacifastacus leniusculus"),
    .fns = ~ if_else(
      dat.source == "craywatch_data" & has_presence & traps_used < 12 & . == 0,
      NA_real_,
      .
    )
  )) %>%
  select(-has_presence, -individuals.caught)

#### Maak intersect met WVLC, CATC en VHAG ###########

watervlakken <- st_read("~/GitHub/Craywatch-Rapport/R/data/input/shapefiles/watervlakken.shp")
vha_catc <- st_read("~/GitHub/Craywatch-Rapport/R/data/input/shapefiles/vhaCattraj.shp")
bekken <- st_read("~/GitHub/Craywatch-Rapport/R/data/input/shapefiles/Wsbekken.shp")

# transformeer craywatch data
final_dataset_sf <- st_as_sf(final_dataset,
                             coords = c("longitude", "latitude"),
                             crs = 4326)
craywatch_sf <- st_transform(final_dataset_sf, crs = st_crs(watervlakken))
vha_catc <- st_transform(vha_catc, crs = st_crs(watervlakken))
bekken <- st_transform(bekken, crs = st_crs(watervlakken))

# Leg buffer rond waterlichamen
watervlakken_buffer <- st_buffer(watervlakken, dist = 10)
catc_buffer <- st_buffer(vha_catc, dist = 10)

# Maak intersect met craywatch data
final_dataset <- craywatch_sf %>%
  st_join(watervlakken_buffer, left = TRUE) %>%
  st_join(catc_buffer, left = TRUE) %>%
  st_join(bekken, left = TRUE)

# Verwijder onnodige kolommen
final_dataset <- final_dataset %>%
  select(-geometry, -OIDN.x, -OIDN.y, -UIDN.x, - UIDN.y, -NAAM.x, -OBJECTID, -WTRLICHC, -HYLAC, -GEBIED, -KRWTYPE, -KRWTYPES, -DIEPKL, -CONNECT, -FUNCTIE, -PEILBEHEER, -OPPWVL, -OMTWVL, -SHAPE_Leng, -SHAPE_Area, -NAAM.y, -LBLCATC, -LENGTE.x, - LENGTE.y, -BEKNAAM, -STRMGEB, -OPPERVL)

# Zet terug om naar lat/long
dataset_analyse <- st_transform(final_dataset, crs = 4326)

# Extraheer de X- en Y-coördinaten
coordinaten_matrix <- st_coordinates(dataset_analyse)

# Voeg de coördinaten toe aan je data frame als nieuwe kolommen
dataset_analyse <- dataset_analyse %>%
  mutate(longitude = coordinaten_matrix[,1],
         latitude = coordinaten_matrix[,2])

# Pas de volgorde van de kolommen aan
final_dataset_analyse <- dataset_analyse %>%
  select(year, date, dat.source, individuals_caught, traps_used, days_sampled, consecutive, CPUE, everything())

write.csv(final_dataset_analyse, file = "~/GitHub/Craywatch-Rapport/R/data/output/analyse_dataset.csv")
write.csv(final_dataset_analyse, file = "~/GitHub/craywatch/R/data/output/analyse_dataset.csv")