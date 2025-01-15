library(sf)
library(xml2)
library(dplyr)
library(lwgeom)


#####LAAD DE KML DATA EN MAAK KLAAR VOOR UPDATE ###############################
# Bestandspad naar de KML-file
kml_file <- "./data/input/localities_2025.kml"

# Lezen van de KML-bestand
kml_data <- st_read(kml_file)

# Functie om de HTML-gecodeerde string om te zetten naar een lijst
parse_description <- function(desc) {
  desc_list <- strsplit(desc, "<br>")[[1]]
  desc_list <- lapply(desc_list, function(x) {
    parts <- strsplit(x, ": ", fixed = TRUE)[[1]]
    if (length(parts) == 2) {
      return(setNames(list(parts[2]), parts[1]))
    } else {
      return(NULL)
    }
  })
  desc_list <- do.call(c, desc_list)
  return(desc_list)
}

# Toepassen van de parse functie op de beschrijving kolom
parsed_data <- lapply(kml_data$Description, parse_description)

# Omzetten naar een dataframe
parsed_data <- lapply(parsed_data, function(x) {
  as.data.frame(t(unlist(x)), stringsAsFactors = FALSE)
})
# Binden van alle rijen in een enkele dataframe
df <- bind_rows(parsed_data) 

# Verwijder de Description kolom van kml_data
kml_data <- kml_data %>% select(-Description)

# Voeg de nieuwe attributen toe aan kml_data
kml_data <- cbind(kml_data, df)

#Transform before doing intersects with other layers
kml_data <- st_transform(kml_data, crs = 31370)

# Hernoem de kolom 'Name' naar 'isReserved'
names(kml_data)[names(kml_data) == "Name"] <- "isReserved"

# Wijs dezelfde waarden toe =aan de nieuwe kolom 'isReserved'
kml_data$isReserved <- kml_data$updateRes

# Voeg een tijdelijke index kolom toe
kml_data <- kml_data %>%
  mutate(temp_id = row_number())


##### LOAD ALL SHAPES ##########################################################
# Lijst van alle shapefiles in de directory
shapefile_dir <- "./data/input/shapefiles/"
shapefiles <- list.files(shapefile_dir, pattern = "\\.shp$", full.names = TRUE)

# Verkrijg de CRS van kml_data
kml_crs <- st_crs(kml_data)

# Dynamisch toewijzen van paden aan variabelen en shapefiles laden en  
# transformeren naar de CRS van kml_data
for (shapefile in shapefiles) {
  shapefile_name <- tools::file_path_sans_ext(basename(shapefile))
  variable_name <- paste0(shapefile_name, "_path")
  assign(variable_name, shapefile)
  
  # Shapefile laden
  shape <- st_read(shapefile)
  
  # Valideer en repareer ongeldige geometrieën
  shape <- st_make_valid(shape)
  
  # Transformeer naar de CRS van kml_data
  shape <- st_transform(shape, crs = kml_crs)
  
  # Toewijzen aan variabele met "_shape" suffix
  assign(paste0(shapefile_name, "_shape"), shape)
}

# Controleer de toegewezen shapes
print(ls(pattern = "_shape"))

# Leg buffer van 5m rond waterlopen om punten te capteren bij intersect
waterlopen_buffer <- st_buffer(vhaCattraj_shape, dist = 5) %>%
                      st_transform(st_crs(kml_data))
OSM_waterways_buffer <- st_buffer(OSM_waterways_shape, dist = 5)%>%
                  st_transform(st_crs(kml_data))
rm(OSM_waterways_shape)

####BEWERKINGEN MET SHAPES - INTERSECT MET KML_DATA ############################
# Intersect met provincies_shape
provincies_intersect <- st_intersection(kml_data, provincies_shape)
# Intersect met postkantons_shape
postkantons_intersect <- st_intersection(kml_data, postkantons_shape)
# Intersect met gemeenten_shape
gemeenten_intersect <- st_intersection(kml_data, gemeenten_shape)
# Intersect met waterlopen_buffer
waterlopen_intersect <- st_intersection(kml_data, waterlopen_buffer)
watering_intersect <- st_intersection(kml_data, watering_shape)
polder_intersect <- st_intersection(kml_data, polder_shape)
anbterrein_intersect <- st_intersection (kml_data, am_patdat_shape)
OSM_waterways_intersect <- st_intersection(kml_data, OSM_waterways_buffer)
OSM_waterbodies_intersect <- st_intersection(kml_data, OSM_waterbodies_shape)


# Bewerk zodat je voor elke temp_id slechts één punt krijgt, het intersect & buffer kan ervoor zorgen dat punten in meerdere polygonen liggen
# Controleer of alle waarden in waterlopen_intersect$temp_id uniek zijn
is_unique <- length(unique(waterlopen_intersect$temp_id)) == nrow(waterlopen_intersect)

# Print het resultaat
if (is_unique) {
  print("Alle waarden in waterlopen_intersect$temp_id zijn uniek.")
} else {
  print("Niet alle waarden in waterlopen_intersect$temp_id zijn uniek.")
}

# Optioneel: Als je de niet-unieke waarden wilt zien
duplicated_ids <- waterlopen_intersect$temp_id[duplicated(waterlopen_intersect$temp_id)]
if (length(duplicated_ids) > 0) {
  print("Niet-unieke temp_id waarden:")
  print(duplicated_ids)
  
  # Filter de duplicaten
  filtered_waterlopen_intersect <- waterlopen_intersect %>%
    group_by(temp_id) %>%
    filter(row_number(st_equals(geometry, first(geometry))) == 1) %>%
    filter(CATC == min(CATC)) %>%
    ungroup()
  
  # Controleer het resultaat
  print(filtered_waterlopen_intersect)
  
  # Als je wilt zien hoeveel rijen er zijn verwijderd
  print(paste("Aantal rijen voor filtering:", nrow(waterlopen_intersect)))
  print(paste("Aantal rijen na filtering:", nrow(filtered_waterlopen_intersect)))
  
  # Werk de originele dataset bij
  waterlopen_intersect <- filtered_waterlopen_intersect
}

#Herhaal voor OSM_waterways_intersect, maar bij duplicaten, verwijder dubbele waarden
# Controleer of alle waarden in OSM_waterways_intersect$temp_id uniek zijn
is_unique <- length(unique(OSM_waterways_intersect$temp_id)) == nrow(OSM_waterways_intersect)

# Print het resultaat
if (is_unique) {
  print("Alle waarden in OSM_waterways_intersect$temp_id zijn uniek.")
} else {
  print("Niet alle waarden in OSM_waterways_intersect$temp_id zijn uniek.")
}

# Optioneel: Als je de niet-unieke waarden wilt zien
duplicated_ids <- OSM_waterways_intersect$temp_id[duplicated(OSM_waterways_intersect$temp_id)]
if (length(duplicated_ids) > 0) {
  print("Niet-unieke temp_id waarden:")
  print(duplicated_ids)
  
  # Verwijder de duplicaten
  filtered_OSM_waterways_intersect <- OSM_waterways_intersect %>%
    group_by(temp_id) %>%
    filter(row_number() == 1) %>%
    ungroup()
  
  # Controleer het resultaat
  print(filtered_OSM_waterways_intersect)
  
  # Als je wilt zien hoeveel rijen er zijn verwijderd
  print(paste("Aantal rijen voor filtering:", nrow(OSM_waterways_intersect)))
  print(paste("Aantal rijen na filtering:", nrow(filtered_OSM_waterways_intersect)))
  
  # Werk de originele dataset bij
  OSM_waterways_intersect <- filtered_OSM_waterways_intersect
}

#########Populate the kml_data dataframe#######################################
# Voeg velden toe indien ze niet bestaan
if (!"provincie" %in% colnames(kml_data)) {
  kml_data$provincie <- NA
}
if (!"postcode" %in% colnames(kml_data)) {
  kml_data$postcode <- NA
}
if (!"gemeente" %in% colnames(kml_data)) {
  kml_data$gemeente <- NA
}
if (!"CATC" %in% colnames(kml_data)) {
  kml_data$CATC <- NA
}
if (!"Beheerder" %in% colnames(kml_data)) {
  kml_data$Beheerder <- NA
}
if (!"Bhrtel" %in% colnames(kml_data)) {
  kml_data$Bhrtel <- NA
}
if (!"Bhremail" %in% colnames(kml_data)) {
  kml_data$Bhremail <- NA
}


# Maak dictionaries van de intersecties
prov_dict <- setNames(provincies_intersect$PROVNAAM, provincies_intersect$temp_id)
post_dict <- setNames(postkantons_intersect$nouveau_PO, postkantons_intersect$temp_id)
gem_dict <- setNames(gemeenten_intersect$GEMNAAM, gemeenten_intersect$temp_id)
wat_catc_dict <- setNames(waterlopen_intersect$CATC, waterlopen_intersect$temp_id)
wat_naam_dict <- setNames(waterlopen_intersect$NAAM, waterlopen_intersect$temp_id)
wat_vhag_dict <- setNames(waterlopen_intersect$VHAG, waterlopen_intersect$temp_id)
pol_email_dict <- setNames(polder_intersect$EMAIL, polder_intersect$temp_id)
pol_tel_dict <- setNames(polder_intersect$TELEFOON, polder_intersect$temp_id)
pol_naam_dict <- setNames(polder_intersect$NAAMPOL, polder_intersect$temp_id)
wat_email_dict <- setNames(watering_intersect$EMAIL, watering_intersect$temp_id)
wat_tel_dict <- setNames(watering_intersect$TELEFOON, watering_intersect$temp_id)
wat_naam_dict <- setNames(watering_intersect$NAAMWAT, watering_intersect$temp_id)
anb_email_dict <- setNames(anbterrein_intersect$contact_em, anbterrein_intersect$temp_id)
anb_naam_dict <- setNames(anbterrein_intersect$beheerder, anbterrein_intersect$temp_id)

OSM_wb_dict <- setNames(OSM_waterbodies_intersect$name, OSM_waterbodies_intersect$temp_id)
OSM_ww_dict <- setNames(OSM_waterways_intersect$name, OSM_waterways_intersect$temp_id)

# Replace 'NA' string with actual NA values in the entire data frame except geometry columns
kml_data <- kml_data %>%
  mutate(across(where(is.character), ~na_if(.x, "NA")))

# Update de velden in kml_data gebaseerd op de intersecties alleen als de velden leeg zijn
kml_data <- kml_data %>%
  rowwise() %>%
  mutate(
    gemeente = if_else(is.na(gemeente) | gemeente == "", gem_dict[as.character(temp_id)], gemeente),
    provincie = if_else(is.na(provincie) | provincie == "", prov_dict[as.character(temp_id)], provincie),
    postcode = if_else(is.na(postcode) | postcode == "", post_dict[as.character(temp_id)], postcode),
    CATC = if_else(is.na(CATC) | CATC == "", wat_catc_dict[as.character(temp_id)], CATC), 
    NAAM = if_else(is.na(NAAM) | NAAM == "" | NAAM == " ", wat_naam_dict[as.character(temp_id)], NAAM),
    VHAG = if_else(is.na(VHAG) | VHAG == 0 | VHAG == "", wat_vhag_dict[as.character(temp_id)], VHAG),
    
    Beheerder = if_else(CATC %in% c(2, 3) & (is.na(Beheerder) | Beheerder == ""), pol_naam_dict[as.character(temp_id)], Beheerder),
    Bhremail = if_else(CATC %in% c(2, 3) & (is.na(Bhremail) | Bhremail == ""), pol_email_dict[as.character(temp_id)], Bhremail),
    Bhrtel = if_else(CATC %in% c(2, 3) & (is.na(Bhrtel) | Bhrtel == ""), pol_tel_dict[as.character(temp_id)], Bhrtel),
    
    Beheerder = if_else(is.na(Beheerder) | Beheerder == "", anb_naam_dict[as.character(temp_id)], Beheerder),
    Bhremail = if_else(is.na(Bhremail) | Bhremail == "", anb_email_dict[as.character(temp_id)], Bhremail),
    ) %>%
  ungroup()

kml_data <- kml_data %>%
  rowwise() %>%
  mutate(
    NAAM = if_else(is.na(NAAM) | NAAM == "" | NAAM == " " | NAAM == "NA", OSM_wb_dict[as.character(temp_id)], NAAM),) %>%
  ungroup() 

kml_data <- kml_data %>%
  rowwise() %>%
  mutate(
    NAAM = if_else(is.na(NAAM) | NAAM == "" | NAAM == " " | NAAM == "NA", OSM_ww_dict[as.character(temp_id)], NAAM),) %>%
  ungroup() %>%
  select(-temp_id) 

# Add manager contacts
kml_data <- kml_data %>%
  mutate(
    Beheerder = if_else((is.na(Beheerder) | Beheerder == "") & CATC == 2 & SystemType != "Lentisch", provincie, Beheerder),
    Bhremail = if_else(Beheerder == "Antwerpen", "hans.vanloy@provincieantwerpen.be", Bhremail),
    Bhremail = if_else(Beheerder == "Vlaams-Brabant", "ingrid.beuls@vlaamsbrabant.be", Bhremail),
    Bhremail = if_else(Beheerder == "Oost-Vlaanderen", "waterbeleid@oost-vlaanderen.be", Bhremail),
    Bhremail = if_else(Beheerder == "Limburg", "christel.bouchet@limburg.be", Bhremail),
    Beheerder = if_else((is.na(Beheerder) | Beheerder == "") & CATC == 1 & SystemType != "Lentisch",    "VMM", Beheerder),
    Bhremail = if_else(Beheerder == "VMM", "d.slootmaekers@vmm.be", Bhremail)
  )

print("VHAG, CATC, Province, postcode, and gemeenten successfully added to localities")

 
####UPDATE LOC_ID###############################################################

library(data.table)

# Convert kml_data to data.table for better performance
kml_data <- st_drop_geometry(kml_data)
kml_data <- as.data.table(kml_data)

# Voeg het veld 'locID' toe als het niet bestaat
if (!"locID" %in% colnames(kml_data)) {
  kml_data[, locID := NA_character_]
}

# Initialize the list to store the highest locID number for each postcode
unique_numbers <- list()

# First pass to populate the unique_numbers list with the highest locID numbers
for (i in seq_len(nrow(kml_data))) {
  row <- kml_data[i]
  postcode <- as.character(row$postcode)
  locID <- as.character(row$locID)
  
  if (!is.na(locID) && locID != "") {
    tryCatch({
      unique_number <- as.integer(strsplit(locID, "_")[[1]][3])
      if (is.null(unique_numbers[[postcode]])) {
        unique_numbers[[postcode]] <- unique_number
      } else {
        unique_numbers[[postcode]] <- max(unique_numbers[[postcode]], unique_number)
      }
    }, error = function(e) {
      # Skip locID values that are not in the expected format
    })
  }
}

# Function to generate unique locID and update unique_numbers
generate_locID <- function(postcode, SugbyINBO) {
  postcode_str <- as.character(postcode)
  if (is.null(unique_numbers[[postcode_str]])) {
    unique_numbers[[postcode_str]] <- 1
  } else {
    unique_numbers[[postcode_str]] <- unique_numbers[[postcode_str]] + 1
  }
  unique_number <- unique_numbers[[postcode_str]]
  prefix <- ifelse(SugbyINBO == "YES", "I", "V")
  sprintf("%s_%s_%s", prefix, postcode_str, unique_number)
}

# Update the 'locID' field for rows where it is empty or NA and SugbyINBO is YES or NO
for (i in seq_len(nrow(kml_data))) {
  row <- kml_data[i]
  locID <- as.character(row$locID)
  SugbyINBO <- row$SugbyINBO
  postcode <- as.character(row$postcode)
  
  if ((is.na(locID) || locID == "") && (SugbyINBO == "YES" || SugbyINBO == "NO")) {
    new_locID <- generate_locID(postcode, SugbyINBO)
    kml_data[i, locID := new_locID]
    # Update unique_numbers list after assigning new locID
    unique_numbers[[postcode]] <- unique_numbers[[postcode]] + 1
  }
}

print("LocID toegevoegd aan nieuwe locaties")

# Check for duplicates in the locID column
duplicate_locIDs <- kml_data[duplicated(locID) | duplicated(locID, fromLast = TRUE)]

# Print the final duplicate locIDs if any
if (nrow(duplicate_locIDs) > 0) {
  print("Duplicate locIDs found after resolving:")
  print(duplicate_locIDs)
} else {
  print("No duplicate locIDs found after resolving.")
}

######################### Save als een csv bestand############################## 
# Definieer het pad naar het CSV-bestand
csv_path <- "~/GitHub/craywatch/R/data/output/localities_2025_updated.csv"

# Schrijf kml_data naar een CSV-bestand
write.csv(kml_data, file = csv_path, row.names = FALSE, sep = ",", dec = ".")

print("subset CSV file successfully saved.")

########################update volledig localities file#########################
#lees de oude localities csv in
localities_full <- read.csv("~/GitHub/craywatch/assets/localities.csv", stringsAsFactors = FALSE)

#lees de 2025 subset in
localities_2025 <- read.csv("~/GitHub/craywatch/R/data/output/localities_2025_updated.csv", stringsAsFactors = FALSE)

# Behoud alleen kolommen die in localities_full voorkomen
localities_2025_cleaned <- localities_2025 %>%
  select(all_of(colnames(localities_full)))

# Zorg dat de kolommen van hetzelfde type zijn
localities_full <- localities_full %>%
  mutate(
    isReserved = as.character(isReserved),
    updateRes = as.character(updateRes)
  )

localities_2025_cleaned <- localities_2025_cleaned %>%
  mutate(
    isReserved = as.character(isReserved),
    updateRes = as.character(updateRes)
  )

# Zorg ervoor dat de kolomnamen overeenkomen
stopifnot(all(colnames(localities_full) %in% colnames(localities_2025_cleaned)))

# Pas de wijzigingen van de subset toe aan de volledige file
localities_updated <- localities_full %>%
  rows_update(localities_2025_cleaned, by = "locID")

# Sla de geupdate volledige dataset op
write.csv(localities_updated, "~/GitHub/craywatch/assets/localities.csv", row.names = FALSE, sep = ",", dec = ".")

print("full CSV file successfully saved.")