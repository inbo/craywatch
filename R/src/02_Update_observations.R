###############################################################################
# Scriptnaam:   import_new_natuurpunt_data.R
# Doel:         Inlezen en verwerken van nieuwe Waarnemingen-exporten voor Craywatch
# Auteur:       Frédérique Steen
# Datum:        [dd-mm-jjjj]
#
# Beschrijving:
#  - Leest de laatste exportbestanden van Natuurpunt in (event-craywatch-*.csv).
#  - Vergelijkt met vorige import (last_import.csv) en houdt enkel nieuwe waarnemingen bij.
#  - Corrigeert locID-namen naar een gestandaardiseerd formaat.
#  - Filtert enkel locaties met 4 opeenvolgende vangstdagen (validatie).
#  - Schrijft resultaten weg naar:
#       * datacheck.csv   : alle (oude + nieuwe) data voor opvolging
#       * cleandata.csv   : gevalideerde data (4 opeenvolgende dagen)
#       * last_import.csv : volledige nieuwe import als referentie voor volgende run
#
# Input:   ./data/observations/event-craywatch-*.csv  (handmatig gedownload van eventpage)
# Output:  ./data/observations/output/datacheck.csv
#          ./data/observations/output/cleandata.csv
#          ./data/observations/output/last_import.csv
###############################################################################

library(dplyr)

#####GET NEW DATA FROM NATUURPUNT EXPORT########################################
#By comparing the id from the observations in the last_import & the new import only new observations are considered
# Set the directory path
directory_path <- "./data/observations/"

# Get the earlier import or initialize an empty data frame
if (file.exists("./data/observations/output/last_import.csv")) {
  last_import <- read.csv("./data/observations/output/last_import.csv")
} else {
  last_import <- data.frame(id = character(), stringsAsFactors = FALSE)
}

# Get the latest export files from Natuurpunt (manually downloaded from eventpage)
# List files in the directory that match the pattern
files <- list.files(directory_path, pattern = "^event-craywatch-.*\\.csv$", full.names = TRUE)

# Function to read and process each file, i.e. add species as column
process_file <- function(file) {
  # Extract the species name from the file name
  species <- gsub("event-craywatch-(.*)\\.csv", "\\1", basename(file))
  species <- gsub("-", " ", species)
  
  # Read the file into a data frame
  data <- read.csv(file)
  
  # Only add the species column if the data is not empty
  if (nrow(data) > 0) {
    data$soort <- species
  }
  
  return(data)
}

# Process all files and combine them into one data frame
all_data <- lapply(files, function(file) {
  df <- process_file(file)
  # Only return non-empty data frames
  if (nrow(df) > 0) return(df)
}) %>% bind_rows()


# sorted by locID
all_data <- arrange(all_data, Code.sample.location)

# Only keep the new data
new_data <- all_data %>% 
  filter(!(id %in% last_import$id))


###CORRECT UGLY locID names ####################################################
# Get all data that has incorrect format in Code.sample.location
# Define the pattern
pattern <- ".*?([IV]_[0-9]{4}_[0-9]{1,2}).*"

# Replace all '-' and '.' with '_'
new_data$locID <- gsub("[-. ]", "_", new_data$Code.sample.location)
# Replace 'v' with 'V' and 'i' with 'I' in the locID column
new_data$locID <- gsub("v", "V", new_data$locID)
new_data$locID <- gsub("i", "I", new_data$locID)
new_data$locID <- gsub("L","I", new_data$locID)

new_data$locID <- gsub(pattern, "\\1", new_data$locID)


###FILTER DATA FOR 4 CONSECUTIVE CATCH DAYS ####################################
#Data met opeenvolgende vangstdagen
# Append the new data to the datacheck of previous download to filter the 4 consecutive catch days
if (file.exists("./data/observations/output/datacheck.csv")) {
  old_datacheck <- read.csv("./data/observations/output/datacheck.csv")
} else {
  old_datacheck <- data.frame(locID = character(), date = character(), stringsAsFactors = FALSE) 
}
datacheck <- rbind(old_datacheck, new_data)



# Bereken aantal entries per Code.sample.location en controleer opeenvolgende dagen
locID_group <- datacheck %>%
  group_by(locID) %>%
  summarise(
    n = n(),
    consecutive_days = all(diff(as.Date(date, format = "%Y-%m-%d")) == 1, na.rm = TRUE)
  )

# Filter voor entries met n == 4 en consecutive_days == TRUE
data_approved <- locID_group %>%
  filter(n == 4, consecutive_days == TRUE)

# Behoud enkel data met valide locID om naar clean data te schrijven
data_approved <- data_approved[grep(pattern, data_approved$locID), ]

# Voeg de nieuwe cleandata toe aan de eerdere
cleandata <- datacheck %>% filter(locID %in% data_approved$locID)

# Voeg de nieuwe cleandata toe aan de eerdere als die bestaat
if (file.exists("./data/observations/output/cleandata.csv")) {
  old_cleandata <- read.csv("./data/observations/output/cleandata.csv")
} else {
  old_cleandata <- data.frame(locID = character(), date = character(), stringsAsFactors = FALSE)
}

cleandata <-rbind(old_cleandata, cleandata)

datacheck <- datacheck %>% filter(!(locID %in% data_approved$locID))

# Save datafiles voor volgende analyse
write.csv(datacheck, "./data/observations/output/datacheck.csv", row.names = FALSE)
write.csv(cleandata, "./data/observations/output/cleandata.csv", row.names = FALSE)
write.csv(all_data, "./data/observations/output/last_import.csv", row.names = FALSE)
