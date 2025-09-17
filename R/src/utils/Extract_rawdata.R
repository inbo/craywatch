library(magrittr)
library(dplyr)

# Hardcoded paden
dir_obs   <- "~/GitHub/craywatch/R/data/observations"
path_loc  <- "~/GitHub/craywatch/assets/localities.csv"
out_file  <- "~/GitHub/craywatch/R/data/observations/data_validation/craywatch_validation_2025.csv"

# Alle exportbestanden inlezen
files <- list.files(dir_obs, pattern = "^observation-event-craywatch-.*\\.csv$", full.names = TRUE)

process_file <- function(file) {
  # Soort uit bestandsnaam halen
  species <- gsub("event-craywatch-(.*)\\.csv", "\\1", basename(file))
  species <- gsub("-", " ", species)
  
  df <- read.csv(file)
  if (nrow(df) > 0) df$soort <- species
  df
}

# Combineer alle niet-lege dataframes
all_data <- lapply(files, function(f) {
  df <- process_file(f)
  if (nrow(df) > 0) df
}) %>% bind_rows()

# locID normalisatie
pattern <- ".*?([IV]_[0-9]{4}_[0-9]{1,2}).*"

all_data$locID <- gsub("[-. ]", "_", all_data$Code.sample.location)
all_data$locID <- gsub("v", "V", all_data$locID)
all_data$locID <- gsub("i", "I", all_data$locID)
all_data$locID <- gsub("L", "I", all_data$locID)
all_data$locID <- gsub(pattern, "\\1", all_data$locID)

# Localities inlezen en koppelen
localities <- read.csv(path_loc)
localities_subset <- localities %>% select(locID, vrijwillID)

all_data <- all_data %>% left_join(localities_subset, by = "locID")

# Wegschrijven
write.csv(all_data, file = out_file, row.names = FALSE)
