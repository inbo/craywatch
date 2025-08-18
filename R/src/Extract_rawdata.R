library(magrittr)
library(dplyr)

#To convert data to validation file
directory_path <- "~/GitHub/craywatch/R/data/observations/exports_2025"
files <- list.files(directory_path, pattern = "^event-craywatch-.*\\.csv$", full.names = TRUE)

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


# Get all data that has incorrect format in Code.sample.location
# Define the pattern
pattern <- ".*?([IV]_[0-9]{4}_[0-9]{1,2}).*"
# Replace all '-' and '.' with '_'
all_data$locID <- gsub("[-. ]", "_", all_data$Code.sample.location)
# Replace 'v' with 'V' and 'i' with 'I' in the locID column
all_data$locID <- gsub("v", "V", all_data$locID)
all_data$locID <- gsub("i", "I", all_data$locID)
all_data$locID <- gsub("L","I", all_data$locID)
all_data$locID <- gsub(pattern, "\\1", all_data$locID)


#read localities file to extract vrijwilID
localities <- read.csv("~/GitHub/craywatch/assets/localities.csv")

localities_subset <- localities %>% select(locID, vrijwillID)
all_data <- all_data %>% left_join(localities_subset, by = "locID")

write.csv(all_data, file = "~/GitHub/craywatch/R/data/observations/data_validation/craywatch_validation_2025.csv", row.names = FALSE)