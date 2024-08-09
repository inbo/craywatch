library(dplyr)

# Set the directory path
directory_path <- "./data/observations/"

# List files in the directory that match the pattern
files <- list.files(directory_path, pattern = "^event-craywatch-.*\\.csv$", full.names = TRUE)

# Function to read and process each file
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

# Save the combined data frame to a CSV file if needed
# write.csv(all_data, file = "combined_data.csv", row.names = FALSE)

# Display the combined data
all_data <- arrange(all_data, Code.sample.location)


####################DATA REGULIER? #############################################
# Bereken aantal entries per Code.sample.location en controleer opeenvolgende dagen
data_check <- all_data %>%
  group_by(Code.sample.location) %>%
  summarise(
    n = n(),
    consecutive_days = all(diff(as.Date(date, format = "%Y-%m-%d")) == 1, na.rm = TRUE)
  )

# Filter voor entries met n == 4 en consecutive_days == TRUE
data_approved <- data_check %>%
  filter(n == 4, consecutive_days == TRUE)
data_notapproved <- setdiff(data_check, data_approved)

###################CREATE 2 DATASETS: APPROVED - NON APPROVED###################

# Extract the approved Code.sample.location values
approved_locations <- data_approved$Code.sample.location

# Filter all_data to create all_data_approved and all_data_nonapproved
all_data_approved <- all_data %>%
  filter(Code.sample.location %in% approved_locations)

all_data_nonapproved <- all_data %>%
  filter(!Code.sample.location %in% approved_locations)

# Display the new tables
print("Approved Data:")
print(all_data_approved)

print("Non-Approved Data:")
print(all_data_nonapproved)


# Make csv for datacheck
all_data_approved$CHECK <- FALSE
all_data_nonapproved$CHECK <-TRUE



datacheck <-rbind(all_data_approved, all_data_nonapproved)
datacheck$export_datum <- Sys.Date() 

write.csv(datacheck, "./data/observations/output/datacheck.csv")
