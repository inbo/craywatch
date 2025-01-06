# Load packages needed
library(dplyr)
library(tidyr)
library(lubridate)

# Read data
craywatch_data <- read.csv("~/GitHub/craywatch/R/data/observations/data_validation/craywatch_validation.csv")
map_data <- read.csv("~/GitHub/craywatch/assets/localities.csv")

# Ensure the 'date' column is in the correct Date format (assuming day-month-year format)
craywatch_data$date <- dmy(craywatch_data$date) # Convert to Date using dmy format

# Group by locID and process data
grouped_craywatch_data <- craywatch_data %>%
  group_by(locID) %>%
  summarize(
    number_of_individuals = sum(number.of.individuals, na.rm = TRUE), 
    number_of_days = n(), # Rename count
    start_date = min(date, na.rm = TRUE), # Get the first date
    end_date = max(date, na.rm = TRUE), # Get the last date
    species = list(
      if (sum(number.of.individuals, na.rm = TRUE) > 0) unique(soort[number.of.individuals > 0]) else "absent"
    ), # Capture species or 'absent'
    consecutive = {
      # Check if the dates are consecutive for each locID
      date_diff <- diff(sort(unique(date))) # Calculate the differences between sorted unique dates
      all(date_diff == 1) # TRUE if all differences are 1, meaning consecutive days
    },
    vrijwillID = first(vrijwillID)  # Take the first 'vrijwillID' value for each group (assuming it is the same for each group)
  ) %>%
  unnest(cols = c(species)) # Separate each species into its own row

# Select only the 'locID', 'Latitude', and 'Longitude' columns from localities
localities_selected <- map_data %>%
  select(locID, Latitude, Longitude)

# Merge with localities to add Latitude and Longitude based on locID
grouped_craywatch_data <- left_join(grouped_craywatch_data, localities_selected, by = "locID")

# Save the new data as a CSV file
write.csv(grouped_craywatch_data, "~/GitHub/craywatch/R/data/observations/data_validation/grouped_craywatch_data.csv", row.names = FALSE)

# Print confirmation
cat("New data saved as '~/GitHub/craywatch/R/data/observations/data_validation/grouped_craywatch_data.csv'\n")

#preview new data
head(grouped_craywatch_data) # Show the first few rows of the saved data