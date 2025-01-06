#R - libraries
library(ggspatial)
library(sf)
library(tidyverse)
library(dplyr)
library(scales)
library(osmdata)
library(ggplot2)
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
  dplyr::select(locID, Latitude, Longitude)

# Merge with localities to add Latitude and Longitude based on locID
grouped_craywatch_data <- left_join(grouped_craywatch_data, localities_selected, by = "locID")

#preview new data
head(grouped_craywatch_data) # Show the first few rows of the data

# Inspect the structure of the dataset to check for lat/long columns
str(grouped_craywatch_data)

# Filter the data we want to use
craywatch_data_filtered <- grouped_craywatch_data %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  filter(!(species == "absent" & consecutive == FALSE)) %>%  # Exclude rows where species is "absent" and consecutive is FALSE
  filter(!(species == "absent" & number_of_days < 4))  # Exclude rows where species is "absent" and number_of_days < 4

craywatch_sf <- st_as_sf(craywatch_data_filtered, coords = c("Longitude", "Latitude"), crs = 4326)

# Read shapefiles
vlaanderen <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/grenzenvlaanderen.shp")
hoofdrivieren <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/hoofdrivieren.shp")
kanalen <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/kanalen.shp")

sf_use_s2(FALSE)

# Make plot
base_plot <- ggplot() +
  geom_sf(data = vlaanderen, fill= "#98AF93", size=0.2, colour= "black") +
  geom_sf(data = hoofdrivieren, size=0.1, colour="#4682B4")+
  geom_sf(data = kanalen, size=0.1, colour="#4682B4")+
  theme_void() +
  theme(legend.title = element_blank(), 
        legend.text=element_text(size=8, face="italic"), 
        legend.key.size = unit(0.2, "cm"),
        legend.position = "bottom",
        plot.title= element_text(face = "italic")) +
  coord_sf()

# Define a color palette for species
species_colors <- c("faxonius limosus" = "#FFD700",
                    "procambarus clarkii" = "#FF0000", "procambarus virginalis" = "#FF00FF",
                    "faxonius virilis" = "#FFA500", "procambarus acutus" = "#000000", "absent" = "lightgrey")

# Update the legend labels for species with italic formatting
species_labels <- c( "faxonius limosus" = expression(italic("Faxonius limosus")),
                     "procambarus clarkii" = expression(italic("Procambarus clarkii")), 
                     "procambarus virginalis" = expression(italic("Procambarus virginalis")), 
                     "faxonius virilis" = expression(italic("Faxonius virilis")), 
                     "procambarus acutus" = expression(italic("Procambarus acutus")), 
                     "absent" = expression(italic("Absence")))


# Create a color scale with updated labels
color_scale <- scale_color_manual(values = species_colors, labels = species_labels)

# Separate the data for 'crayfish.indet' (absence) and other species
crayfish_indet_sf <- craywatch_sf %>% filter(species == "absent")
other_species_sf <- craywatch_sf %>% filter(species != "absent")

# Plot 'crayfish.indet' (absence) points first, then other species
species_plot <- base_plot +
  geom_sf(data = crayfish_indet_sf, aes(color = species), size = 1) +  # lightgrey (absence) points
  geom_sf(data = other_species_sf, aes(color = species), size = 1) +   # other species points
  color_scale  # Apply the color scale based on species


# Save the plot
ggsave(species_plot, file = "~/GitHub/craywatch/R/data/output/validated_craywatch_map.png", 
       width = 15, height = 6.4, units = "cm", dpi = 200)

# Print the number of points included after filtering
num_points <- nrow(craywatch_data_clean)
print(paste("Number of points included in the map:", num_points))

