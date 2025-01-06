#R - libraries
library(ggspatial)
library(sf)
library(tidyverse)
library(dplyr)
library(scales)
library(osmdata)
library(ggplot2)

# Load the CSV file
craywatch_data <- read.csv("~/GitHub/craywatch/R/data/observations/data_validation/grouped_craywatch_data.csv")

# Inspect the structure of the dataset to check for lat/long columns
str(craywatch_data)

craywatch_data_clean <- craywatch_data %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  filter(!(species == "absent" & consecutive == FALSE)) %>%  # Exclude rows where species is "absent" and consecutive is FALSE
  filter(!(species == "absent" & number_of_days < 4))  # Exclude rows where species is "absent" and number_of_days < 4

craywatch_data_clean <- craywatch_data_clean %>%
  filter(Latitude >= -90 & Latitude <= 90 & Longitude >= -180 & Longitude <= 180)

craywatch_sf <- st_as_sf(craywatch_data_clean, coords = c("Longitude", "Latitude"), crs = 4326)


# Read individual shapefiles
vlaanderen <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/grenzenvlaanderen.shp")
hoofdrivieren <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/hoofdrivieren.shp")
kanalen <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/kanalen.shp")

sf_use_s2(FALSE)


# Filter the provincies shapefile for Flanders (assuming the province names are in Dutch)
#flanders_provincies <- provincies %>%
#  filter(PROVNAAM %in% c("Antwerpen", "Limburg", "Oost-Vlaanderen", "Vlaams-Brabant", "West-Vlaanderen"))


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
ggsave(species_plot, file = "~/GitHub/craywatch/R/data/observations/data_validation/output/validated_craywatch_map.png", 
       width = 15, height = 6.4, units = "cm", dpi = 200)

# Print the number of points included after filtering
num_points <- nrow(craywatch_data_clean)
print(paste("Number of points included in the map:", num_points))

