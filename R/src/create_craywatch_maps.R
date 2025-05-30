# ====================================================
# Scriptnaam: create_craywatch_maps.R
# Auteur: Margot Vermeylen
# Datum: 09-01-2025
# Beschrijving: 
# Dit script genereert twee Craywatch kaarten met de absence/presence waarnemingen van 2024
# en een kaart met de Craywatch punten bemonsterd in cat 1 waterlopen
# Het script genereert ook de input voor Select_municipalities
# ====================================================

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
    individuals = sum(number.of.individuals, na.rm = TRUE), 
    days_sampled = n(), # Rename count
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

# Nagaan hoeveel mensen exact het protocol volgden
protocol_followers <- grouped_craywatch_data %>%
  filter(days_sampled == 4 & consecutive == "TRUE") # locaties waar er exact 4 na elkaar dagen gecontroleerd is

n_distinct(protocol_followers$vrijwillID)

# Filter the data we want to use
craywatch_data_usable <- grouped_craywatch_data %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  filter(!(species == "absent" & days_sampled < 4))  # Exclude rows where species is "absent" and number_of_days < 4

craywatch_sf <- st_as_sf(craywatch_data_usable, coords = c("Longitude", "Latitude"), crs = 4326)

# Maak GIS-laag
st_write(craywatch_sf, "~/GitHub/craywatch/R/data/output/GIS-laag/craywatch_results_2024.shp", append=FALSE)  # shapefile
st_write(craywatch_sf, "~/GitHub/craywatch/R/data/output/GIS-laag/craywatch_results_2024.gpkg", layer = "craywatch_observations", append=FALSE)

# Read shapefiles
vlaanderen <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/grenzenvlaanderen.shp")
hoofdrivieren <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/hoofdrivieren.shp")
kanalen <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/kanalen.shp")
gemeenten <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/gemeenten.shp")

# Zorg dat alle shapefiles dezelfde CRS hebben
vlaanderen <- st_transform(vlaanderen, st_crs(hoofdrivieren))
hoofdrivieren <- st_transform(hoofdrivieren, st_crs(vlaanderen))
kanalen <- st_transform(kanalen, st_crs(vlaanderen))
gemeenten <- st_transform(gemeenten, st_crs(vlaanderen))

# Clip de shapefiles tot de grenzen van Vlaanderen
hoofdrivieren_in_vlaanderen <- st_intersection(hoofdrivieren, vlaanderen)
kanalen_in_vlaanderen <- st_intersection(kanalen, vlaanderen)
gemeenten_in_vlaanderen <- st_intersection(gemeenten, vlaanderen)

sf_use_s2(FALSE)

# Make plot
base_plot <- ggplot() +
  geom_sf(data = vlaanderen, fill= "lightgrey", size=0.2, colour= "black") +
  geom_sf(data = hoofdrivieren_in_vlaanderen, size=0.1, colour="#6BA1D3")+
  geom_sf(data = kanalen_in_vlaanderen, size=0.1, colour="#6BA1D3")+
  theme_void() +
  theme(legend.title = element_blank(), 
        legend.text=element_text(size=8, face="italic"), 
        legend.key.size = unit(0.2, "cm"),
        legend.position = "bottom",
        plot.title= element_text(face = "italic")) +
  coord_sf() + 
  annotation_scale(location = "tl", style = "ticks", width_hint = 0.15) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.1, "cm"),
    style = north_arrow_fancy_orienteering(fill = c("black", "black"), line_width = 0.3),
    scale = 0.1 # Maak de noordpijl kleiner
    )

# Make plot
gemeente_plot <- ggplot() +
  geom_sf(data = vlaanderen, fill= "#98AF93", size=0.2, colour= "black") +
  geom_sf(data = gemeenten_in_vlaanderen, size=0.1, colour="grey")+
  geom_sf(data = hoofdrivieren_in_vlaanderen, size=0.1, colour="#6BA1D3")+
  geom_sf(data = kanalen_in_vlaanderen, size=0.1, colour="#6BA1D3")+
  theme_void() +
  theme(legend.title = element_blank(), 
        legend.text=element_text(size=8, face="italic"), 
        legend.key.size = unit(0.2, "cm"),
        legend.position = "bottom",
        plot.title= element_text(face = "italic")) +
  coord_sf() +
  annotation_scale(location = "tl", style = "ticks", width_hint = 0.15) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    pad_x = unit(0.5, "cm"),
    pad_y = unit(0.1, "cm"),
    style = north_arrow_fancy_orienteering(fill = c("black", "black"), line_width = 0.3),
    scale = 0.1 # Maak de noordpijl kleiner
    )

# Define a color palette for species
species_colors <- c("faxonius limosus" = "#FFD700",
                    "procambarus clarkii" = "#FF0000", "procambarus virginalis" = "#FF00FF",
                    "faxonius virilis" = "#FFA500", "procambarus acutus" = "#000000", "absent" = "darkgrey")

# Update the legend labels for species with italic formatting
species_labels <- c( "faxonius limosus" = expression(italic("Faxonius limosus")),
                     "procambarus clarkii" = expression(italic("Procambarus clarkii")), 
                     "procambarus virginalis" = expression(italic("Procambarus virginalis")), 
                     "faxonius virilis" = expression(italic("Faxonius virilis")), 
                     "procambarus acutus" = expression(italic("Procambarus acutus")), 
                     "absent" = expression(italic("Absence")))

# Update the legend labels for species in Dutch 
species_labels_dutch <- c( "faxonius limosus" = expression("gevlekte Amerikaanse rivierkreeft"),
                     "procambarus clarkii" = expression("rode Amerikaanse rivierkreeft"), 
                     "procambarus virginalis" = expression("marmerkreeft"), 
                     "faxonius virilis" = expression("geknobbelde Amerikaanse rivierkreeft"), 
                     "procambarus acutus" = expression("gestreepte Amerikaanse rivierkreeft"), 
                     "absent" = expression("afwezigheid"))

# Create a color scale with updated labels
color_scale <- scale_color_manual(values = species_colors, labels = species_labels)

# Create a color scale with updated labels in Dutch
color_scale_dutch <- scale_color_manual(values = species_colors, labels = species_labels_dutch)

# Separate the data for 'crayfish.indet' (absence) and other species
crayfish_indet_sf <- craywatch_sf %>% filter(species == "absent")
other_species_sf <- craywatch_sf %>% filter(species != "absent")

# Plot 'crayfish.indet' (absence) points first, then other species
species_plot <- base_plot +
  geom_sf(data = crayfish_indet_sf, aes(color = species), size = 1) +  # lightgrey (absence) points
  geom_sf(data = other_species_sf, aes(color = species), size = 1) +   # other species points
  color_scale  # Apply the color scale based on species

# Plot craywatch map with municipalities
species_plot_gemeente <- gemeente_plot +
  geom_sf(data = crayfish_indet_sf, aes(color = species), size = 1) +  # lightgrey (absence) points
  geom_sf(data = other_species_sf, aes(color = species), size = 1) +   # other species points
  color_scale  # Apply the color scale based on species

# Plot craywatch map with municipalities
species_plot_dutch <- gemeente_plot +
  geom_sf(data = crayfish_indet_sf, aes(color = species), size = 1) +  # lightgrey (absence) points
  geom_sf(data = other_species_sf, aes(color = species), size = 1) +   # other species points
  color_scale_dutch  # Apply the color scale based on species

# Save the plot
ggsave(species_plot, file = "~/GitHub/craywatch/R/data/output/craywatch_maps/validated_craywatch_map.png", 
       width = 15, height = 7, units = "cm", dpi = 200)

# Save the plot (gemeente)
ggsave(species_plot_gemeente, file = "~/GitHub/craywatch/R/data/output/craywatch_maps/validated_craywatch_map_gemeenten.png", 
       width = 15, height = 7, units = "cm", dpi = 200)

# Save the plot (Dutch)
ggsave(species_plot_dutch, file = "~/GitHub/craywatch/R/data/output/craywatch_maps/validated_craywatch_map_dutch.png", 
       width = 15, height = 7, units = "cm", dpi = 400)

# Sla het ggplot-object op
dir.create("./data/output/SelectedMunic", showWarnings = FALSE, recursive = TRUE)
saveRDS(species_plot_gemeente, "./data/output/SelectedMunic/species_plot.rds")

# Print the number of points included after filtering
num_points <- nrow(craywatch_data_usable)
print(paste("Number of points included in the map:", num_points))

# Sla finale Craywatch data op als een CSV-bestand
write.csv(craywatch_data_usable, "~/GitHub/craywatch/R/data/output/final_craywatch_data_2024.csv", row.names = FALSE)

# ===================================================================================================================
## Make plot of observations in cat 1 waterways
# read data
final_craywatch_data <- read.csv("~/GitHub/craywatch/R/data/output/final_craywatch_data_2024.csv")
localities <- read.csv("~/GitHub/craywatch/assets/localities.csv")

# shapefile inlezen
waterlopen <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/vhaCattraj.shp")

# cat1 waterlopen er uit halen
cat1_waterlopen <- waterlopen %>%
  filter(CATC == 1)

cat1_waterlopen <- st_transform(cat1_waterlopen, st_crs(vlaanderen))
cat1_waterlopen_in_vlaanderen <- st_intersection(cat1_waterlopen, vlaanderen)

# Make plot
cat1_plot <- ggplot() +
  geom_sf(data = vlaanderen, fill= "#98AF93", size=0.2, colour= "black") +
  geom_sf(data = gemeenten_in_vlaanderen, size=0.1, colour="grey")+
  geom_sf(data = hoofdrivieren_in_vlaanderen, size=0.1, colour="#ADD8E6")+
  geom_sf(data = kanalen_in_vlaanderen, size=0.1, colour="#ADD8E6")+
  geom_sf(data = cat1_waterlopen_in_vlaanderen, size=0.1, colour="#4682B4")+
  theme_void() +
  theme(legend.title = element_blank(), 
        legend.text=element_text(size=8, face="italic"), 
        legend.key.size = unit(0.2, "cm"),
        legend.position = "bottom",
        plot.title= element_text(face = "italic")) +
  coord_sf()

# filter locations done in cat1
cat1_locations <- localities %>%
  filter(isReserved == TRUE & !is.na(CATC) & CATC == 1)

# filter cat1 locations out of the craywatch observations
cat1_observations <- final_craywatch_data %>%
  filter(locID %in% cat1_locations$locID)

craywatch_cat1_sf <- st_as_sf(cat1_observations, coords = c("Longitude", "Latitude"), crs = 4326)

# Separate the data for absence and presence
crayfish_indet_cat1_sf <- craywatch_cat1_sf %>% filter(species == "absent")
other_species_cat1_sf <- craywatch_cat1_sf %>% filter(species != "absent")

# Plot on the gemeente map
species_plot_cat1 <- cat1_plot +
  geom_sf(data = crayfish_indet_cat1_sf, aes(color = species), size = 1) +  # lightgrey (absence) points
  geom_sf(data = other_species_cat1_sf, aes(color = species), size = 1) +   # other species points
  color_scale  # Apply the color scale based on species

# Print the number of observations included
num_points <- nrow(cat1_observations)
print(paste("Number of points in cat1:", num_points))

# Save the plot
ggsave(species_plot_cat1, file = "~/GitHub/craywatch/R/data/output/craywatch_maps/cat1_craywatch_map.png", 
       width = 15, height = 7, units = "cm", dpi = 200)
