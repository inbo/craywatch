library(sf)
library(dplyr)

# laad shapefiles
vlaanderen <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/grenzenvlaanderen.shp")
riparias <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/riparias.shp")

# data lezen
locaties_2025 <- read.csv("~/GitHub/craywatch/assets/open_locations.csv")

colnames(locaties_2025)

# Zet de oorspronkelijke data om van WGS84 (gebruikelijke graad-coördinaten) naar Lambert
locaties_sf <- st_as_sf(
  locaties_2025,
  coords = c("Longitude", "Latitude"),
  crs = 4326 # WGS84 (graad-coördinaten)
)

# Transformeer naar het Lambert CRS
locaties_sf <- st_transform(locaties_sf, crs = st_crs(riparias))


# Controleer welke punten in Riparias liggen
locaties_in_riparias <- st_intersects(locaties_sf, riparias, sparse = FALSE)

# Filter de rijen die binnen Riparias liggen
locaties_in_riparias_sf <- locaties_sf[locaties_in_riparias, ]

# Transformeer de geometrie naar WGS84
locaties_in_riparias_wgs84 <- st_transform(locaties_in_riparias_sf, crs = 4326)

# Voeg Longitude en Latitude toe als aparte kolommen
locaties_in_riparias_wgs84 <- locaties_in_riparias_wgs84 %>%
  mutate(
    Longitude = st_coordinates(.)[, 1], # X-coördinaat (Longitude)
    Latitude = st_coordinates(.)[, 2]  # Y-coördinaat (Latitude)
  )

# Verwijder de geometrie als je een standaard data frame wilt
locaties_in_riparias_wgs84_df <- st_drop_geometry(locaties_in_riparias_wgs84)

# Bekijk het resultaat
head(locaties_in_riparias_wgs84_df)

# Exporteer de dataset (optioneel)
write.csv(locaties_in_riparias_wgs84_df, "~/GitHub/craywatch/assets/locaties_in_riparias.csv", row.names = FALSE)

