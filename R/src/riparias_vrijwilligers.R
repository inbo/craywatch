# ====================================================
# Scriptnaam: riparias_vrijwilligers.R
# Auteur: Margot Vermeylen
# Datum: 19-02-2025
# Beschrijving: 
#   Dit script zoekt uit hoeveel vrijwilligers zich in Riparias gebied bevinden
#   en welke locaties bemonsterd zullen worden
# ====================================================

# Laad de benodigde library
library(dplyr)

# Laad shapefiles
riparias <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/riparias.shp")

# Laad csv
riparias_locaties <- read.csv("~/GitHub/craywatch/R/data/output/locations/locaties_in_riparias.csv")
locaties_2025 <- read.csv("~/GitHub/craywatch/R/data/output/localities_2025_updated.csv")

# Filter locaties_2025 op TRUE en controleer of locID in riparias_locaties staat
true_locaties <- locaties_2025 %>%
  filter(isReserved == TRUE, locID %in% riparias_locaties$locID)

# Aantal unieke locID's
aantal_true_locaties <- n_distinct(true_locaties$locID)
print(aantal_true_locaties)

# Aantal unieke vrijwilligers
aantal_vrijwilligers <- n_distinct(true_locaties$vrijwillID)
print(aantal_vrijwilligers)
