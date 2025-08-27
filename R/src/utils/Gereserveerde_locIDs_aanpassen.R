# ====================================================
# Scriptnaam: Gereserveerde_locIDs_aanpassen.R
# Auteur: Margot Vermeylen
# Datum: 07-01-2025
# Beschrijving: 
#   Dit script past de file localities.csv aan na het 2024 Craywatch seizoen.
#   Enkel de punten waarvan er bruikbare data was krijgen TRUE.
# ====================================================

library(here)

# Pad naar localities.csv
path_localities <- "~/GitHub/craywatch/assets/localities.csv"

# lees csv
localities <- read.csv(path_localities)

colnames(localities)
colnames(craywatch_data_filtered)

# Unieke LocIDs uit gefilterde data
craywatch_locIDs <- unique(craywatch_data_filtered$locID)

# Kolommen aanpassen
localities$isReserved <- ifelse(localities$locID %in% craywatch_locIDs, "TRUE", "FALSE")
localities$updateRes  <- ifelse(localities$locID %in% craywatch_locIDs, "TRUE", "FALSE")

# Controle
table(localities$isReserved)
table(localities$updateRes)

# Overschrijf het originele bestand
write.csv(localities, path_localities, row.names = FALSE)
