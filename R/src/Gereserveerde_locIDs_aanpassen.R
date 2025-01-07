# lees csv
localities <- read.csv("~/GitHub/craywatch/assets/localities.csv")

colnames(localities)
colnames(craywatch_data_filtered)

craywatch_locIDs <- unique(craywatch_data_filtered$locID)  # Unieke LocIDs

localities$isReserved <- ifelse(localities$locID %in% craywatch_locIDs, "TRUE", "FALSE")
localities$updateRes <- ifelse(localities$locID %in% craywatch_locIDs, "TRUE", "FALSE")

table(localities$isReserved)  # Controleer hoeveel TRUE en FALSE waarden er zijn
table(localities$updateRes)  # Controleer hoeveel TRUE en FALSE waarden er zijn

# Sla de bijgewerkte dataset op (overschrijf het originele bestand)
write.csv(localities, "~/GitHub/craywatch/assets/localities.csv", row.names = FALSE)
