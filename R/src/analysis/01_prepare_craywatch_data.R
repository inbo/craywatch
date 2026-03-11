# ====================================================
# Scriptnaam: 01_prepare_craywatch_data.R
# Project: Craywatch
# Datum: 26-11-2025
# Beschrijving:
# - Leest gevalideerde Craywatch data
# - Checkt op validatiefouten
# - Aggregeert per sessie (sampling event)
# - Slaat verwerkte data op voor gebruik in Script 03 
# ====================================================

# --- 0. Instellingen laden ---
source("./src/analysis/config.R")

# --- 1. Data inlezen ---
if (!file.exists(file_craywatch_validated)) {
  stop(paste("Bestand niet gevonden:", file_craywatch_validated))
}

craywatch_raw <- read_csv(file_craywatch_validated, show_col_types = FALSE)
map_data      <- read_csv(file_localities_map, show_col_types = FALSE)

# --- 2. Soortenlijst bepalen ---
# Soorten die in df moeten staan
required_species <- tolower(gbif_species)

# --- 3. Data Validatie Checks ---
# Zijn alle gevangen kreeften op soort gebracht?
indet_errors <- craywatch_raw %>%
  filter(soort == "crayfish indet" & number.of.individuals > 0)

if(nrow(indet_errors) > 0) {
  warning("LET OP: Er zijn 'crayfish indet' met aantallen > 0!")
  print(indet_errors)
} else {
  print("Check OK: Alle 'crayfish indet' waarnemingen zijn 0")
}

# --- 4. Sampling event definiëren ---
# Criterion: >7 days sampling interval = separate sampling event
craywatch_data <- craywatch_raw %>%
  mutate(date = dmy(date)) %>% # Zorg dat datumformaat klopt (dd-mm-yyyy verwacht)
  arrange(locID, date) %>%
  filter(str_detect(locID, "^[A-Z]_[0-9]{4}_[0-9]+$")) %>% # Filter geldige locIDs
  group_by(locID) %>%
  mutate(
    date_diff = c(0, diff(date)),
    # Nieuwe sampling event als verschil groter is dan threshold uit config
    session_nr = paste0(locID, "_", cumsum(date_diff > cray_session_gap_days) + 1)
  ) %>%
  ungroup()

# --- 5. Trapdays Berekenen per Sessie ---
# We vullen ontbrekende dagen in en imputeren het aantal vallen
  traps_per_observed_day <- craywatch_data %>%
    group_by(locID, session_nr, date) %>%
    summarize(
      traps_on_day_obs = max(number.of.traps, na.rm = TRUE),
      .groups = 'drop'
    )

  session_data <- traps_per_observed_day %>%
    group_by(locID, session_nr) %>%
    complete(date = seq(min(date), max(date), by = "day")) %>%
    fill(traps_on_day_obs, .direction = "up") %>%
    summarize(
      start_date    = min(date),
      end_date      = max(date),
      duration_days = n(),                     # Totaal aantal dagen (inclusief de gaten)
      trapdays    = sum(traps_on_day_obs),   # De som van alle dagen (geregistreerd + geïmputeerd)
      .groups = 'drop'
    )%>%                                        # Voeg ineens latlong toe
    left_join(
      map_data %>% select(locID, Latitude, Longitude),
      by = "locID"
    ) %>%
    mutate(
      Latitude  = as.numeric(gsub(",", ".", Latitude)),
      Longitude = as.numeric(gsub(",", ".", Longitude))
    )

  # --- 6. Vangsten aggregeren (catch data) ---
  
catch_data <- craywatch_data %>%
  group_by(locID, session_nr, soort) %>%
  summarize(
    individuals_caught = sum(number.of.individuals, na.rm = TRUE),
    vrijwillID         = first(vrijwillID), 
    .groups            = 'drop'
  ) %>%
  mutate(species = tolower(soort)) %>%
  select(-soort) %>%
  # Zorgt dat elke sessie rijen heeft voor ALLE required_species (ook 0 vangsten)
  complete(
    nesting(locID, session_nr, vrijwillID), 
    species = required_species,
    fill  = list(individuals_caught = 0)
  ) %>%
  filter(species != "crayfish indet") 

# --- 7. Samenvoegen en wide Format ---
# Koppel de sessie-info  aan de vangsten
craywatch_joined <- catch_data %>%
  left_join(session_data, by = c("session_nr", "locID"))

# Bereken variabelen
craywatch_calculated <- craywatch_joined %>%
  mutate(
    dat.source = "craywatch_data",
    year       = year(end_date),   # Jaar van de einddatum
    date       = start_date,       # Datum is de startdatum
    cpue_val   = individuals_caught / trapdays,     # CPUE: vangst / trapdays
    pres_val   = if_else(individuals_caught > 0, 1, 0), 
    # Protocol check: niet gevalideerde afwezigheden
    # Criterium: Als trapdays < 12 EN geen vangsten, dan is afwezigheid onzeker
    is_uncertain_zero = (trapdays < 12 & individuals_caught == 0),
    pres_final = if_else(is_uncertain_zero, NA_real_, pres_val)
  )

# Naar wide format
craywatch_wide <- craywatch_calculated %>%
  select(
    locID, vrijwillID, session_nr, dat.source, year, date, 
    Latitude, Longitude, trapdays, 
    species, pres_final, cpue_val
  ) %>%
  pivot_wider(
    names_from  = species,
    values_from = c(pres_final, cpue_val),
    names_sep   = "_"
  ) %>%
  # kolomnamen opschonen: "pres_final_soort" -> "soort"
  rename_with(~ str_remove(., "pres_final_"), starts_with("pres_final_")) %>% 
  # "cpue_val_soort" -> "CPUE_soort"
  rename_with(~ str_replace(., "cpue_val_", "CPUE_"), starts_with("cpue_val_"))

# --- 8. Opslaan ---
# We slaan op als RDS voor efficiëntie in Script 03
saveRDS(craywatch_wide, file_inter_craywatch_clean)
message(paste("Craywatch data verwerkt en opgeslagen in:", file_inter_craywatch_clean))

# Laat de eerste regels zien ter controle
print(head(craywatch_wide))


