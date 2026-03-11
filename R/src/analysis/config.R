## =========================================================
## config.R – centrale instellingen voor craywatch data pipe
## =========================================================

## ---------- Libraries  ----------
required_packages <- c(
  "here",       # Relatieve paden
  "purrr",
  "sf",         # Ruimtelijke data
  "dplyr",      
  "tidyr",      
  "lubridate",  # Datums
  "readr",      # CSV lezen/schrijven
  "stringr",    
  "rgbif",      # GBIF API
  "leaflet",    # Interactieve kaarten
  "ggplot2",
  "htmltools",
  "ggpattern",
  "htmlwidgets",  # Voor opslaan leaflets (script 06)
  "RColorBrewer", # Kleurpaletten (script 06)
  "rlang",        # Programmeer tools (script 05, 06)
  "lwgeom",       # Ruimtelijke berekeningen (script 04)
  "mapview",      # Interactieve kaarten (script 04)
  "flextable",    # Tabellen opmaak (script 07, 10)
  "officer"       # Word export (script 07, 10)
)


invisible(lapply(required_packages, library, character.only = TRUE))

## ---------- Project paden ----------
# Root van dit rapport-repo
root_rapport <- here::here()

# Parallelle craywatch repo (../craywatch)
root_craywatch_app <- root_rapport

## ---------- Mappenstructuur ----------
dir_data_analysis     <- file.path(root_rapport, "data", "analysis")
dir_data_input        <- file.path(dir_data_analysis, "input")
dir_data_intermediate <- file.path(dir_data_analysis, "intermediate")
dir_data_output       <- file.path(dir_data_analysis, "output")
dir_bescherming_output <- file.path(dir_data_output, "maps", "bescherming")

dir_gbif_input        <- file.path(dir_data_input, "gbif")
dir_gbif_intermediate <- file.path(dir_data_intermediate, "gbif")
dir_shapefiles        <- file.path(dir_data_input, "shapefiles")

dir_craywatch_assets  <- file.path(root_craywatch_app, "assets")

dir_urbanisation_output <- file.path(dir_data_output, "urbanisation")
dir_watertype_output <- file.path(dir_data_output, "watertype")
dir_gridcell_output <- file.path(dir_data_output, "gridcell")
dir_craywatch_output  <- file.path(root_craywatch_app, "R", "data", "output")
dir_trends_output <- file.path(dir_data_output, "fc_trends")

dir_citizen_stats_output <- file.path(dir_data_output, "citizen_stats")
dir_maps_output <- file.path(dir_data_output, "maps")
dir_maps_cpue   <- file.path(dir_data_output, "maps", "cpue")
dir_maps_craywatch <- file.path(dir_maps_output, "craywatch")
dir_maps_catc      <- file.path(dir_maps_output, "catc")
dir_maps_deelbekken <- file.path(dir_maps_output, "deelbekken")


required_dirs <- c(
  dir_data_input, dir_data_intermediate, dir_data_output,dir_bescherming_output,
  dir_gbif_input, dir_gbif_intermediate, dir_shapefiles,
  dir_craywatch_output, dir_urbanisation_output, dir_watertype_output,
  dir_maps_output, dir_maps_cpue, dir_gridcell_output, dir_citizen_stats_output, 
  dir_maps_craywatch, dir_maps_catc, dir_trends_output, dir_maps_deelbekken
)

invisible(lapply(required_dirs, function(x) {
  if (!dir.exists(x)) dir.create(x, recursive = TRUE)
}))


## ---------- Bestanden: input ----------
# Ruwe data
file_craywatch_validated <- file.path(dir_data_input, "craywatch", "craywatch_data.csv") # 10.5281/zenodo.17639074
file_localities_map      <- file.path(dir_craywatch_assets, "localities.csv")

# Shapefiles 
file_vlaanderen_grenzen <- file.path(dir_shapefiles, "grenzenvlaanderen.shp")
file_watervlakken       <- file.path(dir_shapefiles, "watervlakken.shp")
file_vha_catc           <- file.path(dir_shapefiles, "vhaCattraj.shp")
file_bekken             <- file.path(dir_shapefiles, "Wsbekken.shp")
file_overstorten        <- file.path(dir_shapefiles, "P_OS_uitlaat_VHA.shp")
file_watergang          <- file.path(dir_shapefiles, "Wtz.shp")
file_waterloopsegmenten <- file.path(dir_shapefiles, "VHA_waterlopen_VHA_waterloopsegment.shp")

# Shapefiles gebruikt in ruimtelijke analyse
file_n2000_habitats <- file.path(dir_shapefiles, "BwkHab.shp") # natura2000
file_3260_habitats  <- file.path(dir_shapefiles, "Hab3260.shp") #Ranunculoides hab 3260

# Shapefiles gebruikt ter projectie in kaarten
file_hoofdrivieren  <- file.path(dir_shapefiles, "hoofdrivieren.shp") # rivieren
file_kanalen        <- file.path(dir_shapefiles, "kanalen.shp") # kanalen
file_gemeenten      <- file.path(dir_shapefiles, "gemeenten.shp") # gemeentegrenzen

# Shapefile gebruikt voor urbanisatie
file_urban_map <- file.path(dir_shapefiles, "verstedelijking.gpkg")


## ---------- Bestanden: Intermediate (RDS) ----------
file_inter_craywatch_clean <- file.path(dir_data_intermediate, "craywatch_clean.rds")
file_inter_gbif_processed  <- file.path(dir_data_intermediate, "gbif", "gbif_processed.rds")
file_inter_vhag2           <- file.path(dir_data_intermediate, "fysicochemie", "analysis_dataset_vhag2.Rdata")
file_fc_locations_data     <- file.path(dir_data_intermediate, "fysicochemie", "fc_locations_data.Rdata")
file_fc_locations_validated<- file.path(dir_data_intermediate, "fysicochemie", "fc_locations_validated.Rdata")

# GBIF raw output 
file_gbif_occurrences      <- file.path(dir_gbif_input, "gbif_occ_CF.csv")

## ---------- Bestanden: Output ----------
file_analyse_dataset_rapport   <- file.path(dir_data_output, "analyse_dataset.csv")
file_analyse_dataset_craywatch <- file.path(dir_craywatch_output, "analyse_dataset.csv")

# Output paden voor kaarten sbz
file_map_hbtrl <- file.path(dir_bescherming_output, "map_hbtrl.png")
file_map_sbp <- file.path(dir_bescherming_output, "map_sbp.png")

# Output plot bestanden voor stedelijk type
file_plot_urban_distributie <- file.path(dir_urbanisation_output, "plot_urban_distributie.png")
file_plot_urban_community   <- file.path(dir_urbanisation_output, "plot_urban_community.png")
file_plot_urban_vangstsucces<- file.path(dir_urbanisation_output, "plot_urban_vangstsucces.png")

# Output plot bestanden voor watertype
file_plot_water_distributie  <- file.path(dir_watertype_output, "plot_water_distributie.png")
file_plot_water_community    <- file.path(dir_watertype_output, "plot_water_community.png")
file_plot_water_vangstsucces <- file.path(dir_watertype_output, "plot_water_vangstsucces.png")

## ---------- GBIF download instellingen ----------
gbif_species <- c(
  "Procambarus clarkii",
  "Procambarus virginalis",
  "Procambarus acutus",
  "Faxonius limosus",
  "Pacifastacus leniusculus",
  "Faxonius virilis",
  #"Faxonius immunis",
  #"Faxonius juvenilis",
  #"Faxonius rusticus",
  "Pontastacus leptodactylus"
)

gbif_country          <- "BE"
gbif_min_year         <- 2010L
gbif_occurrence_state <- "PRESENT"

# GBIF API credentials via environment 
gbif_user  <- Sys.getenv("GBIF_USER")   
gbif_pwd   <- Sys.getenv("GBIF_PWD")
gbif_email <- Sys.getenv("GBIF_EMAIL")

# Directory waarin de zip van occ_download wordt bewaard
dir_gbif_download <- dir_gbif_intermediate

## ---------- GBIF filterparameters ----------
gbif_issues_to_discard <- c(
  "ZERO_COORDINATE",
  "COORDINATE_OUT_OF_RANGE",
  "COORDINATE_INVALID",
  "COUNTRY_COORDINATE_MISMATCH"
)

gbif_id_status_to_discard <- c(
  "unverified", "not validated","under validation"
)

gbif_max_coordinate_uncertainty_m <- 100  # meter
gbif_required_level1_name         <- "Vlaanderen"

## ---------- Craywatch protocolparameters ----------
# Sessies: nieuwe sessie als er > 7 dagen tussen zit
cray_session_gap_days <- 7L

# Minimale trapdays voor geldige afwezigheid (crayfish indet)
cray_min_trapdays_absence <- 12L

# Drempel voor CPUE-filter: bij traps_used < 12 én CPUE == 0 → NA
cray_min_traps_for_confident_zero <- 12L

## ---------- Ruimtelijke instellingen ----------
crs_wgs84   <- 4326 #GPS
crs_lambert <- 31370 # Belgische Lambert 72

# URL voor WFS/API Deelbekken (Watersystemen)
# OGC API Features URL (Geopunt) - Collectie: WsDeelbek
url_wfs_deelbekken <- "https://geo.api.vlaanderen.be/Watersystemen/ogc/features/collections/WsDeelbek/items?f=json&limit=5000"
url_hbrl <- "https://www.mercator.vlaanderen.be/raadpleegdienstenmercatorpubliek/ps/wfs?SERVICE=WFS&VERSION=2.0.0&REQUEST=GetFeature&TYPENAMES=ps:ps_hbtrl"
url_sbp_pgs <- "https://www.mercator.vlaanderen.be/raadpleegdienstenmercatorpubliek/wfs?service=WFS&version=2.0.0&request=GetFeature&typeNames=lu:lu_sbp_pgs"
url_sbp_pls <- "https://www.mercator.vlaanderen.be/raadpleegdienstenmercatorpubliek/wfs?service=WFS&version=2.0.0&request=GetFeature&typeNames=lu:lu_sbp_pls"

# Filter voor aquatische habitats en soorten
aqua_habcodes <- c('3270', '3260', '3130', '3110', '3140', '3150', '3160', '7230')
aquatische_sbp <- c('Grote modderkruiper', 'Poelkikker', 'Kamsalamander', 'Vroedmeesterpad', 'Heikikker', 'Rugstreeppad', 'Boomkikker', 'Knoflookpad')

# Uniforme kleurenschaal over rapport
species_colors <- c(
  "faxonius limosus"          = "#FFD700",
  "procambarus clarkii"       = "#FF0000", 
  "procambarus virginalis"    = "#FF00FF",
  "faxonius virilis"          = "#FF8C00", 
  "procambarus acutus"        = "#000000", 
  "pontastacus leptodactylus" = "#00FFFF", 
  "pacifastacus leniusculus"  = "#8A2BE2",
  "Afwezigheid"               = "#BEBEBE"
)

species_labels_dutch <- c( 
  "faxonius limosus"          = "Gevlekte Amerikaanse rivierkreeft",
  "procambarus clarkii"       = "Rode Amerikaanse rivierkreeft", 
  "procambarus virginalis"    = "Marmerkreeft", 
  "faxonius virilis"          = "Geknobbelde Amerikaanse rivierkreeft", 
  "procambarus acutus"        = "Gestreepte Amerikaanse rivierkreeft",
  "pontastacus leptodactylus" = "Turkse rivierkreeft",
  "pacifastacus leniusculus"  = "Californische rivierkreeft",
  "absence"                   = "Afwezigheid"
)

color_scale_dutch <- scale_color_manual(values = species_colors, labels = species_labels_dutch)

# baseplot functie

# Definieer de standaardkleur voor waterlopen (uniform)
col_water_uniform <- "#6BA1D3" 

get_baseplot <- function() {
  # Shapefiles inlezen
  vlaanderen <- st_read(file_vlaanderen_grenzen, quiet = TRUE) %>% st_transform(31370)
  vha_raw    <- st_read(file_vha_catc, quiet = TRUE) %>% st_transform(31370)
  
  cat0 <- vha_raw %>% filter(CATC == 0) %>% st_intersection(vlaanderen)
  cat1 <- vha_raw %>% filter(CATC == 1) %>% st_intersection(vlaanderen)
  
  ggplot() +
    geom_sf(data = vlaanderen, fill= "#EEEEEE", size=0.2, colour= "black") +
    # BEIDE categorieën krijgen hier dezelfde kleur (uniform)
    geom_sf(data = cat1, size=0.3, colour=col_water_uniform) +
    geom_sf(data = cat0, size=0.4, colour=col_water_uniform) +
    theme_void() +
    theme(legend.title = element_blank(), 
          legend.text = element_text(size=8, face="italic"), 
          legend.key.size = unit(0.2, "cm"),
          legend.position = "bottom",
          plot.title = element_text(face = "italic"))
}


## ---------- Urbanisatie instellingen ----------
# Factor levels 
urban_levels <- c("landelijk", "randstedelijk", "verstedelijkt")

# Kleurenpalet urbanisatie
urban_colors <- c(
  "landelijk"     = "#1B5E20", 
  "randstedelijk" = "#A29B63", 
  "verstedelijkt" = "#707070" 
)


## ---------- watertype instellingen ----------
# Factor levels
water_levels <- c("gesloten", "open")

# Kleurenpalet 
water_colors <- c(
  "gesloten" = "#2A5959", 
  "open"   = "#2A8C8C"  
)

## ---------- fysico-chemie ----------
# Parameters die we willen behouden uit de brede dataset
# Links = Nieuwe naam (clean), Rechts = Oude naam (in raw data)
fc_parameter_map <- c(
  "O2"   = "O2",
  "BZV5" = "BZV5",
  "pH"   = "pH",
  "T"    = "T",
  "EC20" = "EC 20",
  "Pt"   = "P t",
  "Nt"   = "N t",
  "Cl"   = "Cl-",
  "ZS"   = "ZS",
  "Secchi" = "Secchi",    # Als naam gelijk blijft
  "Ca"     = "Ca o",      # Hernoem 'Ca o' naar 'Ca'
  "oPO4"   = "oPO4"
  # Chlorofyl wordt dynamisch gezocht via contains("Clfyl")
)

# Filter voor het groeiseizoen (maandnummers)
fc_season_months <- 5:10

## ---------- koppeling riviekreeftendata ----------
# Maximale afstand (m) tot dichtstbijzijnde waterloop/watervlak om VHAG/CATC/WVLC toe te kennen
max_link_distance_m <- 10 #(Zet op 15)


## ---------- Qgis omgevingsconfiguratie ----------
# 1. Basispad van de QGIS installatie (DOS-formaat)
qgis_root_path <- "C:/PROGRA~1/QGIS34~1.11" 

# 2. Stel de Omgevingsvariabelen in
Sys.setenv(OSGEO4W_ROOT = qgis_root_path)

qgis_bin_path <- file.path(qgis_root_path, "bin")
# Voeg de bin map toe aan de PATH.
Sys.setenv(PATH = paste(qgis_bin_path, Sys.getenv("PATH"), sep = ";"))

Sys.setenv(QGIS_PREFIX_PATH = file.path(qgis_root_path, "apps", "qgis-ltr"))
Sys.setenv(PYTHONHOME = file.path(qgis_root_path, "apps", "Python312"))

## --------------------