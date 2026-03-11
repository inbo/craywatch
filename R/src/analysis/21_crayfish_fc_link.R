# ====================================================
# Scriptnaam: 21_link_fc_craywatch.R
# Project: Craywatch
# Datum laatste wijziging: 05-12-2025
# Beschrijving: 
# - Koppelt kreeftenlocaties met de fysicochemische (fc) meetpunten
# - correctie voor koppeling binnen het hydrologische netwerk (via VHAG2)
# - Past een driejarig lag-model toe en aggregeert tot zomermediaan
# ====================================================

# --- 0. Setup ---
source("./src/analysis/config.R")

library(qgisprocess)
library(mapview)

# --- 1. Data laden ---
# Waterlopen (nodig voor QGIS-analyse van aangrenzende segmenten)
waterloop <- read_sf(file_vha_catc) %>%
  st_transform(31370)

# gekoppelde entries uit de kreeftendataset (output script 04)
if (!file.exists(file_analyse_dataset_rapport)) stop("Output van script 04 niet gevonden.")
data <- read_csv(file_analyse_dataset_rapport) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(31370) %>%
  filter(link_method != "not linked")

# gekoppelde fysicochemische data (output script 19)
fc_locations_data <- file.path(dir_data_intermediate, "fysicochemie", "fc_locations_data.Rdata")
if (!file.exists(fc_locations_data)) stop("Output van script 18 (fc_locations_data) niet gevonden.")
load(fc_locations_data)

# overstorten shapefile
overstorten <- st_read(file_overstorten, quiet = TRUE) %>% st_transform(crs_lambert)

# --- 2. Ruimtelijke preprocessing ---
# creëren van een topologisch correct waterloopnetwerk
# Dit snapt lijnen aan elkaar en voegt segmenten met dezelfde VHAG samen
message("QGIS processing starten (Snap & Dissolve)...")
# Stap 1: Snap polylines om kleine gaten te dichten (threshold 0.01)
waterloop_merge <- qgis_run_algorithm(
  "grass:v.clean",
  type = "line",
  input = waterloop,
  tool = "snap",
  threshold = 0.01
)
waterloop_merge <- sf::st_as_sf(waterloop_merge)

# Stap 2: Samenvoegen (dissolve) op basis van VHAG-code
waterloop_merge <- qgis_run_algorithm(
  "native:dissolve",
  INPUT = waterloop_merge,
  SEPARATE_DISJOINT = TRUE,
  FIELD = "VHAG"
)

waterloop_merge <- sf::st_as_sf(waterloop_merge) 

# --- 3.Identificatie van aangrenzende VHAG's (VHAG2) ---
# functie bepaalt of een meetpunt nabij een segment-einde ligt (< 2km).
# Zo ja, zoekt het naar aansluitende VHAG's om discontinuïteiten te corrigeren.

dist_end_point <- function(x, data_point = data, polyline = waterloop_merge){
  
  # Controleer of primaire VHAG bekend is
  if(!is.na(data_point$VHAG[x])){
    
    # Subsets maken voor het specifieke punt en bijbehorende waterloop
    data_sub <- data_point %>% dplyr::filter(row_number() == x)
    waterloop_sub <- polyline %>% dplyr::filter(VHAG == data_sub$VHAG)
    
    # Omzetten naar sfc formaat voor geometrische bewerkingen
    data_sub_sfc <- data_sub %>% st_as_sfc()
    waterloop_sub_sfc <- waterloop_sub %>% st_as_sfc()
    
    # Bereken projectie op de segmenten en de totale lijnlengte
    measure_data <- st_line_project(waterloop_sub_sfc, data_sub_sfc)
    waterloop_length <- st_length(waterloop_sub_sfc)
    
    # Filter irrelevante segmenten (waar punt niet op projecteert)
    waterloop_length <- waterloop_length[measure_data != 0]
    measure_data <- measure_data[measure_data != 0]
    
    # Bereken afstand tot beide uiteinden (start en eind)
    measure_data_two_direct <- c(measure_data, as.numeric(waterloop_length) - measure_data)
    
    # Als punt binnen 2km van een uiteinde ligt: zoek aangrenzende VHAG
    if(any(measure_data_two_direct < 2000)){
      
      # Buffer van 1km rondom het punt
      buffer_point <- qgis_run_algorithm(
        "native:buffer",
        INPUT = data_sub,
        DISTANCE = 1000
      )
      buffer_point <- sf::st_as_sf(buffer_point) 
      
      # snijd de huidige VHAG met deze buffer
      waterloop_sub <- st_intersection(waterloop_sub, buffer_point)
      
      # kleine buffer (2m) toevoegen om 'touching' joins met andere lijnen te garanderen
      waterloop_sub <- qgis_run_algorithm(
        "native:buffer",
        INPUT = waterloop_sub,
        DISTANCE = 2
      )
      waterloop_sub <- sf::st_as_sf(waterloop_sub) 
      
      # Identificeer aangrenzende VHAG's uit de volledige dataset
      waterloop_sub_extra <- qgis_run_algorithm(
        "native:joinattributesbylocation",
        INPUT = waterloop_sub,
        PREDICATE = "cross",
        JOIN = waterloop, # ongemergde dataset
        JOIN_FIELDS = "VHAG"
      )
      
      waterloop_sub_extra <- sf::st_as_sf(waterloop_sub_extra) 
      
      # Filter de geometrie van deze gevonden VHAG2-segmenten
      waterloop_sub_extra <- waterloop%>%
        dplyr::filter(VHAG %in% waterloop_sub_extra$VHAG_2)
      
      # Koppel VHAGs die binnen 1km van het punt liggen
      data_buffer <- data_sub%>%
        st_join(., select(waterloop_sub_extra, VHAG), join = st_is_within_distance,
                dist = 1000, suffix = c("","_2"))
      
      # Resultaat opslaan
      VHAG = list(data_buffer$VHAG_2)
      
    }else{
      VHAG = NA
    }
    
  }else{
    VHAG = NA
  } 
  return(VHAG)
}

# Definieer output path
output_file <- here("data", "intermediate", "fysicochemie", "analysis_dataset_vhag2.Rdata")

# voer de functie uit als het bestand nog niet bestaat
if (file.exists(file_inter_vhag2)) {
  message("Bestaand bestand gevonden. Data wordt ingeladen...")
  load(file_inter_vhag2)
  } else {
  message("Bestand niet gevonden. Berekening wordt gestart...")
  data <- data %>%
    mutate(VHAG2 = map(1:nrow(.), dist_end_point))
  save(data, file = file_inter_vhag2)
}


# ---- 4. Fc tijdsreeks voorbereiden voor de kreeftendata ---- 
# Voeg yearGroup variabele toe om rijen te ontdubbelen
# fc van de 2 voorgaande jaren wordt meegenomen

data <- data%>%
  mutate(yearGroup=year)

data <- data%>%
  bind_rows(data%>%
              mutate(yearGroup=yearGroup-1))%>%
  bind_rows(data%>%
              mutate(yearGroup=yearGroup-2))


# --- 5. Koppeling watervlakken (WVLC) ---
# merge data op basis van WVLC (code moet nog verder uitgewerkt worden). Als er
# meerdere fc punten in een watervlak liggen wordt het zomer gemiddelde per 
# watervlak berekend

# calculate mean or concatinate
meancon <- function(x){
  if(is.numeric(x)){
    mean(x,na.rm=T)
  }else{
    paste(unique(x), collapse="; ")
  }
  
}

data_fc_wvlc_notAvg <- data%>%
  dplyr::filter(!is.na(WVLC))%>%
  inner_join(fc_locations_data%>%
               dplyr::filter(!is.na(WVLC))%>%
              st_drop_geometry,by=c("WVLC"="WVLC","yearGroup"="Year"), suffix=c("","fc"), relationship = "many-to-many")%>%
    mutate(distance_cray_FC=st_distance(., fc_locations_data[entryID, ],
                                        by_element = TRUE),
           distance_cray_FC=as.numeric(distance_cray_FC))

  

data_fc_wvlc_notAvg_all <- data%>%
  dplyr::filter(!is.na(WVLC))%>%
  inner_join(fc_locations_data%>%
               dplyr::filter(!is.na(WVLC))%>%
               st_drop_geometry,by=c("WVLC"="WVLC"), suffix=c("","fc"))%>%
  mutate(distance_cray_FC=st_distance(., fc_locations_data[entryID, ],
                                      by_element = TRUE),
         distance_cray_FC=as.numeric(distance_cray_FC))



# --- 6. Koppeling waterlopen (VHAG) ---
# Data wordt gelinkt door voor elke variabele het dichtstbijzijnde punt binnen
# een straal van 1km op dezelfde waterloop (VHAG + VHAG2) te bepalen.  

# filter op data punten met VHAG
data_fc_vhag <- data%>%
  dplyr::filter(!is.na(VHAG))

# verwijder dubbele locaties uit fc dataset
fc_loc <- fc_locations_data%>%
  select(sample_point, VHAG, geometry)%>%
  distinct(sample_point,.keep_all=T)%>%
  dplyr::filter(!is.na(VHAG))

#functie om dichtstbijzijnste waterloop punt te vinden
VHAG_nf <- function(x,data = data_fc_vhag,fc = fc_loc){
  
  fc_sub <- fc[fc$VHAG%in%c(data$VHAG[x],unlist(data$VHAG2[x])),]
  nearest_river_index <- st_nearest_feature(data[x,], fc_sub)
  distances <- st_distance(data[x,], fc_sub[nearest_river_index, ], by_element = TRUE)
  return(list(sample_point=fc_sub$sample_point[nearest_river_index],Distance=distances))
} 

# vindt dichtstbijzijnste waterloop
data_fc_vhag <- data_fc_vhag%>%
  mutate(nearestf=map(1:nrow(.),VHAG_nf),
         sample_point=map_chr(nearestf,1),
         distance_cray_FC=map_dbl(nearestf,2))

# filter op punten binnen de 500m (straal 1 km) 
data_fc_vhag <- data_fc_vhag%>%
  dplyr::filter(distance_cray_FC<1000)

# visualiseer locaties 
data_fc_vhag_sub <- data_fc_vhag%>%
  select(geometry)

fc_loc_sub <- fc_loc%>%
  dplyr::filter(sample_point %in% data_fc_vhag$sample_point)

mapview(waterloop)+mapview(data_fc_vhag_sub, col.regions="orange")+mapview(fc_loc_sub, col.regions="red")

# --- 7. Overstorten filter  ---
# Punten waarbij een overstort tussen het kreeften- en VMM-meetpunt ligt 
# worden hieronder verwijderd (code momenteel inactief)

# link vhag aan overstorten via vhag_segm
# filter overstorten die op VHAG van data liggen
# overstorten_data <- overstorten%>%
#   dplyr::filter(VHAG %in% data_fc_vhag$VHAVispuntWaterloopGewestCode)
# # filter overstorten die op VHAG van data liggen
# overstorten_data <- overstorten%>%
#   dplyr::filter(VHAG %in% data_fc_vhag$VHAVispuntWaterloopGewestCode)
# 
# # functie ligt overstort tussen staalnamepunten en verwijder
# overstort_filter <- function(x, overstort=overstorten_data, data=data_fc_vhag, fc=fc_loc){
#   
#   # create data subsets (enkel overstorten die op dezelfde VHAG liggen)
#   overstort_sub <- overstort[overstort$VHAG%in%c(data$VHAG[x],unlist(data$VHAG2[x])) & !is.na(overstort$VHAG),]
#   data_sub <- do.call("rbind", replicate(nrow(overstort_sub), data[x,], simplify = FALSE))
#   
#   if(nrow(overstort_sub)>0){ # check of er een overstort op de waterloop ligt
#     
#     # bepaal afstanden tussen overstorten en data punt
#     distances <- st_distance(data_sub, overstort_sub, by_element = TRUE)
#     
#     # filter op overstorten die dichter bij liggen dan het VMM punt
#     overstort_sub <- overstort_sub[as.numeric(distances)<data$distance_cray_FC[x], ]
#     
#     
#     if(nrow(overstort_sub)>0){ # check of er overstorten dichter dan het VMM punt liggen
#       
#       # maak VMM subset data
#       fc_sub <- do.call("rbind", replicate(nrow(overstort_sub), fc[fc$sample_point==data$sample_point[x],], simplify = FALSE))
#       # bepaal afstand tussen overstort en VMM punt
#        distances2 <- st_distance(fc_sub, overstort_sub, by_element = TRUE)
#        
#        # als afstand oversort VMM < afstand datapunt VMM -> issue
#         if(any(as.numeric(distances2)<data$distance_cray_FC[x])){
#           
#           overstort_issue=T
#           
#         }else{overstort_issue=F}
#         
#     }else{overstort_issue=F}
#     
#   }else{overstort_issue=F}
#   
#   return(overstort_issue)
# }
# 
# data_fc_vhag <- data_fc_vhag%>%
#   mutate(overstort_issue=map(1:nrow(.),overstort_filter))
# 
# #visualiseer oversort issues
# waterloop_sub <- waterloop%>%
#   dplyr::filter(VHAG%in%data_fc_vhag$VHAG[data_fc_vhag$overstort_issue==T])
  # er zijn geen overstorten aanwezig tussen de kreeften en FC punten


# --- 8. Samenvoegen datasets ---
# merge data_fc_vhag en fc_locations_data
data_fc_vhag_notAvg <- data_fc_vhag%>%
  left_join(fc_locations_data%>%
              dplyr::filter(!is.na(VHAG))%>%
              st_drop_geometry(),by=c("sample_point"="sample_point","yearGroup"="Year", "VHAG"="VHAG"),suffix=c("","fc"))


data_fc_vhag_notAvg_all <- data_fc_vhag%>%
  left_join(fc_locations_data%>%
              dplyr::filter(!is.na(VHAG))%>%
              st_drop_geometry(),by=c("sample_point"="sample_point","VHAG"="VHAG"),suffix=c("","fc"))



 
# --- 9. Export en data aggregatie ---
# ruwe data file met alle fc data per locatie
data_fc_cray_notAvg <- data_fc_wvlc_notAvg_all%>%
  bind_rows(data_fc_vhag_notAvg_all)%>%
  select(-c(WVLCfc, entryID,
            distance_cray_FC, VHAGfc))%>%
  rename(distances_cray_VHAG_WVLC=distances)%>%
  select(-c(nearestf,VHAG2))%>%
  dplyr::filter(!is.na(MaandNr))%>%
  st_drop_geometry()

write.table(data_fc_cray_notAvg,file=here("data","intermediate","fysicochemie","data_fc_cray_notAvg.txt"),sep="\t",row.names=F)

# filter op zomermaanden fc data per locatie
data_fc_cray_notAvg_zomer <- data_fc_wvlc_notAvg%>%
  bind_rows(data_fc_vhag_notAvg)%>%
  select(-c(WVLCfc,  entryID,
            distance_cray_FC, VHAGfc))%>%
  rename(distances_cray_VHAG_WVLC=distances)%>%
  select(-c(nearestf,VHAG2))%>%
  dplyr::filter(!is.na(MaandNr))%>%
  dplyr::filter(MaandNr%in%5:10)%>%
  st_drop_geometry()

write.table(data_fc_cray_notAvg_zomer,file=here("data","intermediate", "fysicochemie","data_fc_cray_notAvg_summer.txt"),sep="\t",row.names=F)


# bereken mediaan mei:oktober metingen
  mediancon <- function(x){
    if(is.numeric(x)){
      median(x,na.rm=TRUE)
    }else{
      paste(unique(x), collapse="; ")
    }
    
  }

  # file met mediane fc waarden van de zomerwaarden per locatie
data_fc_cray <- data_fc_cray_notAvg_zomer%>%
  group_by(vangstID)%>%
  mutate(across(c(sample_point:oPO4,distances_cray_VHAG_WVLC),~mediancon(.x)))%>%
  distinct(vangstID,.keep_all = T)%>%
  st_drop_geometry()%>%
  mutate_all(.,~ifelse(is.nan(.), NA, .)) #%>%
  #select(-c(BZV5,`Clfyl a`,`Ca o`, oPO4))



write.table(data_fc_cray,file=here("data","intermediate", "fysicochemie", "data_fc_cray_analysis.txt"),sep="\t",row.names=F)




