# ====================================================
# Scriptnaam: 12_sbz_leaflet.R
# Auteur: Frédérique Steen
# Doel: Genereren van de integrale interactieve kaart voor beschermingsstatussen.
#
# Beschrijving:
# Dit script bouwt een leaflet die de verspreiding van rivierkreeften toont
# in relatie tot beschermde gebieden (Habitatrichtlijn & Soortenbeschermingsprogramma's).
#
# Belangrijkste functionaliteiten:
# 1. Dataverwerking:
#    - Koppelt waarnemingen ruimtelijk aan beschermde gebieden (SBZ-H & SBP)
#    - Berekent "wolken" (populatieclusters) rond waarnemingen om onzekerheid weer te geven
#    - Bepaalt status per wolk: "In gebied", "Nabij (<1km)" of "Op afstand"
#    - Sorteert polygonen op grootte zodat kleine gebieden zichtbaar blijven boven grote
#
# 2. User Interface:
#    - implementeert een Custom HTML/JS controlepaneel voor hiërarchische selectie
#
# 3. Visualisatie:
#    - Consistente kleurcodering per soort (voor de vulling)
#    - Randkleur geeft status aan (Rood/Oranje/Blauw)
#    - Uitgebreide legende inclusief achtergrondlagen
# ====================================================

# --- 0. Instellingen & Data laden ---
source("./src/analysis/config.R")
library(leaflet)
library(htmlwidgets)
library(htmltools) 

# Laad de basisdata en lagen via script 08
if(!exists("CF_long")) source("./src/analysis/08_load_aq_sbz.R")

message("--- Start genereren Integrale Leaflet Kaart ---")

# --- 1. Data voorbereiden: echte afwezigheden ---
message("Verwerken van afwezigheden...")

CF_absence_sf <- CF_long %>%
  filter(presence == 0) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# --- 2. Data voorbereiden: Analyse-lagen ---
message("Creëren van Analyse-lagen...")

ensure_multipolygon <- function(sf_object) {
  if (nrow(sf_object) == 0) return(sf_object)
  sf_object %>%
    st_cast("GEOMETRYCOLLECTION") %>%
    st_collection_extract("POLYGON") %>%
    st_cast("MULTIPOLYGON") %>%
    st_make_valid()
}

# 1. Habitatrichtlijngebied
hbtrl_calc <- hbtrl %>% st_transform(crs_lambert) %>% ensure_multipolygon()

# 2. SBP gebied
sbp_pgs_calc <- sbp_pgs_aq %>% st_transform(crs_lambert) %>% ensure_multipolygon()

# 3. SBP beekvissen 
if(nrow(sbp_vissen) > 0) {
  sbp_vissen_calc <- sbp_vissen %>%
    st_cast("MULTILINESTRING") %>%   
    st_transform(crs_lambert)        
} else {
  sbp_vissen_calc <- sbp_vissen
}

# Unions (voor snelle ruimtelijke checks)
if(nrow(hbtrl_calc) > 0) { union_hbtrl <- st_union(st_geometry(hbtrl_calc)) } else { union_hbtrl <- st_polygon() }

geoms_sbp <- list()
if(nrow(sbp_pgs_calc) > 0) geoms_sbp[[1]] <- st_geometry(sbp_pgs_calc)
if(nrow(sbp_vissen_calc) > 0) geoms_sbp[[2]] <- st_geometry(sbp_vissen_calc)
if(length(geoms_sbp) > 0) { union_sbp <- st_union(do.call(c, geoms_sbp)) } else { union_sbp <- st_polygon() }

# --- 3. Functie voor statusbepaling ---
calculate_cloud_status <- function(points, reference_geom, layer_name) {
  if(nrow(points) == 0) return(NULL)
  
  # A. Wolken (populatieclusters) vormen
  clouds <- points %>% st_buffer(100) %>% st_union() %>% st_cast("POLYGON") %>% st_sf()
  
  if(nrow(clouds) == 0) return(NULL)
  
  # B. Datum bepalen
  intersects <- st_intersects(clouds, points)
  clouds$first_date <- sapply(intersects, function(idx) {
    if(length(idx) > 0) {
      min_date <- min(points$date[idx], na.rm = TRUE)
      format(min_date, "%Y-%m-%d") 
    } else { NA_character_ }
  })
  
  # C. Analyse
  if(st_is_empty(reference_geom)) {
    clouds <- clouds %>% 
      mutate(analysis_layer = layer_name, status = "Op afstand", border_color = "magenta") %>% 
      st_transform(4326)
    return(clouds)
  }
  
  # 1. Check overlap
  intersect_matrix <- st_intersects(clouds, reference_geom, sparse = FALSE)
  if(length(intersect_matrix) == 0) { is_inside <- rep(FALSE, nrow(clouds)) } 
  else { is_inside <- intersect_matrix[,1] }
  
  # 2. Check nabijheid
  is_nearby <- rep(FALSE, nrow(clouds))
  idx_not_inside <- which(!is_inside)
  if(length(idx_not_inside) > 0) {
    dist_matrix <- st_is_within_distance(clouds[idx_not_inside,], reference_geom, dist = 1000, sparse = FALSE)
    if(length(dist_matrix) > 0) { is_nearby[idx_not_inside] <- dist_matrix[,1] }
  }
  
  # 3. Attributen toekennen
  clouds %>%
    mutate(
      analysis_layer = layer_name,
      status = case_when(
        is_inside ~ paste("In", layer_name),
        is_nearby ~ paste("Nabij", layer_name, "(<1km)"),
        TRUE ~ "Op afstand"
      ),
      border_color = case_when(
        is_inside ~ "red",
        is_nearby ~ "orange",
        TRUE ~ "magenta" 
      )
    ) %>%
    st_transform(4326)
}

# --- 4. Berekenen van populatieclusters ---
message("Berekenen wolken...")
clouds_hbtrl_list <- list()
clouds_sbp_list   <- list()
unique_species <- unique(CF_presence$species)

for (sp in unique_species) {
  sp_points <- CF_presence %>% 
    filter(species == sp) %>% 
    select(species, date, geometry) %>% 
    st_transform(crs_lambert)
  
  if(nrow(sp_points) == 0) next
  sp_color <- species_colors[sp]
  
  # ANALYSE A: SBZ-H
  res_hbtrl <- calculate_cloud_status(sp_points, union_hbtrl, "SBZ-H")
  if(!is.null(res_hbtrl)) {
    res_hbtrl$fill_color <- sp_color
    clouds_hbtrl_list[[sp]] <- res_hbtrl
  }
  
  # ANALYSE B: SBP
  res_sbp <- calculate_cloud_status(sp_points, union_sbp, "SBP")
  if(!is.null(res_sbp)) {
    res_sbp$fill_color <- sp_color
    clouds_sbp_list[[sp]] <- res_sbp
  }
}

# --- 5. Genereren leaflet  ---
message("Kaart opbouwen...")

map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) 

# --- A. Contextlagen ---
# Pas op! De group names moeten exact matchen met wat we in JS gebruiken.
grp_context_hbtrl <- "Context: Habitatrichtlijn"
grp_context_sbp   <- "Context: SBP"

map <- map %>%
  addPolygons(
    data = hbtrl_calc %>% st_transform(4326),
    color = "darkgreen", weight = 1.5, fill = FALSE,
    group = grp_context_hbtrl,
    popup = ~paste("<b>SBZ-H:</b>", naam)
  ) %>%
  addPolygons(
    data = natura_2000_aq %>% st_transform(4326),
    color = "lightgreen", weight = 0.5, fillColor = "lightgreen", fillOpacity = 0.4,
    group = grp_context_hbtrl,
    popup = ~paste("<b>Habitat:</b>", HAB1)
  )

map <- map %>%
  addPolygons(
    data = sbp_pgs_calc %>% 
      mutate(area = st_area(.)) %>% 
      arrange(desc(area)) %>%  # grootste polygonen onderaan
      st_transform(4326),
    color = "blue", weight = 1.0, fillColor = "blue", fillOpacity = 0.2,
    group = grp_context_sbp,
    popup = ~paste("<b>Soort:</b>", soort, "<br><b>Gebied:</b>", gebied)
  ) %>%
  addPolylines(
    data = sbp_vissen_calc %>% st_transform(4326), 
    color = "blue", weight = 2, opacity = 0.6,
    group = grp_context_sbp,
    popup = ~paste("<b>Soort:</b>", soort, "<br><b>Gebied:</b>", gebied)
  )

# --- B. Soortenlagen  ---
species_html_options <- ""
first_sp_code <- ""

for (i in seq_along(unique_species)) {
  sp <- unique_species[i]
  dutch_name <- species_labels_dutch[sp]
  if(is.na(dutch_name)) dutch_name <- sp
  
  # Code voor ID 
  sp_code <- gsub(" ", "_", sp)
  if(i == 1) first_sp_code <- sp_code
  
  # HTML opbouw voor het custom paneel
  checked_attr <- if(i == 1) "checked" else ""
  species_html_options <- paste0(
    species_html_options,
    '<div style="margin: 4px 0;">',
    '<label style="cursor:pointer; font-weight: normal;">',
    '<input type="radio" name="species_selector" value="', sp_code, '" ', checked_attr, '> ',
    dutch_name,
    '</label></div>'
  )
  
  # 1. Afwezigheden
  sp_absences <- CF_absence_sf %>% filter(species == sp)
  abs_popup <- paste0("<b>Soort:</b> ", dutch_name, "<br><b>Type:</b> Afwezigheid (0)<br><b>Datum:</b> ", sp_absences$date)
  
  # --- LAAG 1: SBZ-H resultaten ---
  grp_sbz <- paste0("SBZ_", sp_code)
  if (!is.null(clouds_hbtrl_list[[sp]])) {
    dat <- clouds_hbtrl_list[[sp]]
    popup_txt <- paste0("<b>Soort:</b> ", dutch_name, "<br><b>Analyse:</b> Habitatrichtlijn<br><b>Status:</b> ", dat$status, "<br><b>Eerste wnm:</b> ", dat$first_date)
    
    map <- map %>%
      addPolygons(
        data = dat, group = grp_sbz, 
        color = ~border_color, weight = 2, opacity = 1,
        fillColor = ~fill_color, fillOpacity = 0.4,
        popup = popup_txt
      )
    if(nrow(sp_absences) > 0) {
      map <- map %>% addCircleMarkers(data = sp_absences, group = grp_sbz, radius = 3, color = "black", weight = 1, fillColor = "white", fillOpacity = 1, popup = abs_popup)
    }
  }
  
  # --- LAAG 2: SBP resultaten ---
  grp_sbp <- paste0("SBP_", sp_code)
  if (!is.null(clouds_sbp_list[[sp]])) {
    dat <- clouds_sbp_list[[sp]]
    popup_txt <- paste0("<b>Soort:</b> ", dutch_name, "<br><b>Analyse:</b> SBP<br><b>Status:</b> ", dat$status, "<br><b>Eerste wnm:</b> ", dat$first_date)
    
    map <- map %>%
      addPolygons(
        data = dat, group = grp_sbp, 
        color = ~border_color, weight = 2, opacity = 1,
        fillColor = ~fill_color, fillOpacity = 0.4,
        popup = popup_txt
      )
    if(nrow(sp_absences) > 0) {
      map <- map %>% addCircleMarkers(data = sp_absences, group = grp_sbp, radius = 3, color = "black", weight = 1, fillColor = "white", fillOpacity = 1, popup = abs_popup)
    }
  }
}

# --- D. HTML controlepanel (Custom JS)  ---
custom_panel_html <- paste0(
  '<div id="custom-analysis-control" style="background: white; padding: 10px; border-radius: 5px; border: 1px solid #ccc; font-size: 12px; min-width: 220px;">',
  
  '  <div style="font-weight: bold; margin-bottom: 5px;">Beschermingsstatus</div>',
  '  <div style="margin-bottom: 10px;">',
  '    <label style="cursor:pointer; display:block; margin-bottom: 3px;"><input type="radio" name="analysis_selector" value="SBZ" checked> Habitatrichtlijn (SBZ-H)</label>',
  '    <label style="cursor:pointer; display:block; margin-bottom: 3px;"><input type="radio" name="analysis_selector" value="SBP"> Soortenbescherming (SBP)</label>',
  '  </div>',
  '  <div style="font-weight: bold; margin-bottom: 5px; border-top: 1px solid #eee; padding-top: 5px;">Soort</div>',
  '  <div style="max-height: 300px; overflow-y: auto;">',
  species_html_options,
  '  </div>',
  '</div>'
)

# JavaScript logica om contextlagen te togglen
map <- map %>%
  addControl(html = custom_panel_html, position = "bottomright") %>%
  htmlwidgets::onRender(
    paste0("
    function(el, x) {
      var map = this;
      
      // Hulpfunctie: Zet zichtbaarheid van een laag
      function setLayerVisibility(layer, isVisible) {
          if (isVisible) {
             // Zichtbaar maken
             if(layer.getElement) { var e = layer.getElement(); if(e) e.style.display = ''; }
             if(layer._path) layer._path.style.display = '';
             
             // Opacity terugzetten 
             if(layer.setStyle) layer.setStyle({opacity: 1, fillOpacity: 0.4});
             if(layer.setOpacity) layer.setOpacity(1);
             
          } else {
             // Onzichtbaar maken
             if(layer.getElement) { var e = layer.getElement(); if(e) e.style.display = 'none'; }
             if(layer._path) layer._path.style.display = 'none';
             
             if(layer.setStyle) layer.setStyle({opacity: 0, fillOpacity: 0});
             if(layer.setOpacity) layer.setOpacity(0);
          }
      }

      function updateMapLayers() {
        var analysis = document.querySelector('input[name=\"analysis_selector\"]:checked').value; // 'SBZ' of 'SBP'
        var species_code = document.querySelector('input[name=\"species_selector\"]:checked').value;
        var targetSpeciesGroup = analysis + '_' + species_code;

        map.eachLayer(function(layer) {
          if (layer.options && layer.options.group) {
            var g = layer.options.group;
            
            // 1. LOGICA VOOR CONTEXTLAGEN 
            if (g === 'Context: Habitatrichtlijn') {
                setLayerVisibility(layer, (analysis === 'SBZ'));
            }
            else if (g === 'Context: SBP') {
                setLayerVisibility(layer, (analysis === 'SBP'));
            }
            
            // 2. LOGICA VOOR RESULTATEN 
            else if (g.startsWith('SBZ_') || g.startsWith('SBP_')) {
              if (g === targetSpeciesGroup) {
                 setLayerVisibility(layer, true);
              } else {
                 setLayerVisibility(layer, false);
              }
            }
          }
        });
      }

      var inputs = document.getElementById('custom-analysis-control').querySelectorAll('input');
      inputs.forEach(function(input) {
        input.addEventListener('change', updateMapLayers);
      });

      setTimeout(updateMapLayers, 100);
    }
    ")
  )

# Legende
map <- map %>%
  addControl(
    html = paste0(
      "<style>",
      ".leaflet-bottom.leaflet-left .leaflet-control { min-width: 220px; margin-bottom: 10px; }",
      "</style>",
      "<div style='background:white; padding:10px; border-radius:5px; border:1px solid #ccc; font-size:12px;'>",
      "<b>Populatiestatus (Randkleur)</b><br>",
      "<i style='background:white; border: 2px solid red; width: 12px; height: 12px; display: inline-block;'></i> In gebied<br>",
      "<i style='background:white; border: 2px solid orange; width: 12px; height: 12px; display: inline-block;'></i> Nabij (<1km)<br>",
      "<i style='background:white; border: 2px solid magenta; width: 12px; height: 12px; display: inline-block;'></i> Op afstand<br>",
      "<hr style='margin:5px 0;'>",
      "<b>Afwezigheden</b><br>",
      "<i style='background:white; border: 1px solid black; border-radius: 50%; width: 8px; height: 8px; display: inline-block;'></i> Nulvangst<br>",
      "<hr style='margin:5px 0;'>",
      "<b>Gebieden & Habitats</b><br>",
      "<i style='background:white; border: 2px solid darkgreen; width: 12px; height: 12px; display: inline-block;'></i> Habitatrichtlijngebied<br>",
      "<i style='background:lightgreen; opacity:0.6; width: 12px; height: 12px; display: inline-block;'></i> Aquatische habitattypes (BWK)<br>",
      "<i style='background:blue; opacity:0.4; width: 12px; height: 12px; display: inline-block;'></i> Soortenbeschermingsprogramma (SBP)<br>",
      "</div>"
    ),
    position = "bottomleft"
  )

outfile <- file.path(dir_data_output, "maps", "bescherming", "interactieve_kaart_analyse_split.html")
if(!dir.exists(dirname(outfile))) dir.create(dirname(outfile), recursive=T)

saveWidget(map, file = outfile, selfcontained = TRUE)
message(paste("Kaart succesvol opgeslagen:", outfile))