# 02_app_visualize.R
# GBIF-kreeften (kleur per soort + legenda) + locations als vierkanten (zelfde grootte)
# OSM/Light baselagen; klikpunten als ORANJE, VERPLAATSBARE markers -> CSV
suppressPackageStartupMessages({
  library(shiny)
  library(leaflet)
  library(dplyr)
  library(readr)
})

# ---------- Projectroot detectie ----------
find_project_root <- function(start = getwd(), max_up = 6) {
  cur <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in seq_len(max_up)) {
    if (dir.exists(file.path(cur, "data")) || dir.exists(file.path(cur, ".git"))) return(cur)
    parent <- normalizePath(file.path(cur, ".."), winslash = "/", mustWork = FALSE)
    if (parent == cur) break
    cur <- parent
  }
  normalizePath(start, winslash = "/", mustWork = FALSE)
}
PROJECT_ROOT <- find_project_root()

# Paden
DATA_RDS   <- file.path(PROJECT_ROOT, "data", "input", "gbif_occ", "gbif_crayfish_flanders.rds")
LOC24_PATH <- file.path(PROJECT_ROOT, "data", "output", "locations", "locations_2024.csv")
LOC25_PATH <- file.path(PROJECT_ROOT, "data", "output", "locations", "locations_2025.csv")
OUT_DIR    <- file.path(PROJECT_ROOT, "data", "output")

# ---------- Instellingen ----------
POINT_RAD_PX <- 5                 # radius GBIF-cirkels (px)
SQ_SIZE_PX   <- 2 * POINT_RAD_PX  # vierkant-diameter (px) = 2*radius

# ---------- Helpers ----------
standardize_lonlat <- function(df) {
  nms <- tolower(names(df))
  lon_idx <- match(TRUE, nms %in% c("decimallongitude","longitude","lon","long","lng","x"))
  lat_idx <- match(TRUE, nms %in% c("decimallatitude","latitude","lat","y"))
  if (is.na(lon_idx) || is.na(lat_idx)) stop("Kon geen longitude/latitude-kolommen vinden in extra laag.")
  df |>
    rename(
      decimalLongitude = !!names(df)[lon_idx],
      decimalLatitude  = !!names(df)[lat_idx]
    ) |>
    mutate(
      decimalLongitude = as.numeric(decimalLongitude),
      decimalLatitude  = as.numeric(decimalLatitude)
    ) |>
    filter(!is.na(decimalLongitude), !is.na(decimalLatitude))
}

# Vierkant-icoon (pixelgroot) voor locations
make_square_icon <- function(color = "#2c7fb8", size_px = SQ_SIZE_PX) {
  col_enc <- gsub("#", "%23", color)
  svg <- sprintf(
    "<svg xmlns='http://www.w3.org/2000/svg' width='%d' height='%d'>
       <rect x='0' y='0' width='%d' height='%d' fill='%s' stroke='%s' stroke-width='1'/>
     </svg>",
    size_px, size_px, size_px, size_px, col_enc, col_enc
  )
  leaflet::icons(
    iconUrl = paste0("data:image/svg+xml;utf8,", svg),
    iconWidth = size_px, iconHeight = size_px,
    iconAnchorX = size_px/2, iconAnchorY = size_px/2
  )
}

# Oranje cirkel-icoon (normaal en geselecteerd)
make_orange_circle_icon <- function(radius_px = POINT_RAD_PX + 2,
                                    fill = "#ff8c00", stroke = "#ff8c00", stroke_w = 2) {
  size_px <- 2 * radius_px
  f_enc <- gsub("#", "%23", fill)
  s_enc <- gsub("#", "%23", stroke)
  svg <- sprintf(
    "<svg xmlns='http://www.w3.org/2000/svg' width='%d' height='%d'>
       <circle cx='%d' cy='%d' r='%d' fill='%s' stroke='%s' stroke-width='%d'/>
     </svg>",
    size_px, size_px, radius_px, radius_px, radius_px, f_enc, s_enc, stroke_w
  )
  leaflet::icons(
    iconUrl = paste0("data:image/svg+xml;utf8,", svg),
    iconWidth = size_px, iconHeight = size_px,
    iconAnchorX = radius_px, iconAnchorY = radius_px
  )
}

# ---------- UI ----------
ui <- fluidPage(
  titlePanel("GBIF kreeften in Vlaanderen — context + locations + dragbare/verwijderbare clicks"),
  sidebarLayout(
    sidebarPanel(
      actionButton("clear_clicks", "Clear clicks"),
      actionButton("save_clicks",  "Save (ManualSel_YYYY-MM-DD.csv)"),
      helpText("Klik op de kaart om een oranje marker te plaatsen. Sleep om te verplaatsen."),
      helpText("Klik een oranje marker om te selecteren en druk op Delete/Backspace om te verwijderen.")
    ),
    mainPanel(
      uiOutput("file_status"),
      leafletOutput("map", height = 650),
      tags$hr(),
      h4("Geklikte coördinaten"),
      tableOutput("click_table")
    )
  )
)

# ---------- Server ----------
server <- function(input, output, session) {
  
  # Status
  output$file_status <- renderUI({
    tags$div(
      tags$p(paste0("GBIF RDS: ", DATA_RDS, " — ", ifelse(file.exists(DATA_RDS), "gevonden", "NIET gevonden"))),
      tags$p(paste0("Locations 2024: ", LOC24_PATH, " — ", ifelse(file.exists(LOC24_PATH), "gevonden", "niet gevonden (optioneel)"))),
      tags$p(paste0("Locations 2025: ", LOC25_PATH, " — ", ifelse(file.exists(LOC25_PATH), "gevonden", "niet gevonden (optioneel)")))
    )
  })
  
  # Data
  req(file.exists(DATA_RDS))
  occ <- readRDS(DATA_RDS)
  if (!all(c("decimalLongitude","decimalLatitude") %in% names(occ))) stop("RDS mist decimalLongitude/decimalLatitude.")
  if ("species" %in% names(occ))       occ$sp <- as.character(occ$species)
  else if ("scientificName" %in% names(occ)) occ$sp <- as.character(occ$scientificName)
  else occ$sp <- "Onbekend"
  
  occ <- occ |>
    mutate(
      decimalLongitude = as.numeric(decimalLongitude),
      decimalLatitude  = as.numeric(decimalLatitude)
    ) |>
    filter(!is.na(decimalLongitude), !is.na(decimalLatitude))
  
  species_levels <- sort(unique(occ$sp))
  pal <- colorFactor(palette = "Set1", domain = species_levels)
  
  loc24 <- try({ if (file.exists(LOC24_PATH)) readr::read_csv(LOC24_PATH, show_col_types = FALSE) |> standardize_lonlat() else tibble() }, silent = TRUE)
  loc25 <- try({ if (file.exists(LOC25_PATH)) readr::read_csv(LOC25_PATH, show_col_types = FALSE) |> standardize_lonlat() else tibble() }, silent = TRUE)
  
  icon24 <- make_square_icon(color = "#2c7fb8", size_px = SQ_SIZE_PX) # blauw
  icon25 <- make_square_icon(color = "#41ab5d", size_px = SQ_SIZE_PX) # groen
  icon_orange     <- make_orange_circle_icon(radius_px = POINT_RAD_PX + 2, fill = "#ff8c00", stroke = "#ff8c00", stroke_w = 2)
  icon_orange_sel <- make_orange_circle_icon(radius_px = POINT_RAD_PX + 2, fill = "#ff8c00", stroke = "#000000", stroke_w = 3)
  
  # Reactieve opslag voor klikpunten + selectie
  clicks <- reactiveVal(tibble(id = character(), lon = numeric(), lat = numeric()))
  selected_id <- reactiveVal(NULL)
  
  # Map
  output$map <- renderLeaflet({
    m <- leaflet(options = leafletOptions(minZoom = 7)) %>%
      # baselayers
      addTiles(group = "OSM") %>%
      addProviderTiles("CartoDB.Positron", group = "Light") %>%
      addLayersControl(
        baseGroups = c("OSM", "Light"),
        overlayGroups = c("GBIF occurrences", "Locations 2024", "Locations 2025", "Manual clicks"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      fitBounds(2.5, 50.68, 5.90, 51.50)
    
    # JS: maak Manual clicks draggable; rapporteer dragend; Delete/Backspace -> delete
    htmlwidgets::onRender(m, "
      function(el, x) {
        var map = this;
        function hook(layer){
          try {
            if (layer && layer.options && layer.options.group === 'Manual clicks') {
              if (layer.dragging) layer.dragging.enable();
              layer.on('dragend', function(e){
                var ll = layer.getLatLng();
                var id = layer.options.layerId || null;
                Shiny.setInputValue(el.id + '_manual_drag', {
                  id: id, lng: ll.lng, lat: ll.lat, nonce: Math.random()
                }, {priority: 'event'});
              });
            }
          } catch(e) {}
        }
        map.eachLayer(hook);
        map.on('layeradd', function(e){ hook(e.layer); });

        // keydown Delete/Backspace -> trigger delete
        document.addEventListener('keydown', function(evt){
          if (evt.key === 'Delete' || evt.key === 'Backspace') {
            Shiny.setInputValue(el.id + '_manual_delete_key', {nonce: Math.random()}, {priority: 'event'});
          }
        }, false);
      }
    ")
  })
  
  # GBIF-punten: kleur per soort + legenda
  observe({
    leafletProxy("map") %>%
      clearGroup("GBIF occurrences") %>%
      addCircleMarkers(
        lng = occ$decimalLongitude, lat = occ$decimalLatitude,
        radius = POINT_RAD_PX, stroke = FALSE,
        fillColor = pal(occ$sp), color = pal(occ$sp), fillOpacity = 0.7,
        group = "GBIF occurrences",
        popup = sprintf(
          "<b>%s</b><br/>Jaar: %s<br/>occID: %s",
          occ$sp,
          if ("year" %in% names(occ)) ifelse(is.na(occ$year), "?", occ$year) else "?",
          if ("occurrenceID" %in% names(occ)) ifelse(is.na(occ$occurrenceID), "", occ$occurrenceID) else ""
        )
      ) %>%
      clearControls() %>%
      addLegend(position = "bottomleft",
                colors = pal(species_levels),
                labels = species_levels,
                opacity = 1, title = "Soort")
  })
  
  # Locations als vierkanten (zelfde pixelgrootte)
  observe({
    proxy <- leafletProxy("map") %>% clearGroup("Locations 2024") %>% clearGroup("Locations 2025")
    if (is.data.frame(loc24) && nrow(loc24) > 0) {
      proxy <- proxy %>% addMarkers(
        lng = loc24$decimalLongitude, lat = loc24$decimalLatitude,
        icon = icon24, group = "Locations 2024",
        options = markerOptions(keyboard = FALSE, riseOnHover = TRUE)
      )
    }
    if (is.data.frame(loc25) && nrow(loc25) > 0) {
      proxy <- proxy %>% addMarkers(
        lng = loc25$decimalLongitude, lat = loc25$decimalLatitude,
        icon = icon25, group = "Locations 2025",
        options = markerOptions(keyboard = FALSE, riseOnHover = TRUE)
      )
    }
  })
  
  # Helper: herteken oranje markers (met selectie-icoon)
  redraw_manual_markers <- function() {
    cur <- clicks()
    sel <- selected_id()
    proxy <- leafletProxy("map") %>% clearGroup("Manual clicks")
    if (!nrow(cur)) return(invisible(NULL))
    # maak een icons-set met per punt (geselecteerd/niet) juiste iconUrl etc.
    sel_mask <- cur$id == sel
    icons_set <- leaflet::icons(
      iconUrl     = ifelse(sel_mask, icon_orange_sel$iconUrl, icon_orange$iconUrl),
      iconWidth   = ifelse(sel_mask, icon_orange_sel$iconWidth, icon_orange$iconWidth),
      iconHeight  = ifelse(sel_mask, icon_orange_sel$iconHeight, icon_orange$iconHeight),
      iconAnchorX = ifelse(sel_mask, icon_orange_sel$iconAnchorX, icon_orange$iconAnchorX),
      iconAnchorY = ifelse(sel_mask, icon_orange_sel$iconAnchorY, icon_orange$iconAnchorY)
    )
    proxy %>% addMarkers(
      lng = cur$lon, lat = cur$lat,
      layerId = cur$id,
      icon = icons_set,
      group = "Manual clicks",
      options = markerOptions(draggable = TRUE, keyboard = FALSE, riseOnHover = TRUE)
    )
  }
  
  # Klik op kaart -> nieuw verplaatsbaar punt
  observeEvent(input$map_click, {
    clk <- input$map_click
    if (is.null(clk$lng) || is.null(clk$lat)) return()
    cur <- clicks()
    new_id <- paste0("pt_", as.integer(1e6 * as.numeric(Sys.time())), "_", nrow(cur) + 1)
    cur <- bind_rows(cur, tibble(id = new_id, lon = as.numeric(clk$lng), lat = as.numeric(clk$lat)))
    clicks(cur)
    selected_id(new_id)  # selecteer meteen nieuw punt
    redraw_manual_markers()
  })
  
  # Klik op marker -> selecteer (alleen Manual clicks)
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (is.null(click$id) || is.null(click$group)) return()
    if (identical(click$group, "Manual clicks")) {
      selected_id(click$id)
      redraw_manual_markers()
    }
  })
  
  # Drag-end -> update coords
  observeEvent(input$map_manual_drag, {
    info <- input$map_manual_drag
    if (is.null(info$id) || is.null(info$lng) || is.null(info$lat)) return()
    cur <- clicks()
    idx <- match(info$id, cur$id)
    if (!is.na(idx)) {
      cur$lon[idx] <- as.numeric(info$lng)
      cur$lat[idx] <- as.numeric(info$lat)
      clicks(cur)
      redraw_manual_markers()
    }
  }, ignoreInit = TRUE)
  
  # Delete via toetsenbord
  observeEvent(input$map_manual_delete_key, {
    sel <- selected_id()
    if (is.null(sel) || !nzchar(sel)) return()
    cur <- clicks()
    if (!(sel %in% cur$id)) return()
    cur <- cur[cur$id != sel, , drop = FALSE]
    clicks(cur)
    selected_id(NULL)
    redraw_manual_markers()
  })
  
  # Clear & tabel
  observeEvent(input$clear_clicks, {
    clicks(tibble(id = character(), lon = numeric(), lat = numeric()))
    selected_id(NULL)
    leafletProxy("map") %>% clearGroup("Manual clicks")
  })
  
  output$click_table <- renderTable({
    df <- clicks(); if (!nrow(df)) return(NULL)
    df %>% transmute(longitude = round(lon, 6), latitude = round(lat, 6))
  })
  
  # Save naar data/output/ManualSel_YYYY-MM-DD.csv
  observeEvent(input$save_clicks, {
    df <- clicks()
    if (!nrow(df)) { showNotification("Geen klikpunten om te bewaren.", type = "warning"); return(invisible(NULL)) }
    if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
    fpath <- file.path(OUT_DIR, sprintf("ManualSel_%s.csv", format(Sys.Date(), "%Y-%m-%d")))
    readr::write_csv(df %>% transmute(longitude = lon, latitude = lat), fpath)
    showNotification(paste("Opgeslagen:", fpath), type = "message", duration = 5)
  })
}

shinyApp(ui, server)
