# ====================================================
# Scriptnaam: Select_Municipalities.R
# Auteur: Frédérique Steen
# Datum: 09-01-2025
# Beschrijving: 
#   Dit script maakt het mogelijk om gemeenten (polygonen) 
#   te selecteren op kaart in een shiny app met de ggplot
#   van de verspreiding 2025 als achtergrond. De selectie
#   resulteert in een shape die gebruikt wordt om een subset 
#   van localiteiten te maken
# ====================================================


#R - libraries
library(sf)
library(ggplot2)
library(shiny)

#Based on output from this script, the user can select polygons on the map and save them as a shapefile.

##### 1. Selecteer gemeenten

# Laad data (species_plot & gemeenten shape)
gemeenten <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/gemeenten.shp")
species_plot <- readRDS("./data/output/SelectedMunic/species_plot.rds")
vlaanderen <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/grenzenvlaanderen.shp")
riparias <- st_read("~/GitHub/craywatch/R/data/input/shapefiles/riparias.shp") %>%
  st_intersection(vlaanderen)  # Beperk tot Vlaanderen



# UI voor Shiny app
ui <- fluidPage(
  h3("Selecteer polygonen met een ggplot achtergrond"),
  plotOutput("map", click = "plot_click"),
  verbatimTextOutput("selectedPolygons"),
  actionButton("save", "Opslaan als Shapefile")  # Knop om selectie op te slaan
)

# Serverlogica
server <- function(input, output, session) {
  selected <- reactiveVal(rep(FALSE, nrow(gemeenten)))
  
  # Observeer klik en update selectie
  observeEvent(input$plot_click, {
    click_point <- st_sfc(st_point(c(input$plot_click$x, input$plot_click$y)), crs = st_crs(gemeenten))
    clicked <- st_contains(gemeenten, click_point, sparse = FALSE)
    if (any(clicked)) {
      sel <- selected()
      sel[clicked] <- !sel[clicked]
      selected(sel)
    }
  })
  
  # Combineer species_plot en polygonen
  output$map <- renderPlot({
    gemeenten$selected <- as.character(selected())  # Voeg selectie toe als string waarden
    
    # Combineer species_plot met polygonen
    species_plot +
      geom_sf(data = gemeenten, aes(fill = selected), color = "black", lwd = 0.2) +  # Vul polygonen met kleuren
      geom_sf(data = riparias, color = "red", fill = NA, linewidth = 2) +  # Outline van riparias
      scale_fill_manual(values = c("FALSE" = "transparent", "TRUE" = "red")) +
      labs(fill = "Geselecteerd") +
      theme_minimal()  # Houd de kaart overzichtelijk
  })
  
  # Toon geselecteerde polygonen
  output$selectedPolygons <- renderPrint({
    if (any(selected())) {
      gemeenten[selected(), ]
    } else {
      "Geen polygonen geselecteerd"
    }
  })
  
  # Opslaan als Shapefile
  observeEvent(input$save, {
    # Zorg dat de hoofddirectory bestaat
    dir.create("./data/output/SelectedMunic", showWarnings = FALSE, recursive = TRUE)
    
    # Pad naar het bestand
    file_path <- "./data/output/SelectedMunic/SelectedMunic.shp"
    
    # Filter geselecteerde polygonen
    selected_sf <- gemeenten[selected(), ]
    
    # Controleer of er polygonen geselecteerd zijn
    if (nrow(selected_sf) == 0) {
      showNotification("Geen polygonen geselecteerd om op te slaan!", type = "error")
      return()
    }
    
    # Sla op als Shapefile
    tryCatch({
      st_write(selected_sf, file_path, driver = "ESRI Shapefile", delete_dsn = TRUE)
      showNotification("Shapefile succesvol opgeslagen in ./data/output/SelectedMunic/SelectedMunic.shp", type = "message")
    }, error = function(e) {
      showNotification(paste("Fout bij opslaan:", e$message), type = "error")
    })
  })
}

# Start de app
shinyApp(ui, server)


#2. Maak intersect met de localities.csv
gemeenten_2025 <- st_read("./data/output/SelectedMunic/SelectedMunic.shp")
library(dplyr)

library(sf)
library(dplyr)

# Stap 1: Lees het CSV-bestand in en maak er een sf-object van
localities <- read.csv("~/GitHub/craywatch/assets/localities.csv") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% # WGS84 CRS
  
  # Stap 2: Transformeer naar Lambert 31370
  st_transform(31370)

# Stap 3: Maak een intersect met gemeenten_2025
localities_intersect <- st_intersection(localities, gemeenten_2025)

# Stap 4: Voeg de originele WGS84-coördinaten toe
localities_2025 <- localities_intersect %>%
  mutate(
    Longitude = st_coordinates(st_transform(geometry, 4326))[, 1],
    Latitude = st_coordinates(st_transform(geometry, 4326))[, 2]
  )

# Stap 5: Controleer op geldige coördinaten
invalid_coords <- localities_2025 %>%
  filter(
    Latitude < -90 | Latitude > 90 | Longitude < -180 | Longitude > 180
  )

if (nrow(invalid_coords) > 0) {
  warning("Er zijn ongeldige Latitude of Longitude coördinaten in de dataset.")
  print(invalid_coords)
} else {
  message("Alle Latitude en Longitude waarden zijn valide.")
}

# Stap 6: Schrijf het resultaat naar een nieuw bestand
st_drop_geometry(localities_2025) %>%
  write.csv("./data/output/SelectedMunic/localities_2025.csv", row.names = FALSE)