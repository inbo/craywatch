# generate_figures.R

library(ggplot2)
library(treemapify)
library(sf)
library(dplyr)
library(httr)
library(jsonlite)
library(imguR)
library(stringr)
library(readr)

client_id <- Sys.getenv("IMGUR_CLIENT_ID")
client_secret <- Sys.getenv("IMGUR_CLIENT_SECRET")

# Laad en verwerk je shapefile
shapefile_path <- "./data/input/shapefiles/postkantons.shp"
shapefile <- st_read(shapefile_path)
shapefile <- st_make_valid(shapefile)

# Authenticeer en laad de data
data <- read.csv("./data/input/registration.tsv", sep="\t")

names(data) <- c(
  "VrijwilID", "PrimaireGemeente",
  "SecundaireGemeente", "Provincie", "VoorgesteldeLocatie", "Deelnamedatum", "Reden",
  "AanmeldingBeantwoord", "GecontacteerdMetLocaties",
  "VoorgesteldeLocaties", "OvereengekomenLocatie", "StartdatumVeldwerk",
  "DeadlineFuikenVerzenden", "AantalSetsFuiken", "FuikenVerzonden", "Bevestigingsmail",
  "WaarnemingenOntvangen", "FuikenTeruggekregen", "BedankingGestuurd"
)

# Verwerk de data
filtered_data <- data %>%
  filter(!is.na(VrijwilID) & VrijwilID != "")

province_counts <- filtered_data %>%
  count(Provincie) %>%
  arrange(desc(n)) %>%
  filter(Provincie !="")

extract_postcode <- function(x) as.numeric(substring(x, 1, 4))
filtered_data <- filtered_data %>%
  mutate(
    PrimairePostcode = extract_postcode(PrimaireGemeente),
    SecundairePostcode = extract_postcode(SecundaireGemeente)
  )

all_postcodes <- c(filtered_data$PrimairePostcode, filtered_data$SecundairePostcode)
postcode_counts <- as.data.frame(table(all_postcodes))
colnames(postcode_counts) <- c("postcode", "AantalRegistraties")


# Genereer de treemap
png(filename = "./images/treemap.png", width = 300, height = 300, res = 72)
colors <- c( "#217a79", "#d3f2a3", "#074050", "#6cc08b", "#4c9b82" )
ggplot(province_counts, aes(area = n, fill = Provincie, label = paste(Provincie, "\n", n))) +
  geom_treemap() +
  geom_treemap_text(size = 18, colour = "grey40", place = "centre", grow = FALSE, reflow = TRUE) +
  scale_fill_manual(values = colors) +
  theme(legend.position = "none")
dev.off()


# Genereer de kaart
png(filename = "./images/map.png", width = 800, height = 500, res = 72)

shapefile <- shapefile %>%
  left_join(postcode_counts, by = c("nouveau_PO" = "postcode"))
shapefile$AantalRegistraties[is.na(shapefile$AantalRegistraties)] <- 0

ggplot(data = shapefile) +
  geom_sf(aes(fill = AantalRegistraties), color = "darkgrey", lwd = 0.2) +
  scale_fill_gradient(low = "#d3f2a3", high = "#074050", name = "Aantal Registraties") +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 15),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )
dev.off()

# Genereer output Data
localities <- read_csv("../assets/localities.csv")
num_reserved <- sum(as.logical(localities$isReserved), na.rm = TRUE)
count_sent <- sum(tolower(data$FuikenVerzonden) == 'true', na.rm = TRUE)


save(filtered_data, province_counts, num_reserved, count_sent, file = "./data/output/processed_data.RData")

#Uploading images to webserver

upload_image_to_imgur <- function(image_path, client_id) {
  # Read the image file
  image_file <- upload_file(image_path)
  
  # Make the POST request to upload the image
  response <- POST(
    url = "https://api.imgur.com/3/upload",
    add_headers(Authorization = paste("Client-ID", client_id)),
    body = list(image = image_file)
  )
  
  # Parse the response
  response_content <- content(response, as = "text")
  response_json <- fromJSON(response_content)
  
  # Check for success
  if (response$status_code == 200) {
    # Return the link to the uploaded image
    return(response_json$data$link)
  } else {
    # Return the error message
    stop(response_json$data$error)
  }
}

# Example usage
image_path <- "./images/map.png"
imgur_link <- upload_image_to_imgur(image_path, client_id)
map_link <-imgur_link 