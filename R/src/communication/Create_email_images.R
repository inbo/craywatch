# ====================================================
# Scriptnaam:   Create_email_images.R
# Auteur:       Frédérique Steen
# Datum:        01-07-2024
#
# Beschrijving:
# Dit script verwerkt registratiedata van vrijwilligers en koppelt die 
# aan geografische informatie (postkantons). Het genereert verschillende 
# visualisaties (treemap per provincie, kaart met aantallen registraties), 
# en slaat deze op als afbeeldingen. De afbeeldingen worden vervolgens 
# automatisch naar Imgur geüpload en de links, samen met verwerkte data 
# (bv. aantal reservaties, aantal fuiken verzonden), worden gebundeld in 
# een .RData-bestand voor gebruik in R Markdown rapportage of e-mails.
#
# Input:
# - ./data/input/shapefiles/postkantons.shp
# - ./data/input/registration.tsv
# - ../assets/localities.csv
#
# Output:
# - ./images/treemap.png
# - ./images/map.png
# - ./data/output/processed_data.RData
#
# Benodigde packages:
# ggplot2, treemapify, sf, dplyr, httr, jsonlite, imguR, stringr, readr,
# tm, wordcloud, RColorBrewer, knitr, RCurl, XML
# ====================================================


library(ggplot2)
library(treemapify)
library(sf)
library(dplyr)
library(httr)
library(jsonlite)
library(imguR)
library(stringr)
library(readr)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(knitr)
library(RCurl)
library(XML)
library(here)



client_id <- Sys.getenv("IMGUR_CLIENT_ID")
client_secret <- Sys.getenv("IMGUR_CLIENT_SECRET")

PATHS <- list(
shapefile = here("data", "input", "shapefiles", "postkantons.shp"),
registration = here("data", "input", "registration.tsv"),
images_dir = here("images"),
output_rdata = here("data", "output", "processed_data.RData"),
localities = here("assets", "localities.csv")
)

# Zorg dat outputmappen bestaan
if (!dir.exists(PATHS$images_dir)) dir.create(PATHS$images_dir, recursive = TRUE)
if (!dir.exists(here("data", "output"))) dir.create(here("data", "output"), recursive = TRUE)

# Laad en verwerk je shapefile
shapefile <- st_read(PATHS$shapefile, quiet = TRUE) |> st_make_valid()


# Authenticeer en laad de data
data <- read.csv(PATHS$registration, sep = "\t")

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
png(filename = file.path(PATHS$images_dir, "treemap.png"), width = 300, height = 300, res = 72)
colors <- c( "#217a79", "#d3f2a3", "#074050", "#6cc08b", "#4c9b82" )
ggplot(province_counts, aes(area = n, fill = Provincie, label = paste(Provincie, "\n", n))) +
  geom_treemap() +
  geom_treemap_text(size = 18, colour = "grey40", place = "centre", grow = FALSE, reflow = TRUE) +
  scale_fill_manual(values = colors) +
  theme(legend.position = "none")
dev.off()


# Genereer de kaart
png(filename = file.path(PATHS$images_dir, "map.png"), width = 800, height = 500, res = 72)

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
localities <- readr::read_csv(PATHS$localities, show_col_types = FALSE)
num_reserved <- sum(as.logical(localities$isReserved), na.rm = TRUE)
count_sent <- sum(tolower(data$FuikenVerzonden) == 'true', na.rm = TRUE)


#Uploading images to webserver

# Uploading images to webserver (met here::here, minimale wijzigingen)

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
  response_json <- fromJSON(content(response, as = "text"))
  
  # Check for success
  if (response$status_code == 200) {
    # Return the link to the uploaded image
    return(response_json$data$link)
  } else {
    # Return the error message
    stop(response_json$data$error)
  }
}

# upload each image in images/ to imgur & extract link for rmarkdown script 
# List all image files in the images/ directory (vanaf projectroot)
image_files <- list.files(path = here::here("images"), pattern = "\\.(png|jpg)$", full.names = TRUE)
image_links <- list()

# Loop through each image file, upload to Imgur, and store the link in a variable
for (image_path in image_files) {
  file_name <- tools::file_path_sans_ext(basename(image_path))
  imgur_link <- knitr::imgur_upload(image_path, key = client_id)
  image_links[[file_name]] <- imgur_link[1]
}

# Save all needed variables in the processed_data.RData file (vanaf projectroot)
save(filtered_data, province_counts, num_reserved, count_sent, image_links,
     file = here::here("data", "output", "processed_data.RData"))
