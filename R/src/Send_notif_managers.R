library(readr)
library(dplyr)
library(lubridate)
library(sf)
library(knitr)
library(kableExtra)
library(zip)
library(blastula)
library(keyring)

library(httr)

# Stel een langere timeout in, door de grote bijlagen werd de initiële time-out overschreden en de email niet verzonden

set_config(config(timeout(60)))


# Define the function to send email
send_email <- function(to_email, subject, body, attachments) {
  from <- "craywatch@inbo.be"
  
  # Maak een e-mail object
  email <- compose_email(
    body = md(body)
  )
  # Voeg de bijlage toe aan de e-mail
  email <- add_attachment(
    email,
    file = zip_file,
    )
  
  # Send email
  smtp_send(
    email,
    from = from,
    to = to_email,
    subject = subject,
    credentials = creds_key(id = "gmail")
  )
}
# Load the beheerder list
beheerder_list

output_dir <- "C:/Users/frederique_steen/Documents/GitHub/craywatch/R/data/output/notif_beheerder"

# Get system date
system_date <- format(Sys.Date(), "%d-%m-%Y")
output_dir_date <- file.path(output_dir, system_date)

# Loop over each beheerder and send email with the respective files
for (row in 1:nrow(beheerder_list)) {
  beheerder <- beheerder_list$Beheerder[row]
  valid_name <- beheerder_list$valid_name[row]
  email <- beheerder_list$Bhremail[row]
  
  # Check if email is available
  if (!is.na(email) && email != "") {
    # Paths to files
    html_file <- file.path(output_dir_date, paste0("table_", valid_name, ".html"))
    zip_file <- file.path(output_dir_date, paste0("table_", valid_name, ".zip"))
    
    # Check if files exist
    if (file.exists(html_file) && file.exists(zip_file)) {
      # Read the HTML content
      html_content <- readLines(html_file)
      body <- paste0(
        "Geachte beheerder,\n\n",
        "Sinds 15 juni is het wetenschappelijk project Craywatch van start gegaan. Binnen dit project bemonsteren burgerwetenschappers waterlichamen op Vlaams grondgebied om de aanwezigheid van invasieve rivierkreeften in kaart te brengen.\n\n",
        "Wij houden u graag op de hoogte van opkomende staalnamelocaties. In onderstaande tabel worden voorziene staalnamepunten en hun bemonsteringsdata vermeld. De data vertegenwoordigen de startdata van het staalnameprotocol. De vallen blijven in principe 4 dagen in het watersysteem aanwezig. Indien we de startdatum nog dienen te vernemen wordt dit aangeduid met 'TBD'. Wij geven u graag de wekelijkse update van de punten.\n\n",
        paste(html_content, collapse = "\n"),
        "\n\nWe streven ernaar uw werking niet te compromitteren en zoveel als mogelijk met uw suggesties rekening te houden. Het staalnameprotocol kunt u <a href='https://craywatch.inbo.be/instructies/'>hier</a> uitgebreid terugvinden. In bijlage vindt u de bovenstaande tabel nogmaals terug als shapefile en als csv-file. Met vragen of opmerkingen kunt u ons steeds bereiken op craywatch@inbo.be.\n\n",
        "Hoogachtend,\n\n",
        "Het Craywatch project-team\n"
      )
      
      # Send email
      send_email(
        to = email, 
        subject = paste0("Notificatiebericht craywatch staalnamelocaties"), 
        body = body, 
        attachment = zip_file
      )
      
      cat(paste("Email sent to:", email, "\n"))
    } else {
      cat(paste("Files not found for beheerder:", beheerder, "\n"))
    }
  } else {
    cat(paste("No email for beheerder:", beheerder, "\n"))
  }
}