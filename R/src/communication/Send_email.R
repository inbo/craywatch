# ====================================================
# Scriptnaam:   Send_email_report.R
# Auteur:       Frédérique Steen
# Datum:        01-07-2024
#
# Beschrijving:
# Dit script rendert een RMarkdown-bestand (email_report.Rmd) naar HTML 
# en gebruikt vervolgens het 'blastula'-pakket om de inhoud als e-mail 
# te versturen. De HTML wordt ingelezen, ingekapseld in een lay-outdiv, 
# en via SMTP verstuurd met de opgegeven credentials (keyring).
#
# Input:
# - ./src/email_report.Rmd
#
# Output:
# - ./data/output/email_report.html
# - verzonden e-mail ("Craywatch Nieuws")
#
# Benodigde packages:
# blastula, keyring, rmarkdown
#
# Credentials:
# - Worden opgehaald via keyring (id = "gmail")
# ====================================================

#install.packages("blastula")
#install.packages("keyring")

library(blastula)
library(keyring)
library(rmarkdown)

# Render het RMarkdown-bestand naar HTML
render("./src/email_report.Rmd", output_file = "../data/output/email_report.html")

html_content <- paste(readLines("./data/output/email_report.html"), collapse = "\n")
# Maak een e-mail met blastula
email <- compose_email(
  body = md(paste0('<div style="max-width: 600px; margin: auto; text-align: justify;">', html_content, '</div>'))
)

smtp_send(
  email,
  from = "craywatch@inbo.be",
  to = "craywatch@inbo.be",
  subject = "Craywatch Nieuws",
  credentials = creds_key(
    id = "gmail"
  ),
  verbose = TRUE,
  timeout = 600,  # Time-out in seconden
  bcc = bcc_emails
  )