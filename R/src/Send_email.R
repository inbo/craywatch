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
  to = "",
  subject = "Craywatch Nieuws",
  credentials = creds_key(
    id = "gmail"
  ),
  verbose = TRUE,
  timeout = 60  # Time-out in seconden
)