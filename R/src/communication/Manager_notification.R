# ====================================================
# Scriptnaam:   Generate_locality_reports.R
# Auteur:       Frédérique Steen
# Datum:        01-07-2024 (herzien: 26-08-2025)
#
# Beschrijving:
# Dit script filtert gereserveerde locaties uit een CSV-bestand, herformatteert 
# en valideert startdatums, en selecteert per 'Beheerder' de relevante gegevens. 
# Voor elke beheerder wordt een aparte tabel aangemaakt en opgeslagen in meerdere 
# formaten (shapefile, CSV, HTML). De bestanden worden vervolgens per beheerder 
# gecomprimeerd in een zip-archief. Directories worden dynamisch aangemaakt per datum.
#
# Input:
# - assets/localities.csv
#
# Output:
# - data/output/notif_beheerder/<datum>/table_<Beheerder>.shp
# - data/output/notif_beheerder/<datum>/table_<Beheerder>.csv
# - data/output/notif_beheerder/<datum>/table_<Beheerder>.html
# - data/output/notif_beheerder/<datum>/table_<Beheerder>.zip
#
# Benodigde packages:
# readr, dplyr, lubridate, sf, knitr, kableExtra, zip, here
# ====================================================

library(readr)
library(dplyr)
library(lubridate)
library(sf)
library(knitr)
library(kableExtra)
library(zip)
library(here)

# Input-pad via projectroot
csv_path <- here("assets", "localities.csv")

localities <- read.csv(csv_path)

localities_reserved <- localities %>%
  filter(isReserved == TRUE)

##### ADJUST DATES ############################################################

localities_reserved$startDate <- ifelse(is.na(localities_reserved$startDate), "TBD", localities_reserved$startDate)

reformat_dates <- function(date) {
  if (date == "TBD") {
    return(date)
  } else if (grepl("^\\d{2}-\\d{2}-\\d{4}$", date)) {
    return(date)
  } else if (grepl("^\\d{2}/\\d{2}/\\d{2}$", date)) {
    return(format(dmy(paste0(substr(date, 1, 6), "20", substr(date, 7, 8))), "%d-%m-%Y"))
  } else if (grepl("^\\d{2}/\\d{2}/\\d{4}$", date)) {
    return(format(dmy(date), "%d-%m-%Y"))
  } else if (grepl("^\\d{2}-\\d{2}-\\d{2}$", date)) {
    return(format(dmy(paste0(substr(date, 1, 6), "20", substr(date, 7, 8))), "%d-%m-%Y"))
  } else {
    return(NA)
  }
}

localities_reserved$startDate <- sapply(localities_reserved$startDate, reformat_dates)
localities_reserved$startDateDate <- as.Date(localities_reserved$startDate, format="%d-%m-%Y")
system_date <- format(Sys.Date(), "%d-%m-%Y")

localities_filtered <- localities_reserved %>%
  filter(startDate == "TBD" | (!is.na(startDateDate) & startDateDate > dmy(system_date)))

##### SELECT MANAGERS ############################################################

beheerder_list <- localities_filtered %>%
  distinct(Beheerder, Bhremail, Bhrtel) %>%
  mutate(valid_name = make.names(Beheerder, unique = TRUE) %>% gsub("\\.", "_", .))

localities_filtered <- st_as_sf(localities_filtered, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

beheerders <- unique(localities_filtered$Beheerder)

# Output-directory via here()
output_dir <- here("data", "output", "notif_beheerder", system_date)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

columns_to_keep <- c("NAAM", "OMSCHR", "CATC", "startDate", "provincie", "gemeente", "postcode", "VHAG", "Latitude", "Longitude")

for (row in 1:nrow(beheerder_list)) {
  beheerder <- beheerder_list$Beheerder[row]
  valid_name <- beheerder_list$valid_name[row]
  
  beheerder_dir <- file.path(output_dir, paste0("table_", valid_name))
  if (!dir.exists(beheerder_dir)) dir.create(beheerder_dir)
  
  if (is.na(beheerder)) {
    beheerder_table <- localities_filtered %>% filter(is.na(Beheerder)) %>% select(all_of(columns_to_keep)) %>% arrange(startDate)
    assign("table_NA", beheerder_table)
    st_write(beheerder_table, file.path(beheerder_dir, "table_NA.shp"), delete_layer = TRUE)
    beheerder_table_no_geom <- st_drop_geometry(beheerder_table)
    write.csv(beheerder_table_no_geom, file.path(beheerder_dir, "table_NA.csv"), row.names = FALSE)
    html_table <- kable(beheerder_table_no_geom, format = "html", table.attr = "style='width:100%;'") %>%
      kable_styling(full_width = F, position = "left") %>%
      row_spec(0, bold = TRUE, extra_css = "border-bottom: 2px solid;")
    writeLines(html_table, con = file.path(output_dir, "table_NA.html"))
    zip_file <- file.path(output_dir, "table_NA.zip")
    zip::zipr(zip_file, files = list.files(beheerder_dir, full.names = TRUE, pattern = "\\.shp$|\\.csv$"))
  } else {
    beheerder_table <- localities_filtered %>% filter(Beheerder == beheerder) %>% select(all_of(columns_to_keep)) %>% arrange(startDate)
    assign(paste0("table_", valid_name), beheerder_table)
    st_write(beheerder_table, file.path(beheerder_dir, paste0("table_", valid_name, ".shp")), delete_layer = TRUE)
    beheerder_table_no_geom <- st_drop_geometry(beheerder_table)
    write.csv(beheerder_table_no_geom, file.path(beheerder_dir, paste0("table_", valid_name, ".csv")), row.names = FALSE)
    html_table <- kable(beheerder_table_no_geom, format = "html", table.attr = "style='width:100%;'") %>%
      kable_styling(full_width = F, position = "left") %>%
      row_spec(0, bold = TRUE, extra_css = "border-bottom: 2px solid;")
    writeLines(html_table, con = file.path(output_dir, paste0("table_", valid_name, ".html")))
    zip_file <- file.path(output_dir, paste0("table_", valid_name, ".zip"))
    zip::zipr(zip_file, files = list.files(beheerder_dir, full.names = TRUE, pattern = "\\.shp$|\\.csv$"))
  }
}

unlink(list.dirs(output_dir, recursive = FALSE), recursive = TRUE)
