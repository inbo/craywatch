library(readr)
library(dplyr)
library(lubridate)
library(sf)
library(knitr)
library(kableExtra)


csv_path <- "../assets/localities.csv"

localities <- read.csv(csv_path)

localities_reserved <- localities %>%
  filter(isReserved == TRUE)

# Select Managers & data
unique_entries <- localities_reserved %>%
  distinct(Beheerder, Bhremail, Bhrtel)

# Ensure the localities data frame has geometry columns for shapefile creation
# Replace 'Longitude' and 'Latitude' with your actual column names
localities_reserved <- st_as_sf(localities_reserved, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

# Get unique list of 'Beheerder', including NA
beheerders <- unique(localities_reserved$Beheerder)

# Replace NA in startDate with "TBD"
localities_reserved$startDate <- ifelse(is.na(localities_reserved$startDate), "TBD", localities_reserved$startDate)

# Function to reformat dates
reformat_dates <- function(date) {
  if (date == "TBD") {
    return(date)
  } else if (grepl("^\\d{2}-\\d{2}-\\d{4}$", date)) {
    return(date)  # Date is already in the correct format
  } else if (grepl("^\\d{2}/\\d{2}/\\d{2}$", date)) {
    # Assuming year is in the format yy and should be 20yy
    return(format(dmy(paste0(substr(date, 1, 6), "20", substr(date, 7, 8))), "%d-%m-%Y"))
  } else if (grepl("^\\d{2}/\\d{2}/\\d{4}$", date)) {
    return(format(dmy(date), "%d-%m-%Y"))
  } else if (grepl("^\\d{2}-\\d{2}-\\d{2}$", date)) {
    # Assuming year is in the format yy and should be 20yy
    return(format(dmy(paste0(substr(date, 1, 6), "20", substr(date, 7, 8))), "%d-%m-%Y"))
  } else {
    return(NA)  # Invalid date format, returning NA
  }
}

# Apply the reformat_dates function
localities_reserved$startDate <- sapply(localities_reserved$startDate, reformat_dates)


# Get system date
system_date <- Sys.Date() %>%
  format("%d-%m-%Y")

# Filter entries that are later than the system date or have startDate "TBD"
localities_filtered <- localities_reserved %>%
  filter(startDate == "TBD" | (!is.na(startDate) & startDate > system_date))


# Create the output directory path
output_dir <- file.path("./data/output/notif_beheerder", system_date)

# Create the directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Specify columns to keep
columns_to_keep <- c("NAAM", "OMSCHR", "CATC", "startDate", "provincie", "gemeente", "postcode", "VHAG", "Latitude", "Longitude")

# Create separate tables for each 'Beheerder'
for (beheerder in beheerders) {
  # Handle NA values separately
  if (is.na(beheerder)) {
    beheerder_table <- localities_filtered %>% filter(is.na(Beheerder)) %>% select(all_of(columns_to_keep))
    beheerder_table <- beheerder_table %>% arrange(startDate)
    assign("table_NA", beheerder_table)
    
    # Save the table as a shapefile
    st_write(beheerder_table, file.path(output_dir, "table_NA.shp"), delete_layer = TRUE)
    
    # Drop geometry column
    beheerder_table_no_geom <- st_drop_geometry(beheerder_table)
    
    # Save the table as a CSV file
    write.csv(beheerder_table_no_geom, file.path(output_dir, "table_NA.csv"), row.names = FALSE)
    
    # Save the table as an HTML file with a line under the header row
    html_table <- kable(beheerder_table_no_geom, format = "html", table.attr = "style='width:100%;'") %>%
      kable_styling(full_width = F, position = "left") %>%
      row_spec(0, bold = TRUE, extra_css = "border-bottom: 2px solid;")
    writeLines(html_table, con = file.path(output_dir, "table_NA.html"))
  } else {
    # Filter the localities for the current 'Beheerder'
    beheerder_table <- localities_filtered %>% filter(Beheerder == beheerder) %>% select(all_of(columns_to_keep))
    
    # Sort the table by startDate in ascending order
    beheerder_table <- beheerder_table %>% arrange(startDate)
    
    # Create a valid R variable name from 'Beheerder'
    valid_name <- make.names(beheerder, unique = TRUE)
    valid_name <- gsub("\\.", "_", valid_name)  # Replace periods with underscores
    
    # Assign the table to a variable with the name of the 'Beheerder'
    assign(paste0("table_", valid_name), beheerder_table)
    
    # Save the table as a shapefile
    st_write(beheerder_table, file.path(output_dir, paste0("table_", valid_name, ".shp")), delete_layer = TRUE)
    
    # Drop geometry column
    beheerder_table_no_geom <- st_drop_geometry(beheerder_table)
    
    # Save the table as a CSV file
    write.csv(beheerder_table_no_geom, file.path(output_dir, paste0("table_", valid_name, ".csv")), row.names = FALSE)
    
    # Save the table as an HTML file with a line under the header row
    html_table <- kable(beheerder_table_no_geom, format = "html", table.attr = "style='width:100%;'") %>%
      kable_styling(full_width = F, position = "left") %>%
      row_spec(0, bold = TRUE, extra_css = "border-bottom: 2px solid;")
    writeLines(html_table, con = file.path(output_dir, paste0("table_", valid_name, ".html")))
  }
}