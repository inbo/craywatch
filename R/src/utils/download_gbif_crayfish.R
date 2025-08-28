# 01_download_gbif_crayfish.R
# Volledige GBIF Occurrence Download voor Vlaamse kreeften (presence-only, 2000–2025)
# Output: data/gbif_crayfish_flanders.csv.gz + .rds + metadata.json
# Vereist: rgbif, dplyr, readr, jsonlite, glue, withr, purrr

suppressPackageStartupMessages({
  library(rgbif)
  library(dplyr)
  library(readr)
  library(jsonlite)
  library(glue)
  library(withr)
  library(purrr)
})

CRAYFISH_SPECIES <- c(
  "Faxonius limosus",
  "Procambarus clarkii",
  "Procambarus virginalis",
  "Faxonius virilis",
  "Procambarus acutus",
  "Astacus astacus",
  "Pontastacus leptodactylus"
)

# ---- Vlaanderen bbox (WGS84) ----
bbox_to_wkt <- function(w, s, e, n) {
  sprintf("POLYGON((%f %f,%f %f,%f %f,%f %f,%f %f))",
          w, s, e, s, e, n, w, n, w, s)
}
FLANDERS_WKT <- bbox_to_wkt(2.5, 50.68, 5.90, 51.50)

dir.create("data", showWarnings = FALSE, recursive = TRUE)

# ---- GBIF credentials via ENV ----
gbif_user  <- Sys.getenv("GBIF_USER")
gbif_pwd   <- Sys.getenv("GBIF_PWD")
gbif_email <- Sys.getenv("GBIF_EMAIL")
if (!nzchar(gbif_user) || !nzchar(gbif_pwd) || !nzchar(gbif_email)) {
  stop("Zet GBIF_USER / GBIF_PWD / GBIF_EMAIL als omgevingvariabelen (geen secrets in code).")
}

# ---- Naamresolutie → taxonKeys ----
resolve_taxonkeys <- function(names_vec){
  map_dfr(names_vec, function(x){
    bk <- rgbif::name_backbone(name = x, rank = "SPECIES")
    tibble(
      input        = x,
      matched      = bk$scientificName %||% NA_character_,
      status       = bk$status %||% NA_character_,
      usageKey     = bk$usageKey %||% NA_integer_,
      note         = if (isTRUE(bk$matchType == "EXACT")) "exact" else "check"
    )
  })
}

resolved <- resolve_taxonkeys(CRAYFISH_SPECIES)
write_csv(resolved, "../data/input/gbif_occ/gbif_taxon_resolution.csv")
print(resolved)

taxon_keys <- resolved$usageKey |> unique() |> na.omit()
if (length(taxon_keys) == 0L) {
  stop("Geen geldige taxonKeys gevonden. Controleer soortnamen/naamresolutie.")
}

# ---- Predicates (robuust voor 1 of >1 keys) ----
taxon_pred <- if (length(taxon_keys) == 1L) {
  rgbif::pred("taxonKey", taxon_keys[[1]])
} else {
  rgbif::pred_in("taxonKey", taxon_keys)
}

preds <- rgbif::pred_and(
  taxon_pred,
  rgbif::pred("country", "BE"),
  rgbif::pred_within(FLANDERS_WKT),
  rgbif::pred("hasCoordinate", TRUE),
  rgbif::pred("occurrenceStatus", "PRESENT"),
  rgbif::pred_gte("year", 2000),
  rgbif::pred_lte("year", 2025)
)

# ---- Start download ----
message("Indienen GBIF download…")
dl_key <- rgbif::occ_download(
  predicate = preds,
  format    = "SIMPLE_CSV",
  user      = gbif_user,
  pwd       = gbif_pwd,
  email     = gbif_email
)
writeLines(dl_key, "../data/input/gbif_occ/gbif_download_key.txt")
message(glue("Download key: {dl_key}"))

# ---- Wachten met expliciete status-polling ----
repeat {
  m <- try(rgbif::occ_download_meta(dl_key), silent = TRUE)
  if (inherits(m, "try-error") || is.null(m$status)) {
    cat(sprintf("Status: (onbekend; retry) | %s\n", format(Sys.time(), "%H:%M:%S")))
    Sys.sleep(5)
    next
  }
  cat(sprintf("Status: %s | %s\n", m$status, format(Sys.time(), "%H:%M:%S")))
  if (m$status %in% c("SUCCEEDED","KILLED","CANCELLED","FAILED")) break
  Sys.sleep(5)
}
if (m$status != "SUCCEEDED") stop(glue("GBIF download niet geslaagd: status={m$status}"))

# ---- Binnenhalen (versie-onafhankelijk) ----
get_res <- rgbif::occ_download_get(key = dl_key, overwrite = TRUE)
zipfile <- if (is.character(get_res)) {
  get_res
} else if (!is.null(get_res$path)) {
  get_res$path
} else {
  stop("occ_download_get() gaf een onverwacht object terug.")
}
stopifnot(file.exists(zipfile))

# ---- Importeren ----
occ <- rgbif::occ_download_import(zipfile)

# ---- (Optioneel) kwaliteitsfilters ----
issue_has <- function(x, pattern) { !is.na(x) & grepl(pattern, x, fixed = TRUE) }

occ_clean <- occ %>%
  select(
    species, scientificName, taxonKey, kingdom, phylum, class, order, family, genus,
    decimalLongitude, decimalLatitude,
    year, month, day, eventDate,
    basisOfRecord, occurrenceID, datasetKey,
    coordinateUncertaintyInMeters, countryCode, stateProvince, locality
  ) %>%
  mutate(
    decimalLongitude = as.numeric(decimalLongitude),
    decimalLatitude  = as.numeric(decimalLatitude)
  ) %>%
  filter(!is.na(decimalLongitude), !is.na(decimalLatitude))

# ---- Opslaan ----
csv_path <- "../data/input/gbif_occ/gbif_crayfish_flanders.csv.gz"
rds_path <- "../data/input/gbif_occ/gbif_crayfish_flanders.rds"
write_csv(occ_clean, csv_path)
saveRDS(occ_clean, rds_path)

# ---- Metadata voor reproduceerbaarheid ----
library(jsonlite)
library(glue)

meta <- list(
  timestamp_utc    = format(Sys.time(), tz = "UTC"),
  gbif_download_key = as.character(dl_key),
  rgbif_version    = as.character(utils::packageVersion("rgbif")),
  r_version        = R.version.string,
  query = list(
    input_species  = as.character(CRAYFISH_SPECIES),
    resolved       = as.data.frame(resolved),  # of as.list() afhankelijk van structuur
    taxon_keys     = as.character(taxon_keys),
    country        = "BE",
    years          = c(2000, 2025),
    geometry_wkt   = FLANDERS_WKT,
    presence_only  = TRUE,
    has_coordinate = TRUE,
    format         = "SIMPLE_CSV"
  ),
  n_records_raw   = nrow(occ),
  n_records_clean = nrow(occ_clean),
  files = list(
    csv = csv_path,
    rds = rds_path
  )
)

# sessionInfo bewaren als tekstbestand en ernaar verwijzen
writeLines(capture.output(utils::sessionInfo()), "../data/input/gbif_occ/sessionInfo.txt")
meta$sessionInfo_file <- "../data/input/gbif_occ/sessionInfo.txt"

# JSON schrijven
writeLines(
  toJSON(meta, auto_unbox = TRUE, pretty = TRUE),
  "../data/input/gbif_occ/gbif_crayfish_flanders_metadata.json"
)

message(glue(
  "Klaar. {nrow(occ_clean)} records opgeslagen in:\n- {csv_path}\n- {rds_path}\n",
  "Metadata: ../data/input/gbif_occ/gbif_crayfish_flanders_metadata.json\n",
  "Naamresolutie: ../data/input/gbif_occ/gbif_taxon_resolution.csv\n",
  "GBIF key: {dl_key}"
))

