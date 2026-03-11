# ====================================================
# Scriptnaam: 12_export_geojson_per_soort_per_laag.R
# Doel:
# Exporteert per soort en per analysetype (SBZ-H / SBP)
# aparte GeoJSON-bestanden, analoog aan 12_sbz_leaflet.R
# ====================================================

source("./src/analysis/config.R")

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(lubridate)
  library(jsonlite)
  library(purrr)
})

if (!exists("CF_long")) source("./src/08_load_aq_sbz.R")

message("--- Start export GeoJSON per soort per laag ---")

# ----------------------------------------------------
# 1. Hulpfuncties
# ----------------------------------------------------

make_slug <- function(x) {
  x |>
    tolower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_|_$", "")
}

ensure_multipolygon <- function(sf_object) {
  if (nrow(sf_object) == 0) return(sf_object)
  sf_object %>%
    st_cast("GEOMETRYCOLLECTION") %>%
    st_collection_extract("POLYGON") %>%
    st_cast("MULTIPOLYGON") %>%
    st_make_valid()
}

sanitize_for_geojson <- function(x) {
  stopifnot(inherits(x, "sf"))
  
  geom_col <- attr(x, "sf_column")
  
  for (nm in names(x)) {
    if (nm == geom_col) next
    
    col <- x[[nm]]
    
    if (inherits(col, "units")) {
      x[[nm]] <- as.numeric(col)
    } else if (is.factor(col)) {
      x[[nm]] <- as.character(col)
    } else if (inherits(col, "Date")) {
      x[[nm]] <- as.character(col)
    } else if (inherits(col, c("POSIXct", "POSIXt"))) {
      x[[nm]] <- format(col, "%Y-%m-%d %H:%M:%S")
    } else if (is.list(col)) {
      x[[nm]] <- vapply(
        col,
        function(el) {
          if (is.null(el) || length(el) == 0) return(NA_character_)
          paste(as.character(unlist(el)), collapse = "; ")
        },
        character(1)
      )
    }
  }
  
  x
}

standardize_geom_name <- function(x, geom_name = "geometry") {
  stopifnot(inherits(x, "sf"))
  current_geom <- attr(x, "sf_column")
  if (!identical(current_geom, geom_name)) {
    names(x)[names(x) == current_geom] <- geom_name
    sf::st_geometry(x) <- geom_name
  }
  x
}

select_export_fields <- function(x) {
  x <- standardize_geom_name(x, "geometry")
  
  keep <- c(
    "target_species",
    "target_species_slug",
    "target_species_dutch",
    "analysis_layer",
    "feature_type",
    "context_type",
    "status",
    "first_date",
    "fill_color",
    "fill_opacity",
    "border_color",
    "border_weight",
    "point_color",
    "point_radius",
    "point_fill",
    "point_fill_opacity",
    "popup_text",
    "naam",
    "HAB1",
    "sbp",
    "soort",
    "gebied",
    "geometry"
  )
  
  keep <- intersect(keep, names(x))
  x <- x[, keep, drop = FALSE]
  sanitize_for_geojson(x)
}

align_schema <- function(lst) {
  lst <- lapply(lst, standardize_geom_name, geom_name = "geometry")
  
  non_geom_cols <- unique(unlist(lapply(lst, function(z) setdiff(names(z), "geometry"))))
  
  lst2 <- lapply(lst, function(z) {
    miss <- setdiff(non_geom_cols, names(z))
    for (m in miss) {
      z[[m]] <- NA
    }
    z <- z[, c(non_geom_cols, "geometry"), drop = FALSE]
    sf::st_as_sf(z)
  })
  
  lst2
}

write_geojson_safe <- function(x, outfile) {
  x <- standardize_geom_name(x, "geometry")
  x <- select_export_fields(x)
  x <- standardize_geom_name(x, "geometry")
  st_write(x, outfile, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
}

# ----------------------------------------------------
# 2. Afwezigheden analoog aan leafletscript
# ----------------------------------------------------

message("Afwezigheden voorbereiden...")

CF_absence_sf <- CF_long %>%
  filter(presence == 0) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# ----------------------------------------------------
# 3. Analyse-lagen voorbereiden
# ----------------------------------------------------

message("Analyse-lagen voorbereiden...")

hbtrl_calc <- hbtrl %>%
  st_transform(crs_lambert) %>%
  ensure_multipolygon()

sbp_pgs_calc <- sbp_pgs_aq %>%
  st_transform(crs_lambert) %>%
  ensure_multipolygon()

if (nrow(sbp_vissen) > 0) {
  sbp_vissen_calc <- sbp_vissen %>%
    st_cast("MULTILINESTRING") %>%
    st_transform(crs_lambert)
} else {
  sbp_vissen_calc <- sbp_vissen
}

if (nrow(hbtrl_calc) > 0) {
  union_hbtrl <- st_union(st_geometry(hbtrl_calc))
} else {
  union_hbtrl <- st_sfc(st_polygon(), crs = crs_lambert)
}

geoms_sbp <- list()
if (nrow(sbp_pgs_calc) > 0) geoms_sbp[[1]] <- st_geometry(sbp_pgs_calc)
if (nrow(sbp_vissen_calc) > 0) geoms_sbp[[2]] <- st_geometry(sbp_vissen_calc)

if (length(geoms_sbp) > 0) {
  union_sbp <- st_union(do.call(c, geoms_sbp))
} else {
  union_sbp <- st_sfc(st_polygon(), crs = crs_lambert)
}

# ----------------------------------------------------
# 4. Zelfde wolken/statuslogica als in 12_sbz_leaflet.R
# ----------------------------------------------------

calculate_cloud_status <- function(points, reference_geom, layer_name) {
  if (nrow(points) == 0) return(NULL)
  
  clouds <- points %>%
    st_buffer(100) %>%
    st_union() %>%
    st_cast("POLYGON") %>%
    st_sf()
  
  if (nrow(clouds) == 0) return(NULL)
  
  intersects <- st_intersects(clouds, points)
  clouds$first_date <- sapply(intersects, function(idx) {
    if (length(idx) > 0) {
      min_date <- suppressWarnings(min(points$date[idx], na.rm = TRUE))
      if (is.infinite(min_date)) NA_character_ else format(min_date, "%Y-%m-%d")
    } else {
      NA_character_
    }
  })
  
  if (all(st_is_empty(reference_geom))) {
    clouds <- clouds %>%
      mutate(
        analysis_layer = layer_name,
        status = "Op afstand",
        border_color = "magenta"
      ) %>%
      st_transform(4326)
    return(clouds)
  }
  
  intersect_list <- st_intersects(clouds, reference_geom)
  is_inside <- lengths(intersect_list) > 0
  
  is_nearby <- rep(FALSE, nrow(clouds))
  idx_not_inside <- which(!is_inside)
  
  if (length(idx_not_inside) > 0) {
    dist_list <- st_is_within_distance(
      clouds[idx_not_inside, ],
      reference_geom,
      dist = 1000
    )
    is_nearby[idx_not_inside] <- lengths(dist_list) > 0
  }
  
  clouds %>%
    mutate(
      analysis_layer = layer_name,
      status = case_when(
        is_inside ~ paste("In", layer_name),
        is_nearby ~ paste("Nabij", layer_name, "(<1km)"),
        TRUE ~ "Op afstand"
      ),
      border_color = case_when(
        is_inside ~ "red",
        is_nearby ~ "orange",
        TRUE ~ "magenta"
      )
    ) %>%
    st_transform(4326)
}

# ----------------------------------------------------
# 5. Wolken per soort berekenen
# ----------------------------------------------------

message("Wolken berekenen...")

clouds_hbtrl_list <- list()
clouds_sbp_list   <- list()

unique_species <- unique(CF_presence$species)

for (sp in unique_species) {
  sp_points <- CF_presence %>%
    filter(species == sp) %>%
    select(species, date, geometry) %>%
    st_transform(crs_lambert)
  
  if (nrow(sp_points) == 0) next
  
  sp_color <- species_colors[sp]
  if (length(sp_color) == 0 || is.na(sp_color)) sp_color <- "#d73027"
  
  res_hbtrl <- calculate_cloud_status(sp_points, union_hbtrl, "SBZ-H")
  if (!is.null(res_hbtrl)) {
    res_hbtrl$fill_color <- sp_color
    clouds_hbtrl_list[[sp]] <- res_hbtrl
  }
  
  res_sbp <- calculate_cloud_status(sp_points, union_sbp, "SBP")
  if (!is.null(res_sbp)) {
    res_sbp$fill_color <- sp_color
    clouds_sbp_list[[sp]] <- res_sbp
  }
}

# ----------------------------------------------------
# 6. Contextlagen voorbereiden voor export
# ----------------------------------------------------

prepare_context_hbtrl <- function(sp) {
  hb <- hbtrl_calc %>%
    st_transform(4326) %>%
    mutate(
      target_species = sp,
      target_species_slug = make_slug(sp),
      target_species_dutch = unname(species_labels_dutch[tolower(sp)]),
      analysis_layer = "SBZ-H",
      feature_type = "hb_area",
      context_type = "hb_area",
      status = NA_character_,
      first_date = NA_character_,
      fill_color = NA_character_,
      fill_opacity = NA_real_,
      border_color = "darkgreen",
      border_weight = 1.5,
      point_color = NA_character_,
      point_radius = NA_real_,
      point_fill = NA_character_,
      point_fill_opacity = NA_real_,
      popup_text = paste0("<b>SBZ-H:</b> ", naam)
    )
  
  nat <- natura_2000_aq %>%
    st_transform(4326) %>%
    mutate(
      target_species = sp,
      target_species_slug = make_slug(sp),
      target_species_dutch = unname(species_labels_dutch[tolower(sp)]),
      analysis_layer = "SBZ-H",
      feature_type = "aq_habitat",
      context_type = "aq_habitat",
      status = NA_character_,
      first_date = NA_character_,
      fill_color = "lightgreen",
      fill_opacity = 0.4,
      border_color = "lightgreen",
      border_weight = 0.5,
      point_color = NA_character_,
      point_radius = NA_real_,
      point_fill = NA_character_,
      point_fill_opacity = NA_real_,
      popup_text = paste0("<b>Habitat:</b> ", HAB1)
    )
  
  list(hb, nat)
}

prepare_context_sbp <- function(sp) {
  pgs <- sbp_pgs_calc %>%
    st_transform(4326) %>%
    mutate(
      target_species = sp,
      target_species_slug = make_slug(sp),
      target_species_dutch = unname(species_labels_dutch[tolower(sp)]),
      analysis_layer = "SBP",
      feature_type = "sbp_polygon",
      context_type = "sbp_polygon",
      status = NA_character_,
      first_date = NA_character_,
      fill_color = "blue",
      fill_opacity = 0.2,
      border_color = "blue",
      border_weight = 1.0,
      point_color = NA_character_,
      point_radius = NA_real_,
      point_fill = NA_character_,
      point_fill_opacity = NA_real_,
      popup_text = paste0("<b>Soort:</b> ", soort, "<br><b>Gebied:</b> ", gebied)
    )
  
  pls <- sbp_vissen_calc %>%
    st_transform(4326) %>%
    mutate(
      target_species = sp,
      target_species_slug = make_slug(sp),
      target_species_dutch = unname(species_labels_dutch[tolower(sp)]),
      analysis_layer = "SBP",
      feature_type = "sbp_line",
      context_type = "sbp_line",
      status = NA_character_,
      first_date = NA_character_,
      fill_color = NA_character_,
      fill_opacity = NA_real_,
      border_color = "blue",
      border_weight = 2,
      point_color = NA_character_,
      point_radius = NA_real_,
      point_fill = NA_character_,
      point_fill_opacity = NA_real_,
      popup_text = paste0("<b>Soort:</b> ", soort, "<br><b>Gebied:</b> ", gebied)
    )
  
  list(pgs, pls)
}

prepare_clouds_export <- function(x, sp, layer_label, dutch_name) {
  if (is.null(x) || nrow(x) == 0) return(NULL)
  
  x %>%
    mutate(
      target_species = sp,
      target_species_slug = make_slug(sp),
      target_species_dutch = dutch_name,
      feature_type = "species_cloud",
      context_type = NA_character_,
      fill_opacity = 0.4,
      border_weight = 2,
      point_color = NA_character_,
      point_radius = NA_real_,
      point_fill = NA_character_,
      point_fill_opacity = NA_real_,
      popup_text = paste0(
        "<b>Soort:</b> ", dutch_name,
        "<br><b>Analyse:</b> ", layer_label,
        "<br><b>Status:</b> ", status,
        "<br><b>Eerste wnm:</b> ", first_date
      )
    )
}

prepare_absence_export <- function(x, sp, layer_label, dutch_name) {
  if (nrow(x) == 0) return(NULL)
  
  x %>%
    mutate(
      target_species = sp,
      target_species_slug = make_slug(sp),
      target_species_dutch = dutch_name,
      analysis_layer = layer_label,
      feature_type = "absence_point",
      context_type = NA_character_,
      status = "Afwezigheid",
      first_date = as.character(date),
      fill_color = NA_character_,
      fill_opacity = NA_real_,
      border_color = "black",
      border_weight = 1,
      point_color = "black",
      point_radius = 3,
      point_fill = "white",
      point_fill_opacity = 1,
      popup_text = paste0(
        "<b>Soort:</b> ", dutch_name,
        "<br><b>Type:</b> Afwezigheid (0)",
        "<br><b>Datum:</b> ", date
      )
    )
}

# ----------------------------------------------------
# 7. Outputmappen
# ----------------------------------------------------

out_root <- file.path(dir_bescherming_output, "geojson_per_soort_per_laag")
out_sbz  <- file.path(out_root, "sbz_h")
out_sbp  <- file.path(out_root, "sbp")

dir.create(out_root, recursive = TRUE, showWarnings = FALSE)
dir.create(out_sbz, recursive = TRUE, showWarnings = FALSE)
dir.create(out_sbp, recursive = TRUE, showWarnings = FALSE)

# ----------------------------------------------------
# 8. Export per soort
# ----------------------------------------------------

message("GeoJSON export per soort...")

index_list <- list()
idx <- 1L

for (sp in unique_species) {
  message("Verwerk soort: ", sp)
  
  slug <- make_slug(sp)
  dutch_name <- unname(species_labels_dutch[tolower(sp)])
  if (length(dutch_name) == 0 || is.na(dutch_name)) dutch_name <- sp
  
  sp_absences <- CF_absence_sf %>%
    filter(species == sp)
  
  # ------------------------------
  # SBZ-H
  # ------------------------------
  sbz_context <- prepare_context_hbtrl(sp)
  sbz_clouds <- prepare_clouds_export(clouds_hbtrl_list[[sp]], sp, "Habitatrichtlijn", dutch_name)
  sbz_abs <- prepare_absence_export(sp_absences, sp, "SBZ-H", dutch_name)
  
  sbz_parts <- c(sbz_context, list(sbz_clouds, sbz_abs))
  sbz_parts <- sbz_parts[!vapply(sbz_parts, is.null, logical(1))]
  sbz_parts <- sbz_parts[vapply(sbz_parts, nrow, integer(1)) > 0]
  
  if (length(sbz_parts) > 0) {
    sbz_parts <- align_schema(sbz_parts)
    sbz_out <- bind_rows(sbz_parts)
    write_geojson_safe(sbz_out, file.path(out_sbz, paste0(slug, ".geojson")))
  }
  
  # ------------------------------
  # SBP
  # ------------------------------
  sbp_context <- prepare_context_sbp(sp)
  sbp_clouds <- prepare_clouds_export(clouds_sbp_list[[sp]], sp, "SBP", dutch_name)
  sbp_abs <- prepare_absence_export(sp_absences, sp, "SBP", dutch_name)
  
  sbp_parts <- c(sbp_context, list(sbp_clouds, sbp_abs))
  sbp_parts <- sbp_parts[!vapply(sbp_parts, is.null, logical(1))]
  sbp_parts <- sbp_parts[vapply(sbp_parts, nrow, integer(1)) > 0]
  
  if (length(sbp_parts) > 0) {
    sbp_parts <- align_schema(sbp_parts)
    sbp_out <- bind_rows(sbp_parts)
    write_geojson_safe(sbp_out, file.path(out_sbp, paste0(slug, ".geojson")))
  }
  
  index_list[[idx]] <- list(
    species = sp,
    slug = slug,
    dutch_name = dutch_name,
    sbz_h_file = file.path("sbz_h", paste0(slug, ".geojson")),
    sbp_file = file.path("sbp", paste0(slug, ".geojson")),
    n_clouds_hbtrl = if (is.null(clouds_hbtrl_list[[sp]])) 0 else nrow(clouds_hbtrl_list[[sp]]),
    n_clouds_sbp = if (is.null(clouds_sbp_list[[sp]])) 0 else nrow(clouds_sbp_list[[sp]]),
    n_absences = nrow(sp_absences)
  )
  
  idx <- idx + 1L
}

# ----------------------------------------------------
# 9. Indexbestand
# ----------------------------------------------------

write_json(
  index_list,
  path = file.path(out_root, "species_layer_index.json"),
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

message("Klaar.")
message("SBZ-H bestanden: ", out_sbz)
message("SBP bestanden: ", out_sbp)
message("Index: ", file.path(out_root, "species_layer_index.json"))