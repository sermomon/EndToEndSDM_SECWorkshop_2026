
# Download CMEMS data using CopernicusMarine R

# This script is used as an entry point to download the CMEMS data for the workshop. The script avoids the
# use of Copernicus Marine Toolbox (Python API), so no Anaconda/Miniconda installation is required.
# The code uses the following R package: https://cran.r-project.org/web/packages/CopernicusMarine/index.html
# Code requires additional libraries: sf, stars, CFtime, ncdf4 and rstudioapi; GDAL >= 3.11 with BLOSC support.
# R version (tested with R 4.6.1). Older R versions get outdated binaries from CRAN.

# Sergio Morell-Monzó [sermomon@upv.es]

##### Load libraries and login to CMEMS #####

if(!requireNamespace("CopernicusMarine", quietly = TRUE)) {
  install.packages("CopernicusMarine")
}

library(CopernicusMarine)

CMS_USERNAME <- rstudioapi::showPrompt("Copernicus Marine", "Username:")
CMS_PASSWORD <- rstudioapi::askForPassword("Copernicus Marine password:")

#%% Define data to download

DATASETS <- list(
  list(
    name          = "phy_2d",
    product       = "GLOBAL_MULTIYEAR_PHY_001_030",
    layer         = "cmems_mod_glo_phy_my_0.083deg_P1D-m",
    variables     = c("mlotst", "zos")
  ),
  list(
    name          = "phy_3d",
    product       = "GLOBAL_MULTIYEAR_PHY_001_030",
    layer         = "cmems_mod_glo_phy_my_0.083deg_P1D-m",
    variables     = c("thetao", "so", "uo", "vo"),
    verticalrange = c(0, -1)
  ),
  list(
    name          = "bgc",
    product       = "GLOBAL_MULTIYEAR_BGC_001_029",
    layer         = "cmems_mod_glo_bgc_my_0.25deg_P1D-m",
    variables     = c("chl", "nppv"),
    verticalrange = c(0, -1)
  )
)

DATES <- c(
  "2015-02-01", "2015-02-02", "2015-02-03", "2015-02-04", "2015-02-05", "2015-02-06", 
  "2015-02-07", "2015-02-08", "2015-02-09", "2015-02-10", "2015-02-11", "2015-02-12", 
  "2015-02-13", "2015-02-14", "2015-02-15", "2015-02-16", "2015-02-17", "2015-02-18", 
  "2015-02-19", "2015-02-20", "2015-02-21", "2015-02-22", "2015-02-23", "2015-02-24",
  "2015-02-25", "2015-02-26"
) # Format: YYYY-MM-DD

MIN_LON <- -3; MAX_LON <- 15
MIN_LAT <- 35; MAX_LAT <- 46

OVERWRITE <- FALSE

OUT_DIR <- file.path("00inputOutput", "00input", "00rawData", "00enviro", "01CMEMS")

##### Setup process before launch #####

if (!is.null(CMS_USERNAME)) {
  CopernicusMarine::cms_set_username(CMS_USERNAME)
  CopernicusMarine::cms_set_password(CMS_PASSWORD)
} else {
  stop("Copernicus Marine credentials were not provided.")
}

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

region <- c(MIN_LON, MIN_LAT, MAX_LON, MAX_LAT)
dates <- format(as.Date(DATES), "%Y-%m-%d")

print(paste("Datasets to process:", length(DATASETS)))
print(paste("Dates to process:", length(dates)))
print(paste("LON:", MIN_LON, "to", MAX_LON))
print(paste("LAT:", MIN_LAT, "to", MAX_LAT))
print(paste("Output directory:", normalizePath(OUT_DIR)))

##### Download CMEMS data #####

# Name formaters
format_lon <- function(x) paste0(sprintf("%.2f", abs(x)), ifelse(x < 0, "W", "E"))
format_lat <- function(x) paste0(sprintf("%.2f", abs(x)), ifelse(x < 0, "S", "N"))

get_depth_label <- function(x) {
  dims <- names(stars::st_dimensions(x))
  depth_dim <- grep("depth|elevation", dims, value = TRUE)
  if (length(depth_dim) == 0) return(NULL)
  values <- abs(as.numeric(stars::st_get_dimension_values(x, depth_dim[1])))
  if (length(values) == 1) {
    paste0(sprintf("%.2f", values), "m")
  } else {
    paste0(sprintf("%.2f", min(values)), "-", sprintf("%.2f", max(values)), "m")
  }
}

# Main function to download
download_data <- function(dataset, dates) {
  vars_label <- if (is.null(dataset$variables)) "multi-vars" else paste(dataset$variables, collapse = "-")
  prefix <- paste(
    dataset$layer,
    vars_label,
    paste0(format_lon(MIN_LON), "-", format_lon(MAX_LON)),
    paste0(format_lat(MIN_LAT), "-", format_lat(MAX_LAT)),
    sep = "_"
  )
  out_files <- character(0)
  for (date in dates) {
    existing <- list.files(OUT_DIR)
    existing <- existing[startsWith(existing, paste0(prefix, "_")) & endsWith(existing, paste0("_", date, ".nc"))]
    if (length(existing) > 0 && !OVERWRITE) {
      message("Already exists, skipping: ", existing[1])
      out_files <- c(out_files, file.path(OUT_DIR, existing[1]))
      next
    }
    args <- list(
      product = dataset$product,
      layer = dataset$layer,
      region = region,
      timerange = c(paste(date, "00:00:00 UTC"), paste(date, "23:59:59 UTC")),
      progress = FALSE
    )
    if (!is.null(dataset$variables)) args$variable <- dataset$variables
    if (!is.null(dataset$verticalrange)) args$verticalrange <- dataset$verticalrange
    x <- do.call(cms_download_subset, args)
    out_name <- paste(c(prefix, get_depth_label(x), date), collapse = "_")
    out_file <- file.path(OUT_DIR, paste0(out_name, ".nc"))
    cms_write_ncdf(x, out_file)
    message("  Saved: ", basename(out_file))
    out_files <- c(out_files, out_file)
  }
  invisible(out_files)
}

# Download data (loop over the datasets)
downloaded_files <- character(0)
for (dataset in DATASETS) {
  message("Dataset: ", dataset$name)
  downloaded_files <- c(downloaded_files, download_data(dataset, dates))
}

# Downloaded files
files_info <- data.frame(
  file    = basename(downloaded_files),
  size_mb = round(file.size(downloaded_files) / 1024^2, 2)
)
print(files_info, row.names = FALSE)
message("Total files: ", nrow(files_info))
message("Total size: ", round(sum(files_info$size_mb), 2), " MB")

print(files_info, row.names = FALSE)

message("Total files: ", nrow(files_info))
message("Total size: ", round(sum(files_info$size_mb), 2), " MB")

