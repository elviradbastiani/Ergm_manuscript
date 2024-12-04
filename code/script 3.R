#*****************************************************************************************
# Package to define the social interaction among Vultures
# Author: Elvira D'Bastiani
# Department of Ecology and Evolutionary Biology, University of California Los Angeles
# Corresponding Author: Elvira D'Bastiani, Email: elviradbastiani@gmail.com
#*****************************************************************************************

# Load necessary packages and handle global variables for compatibility
if (getRversion() >= "2.15.1") utils::globalVariables(".")

#renv
# renv::init()
# 
# renv::install("RcppArmadillo")
# 
# renv::snapshot()

#describe where the packages are - which library
# .libPaths()
# .find.package("ggplot2")
# 

# Install and load required packages -----------------------------------------------------

# List of packages to install
packages <- c("spatsoc", "renv", "here", "move", "checkmate", "dplyr", 
              "lubridate", "igraph", "purrr", "sf", "readxl", "suncalc", 
              "tidyr", "mapview", "ggplot2", "knitr", "curl")

# Function to check and install missing packages
install_if_missing <- function(packages) {
  installed <- rownames(installed.packages())
  missing <- packages[!(packages %in% installed)]
  if (length(missing) > 0) {
    install.packages(missing, dependencies = TRUE)
  }
}

# Install the packages
install_if_missing(packages)

# Load the packages to confirm installation
lapply(packages, library, character.only = TRUE)

#Run garbage collection to free up unused memory:
gc()

memory.size()  # Check current memory usage
memory.limit() # Check the memory limit

library(dplyr)
library(ggplot2)
library(readxl)
library(sf)
library(mapview)
library(tidyverse)
library(here)

# List of required packages
packages <- c("spatsoc", "renv", "here", "move", "checkmate", "dplyr", 
              "lubridate", "igraph", "purrr", "sf", "readxl", "suncalc", 
              "tidyr", "mapview", "ggplot2", "knitr", "curl")

# Install missing packages and then load them
lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
})


# Ensure the directory exists to save the figures
dir.create("~/Documents/GitHub/Ergm_manuscript/figures", recursive = TRUE, showWarnings = FALSE)


# library("devtools")
# devtools::install_github("kaijagahm/vultureUtils")# 
# library(vultureUtils)

#install.packages("knitr", dependencies = TRUE)

# Set the project directory
#setwd("~/Documents/GitHub/Ergm_manuscript/data")

#dir.create(file.path("Project 2"), recursive = TRUE)


# Initialize renv in the project folder (only if you intend to use renv)
# renv::init()


# Function Definitions -------------------------------------------------------------------

#' Step 1.

#' Download Vulture Data
#'
#' Downloads data from the Israel vulture study Movebank repository.
#' Requires Movebank credentials to access the data.
#'
#' @param loginObject A Movebank login object created with move::movebankLogin
#' @param extraSensors Logical; include extra sensors (default is FALSE)
#' @param removeDup Logical; remove duplicated timestamps (default is TRUE)
#' @param dateTimeStartUTC Start time as POSIXct in UTC (default is NULL)
#' @param dateTimeEndUTC End time as POSIXct in UTC (default is NULL)
#' @param addDateOnly Logical; add date-only column extracted from timestamp (default is TRUE)
#' @param dfConvert Logical; convert the returned data to a data frame (default is TRUE)
#' @param quiet Logical; suppress warnings for duplicate records (default is FALSE)
#' @param ... Additional arguments for move::getMovebankData()
#' @return A moveStack or data.frame object
#' @export
#'
downloadVultures <- function(loginObject, extraSensors = FALSE, removeDup = TRUE,
                             dateTimeStartUTC = NULL, dateTimeEndUTC = NULL,
                             addDateOnly = TRUE, dfConvert = TRUE, quiet = FALSE, ...) {
  # Argument checks
  checkmate::assertClass(loginObject, "MovebankLogin")
  checkmate::assertLogical(extraSensors, len = 1)
  checkmate::assertLogical(removeDup, len = 1)
  
  # Convert datetime arguments to POSIXct if necessary
  if (is.character(dateTimeStartUTC)) dateTimeStartUTC <- as.POSIXct(dateTimeStartUTC)
  if (is.character(dateTimeEndUTC)) dateTimeEndUTC <- as.POSIXct(dateTimeEndUTC)
  checkmate::assertPOSIXct(dateTimeStartUTC, null.ok = TRUE)
  checkmate::assertPOSIXct(dateTimeEndUTC, null.ok = TRUE)
  
  # Download data using specified parameters
  if (quiet) {
    dat <- suppressWarnings(suppressMessages(
      move::getMovebankData(
        study = "Ornitela_Vultures_Gyps_fulvus_TAU_UCLA_Israel",
        login = loginObject,
        includeExtraSensors = extraSensors,
        deploymentAsIndividuals = FALSE,
        removeDuplicatedTimestamps = removeDup,
        timestamp_start = dateTimeStartUTC,
        timestamp_end = dateTimeEndUTC,
        ...
      )
    ))
  } else {
    dat <- move::getMovebankData(
      study = "Ornitela_Vultures_Gyps_fulvus_TAU_UCLA_Israel",
      login = loginObject,
      includeExtraSensors = extraSensors,
      deploymentAsIndividuals = FALSE,
      removeDuplicatedTimestamps = removeDup,
      timestamp_start = dateTimeStartUTC,
      timestamp_end = dateTimeEndUTC,
      ...
    )
  }
  
  # Add date-only column if specified
  if (addDateOnly) dat$dateOnly <- as.Date(as.character(dat$timestamp))
  
  # Convert to data frame if specified
  if (dfConvert) {
    dat <- methods::as(dat, "data.frame")
    if (!("trackId" %in% names(dat))) dat$trackId <- dat$local_identifier
  }
  
  return(dat)
}



# Step 1. Movebank Login ------------------------------------------------------------------------

# Load Movebank credentials and login
base::load(here::here("~/Documents/GitHub/Ergm_manuscript/data/movebankCredentials/mypassword.Rda"))
MB.LoginObject <- move::movebankLogin(username = "edbastiani", password = mypassword)

# Remove password from environment for security
rm(mypassword)


# Step 2. Download Data ------------------------------------------------------------------------

# Set date range for data download
minDate <- "2020-09-15 00:00"
maxDate <- "2020-12-31 00:00"

mydata_2020 <- downloadVultures(loginObject = MB.LoginObject,
                                removeDup = T,
                                dfConvert = T,
                                quiet = T,
                                dateTimeStartUTC = minDate,
                                dateTimeEndUTC = maxDate)

minDate <- "2021-01-01 00:00"
maxDate <- "2021-12-31 00:00"

mydata_2021 <- downloadVultures(loginObject = MB.LoginObject,
                                removeDup = T,
                                dfConvert = T,
                                quiet = T,
                                dateTimeStartUTC = minDate,
                                dateTimeEndUTC = maxDate)

# Set min and max date
minDate <- "2022-01-01 00:00"
maxDate <- "2022-12-31 00:00"

mydata_2022 <- downloadVultures(loginObject = MB.LoginObject,
                                removeDup = T,
                                dfConvert = T,
                                quiet = T,
                                dateTimeStartUTC = minDate,
                                dateTimeEndUTC = maxDate)

# Set min and max date
minDate <- "2023-01-01 00:00"
maxDate <- "2023-12-31 00:00"

mydata_2023 <- downloadVultures(loginObject = MB.LoginObject,
                                removeDup = T,
                                dfConvert = T,
                                quiet = T,
                                dateTimeStartUTC = minDate,
                                dateTimeEndUTC = maxDate)

# Set min and max date
minDate <- "2024-01-01 00:00"
maxDate <- "2024-09-15 00:00"

mydata_2024 <- downloadVultures(loginObject = MB.LoginObject,
                                removeDup = T,
                                dfConvert = T,
                                quiet = T,
                                dateTimeEndUTC = maxDate)


dim(mydata_2020)+
  dim(mydata_2021)+
  dim(mydata_2022)+
  dim(mydata_2023)+
  dim(mydata_2024)


movebank_raw_data_2020_2024<-data.frame(rbind(mydata_2020, 
                                              mydata_2021,
                                              mydata_2022,
                                              mydata_2023,
                                              mydata_2024))

# Step 3. Save the Download Data ------------------------------------------------------------------------

# Define the file path for saving the downloaded data
file_path <- "~/Documents/GitHub/Ergm_manuscript/data/raw_data/movebank_raw_data_2020_2024.Rda"

# Save the data as an .Rda file
#save(movebank_raw_data_2020_2024, file = file_path)

load(file_path)

# Step 4. Fix Vulture Names in Who's Who and Movebank Data-----------------------------------------------------------------

#' Fix Vulture Names in Who's Who and Movebank Data
#' This function reads the vulture data and "Who's Who" dataset, standardizes names,
#' identifies and fixes mismatches, and saves the cleaned dataset to a specified file path.
#' @param mydata The raw Movebank vulture data as a data frame.
#' @param who_file_path The file path to the "Who's Who" Excel file.
#' @param save_path The file path to save the cleaned data.
#' @return A data frame with cleaned and standardized vulture names.
#' @export
#'
fix_vulture_names <- function(mydata,
                              who_file_path = "~/Documents/GitHub/Ergm_manuscript/data/raw_data/whoswho_vultures_20230920_new.xlsx") {
  
  # Step - Load and Prepare Who's Who Data -----------------------------------
  
  # Read the Who's Who data
  ww <- readxl::read_excel(who_file_path, sheet = "all gps tags")
  
  # Select only relevant columns and remove duplicates
  ww_tojoin <- ww %>%
    dplyr::select(Nili_id, Movebank_id) %>%
    dplyr::distinct()
  
  # Step - Identify Mismatched Identifiers -----------------------------------
  
  # Find identifiers in `mydata` that are missing in `ww_tojoin`
  problems <- mydata %>%
    dplyr::filter(!(local_identifier %in% ww_tojoin$Movebank_id)) %>%
    dplyr::pull(local_identifier) %>%
    unique()
  print(problems) # Review mismatches
  
  # Step - Fix Known Mismatches in Who's Who ---------------------------------
  
  # Correct typos in `Movebank_id` column of `ww_tojoin`
  ww_tojoin <- ww_tojoin %>%
    dplyr::mutate(Movebank_id = dplyr::case_when(
      Movebank_id == "A65 Whiite" ~ "A65 White",
      TRUE ~ Movebank_id
    ))
  
  # Step - Fix Known Mismatches in `mydata` ----------------------------------
  
  # Apply fixes to known issues in `mydata`
  mydata <- mydata %>%
    dplyr::mutate(local_identifier = dplyr::case_when(
      local_identifier == "E86 White" ~ "E86",
      local_identifier == "E88 White" ~ "E88w",
      local_identifier == "B38w (T61w)" ~ "T61w",
      TRUE ~ local_identifier
    ))
  
  # Step - Verify Remaining Mismatches ---------------------------------------
  
  # Recheck for any remaining mismatches after initial fixes
  problems <- mydata %>%
    dplyr::filter(!(local_identifier %in% ww_tojoin$Movebank_id)) %>%
    dplyr::pull(local_identifier) %>%
    unique()
  print(problems) # Inspect remaining issues
  
  # Step - Additional Fixes for Remaining Issues -----------------------------
  
  # Apply additional manual fixes based on review
  mydata <- mydata %>%
    dplyr::left_join(ww_tojoin, by = c("local_identifier" = "Movebank_id")) %>%
    dplyr::mutate(Nili_id = dplyr::case_when(
      is.na(Nili_id) & local_identifier == "E66 White" ~ "E66",
      is.na(Nili_id) & local_identifier == "Y01>T60 W" ~ "tammy",
      TRUE ~ Nili_id
    ))
  
  # Verify that no NAs remain in Nili_id
  nas <- mydata %>%
    dplyr::filter(is.na(Nili_id)) %>%
    dplyr::pull(local_identifier) %>%
    unique()
  print(length(nas)) # Confirm no remaining NAs
  
  # Return the cleaned data
  return(mydata)
}

movebank_raw_data_1 <- fix_vulture_names(movebank_raw_data_2020_2024)

# [1] "B38w (T61w)"        "B67w"               "B80w"              
# [4] "E97w (A67>T40>Y14)" "E99w (T98<Y11)"    
# [1] "B67w"               "B80w"               "E97w (A67>T40>Y14)"
# [4] "E99w (T98<Y11)"    
# [1] 4

# Define the file path for saving the downloaded data
file_path <- "~/Documents/GitHub/Ergm_manuscript/data/raw_data/movebank_raw_data_1.Rda"

# Save the data
#save(movebank_raw_data_1, file = file_path)

#load the data if needed
load(file_path)

# Step 5. Clean GPS Data-----------------------------------------------------------------

#****************************************************
# cleanData Helpers Functions - start
#****************************************************
#' Get Stats
#'
#' Retrieves basic statistics from a dataset, including the number of rows, columns, and unique individuals.
#' @param df A data frame containing data points.
#' @param idCol Column name containing vulture IDs. Default is "Nili_id".
#' @return A named vector with `rows`, `cols`, and `indivs`.
#' @export
getStats <- function(df, idCol = "Nili_id") {
  rows <- nrow(df)
  cols <- ncol(df)
  indivs <- length(unique(df[[idCol]]))
  return(c("rows" = rows, "cols" = cols, "indivs" = indivs))
}

#' Temperature, Barometric Height, and Ground Speed Filter
#'
#' Filters out rows where `external_temperature`, `barometric_height`, and `ground_speed` are all zero or NA.
#' @param dataset A data frame with columns: `external_temperature`, `barometric_height`, `ground_speed`.
#' @return A filtered dataset.
#' @export
tempHeightSpeedFilter <- function(dataset) {
  dataset %>%
    dplyr::mutate(outlier = ifelse(
      external_temperature == 0 & barometric_height == 0 & ground_speed == 0, 1, 0
    )) %>%
    dplyr::filter(is.na(outlier) | outlier == 0) %>%
    dplyr::select(-outlier)
}

#' GPS Time to Fix Filter
#'
#' Filters out rows with GPS time to fix greater than a specified maximum time.
#' @param dataset A data frame with column `gps_time_to_fix`.
#' @param maxTime Maximum allowable GPS fix time. Default is 89.
#' @return A filtered dataset.
#' @export
gpsTimeFilter <- function(dataset, maxTime = 89) {
  dataset %>% dplyr::filter(gps_time_to_fix <= maxTime)
}

#' Heading Filter
#'
#' Filters out rows with invalid heading values (outside 0-360 degrees).
#' @param dataset A data frame with column `heading`.
#' @return A filtered dataset.
#' @export
headingFilter <- function(dataset) {
  dataset %>% dplyr::filter(heading >= 0 & heading <= 360)
}

#' Satellite Filter
#'
#' Filters out rows with GPS satellite counts below a specified minimum.
#' @param dataset A data frame with column `gps_satellite_count`.
#' @param minSatellites Minimum required satellites. Default is 3.
#' @return A filtered dataset.
#' @export
satelliteFilter <- function(dataset, minSatellites = 3) {
  dataset %>% dplyr::filter(gps_satellite_count >= minSatellites)
}

#' Precise Filter
#'
#' Filters out rows with fewer than 4 satellites or GPS HDOP greater than 5.
#' @param dataset A data frame with columns `gps_satellite_count` and `gps_hdop`.
#' @return A filtered dataset.
#' @export
preciseFilter <- function(dataset) {
  dataset %>% dplyr::filter(gps_satellite_count > 4 & gps_hdop < 5)
}

#' Spiky Speeds Filter
#'
#' Filters out unrealistic speeds based on distance and time between points.
#' Also removes points with high travel distances during night time.
#' @param dataset A data frame with timestamp and location columns.
#' @param idCol Column name for vulture IDs. Default is "Nili_id".
#' @param longCol Column name for longitude. Default is "location_long.1".
#' @param latCol Column name for latitude. Default is "location_lat.1".
#' @return A list with the filtered dataset and statistics on spiky speeds.
#' @export
spikySpeedsFilter <- function(dataset, idCol = "Nili_id", longCol = "location_long.1", latCol = "location_lat.1") {
  df <- calcSpeeds(dataset, grpCol = idCol, longCol = longCol, latCol = latCol)
  
  df2 <- df %>%
    dplyr::filter(lead_speed_m_s <= 50 & abs(lag_speed_m_s) <= 50) %>%
    dplyr::select(-c(lead_hour_diff_sec, lead_dist_m, lead_speed_m_s, lag_hour_diff_sec, lag_dist_m, lag_speed_m_s))
  
  df2 <- calcSpeeds(df2, grpCol = idCol, longCol = longCol, latCol = latCol)
  df3 <- df2 %>% dplyr::filter(lead_speed_m_s <= 50)
  
  times <- suncalc::getSunlightTimes(
    date = unique(lubridate::date(df3$timestamp)),
    lat = 31.434306, lon = 34.991889, keep = c("sunrise", "sunset")
  ) %>% dplyr::select("dateOnly" = date, sunrise, sunset)
  
  df4 <- df3 %>%
    dplyr::mutate(dateOnly = lubridate::ymd(dateOnly)) %>%
    dplyr::left_join(times, by = "dateOnly") %>%
    dplyr::mutate(daylight = ifelse(timestamp >= sunrise & timestamp <= sunset, "day", "night")) %>%
    dplyr::select(-c(sunrise, sunset))
  
  df4 <- calcSpeeds(df4, grpCol = idCol, longCol = longCol, latCol = latCol)
  
  df5 <- df4 %>%
    dplyr::mutate(day_diff = as.numeric(difftime(dplyr::lead(lubridate::date(timestamp)), lubridate::date(timestamp), units = "days")),
                  night_outlier = ifelse(daylight == "night" & day_diff %in% c(0, 1) & dplyr::lead(daylight) == "night" & lead_dist_m > 10000, TRUE, FALSE)) %>%
    dplyr::filter(!night_outlier) %>%
    dplyr::select(-c(lead_hour_diff_sec, lag_hour_diff_sec, lead_dist_m, lag_dist_m, lead_speed_m_s, lag_speed_m_s))
  
  list("dataset" = df5, "spikySpeeds" = getStats(df5, idCol))
}

#' Spiky Altitudes Filter
#'
#' Filters out unrealistic altitudes based on vertical speed thresholds.
#' @param dataset A data frame with altitude column.
#' @param idCol Column name for vulture IDs. Default is "Nili_id".
#' @return A list with the filtered dataset and count of NA altitudes.
#' @export
spikyAltitudesFilter <- function(dataset, idCol = "Nili_id") {
  dfAlt <- calcSpeedsVert(dataset, grpCol = idCol, altCol = "height_above_msl")
  dfAlt$height_above_msl[abs(dfAlt$lead_speed_m_s) > 2 | abs(dfAlt$lag_speed_m_s) > 2] <- NA
  
  nAltitudesToNA <- sum(is.na(dfAlt$height_above_msl)) - sum(is.na(dataset$height_above_msl))
  list("dataset" = dfAlt, "nAltitudesToNA" = nAltitudesToNA)
}

#' Calculate Speeds
#'
#' Calculates speeds based on latitude and longitude values.
#' @param df A data frame with timestamp and location columns.
#' @param grpCol Column name for grouping by individual.
#' @param longCol Column name for longitude.
#' @param latCol Column name for latitude.
#' @return A data frame with speed columns added.
#' @export
calcSpeeds <- function(df, grpCol, longCol, latCol) {
  df %>%
    dplyr::group_by(.data[[grpCol]]) %>%
    dplyr::arrange(timestamp) %>%
    dplyr::mutate(
      lead_hour_diff_sec = round(as.numeric(difftime(dplyr::lead(timestamp), timestamp, units = "secs")), 3),
      lag_hour_diff_sec = round(as.numeric(difftime(dplyr::lag(timestamp), timestamp, units = "secs")), 3),
      lead_dist_m = geosphere::distGeo(cbind(dplyr::lead(.data[[longCol]]), dplyr::lead(.data[[latCol]])), cbind(.data[[longCol]], .data[[latCol]])),
      lag_dist_m = geosphere::distGeo(cbind(dplyr::lag(.data[[longCol]]), dplyr::lag(.data[[latCol]])), cbind(.data[[longCol]], .data[[latCol]])),
      lead_speed_m_s = round(lead_dist_m / lead_hour_diff_sec, 2),
      lag_speed_m_s = round(lag_dist_m / lag_hour_diff_sec, 2)
    ) %>%
    dplyr::ungroup()
}

#' Calculate Vertical Speeds
#'
#' Calculates vertical speeds based on altitude changes.
#' @param df A data frame with altitude values.
#' @param grpCol Column name for grouping by individual.
#' @param altCol Column name for altitude.
#' @return A data frame with vertical speed columns added.
#' @export
calcSpeedsVert <- function(df, grpCol, altCol) {
  df %>%
    dplyr::group_by(.data[[grpCol]]) %>%
    dplyr::arrange(timestamp) %>%
    dplyr::mutate(
      lead_hour_diff_sec = round(as.numeric(difftime(dplyr::lead(timestamp), timestamp, units = "secs")), 3),
      lag_hour_diff_sec = round(as.numeric(difftime(dplyr::lag(timestamp), timestamp, units = "secs")), 3),
      lead_dist_mV = round(dplyr::lead(.data[[altCol]]) - .data[[altCol]], 3),
      lag_dist_mV = round(dplyr::lag(.data[[altCol]]) - .data[[altCol]], 3),
      lead_speed_m_s = round(lead_dist_mV / lead_hour_diff_sec, 2),
      lag_speed_m_s = round(lag_dist_mV / lag_hour_diff_sec, 2)
    ) %>%
    dplyr::ungroup()
}


#' Remove Unnecessary Variables
#'
#' Removes specified unnecessary variables from a dataset, making it smaller and more manageable.
#' Allows for additional variables to be removed and for certain variables to be kept, even if they would otherwise be removed.
#' @param dataset A data frame from which to remove variables.
#' @param addlVars A character vector of additional variables to remove (optional).
#' @param keepVars A character vector of variables to keep, even if they are listed for removal (optional).
#' @return A data frame with specified variables removed.
#' @export
removeUnnecessaryVars <- function(dataset, addlVars = NULL, keepVars = NULL) {
  # Argument validation
  checkmate::assertDataFrame(dataset)
  checkmate::assertCharacter(addlVars, null.ok = TRUE)
  checkmate::assertCharacter(keepVars, null.ok = TRUE)
  
  # Default list of variables to remove
  defaultVars <- c(
    "sensor_type_id", "taxon_canonical_name", "nick_name", "earliest_date_born", "sensor", "optional",
    "sensor_type", "mw_activity_count", "eobs_accelerations_raw", "eobs_acceleration_sampling_frequency_per_axis",
    "eobs_acceleration_axes", "argos_valid_location_algorithm", "argos_sensor_4", "argos_sensor_3", "argos_sensor_2",
    "argos_sensor_1", "argos_semi_minor", "argos_semi_major", "argos_pass_duration", "argos_orientation", "argos_nopc",
    "argos_lat1", "argos_lat2", "1084088", "argos_lon1", "argos_lon2", "argos_nb_mes", "argos_nb_mes_120",
    "eobs_key_bin_checksum", "eobs_fix_battery_voltage", "eobs_battery_voltage", "eobs_status",
    "eobs_start_timestamp", "eobs_type_of_fix", "eobs_used_time_to_get_fix", "eobs_temperature",
    "gps_dop", "magnetic_field_raw_x", "magnetic_field_raw_y", "magnetic_field_raw_z", "ornitela_transmission_protocol",
    "tag_voltage", "algorithm_marked_outlier", "argos_altitude", "argos_best_level", "argos_lc", "argos_iq",
    "argos_gdop", "argos_error_radius", "argos_calcul_freq", "timestamps", "height_raw",
    "barometric_pressure", "barometric_height", "battery_charging_current", "eobs_activity", "manually_marked_outlier",
    "eobs_activity_samples", "acceleration_raw_y", "data_decoding_software", "gps_vdop", "height_above_ellipsoid",
    "acceleration_raw_x", "acceleration_raw_z", "eobs_horizontal_accuracy_estimate", "eobs_speed_accuracy_estimate"
  )
  
  # Combine default and additional variables to remove
  toRemove <- unique(c(defaultVars, addlVars))
  
  # Exclude any variables specified to be kept
  if (!is.null(keepVars)) {
    toRemove <- toRemove[!toRemove %in% keepVars]
  }
  
  # Remove specified variables from the dataset
  newDataset <- dataset %>%
    dplyr::select(-tidyselect::any_of(toRemove))
  
  return(newDataset)
}

#****************************************************
# cleanData Helpers Functions - end
#****************************************************

#Clean GPS Data function

#' Cleans and filters raw GPS data downloaded from Movebank, performing a series of filtering
#' steps based on GPS quality and data integrity. The cleaned dataset can be used directly in
#' other analysis functions.
#'
#' @param dataset The GPS dataset to be used, containing columns for longitude, latitude, and timestamp.
#'        Additional columns for "gps_time_to_fix", "heading", "gps_satellite_count", and "ground_speed"
#'        are also required for data cleaning steps.
#' @param gpsMaxTime Maximum time for GPS to communicate with satellites. If < 0, no filter is applied. Default is -1.
#' @param precise Logical; if TRUE, applies a higher quality filter (satellites > 4 and hdop < 5). Default is FALSE.
#' @param removeVars Logical; if TRUE, removes unnecessary variables from Movebank download. Default is TRUE.
#' @param report Logical; if TRUE, prints a report of removed rows/individuals in each step. Default is TRUE.
#' @param longCol Name of the column with longitude values. Default is "location_long.1".
#' @param latCol Name of the column with latitude values. Default is "location_lat.1".
#' @param idCol Name of the column with vulture ID's. Default is "Nili_id".
#' @param ... Additional arguments passed to other functions for variable management.
#' @return A cleaned GPS dataset with filtered data.
#' @export
#'

cleanData <- function(dataset,
                      gpsMaxTime = -1,
                      precise = F,
                      longCol = "location_long.1",
                      latCol = "location_lat.1",
                      idCol = "Nili_id",
                      removeVars = T,
                      report = T, ...){
  # Argument checks
  checkmate::assertDataFrame(dataset)
  checkmate::assertCharacter(longCol, len = 1)
  checkmate::assertCharacter(latCol, len = 1)
  checkmate::assertChoice("gps_time_to_fix", names(dataset))
  checkmate::assertChoice("heading", names(dataset))
  checkmate::assertChoice("gps_satellite_count", names(dataset))
  checkmate::assertChoice("ground_speed", names(dataset))
  checkmate::assertChoice("external_temperature", names(dataset))
  checkmate::assertChoice("barometric_height", names(dataset))
  checkmate::assertClass(dataset$timestamp, "POSIXct")
  
  # For checking as we go along and getting a report: a little function to calculate rows, columns, and individuals.
  filterNames <- c("Input data")
  reportData <- data.frame()
  init <- getStats(dataset, idCol) # get an initial baseline from the input data.
  reportData <- dplyr::bind_rows(reportData, init)
  
  # Basic data quality filters ----------------------------------------------
  # Remove outlier points based on zeroes (Marta's code)
  dataset <- tempHeightSpeedFilter(dataset)
  outliers <- getStats(dataset, idCol) # AAA
  reportData <- dplyr::bind_rows(reportData, outliers)
  filterNames <- append(filterNames, "Removed outliers with zeroes in three columns")
  
  # filter out bad gps data
  if(gpsMaxTime > 0){
    dataset <- gpsTimeFilter(dataset, maxTime = gpsMaxTime)
    badTimeToFix <- getStats(dataset, idCol) # AAA
    reportData <- dplyr::bind_rows(reportData, badTimeToFix)
    filterNames <- append(filterNames, "Removed points that took too long to get GPS fix")
  }
  # filter out bad heading data
  dataset <- headingFilter(dataset)
  badHeading <- getStats(dataset, idCol) # AAA
  reportData <- dplyr::bind_rows(reportData, badHeading)
  filterNames <- append(filterNames, "Removed points with invalid heading data")
  
  # only take locs that have at least 3 satellites
  dataset <- satelliteFilter(dataset, minSatellites = 3)
  badSatellites <- getStats(dataset, idCol) # AAA
  reportData <- dplyr::bind_rows(reportData, badSatellites)
  filterNames <- append(filterNames, "Removed points with too few satellites")
  
  # precise filter: remove points with < 4 satellites and gps_hdop > 5
  if(precise){
    dataset <- preciseFilter(dataset)
    lowQualityPoints <- getStats(dataset, idCol) # AAA
    reportData <- dplyr::bind_rows(reportData, lowQualityPoints)
    filterNames <- append(filterNames, "Removed points with < 4 satellites and hdop > 5")
  }
  
  # SPIKY SPEEDS
  values <- spikySpeedsFilter(dataset, idCol=idCol, longCol=longCol, latCol=latCol)
  dataset <- values$dataset
  spikySpeeds <- values$spikySpeeds
  nightDistance <- getStats(dataset, idCol) # AAA
  
  reportData <- dplyr::bind_rows(reportData, spikySpeeds)
  reportData <- dplyr::bind_rows(reportData, nightDistance)
  filterNames <- append(filterNames, "Removed spiky speeds")
  filterNames <- append(filterNames, "Removed points that moved too far at night")
  
  # remove unrealistic "spiky" altitude values (XXX TO DO)
  values <- spikyAltitudesFilter(dataset, idCol=idCol)
  dataset <- values$dataset
  nAltitudesToNA <- values$nAltitudesToNA
  
  # Remove unnecessary variables, if desired. ---------------------------
  if(removeVars == T){
    varsRemoved <- removeUnnecessaryVars(dataset)
    nColsRemoved <- ncol(dataset)-ncol(varsRemoved) # AAA
    dataset <- varsRemoved
  }
  
  final <- getStats(dataset, idCol)
  reportData <- dplyr::bind_rows(reportData, final)
  filterNames <- append(filterNames, "Final")
  
  if(report){
    steps <- filterNames
    df <- reportData %>%
      dplyr::mutate(step = steps) %>%
      dplyr::relocate(step) %>%
      dplyr::mutate(rowsLost = dplyr::lag(rows) - rows,
                    propRowsLost = round(rowsLost/dplyr::lag(rows), 3))
    print(df)
  }
  out <- dataset
  return(out %>%
           dplyr::ungroup())
}

# Clean and save movebank_raw_data_1
movebank_raw_data_cleaned_2 <- cleanData(movebank_raw_data_1,
                                         gpsMaxTime = -1,
                                         precise = FALSE,
                                         longCol = "location_long",
                                         latCol = "location_lat",
                                         idCol = "Nili_id",
                                         removeVars = TRUE,
                                         report = TRUE)

#remove data set movebank_raw_data_1
remove(movebank_raw_data_1)

#save
file_path <- "~/Documents/GitHub/Ergm_manuscript/data/raw_data/movebank_raw_data_cleaned_2.Rda"

# Save the data
#save(movebank_raw_data_cleaned_2, file = file_path)

# Load the .Rda file
load(file_path)


# Step 6. In Mask Filter-----------------------------------------------------------------

#****************************************************
# Mask filter helpers functions - start
#****************************************************
#' Mask dataset
#'
#' Given a mask and a dataset, apply the mask to the dataset and return only locations inside the mask.
#' @param dataset a dataset to mask
#' @param mask an sf object to use as the mask.
#' @param longCol the name of the column in the dataset containing longitude (or x coordinate) values
#' @param latCol the name of the column in the dataset containing latitude (or y coordinate) values
#' @param crsToSet If `dataset` is not already an sf object, a character string to be passed to `st_set_crs()`). One of (i) character: a string accepted by GDAL, (ii) integer, a valid EPSG value (numeric), or (iii) an object of class crs. Default is "WGS84".
#' @param op Operation to use with mask. Default is "intersects", returning points in dataset that lie in mask. Can also be "difference", to return points in dataset that lie outside of mask.
#' @return A masked data set.
#' @export
maskData <- function(dataset,
                     mask,
                     longCol = "location_long",
                     latCol = "location_lat",
                     crsToSet = "WGS84",
                     op = "intersect"){
  # argument checks
  checkmate::assertClass(mask, "sf")
  checkmate::assertDataFrame(dataset)
  checkmate::assertCharacter(longCol, len = 1)
  checkmate::assertCharacter(latCol, len = 1)
  
  # check if the dataset is already an sf object
  issf <- checkmate::testClass(dataset, "sf")
  
  # if not, convert it to an sf object
  if(issf == FALSE){
    checkmate::assertSubset(x = c(longCol, latCol), choices = names(dataset))
    dataset_sf <- sf::st_as_sf(dataset, coords = c(longCol, latCol), remove = FALSE)
    dataset_sf <- sf::st_set_crs(dataset_sf, value = crsToSet)
  }else{
    dataset_sf <- dataset
  }
  
  # check the CRS: is it the same as the mask CRS?
  same <- sf::st_crs(mask) == sf::st_crs(dataset_sf)
  if(!same){
    dataset_sf <- sf::st_transform(dataset_sf, crs = sf::st_crs(mask))
  }
  # mask the dataset
  if(op == "intersect")
    masked <- dataset_sf[mask, , op = sf::st_intersects] # XXX i think i can speed this up if i I just use st_intersects directly, maybe?
  else if(op == "difference")
    masked <- dataset_sf[lengths(sf::st_intersects(dataset_sf, mask)) == 0, ]
  
  # return the masked dataset
  return(masked)
}


#' Which individuals spent time mostly in the masked area?
#'
#' Compare masked and unmasked data. Return IDs of individual birds that spent at least `thresh` proportion of days in the masked area (vs. outside of the masked area). This function should be used after creating a masked dataset with vultureUtils::maskData(). Note: this function does its calculations based on numbers of *days*. Later modifications might allow for other units of time, but for now everything is in days.
#' @param dataset the full dataset, before masking.
#' @param maskedDataset the dataset after being masked to Israel (output of maskIsrael function)
#' @param thresh proportion (between 0 and 1) of a vulture's total tracked days that it spent in the mask, or number of days the vulture must spend in the mask (values > 1)
#' @param dateCol the name of the column containing dates (must be the same in `dataset` and `maskedDataset`). Defaults to "dateOnly".
#' @param idCol the name of the column containing vulture ID's.
#' @return A vector of id's for vultures
#' @export
mostlyInMask <- function(dataset, maskedDataset, thresh = 0.333, dateCol = "dateOnly", idCol = "Nili_id"){
  # argument checks
  checkmate::assertDataFrame(dataset)
  checkmate::assertDataFrame(maskedDataset)
  checkmate::assertNumeric(thresh, len = 1)
  checkmate::assertCharacter(dateCol, len = 1)
  checkmate::assertSubset(idCol, names(dataset))
  checkmate::assertSubset(idCol, names(maskedDataset))
  checkmate::assertSubset(dateCol, names(dataset))
  checkmate::assertSubset(dateCol, names(maskedDataset))
  # check that the `dateCol` columns actually are dates.
  checkmate::assertClass(dataset %>% dplyr::pull({{dateCol}}), "Date")
  checkmate::assertClass(maskedDataset %>% dplyr::pull({{dateCol}}), "Date")
  
  # Look at date durations in the full dataset
  dates <- dataset %>%
    dplyr::group_by(.data[[idCol]]) %>%
    dplyr::summarize(nDays = length(unique(.data[[dateCol]])))
  
  
  # Look at date durations in the masked Israel dataset
  datesInMask <- maskedDataset %>%
    as.data.frame() %>%
    dplyr::group_by(.data[[idCol]]) %>%
    dplyr::summarize(nDaysInMask = length(unique(.data[[dateCol]])))
  
  # Compare the two dates and calculate proportion
  datesCompare <- dplyr::left_join(dates, datesInMask %>%
                                     dplyr::select(tidyselect::all_of(idCol), nDaysInMask)) %>%
    dplyr::mutate(propDaysInMask = .data$nDaysInMask/.data$nDays) # compute proportion of days spent in the mask a
  
  if(thresh > 1){
    print("thresholding by number of days")
    whichInMaskLongEnough <- datesCompare %>%
      dplyr::filter(.data$nDaysInMask > thresh) %>%
      dplyr::pull(.data[[idCol]]) %>%
      unique()
  }else{
    print("thresholding by proportion of duration")
    whichInMaskLongEnough <- datesCompare %>%
      dplyr::filter(.data$propDaysInMask > thresh) %>%
      dplyr::pull(.data[[idCol]]) %>%
      unique()
  }
  
  return(whichInMaskLongEnough)
}

#' In Mask Filter
#'
#' Filters out individuals that have a percentage of points outside of a specified mask area.
#' The function first identifies individuals who spend a minimum proportion of their time within the mask,
#' and optionally removes points falling outside the mask.
#'
#' @param dataset A dataset with columns: longCol, latCol, dateCol, idCol.
#' @param mask An `sf` object representing the area to use for masking.
#' @param inMaskThreshold Numeric; proportion of time an individual must spend within the mask. Default is 0.33.
#'        If a value > 1 is supplied, it will be interpreted as a minimum number of days in the mask.
#' @param crs Coordinate Reference System to apply to both dataset and mask. Default is "WGS84".
#' @param longCol Column name for longitude values. Default is "location_long.1".
#' @param latCol Column name for latitude values. Default is "location_lat.1".
#' @param dateCol Column name for date values. Default is "dateOnly".
#' @param idCol Column name for individual IDs. Default is "Nili_id".
#' @param reMask Logical; whether to re-apply the mask after filtering individuals. Default is TRUE.
#' @param quiet Logical; if TRUE, suppresses messages from spatial operations. Default is TRUE.
#' @return A data frame of the masked dataset with attributes `firstMask` and `secondMask` indicating statistics before and after re-masking.
#' @export
#'

inMaskFilter <- function(dataset,
                         mask,
                         inMaskThreshold = 0.33,
                         crs = "WGS84",
                         longCol = "location_long.1",
                         latCol = "location_lat.1",
                         dateCol = "dateOnly",
                         idCol = "Nili_id",
                         reMask = TRUE,
                         quiet = TRUE) {
  # Argument validation
  checkmate::assertClass(mask, "sf", null.ok = TRUE)
  checkmate::assertNumeric(inMaskThreshold, len = 1, null.ok = TRUE)
  checkmate::assertCharacter(dateCol, len = 1)
  
  # Step 1: Initial Mask Filtering
  if (!is.null(inMaskThreshold)) {
    inMask <- if (quiet) {
      suppressMessages(maskData(dataset = dataset, mask = mask, longCol = longCol, latCol = latCol, crs = crs))
    } else {
      maskData(dataset = dataset, mask = mask, longCol = longCol, latCol = latCol, crs = crs)
    }
    
    # Identify individuals meeting the in-mask threshold
    longEnoughIndivs <- if (quiet) {
      suppressMessages(mostlyInMask(dataset = dataset, maskedDataset = inMask, thresh = inMaskThreshold, dateCol = dateCol, idCol = idCol))
    } else {
      mostlyInMask(dataset = dataset, maskedDataset = inMask, thresh = inMaskThreshold, dateCol = dateCol, idCol = idCol)
    }
    
    # Filter dataset to include only qualifying individuals
    dataset <- dataset %>% dplyr::filter(.data[[idCol]] %in% longEnoughIndivs)
    firstMask <- getStats(dataset, idCol)
  } else {
    firstMask <- c("rows" = NA, "cols" = NA, "indivs" = NA)
  }
  
  # Step 2: Re-Mask to Remove Out-of-Mask Points (if specified)
  if (reMask) {
    cleanedInMask <- if (quiet) {
      suppressMessages(maskData(dataset = dataset, mask = mask, longCol = longCol, latCol = latCol, crs = crs))
    } else {
      maskData(dataset = dataset, mask = mask, longCol = longCol, latCol = latCol, crs = crs)
    }
    secondMask <- getStats(cleanedInMask, idCol)
    out <- cleanedInMask
  } else {
    secondMask <- c("rows" = NA, "cols" = NA, "indivs" = NA)
    out <- dataset
  }
  
  # Step 3: Convert Output to Data Frame and Attach Mask Statistics
  out_df <- as.data.frame(out)
  attr(out_df, "firstMask") <- firstMask
  attr(out_df, "secondMask") <- secondMask
  
  return(out_df)
}

#****************************************************
# Mask filter helpers functions - end
#****************************************************

# Step Load Supporting Data
# Load supporting KML files. CutOffRegion.kml outlines Israel and is used to
# exclude vulture points outside of Israel (e.g., long-range forays).
# AllRoostPolygons.kml contains polygons representing roost sites.

# Load the Israel war mask
mask <- sf::st_read("~/Documents/GitHub/Ergm_manuscript/data/raw_data/GPS_jamming_5_october_2024.kml")
mask<-sf::st_zm(mask)
#GPS_jamming_5 (1) this is the newest

mapview::mapview(mask) # st_zm -> fixed the problem to plot

#Visualize the Mask just to check
# Plot the mask using ggplot2
plot_mask<-ggplot() +
  geom_sf(data = mask, fill = "lightblue", color = "darkblue") +  # Customize fill and color
  ggtitle("Region Mask") +
  theme_minimal()

# Save the plot
ggsave(
  filename = "~/Documents/GitHub/Ergm_manuscript/figures/Vulture_GPS_plot_mask.png", # Set desired filename and format (e.g., .png, .jpg, .pdf)
  plot = plot_mask,
  width = 10,          # Set width (in inches)
  height = 8,          # Set height (in inches)
  dpi = 300            # Set resolution (recommended for high quality)
)

#remove the war data points
#' GPS jamming filter------------------------------------------------------------
#'
#' This function takes in a dataset and removes points that lie within mask,
#' given that mask is an sf object containing
#' polygons where GPS points are shifted to after jamming.
#' @param dataset A dataset with columns: longCol, latCol, idCol
#' @param mask The mask to remove GPS jammed points. Must be an sf object with appropriate crs.
#' @param longCol The name of the column in the dataset containing longitude values. Defaults to "location_long.1". Passed to `vultureUtils::maskData()`.
#' @param latCol The name of the column in the dataset containing latitude values. Defaults to "location_lat.1". Passed to `vultureUtils::maskData()`.
#' @param idCol The name of the column in the dataset containing vulture ID's. Defaults to "Nili_id" (assuming you have joined the Nili_ids from the who's who table).
#' @return A dataset with GPS jammed points removed
#' @export
gpsJamFilter <- function(dataset,
                         mask,
                         longCol = "location_long.1",
                         latCol = "location_lat.1",
                         idCol = "Nili_id"){
  
  before <- getStats(dataset, idCol)
  dataset <- maskData(dataset = dataset,
                      mask = mask,
                      longCol = longCol,
                      latCol = latCol,
                      crsToSet = "WGS84",
                      op = "difference")
  after <- getStats(dataset, idCol)
  lost <- before - after
  # print(lost)
  dataset
}

movebank_raw_data_3_gpsJamFilter<-gpsJamFilter(movebank_raw_data_cleaned_2,
                                               mask,
                                               longCol = "location_long.1",
                                               latCol = "location_lat.1",
                                               idCol = "Nili_id")

#so if any point was removed this df should have the same size. In the war period we should have data removed.
dim(movebank_raw_data_cleaned_2)
dim(movebank_raw_data_3_gpsJamFilter)

#save
file_path <- "~/Documents/GitHub/Ergm_manuscript/data/raw_data/movebank_raw_data_3_gpsJamFilter.Rda"

# Save the data
#save(movebank_raw_data_3_gpsJamFilter, file = file_path)

# Load the .Rda file
load(file_path)

# Load the Israel boundary mask
mask_Israel_boundary <- sf::st_read("~/Documents/GitHub/Ergm_manuscript/data/raw_data/CutOffRegion.kml")

#Apply In-Mask Filter to Data

# Filter the dataset to include only points within the mask, based on the inMaskThreshold.
# Points falling outside the mask are optionally re-masked based on the threshold.
movebank_raw_data_cleaned_4 <- inMaskFilter(dataset = movebank_raw_data_3_gpsJamFilter,
                                            mask = mask_Israel_boundary,
                                            inMaskThreshold = 0.33,
                                            crs = "WGS84",
                                            longCol = "location_long.1",
                                            latCol = "location_lat.1",
                                            dateCol = "dateOnly",
                                            idCol = "Nili_id",
                                            reMask = TRUE,
                                            quiet = TRUE)

# Display the dimensions of the filtered dataset
dim(movebank_raw_data_3_gpsJamFilter)
dim(movebank_raw_data_cleaned_4)

# Define the file path for saving the downloaded data
file_path <- "~/Documents/GitHub/Ergm_manuscript/data/raw_data/movebank_raw_data_cleaned_4.Rda"

# Save the data
#save(movebank_raw_data_cleaned_4, file = file_path)

# Load the .Rda file
load(file_path)


#check the data
# Plot the points with ggplot2
vulture_plot<-ggplot(data = movebank_raw_data_3_gpsJamFilter) +
  geom_sf(aes(geometry = geometry),
          color = "red", size = 0.5, alpha = 0.7) +  # Plot points
  ggtitle("Vulture GPS Points") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12)
  )

# Save the plot
ggsave(
  filename = "~/Documents/GitHub/Ergm_manuscript/figures/Vulture_GPS_Points_raw_data.png", # Set desired filename and format (e.g., .png, .jpg, .pdf)
  plot = vulture_plot,
  width = 10,          # Set width (in inches)
  height = 8,          # Set height (in inches)
  dpi = 300            # Set resolution (recommended for high quality)
)

# Plot the points with ggplot2
vulture_plot_1<-ggplot(data = movebank_raw_data_cleaned_4) +
  geom_sf(aes(geometry = geometry),
          color = "red", size = 0.5, alpha = 0.7) +  # Plot points
  ggtitle("Vulture GPS Points") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12)
  )

# Save the plot
ggsave(
  filename = "~/Documents/GitHub/Ergm_manuscript/figures/Vulture_GPS_Points_cleaned_data.png", # Set desired filename and format (e.g., .png, .jpg, .pdf)
  plot = vulture_plot_1,
  width = 10,          # Set width (in inches)
  height = 8,          # Set height (in inches)
  dpi = 300            # Set resolution (recommended for high quality)
)

#' Remove Invalid Periods---------------------------------------------------------------
#'
#' This function takes in a dataset of vultures and removes data points which contain known invalid periods (hospital etc.) for individuals.
#' Note that it is important that the periodsToRemove is provided after being read in as a data.frame as it is necessary for this function to work.
#' @param dataset A dataset
#' @param periodsToRemove A data frame of vulture names and information about each vulture. IMPORTANT: read in with readxl::read_excel("/pathtowhoswho", sheet = "periods_to_remove")
#' @return A dataset with invalid periods removed
#' @export
removeInvalidPeriods <- function(dataset, periodsToRemove){
  checkmate::assertSubset("remove_start", names(periodsToRemove))
  checkmate::assertSubset("remove_end", names(periodsToRemove))
  checkmate::assertSubset("Nili_id", names(periodsToRemove))
  checkmate::assertSubset("location_long", names(dataset))
  checkmate::assertSubset("location_lat", names(dataset))
  checkmate::assertSubset("Nili_id", names(dataset))
  
  periods_to_remove <- periodsToRemove %>%
    dplyr::select(Nili_id, remove_start, remove_end) %>%
    dplyr::mutate(dplyr::across(contains("remove"), lubridate::ymd)) %>%
    dplyr::filter(!is.na(remove_end)) %>%
    # The following steps generate a sequence of days between the start and end date, so we can then join them to the original data
    dplyr::group_by(Nili_id) %>%
    dplyr::mutate(dateOnly = purrr::map2(remove_start, remove_end, seq, by = "1 day")) %>%
    tidyr::unnest(cols = c(dateOnly)) %>%
    dplyr::select(Nili_id, dateOnly) %>%
    dplyr::mutate(remove = T)
  
  removal_annotated <- dataset %>%
    dplyr::mutate(dateOnly = lubridate::date(timestamp)) %>%
    dplyr::left_join(periods_to_remove, by = c("Nili_id", "dateOnly"))
  
  removed_periods <- removal_annotated %>%
    dplyr::filter(is.na(remove)) %>%
    sf::st_as_sf(coords = c("location_long", "location_lat"), crs = "WGS84", remove = F)
  removed_periods
}



#data frame of vulture names and information about each vulture. IMPORTANT: read in with readxl::read_excel("/pathtowhoswho", sheet = "periods_to_remove")
periods_to_remove <- read_excel("~/Documents/GitHub/Ergm_manuscript/data/raw_data/whoswho_vultures_20230920_new.xlsx", sheet = "periods_to_remove")

movebank_raw_data_cleaned_5<-removeInvalidPeriods(movebank_raw_data_cleaned_4,
                                                  periodsToRemove=periods_to_remove)

dim(movebank_raw_data_cleaned_4)
dim(movebank_raw_data_cleaned_5)

# Define the file path for saving the downloaded data
file_path <- "~/Documents/GitHub/Ergm_manuscript/data/raw_data/movebank_raw_data_cleaned_5.Rda"

# Save the data
#save(movebank_raw_data_cleaned_5, file = file_path)

# Load the .Rda file
load(file_path)



# Step 8. -----------------------------------------------------------------

#' Remove Capture Cage periods
#'
#' This function takes in a dataset of vultures and removes data points which contain known times the individual was in a capture cage.
#' Note that it is important that the roosts, captureSites, and AllCarmelDates is provided after being read in as a data.frame as it is necessary for this function to work.
#' @param dataset A dataset
#' @param captureSites A data frame of capture sites
#' @param AllCarmelDates A data frame of dates containing captures in Carmel
#' @param idCol Name of the column in the data containing individual IDs. Default is Nili_id. Used for getting roosts.
#' @param start.day When combined with `start.month`, starting date of the capture period. Default is 1.
#' @param start.month Starting month of the capture period. Default is August.
#' @param end.day When combined with `end.month`, ending date of the capture period. Default is 30.
#' @param end.month Ending month of the capture period. Default is November.
#' @return A dataset with capture cage periods removed
#' @export
removeCaptures <- function(data, 
                           captureSites, 
                           AllCarmelDates, 
                           distance = 500, 
                           idCol = "Nili_id", 
                           start.day = 1, 
                           start.month = 8,
                           end.day = 30, 
                           end.month = 11){
  # Identify the period of time during which the capture sites are open (when we need to do this exclusion)
  # character string for reporting out the roosting period
  period <- paste(start.day, format(ISOdate(2004,1:12,1),"%B")[start.month], "-", end.day, format(ISOdate(2004,1:12,1),"%B")[end.month], sep = " ")
  
  roosts <- get_roosts_df(data, id = "Nili_id")
  
  # Get roosts that fall in the capture period
  sub.roosts <- roosts %>%
    dplyr::mutate(start_date = lubridate::dmy(paste(start.day, start.month,
                                                    lubridate::year(date), sep = "-")),
                  end_date = lubridate::dmy(paste(end.day, end.month,
                                                  lubridate::year(date), sep = "-"))) %>%
    dplyr::filter(date >= start_date & date <= end_date)
  
  if(nrow(sub.roosts) > 0){
    # Calculate the roost distance to each of the capture cages. If it is less than 500m, keep that line.
    crds <- matrix(c(sub.roosts$location_long, sub.roosts$location_lat),
                   nrow = nrow(sub.roosts), ncol = 2) # roost locs as simple lat/long coords
    distanceMat <- matrix(ncol = nrow(captureSites),
                          nrow = nrow(crds))
    colnames(distanceMat) <-  unique(captureSites$name)
    
    for(i in 1:nrow(crds)){
      distanceMat[i,] <- round(geosphere::distm(crds[i,],
                                                captureSites[,c(3,2)]), 2)
    }
    closestCaptureSite <- colnames(distanceMat)[apply(distanceMat, 1, which.min)] # ID of closest capture site
    closestCaptureDist <- apply(distanceMat, 1, min) # distance from closest capture site
    
    sub.roosts <- cbind(sub.roosts, closestCaptureSite, closestCaptureDist)
    sub.roosts$captured <- ifelse(sub.roosts$closestCaptureDist <= distance, T, F)
    sub.captured.dates <- sub.roosts %>%
      dplyr::filter(captured) %>%
      dplyr::select(Nili_id, date, closestCaptureSite,
                    closestCaptureDist, captured)
    
    ## For Carmel--different protocol.
    sub.captured.no.carmel <- subset(sub.captured.dates, closestCaptureSite != "Carmel")
    sub.captured.carmel <- subset(sub.captured.dates, closestCaptureSite == "Carmel")
    
    AllCarmelDates$Date <- as.Date(AllCarmelDates$Date, format = "%d/%m/%Y")
    
    AllCarmelDates.1 <- data.frame(Date = as.Date(paste(AllCarmelDates$Date-1)))
    AllCarmelDates.2 <- data.frame(Date = as.Date(paste(AllCarmelDates$Date-2)))
    AllCarmelDates.3 <- data.frame(Date = as.Date(paste(AllCarmelDates$Date-3)))
    
    AllCarmelDates.all <- rbind(AllCarmelDates, AllCarmelDates.1, AllCarmelDates.2, AllCarmelDates.3)
    
    sub.captured.carmel <- sub.captured.carmel %>%
      dplyr::mutate(known_capture = ifelse(date %in% AllCarmelDates.all$Date, 1, 0),
                    captured = ifelse(known_capture == 1 & closestCaptureDist <= 50, T, F)) %>%
      dplyr::filter(captured) %>%
      dplyr::select(-c(known_capture))
    
    sub.captured.dates <- rbind(sub.captured.no.carmel, sub.captured.carmel)
    
    # We also need to exclude the day after the bird was captured
    sub.captured.dates.1 <- sub.captured.dates
    sub.captured.dates.1$date <- sub.captured.dates.1$date+1
    
    sub.captured.dates <- rbind(sub.captured.dates, sub.captured.dates.1)
    sub.captured.dates <- sub.captured.dates %>%
      dplyr::distinct(Nili_id, date, .keep_all = T)
    
    # It all looks ok, so we can subset the dataset to exclude the capture dates
    removed_captures <- data %>%
      dplyr::left_join(sub.captured.dates, by = c("Nili_id", "dateOnly" = "date"))
    nrow(data) == nrow(removed_captures) # should be TRUE. NOW we can filter.
    out <- removed_captures %>%
      dplyr::filter(!captured |is.na(captured)) # remove the individual*days when they were captured
  }else{
    message(paste0("No roosts detected within the capture period: ", period, ". Did not remove any data."))
    out <- data
  }
  
  return(data)
}

cs <- read.csv("~/Documents/GitHub/Ergm_manuscript/data/raw_data/capture_sites.csv", sep = ",", header = T)
cml <- read.csv("~/Documents/GitHub/Ergm_manuscript/data/raw_data/carmel.csv", sep = ",", header = T)

#' Get roosts (data frame version)
#'
#' With several methods, get the roost locations of a vulture on each night.
#' This is an adaptation of Marta Acácio's get_roosts function that takes a data
#' frame as input instead of several vectors. The function consecutively calculates
#' the night roost based on the following five methods:
#' 1.  It is the last GPS location of the day, and it is at night (and during the "night hours"), and the speed is equal or less than 4m/s (i.e., the bird was considered to not be flying);
#' 2.  It is the last GPS location of the day, and it is at night (and during the "night hours"), and the speed is NA;
#' 3.  It is the first GPS location of the day, and it is at night (and during the "morning hours"), and the speed is equal or less than 4m/s;
#' 4.  It is the first GPS location of the day, and it is at night (and during the "morning hours"), and the speed is NA;
#' 5.  The last GPS location of the day (independently of the light or the hours) is within the defined buffer (pre-defined, 1km) of the first GPS location of the following day.
#' The roost day is assigned in the following way:
#' 1.  If it is the last location of the day, it is that day's night roost;
#' 2.  If it was the first location of the day, it was the previous day's night roost;
#' 3.  If for a particular date, the roost was calculated using more than 1 method, the selected roost is the earliest calculated roost.
#' @param df data frame containing the `id`, `timestamp`, `x`, `y`, and `ground_speed` columns.
#' @param id column containing animal identifiers
#' @param timestamp column of class `as.POSIXct` (timestamps for GPS fixes)
#' @param x column containing longitude, in decimal degrees
#' @param y column containing latitude, in decimal degrees
#' @param ground_speed column containing ground speed of the animal
#' @param speed_units units of speed (either "m/s" or "km/h"). If speed is provided in "km/h" it is transformed to "m/s".
#' @param buffer optional, numerical value indicating the number of kms to consider the buffer for the roost calculation. The pre-defined value is 1km
#' @param twilight optional, numerical value indicating number of minutes to consider the twilight for calculating the day and night positions. If set to 0, the night period starts at sunset and the day period starts at sunrise. The pre-defined value is 61, so the night period starts 61 minutes before sunset and the day period starts 61 minutes after sunrise
#' @param morning_hours optional, vector indicating the range of hours (in UTC) that are considered the morning period. The pre-defined vector is `c(0:12)`
#' @param night_hours optional, vector indicating the range of hours (in UTC) that are considered the night period. The pre-defined vector is `c(13:23)`
#' @param quiet If F (default), prints time warning/progress message. If T, silences this message.
#' @return a data frame of the calculated roosts for every animal.
#' @export
get_roosts_df <- function(df,
                          id = "local_identifier",
                          timestamp = "timestamp",
                          x = "location_long",
                          y = "location_lat",
                          ground_speed = "ground_speed",
                          speed_units = "m/s",
                          buffer = 1,
                          twilight = 61,
                          morning_hours = c(0:12),
                          night_hours = c(13:23),
                          quiet = F){
  # setup for time warning
  if(!quiet){
    cat("\nFinding roosts... this may take a while if your dataset is large.\n")
    start <- Sys.time()
  }
  
  # Argument checks
  checkmate::assertDataFrame(df)
  checkmate::assertCharacter(id, len = 1)
  checkmate::assertSubset(id, names(df))
  checkmate::assertCharacter(timestamp, len = 1)
  checkmate::assertSubset(timestamp, names(df))
  checkmate::assertCharacter(x, len = 1)
  checkmate::assertSubset(x, names(df))
  checkmate::assertCharacter(y, len = 1)
  checkmate::assertSubset(y, names(df))
  checkmate::assertCharacter(ground_speed, len = 1)
  checkmate::assertSubset(ground_speed, names(df))
  checkmate::assertCharacter(speed_units, len = 1)
  checkmate::assertSubset(speed_units, c("m/s", "km/h"))
  checkmate::assertNumeric(buffer, len = 1)
  checkmate::assertNumeric(twilight, len = 1)
  checkmate::assertNumeric(morning_hours, upper = 24, lower = 0)
  checkmate::assertNumeric(night_hours, upper = 24, lower = 0)
  checkmate::assertNumeric(df[[x]])
  checkmate::assertNumeric(df[[y]])
  checkmate::assertNumeric(df[[ground_speed]])
  
  # If the data is an sf object, remove the geometry to make it easier to work with. The computations in this function just depend on lat/long columns being present.
  if("sf" %in% class(df)){
    df <- df %>%
      sf::st_drop_geometry()
  }
  
  # Transform the twilight period into seconds
  twilight_secs <- twilight * 60
  
  # If the speed is in km/h transform into m/s
  if(speed_units == "km/h"){
    df <- df %>%
      dplyr::mutate({{ground_speed}} := round(.data[[ground_speed]] / 3.6, 3))
  }
  
  df[[timestamp]] <- as.POSIXct(df[[timestamp]],
                                format = "%Y-%m-%d %H:%M:%S",
                                tz = "UTC")
  
  if(sum(is.na(df[[timestamp]])) > 0){
    stop("Timestamp needs to be defined as.POSIXct (%Y-%m-%d %H:%M:%S)")
  }
  
  df$date <- as.Date(df[[timestamp]])
  
  # Separate into a list of individuals
  indivs <- df %>%
    dplyr::group_by(.data[[id]]) %>%
    dplyr::group_split(.keep = T)
  
  roosts <- purrr::map_dfr(indivs, ~{
    temp.id <- unique(.x[[id]])
    
    id.df <- .x %>%
      dplyr::group_by(date) %>%
      dplyr::arrange({{timestamp}}) %>%
      dplyr::mutate(
        row_id = dplyr::case_when(
          dplyr::row_number() == 1 ~ "first",
          dplyr::row_number() == max(dplyr::row_number()) ~ "last"),
        hour = lubridate::hour(.data[[timestamp]])) %>%
      dplyr::filter(row_id %in% c("first", "last")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(day_diff = round(difftime(dplyr::lead(date), date, units="days")))
    
    matrix <- as.matrix(id.df[,c(x, y)])
    leadMatrix <- as.matrix(cbind(dplyr::lead(id.df[[x]]),
                                  dplyr::lead(id.df[[y]])))
    distances <- geosphere::distGeo(p1 = matrix, p2 = leadMatrix)*0.001 %>%
      round(., 2)
    id.df$dist_km <- distances
    id.df$dist_km[id.df$day_diff != 1] <- NA
    
    # Ryan's Code: I think maptools::sunriset can be replaced with suncalc::getSunlightTimes since its used in other places
    # SEE: https://cran.r-project.org/web/packages/suncalc/ https://cran.r-project.org/web/packages/suncalc/suncalc.pdf
    
    data <- data.frame(date = as.Date(id.df[[timestamp]]), lat = id.df[[y]], lon = id.df[[x]])
    
    id.df$sunrise <- suncalc::getSunlightTimes(data = data, keep = c("sunrise"))$sunrise
    id.df$sunset <- suncalc::getSunlightTimes(data = data, keep = c("sunset"))$sunset
    
    # Set the twilight
    id.df$sunrise_twilight <- id.df$sunrise + twilight_secs
    id.df$sunset_twilight <- id.df$sunset - twilight_secs
    
    id.df <- id.df %>%
      dplyr::mutate(daylight = ifelse(.data[[timestamp]] >= sunrise_twilight &
                                        .data[[timestamp]] <= sunset_twilight,
                                      "day", "night"))
    
    # Identify the roosts
    id.df <- id.df %>%
      dplyr::mutate(
        is_roost = dplyr::case_when(
          row_id == "last" & daylight == "night" & hour %in% night_hours & ({{ground_speed}} <= 4 |
                                                                              is.na({{ground_speed}})) ~ 1,
          row_id == "first" & daylight == "night" & hour %in% morning_hours & ({{ground_speed}} <= 4 |
                                                                                 is.na({{ground_speed}})) ~ 1,
          dist_km <= buffer ~ 1
        ),
        roost_date = dplyr::case_when(
          is_roost == 1 & row_id == "last" ~ paste(as.character(date)),
          is_roost == 1 & row_id == "first" ~ paste(as.character(date-1))
        ),
        roost_date = as.Date(roost_date)
      )
    
    temp.id.roosts <- dplyr::filter(id.df, is_roost == 1)
    
    # If there is more than 1 roost per day, keep the earliest roost (night roost)
    temp.id.roosts <- temp.id.roosts %>%
      dplyr::group_by(roost_date) %>%
      dplyr::arrange({{timestamp}}) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c("row_id", "hour"))
    
    temp.id.roosts <- temp.id.roosts %>%
      dplyr::select({{id}}, date, roost_date, sunrise, sunset, sunrise_twilight, sunset_twilight, daylight, is_roost, location_lat, location_long)
    
    return(temp.id.roosts)
  })
  
  # complete the time message
  if(!quiet){
    end <- Sys.time()
    duration <- difftime(end, start, units = "secs")
    cat(paste0("Roost computation completed in ", duration, " seconds."))
  }
  
  roosts <- roosts %>%
    dplyr::select({{id}}, date, roost_date, sunrise, sunset, sunrise_twilight, sunset_twilight, daylight, is_roost, location_lat, location_long)
  
  # return
  return(roosts)
  
}


movebank_raw_data_cleaned_6<-removeCaptures(data = movebank_raw_data_cleaned_5, 
                                            captureSites = cs, 
                                            AllCarmelDates = cml, 
                                            distance = 500,
                                            idCol = "Nili_id")

age_sex <- read_excel("~/Documents/GitHub/Ergm_manuscript/data/raw_data/whoswho_vultures_20230920_new.xlsx", sheet = "all gps tags")[,1:35] %>%
  dplyr::select(Nili_id, birth_year, sex) %>%
  distinct()

movebank_raw_data_cleaned_7 <- movebank_raw_data_cleaned_6 %>%
  dplyr::select(-c("sex")) %>%
  left_join(age_sex, by = "Nili_id")


# Define the file path for saving the downloaded data
file_path <- "~/Documents/GitHub/Ergm_manuscript/data/raw_data/movebank_raw_data_cleaned_7.Rda"

# Save the data
#save(movebank_raw_data_cleaned_7, file = file_path)

# Load the .Rda file
load(file_path)

# Step 9. -----------------------------------------------------------------

# Restrict to southern individuals ----------------------------------------
# Based on previous investigations for the 2022 breeding and non-breeding seasons, have found that a good cutoff for southern vs. non-southern is 3550000 (in ITM)
## Transform to SF object, so we can get centroids
sf <- movebank_raw_data_cleaned_7 %>%
  sf::st_as_sf(coords = c("location_long.1","location_lat.1"), remove = F) %>%
  sf::st_set_crs("WGS84") %>%
  sf::st_transform(32636)

## Get centroids, so we can see who's "southern" for that season.
centroids <- sf %>% group_by(Nili_id) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_centroid()

## Examine a histogram of centroid latitudes
hist(st_coordinates(centroids[,2])) # looks like 3550000 is generally a good cutoff point here

## Get southern individuals for each season, so we can filter the data
southernIndividuals <- centroids %>%
  filter(st_coordinates(.)[,2] <= 3550000) %>% #06/13/2023 -I changed here to 350000 to select exclusively the individuals from negev# you will probably have to add a second filter here to remove the ones that are too far to the south, as we talked about.
  pull(Nili_id)

## Remove individuals from the dataset that are not in the south. Note that even if a "southern individual" has some points that are not in the south, the individual will still be kept and the points will also be kept.
movebank_raw_data_cleaned_8<- sf %>% filter(Nili_id %in% southernIndividuals)

# Define the file path for saving the downloaded data
file_path <- "~/Documents/GitHub/Ergm_manuscript/data/raw_data/movebank_raw_data_cleaned_8.Rda"

# Save the data
save(movebank_raw_data_cleaned_8, file = file_path)

load(file_path)


# Plot the points with ggplot2
vulture_plot_2<-ggplot(data = movebank_raw_data_cleaned_8) +
  geom_sf(aes(geometry = geometry),
          color = "red", size = 0.5, alpha = 0.7) +  # Plot points
  ggtitle("Vulture GPS Points") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12)
  )

# Save the plot
ggsave(filename = "~/Documents/GitHub/Ergm_manuscript/figures/Vulture_GPS_Points_1.png", # Set desired filename and format (e.g., .png, .jpg, .pdf)
  plot = vulture_plot_2,
  width = 10,          # Set width (in inches)
  height = 8,          # Set height (in inches)
  dpi = 300)            # Set resolution (recommended for high quality))

dim(movebank_raw_data_cleaned_2)
dim(movebank_raw_data_cleaned_3)
dim(movebank_raw_data_cleaned_4)
dim(movebank_raw_data_cleaned_5)
dim(movebank_raw_data_cleaned_6)
dim(movebank_raw_data_cleaned_7)
dim(movebank_raw_data_cleaned_8)

# Define the file path for load the data
file_path <- "~/Documents/GitHub/Ergm_manuscript/data/raw_data/movebank_raw_data_cleaned_8.Rda"

load(file_path)

# Separate the seasons -----------------------------

# Convert exact_date_of_birth to Date format, handling empty strings
movebank_raw_data_cleaned_9 <- movebank_raw_data_cleaned_8 %>%
  mutate(exact_date_of_birth = ymd_hms(exact_date_of_birth, quiet = TRUE),
         year_of_birth = year(exact_date_of_birth))

# Fix time zone so dates make sense ---------------------------------------
## Overwrite the dateOnly column from the new times #data_masked
movebank_raw_data_cleaned_10<- movebank_raw_data_cleaned_9 %>%
  mutate(timestampIsrael = lubridate::with_tz(timestamp, tzone = "Israel"),
         dateOnly = lubridate::date(timestampIsrael),
         month = lubridate::month(dateOnly),
         year = lubridate::year(dateOnly))

# Define the file path for saving the downloaded data
file_path <- "~/Documents/GitHub/Ergm_manuscript/data/raw_data/movebank_raw_data_cleaned_10.Rda"

# Save the data
#save(movebank_raw_data_cleaned_10, file = file_path)

load(file_path)



# Step 7. -----------------------------------------------------------------

# Split into 3 seasons 
split_seasons <- movebank_raw_data_cleaned_10 %>% mutate(month = lubridate::month(timestampIsrael),
                                                         day = lubridate::day(timestampIsrael),
                                                         year = lubridate::year(timestampIsrael)) %>%
    mutate(season = case_when(((month == 12 & day >= 15) | 
                                 (month %in% 1:4) | 
                                 (month == 5 & day < 15)) ~ "breeding",
                              ((month == 5 & day >= 15) | 
                                 (month %in% 6:8) | 
                                 (month == 9 & day < 15)) ~ "post_breeding",#summer
                              .default = "pre_breeding")) %>%#fall
    mutate(seasonUnique = case_when(season == "breeding" & month == 12 ~
                                      paste(as.character(year + 1), season, sep = "_"),
                                    .default = paste(as.character(year), season, sep = "_"))) %>%
    mutate(seasonUnique = factor(seasonUnique, levels = c("2020_post_breeding", 
                                                          "2020_pre_breeding", 
                                                          "2021_breeding", 
                                                          "2021_post_breeding", 
                                                          "2021_pre_breeding", 
                                                          "2022_breeding", 
                                                          "2022_post_breeding",
                                                          "2022_pre_breeding",
                                                          "2023_breeding", 
                                                          "2023_post_breeding",
                                                          "2023_pre_breeding", 
                                                          "2024_breeding", 
                                                          "2024_post_breeding")),
           
           season = factor(season, levels = c("breeding", "post_breeding", "pre_breeding")))
  
  # Add ages (based on season, which is why this goes here rather than above)
split_seasons <- split_seasons %>%
    mutate(age = year - year_of_birth) %>%
    mutate(age = ifelse(season == "breeding" & month %in% c(1, 12), age + 1, age)) %>%
    group_by(Nili_id, year, month) %>%
    mutate(age = ifelse(month == 2, min(age) + 1, age)) %>%
    mutate(age_group = case_when(age <= 1 ~ "Immature",
                                 age > 1 & age < 5 ~ "Subadult",
                                 age >= 5 ~ "Adult",
                                 .default = NA)) %>%
    ungroup()
  
  
# Separate the seasons -----------------------------
movebank_raw_data_cleaned_11 <- split_seasons %>%
    group_by(seasonUnique) %>%
    group_split(.keep = T)


# Define the file path for saving the downloaded data
file_path <- "~/Documents/GitHub/Ergm_manuscript/data/raw_data/movebank_raw_data_cleaned_11.Rda"

# Save the data
save(movebank_raw_data_cleaned_11, file = file_path)

# Load the .Rda file
load(file_path)

#remove the individuals with few days
remove_lfr <- function(data){
  Mode <- function(x) { 
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # Calculate daily modes and add them to the dataset (using the function defined above)
  with_modes <- map(data, ~.x %>%
                      group_by(dateOnly, Nili_id) %>%
                      mutate(diff = as.numeric(difftime(lead(timestamp), timestamp, units = "mins"))) %>%
                      group_by(dateOnly, Nili_id) %>%
                      mutate(mode = Mode(round(diff))) %>%
                      ungroup())
  
  # Identify individuals that never have a daily mode of 10 minutes
  low_fix_rate_indivs <- map(with_modes, ~.x %>%
                               sf::st_drop_geometry() %>%
                               group_by(Nili_id) %>%
                               summarize(minmode = min(mode, na.rm = T)) %>%
                               filter(minmode > 10) %>%
                               pull(Nili_id) %>%
                               unique())
  
  # Don't need to save with_modes because we've already used this step to identify low fix rate individuals.
  removed_lfr <- map2(data, low_fix_rate_indivs,                     
                      ~.x %>% filter(!(Nili_id %in% .y)))
  return(removed_lfr)
}

movebank_raw_data_cleaned_12<-remove_lfr(movebank_raw_data_cleaned_11)

# Define the file path for saving the downloaded data
file_path <- "~/Documents/GitHub/Ergm_manuscript/data/raw_data/movebank_raw_data_cleaned_12.Rda"

# Save the data
#save(movebank_raw_data_cleaned_12, file = file_path)

# Load the .Rda file
load(file_path)

downsample_10min <- function(data){
  # Define function for downsampling 
  down <- function(df, mins){
    df <- dplyr::arrange(df, timestamp)
    howmuch <- paste(mins, "minute", sep = " ")
    rounded <- lubridate::round_date(df$timestamp, howmuch)
    keep <- !duplicated(rounded)
    return(df[keep,])
  }
  
  # Split by individual so we can downsample more efficiently
  lst <- map(data, ~.x %>% group_split(Nili_id))
  
  # Perform downsampling
  downsampled_10min <- map(lst, ~{
    purrr::list_rbind(map(.x, ~down(df = .x, mins = 10), .progress = T))
  }, .progress = T)
  
  # Add information on # of days tracked for each individual
  downsampled_10min <- map(downsampled_10min, ~.x %>% group_by(Nili_id) %>% 
                             mutate(daysTracked = length(unique(dateOnly))))
  return(downsampled_10min)
}


movebank_raw_data_cleaned_13_TenMin <- downsample_10min(movebank_raw_data_cleaned_12)

dim(movebank_raw_data_cleaned_13_TenMin[[1]])
dim(movebank_raw_data_cleaned_13_TenMin[[2]])
dim(movebank_raw_data_cleaned_13_TenMin[[3]])
dim(movebank_raw_data_cleaned_13_TenMin[[4]])
dim(movebank_raw_data_cleaned_13_TenMin[[5]])
dim(movebank_raw_data_cleaned_13_TenMin[[6]])
dim(movebank_raw_data_cleaned_13_TenMin[[7]])
dim(movebank_raw_data_cleaned_13_TenMin[[8]])
dim(movebank_raw_data_cleaned_13_TenMin[[9]])
dim(movebank_raw_data_cleaned_13_TenMin[[10]])
dim(movebank_raw_data_cleaned_13_TenMin[[11]])
dim(movebank_raw_data_cleaned_13_TenMin[[12]])

# Define the file path for saving the downloaded data
file_path <- "~/Documents/GitHub/Ergm_manuscript/data/raw_data/movebank_raw_data_cleaned_13_TenMin.Rda"

# Save the data
#save(movebank_raw_data_cleaned_13_TenMin, file = file_path)

# Load the .Rda file
load(file_path)


remove_too_few_days <- function(removed_lowppd){
  indivsToKeep <- map(removed_lowppd, ~.x %>%
                        st_drop_geometry() %>%
                        group_by(Nili_id) %>%
                        summarize(nDaysTracked =
                                    length(unique(dateOnly))) %>%
                        filter(nDaysTracked >= 30) %>%
                        pull(Nili_id), .progress = T)
  
  ## remove those individuals not tracked for enough days
  removed_too_few_days <- map2(.x = removed_lowppd,
                               .y = indivsToKeep, ~.x %>%
                                 filter(Nili_id %in% .y))
  return(removed_too_few_days)
}

movebank_raw_data_cleaned_final_14<-remove_too_few_days(movebank_raw_data_cleaned_13_TenMin)

dim(movebank_raw_data_cleaned_final_14[[1]])
dim(movebank_raw_data_cleaned_final_14[[2]])
dim(movebank_raw_data_cleaned_final_14[[3]])
dim(movebank_raw_data_cleaned_final_14[[4]])
dim(movebank_raw_data_cleaned_final_14[[5]])
dim(movebank_raw_data_cleaned_final_14[[6]])
dim(movebank_raw_data_cleaned_final_14[[7]])
dim(movebank_raw_data_cleaned_final_14[[8]])
dim(movebank_raw_data_cleaned_final_14[[9]])
dim(movebank_raw_data_cleaned_final_14[[10]])
dim(movebank_raw_data_cleaned_final_14[[11]])
dim(movebank_raw_data_cleaned_final_14[[12]])

# Define the file path for saving the downloaded data
file_path <- "~/Documents/GitHub/Ergm_manuscript/data/raw_data/movebank_raw_data_cleaned_final_14.Rda"

# Save the data
#save(movebank_raw_data_cleaned_final_14, file = file_path)

# Load the .Rda file
load(file_path)

movebank_cleaned_data_2020_2024<-movebank_raw_data_cleaned_final_14

pre_breeding_2020<-movebank_cleaned_data_2020_2024[[1]]
breeding_2021<-movebank_cleaned_data_2020_2024[[2]]
post_breeding_2021<-movebank_cleaned_data_2020_2024[[3]]

pre_breeding_2021<-movebank_cleaned_data_2020_2024[[4]]
breeding_2022<-movebank_cleaned_data_2020_2024[[5]]
post_breeding_2022<-movebank_cleaned_data_2020_2024[[6]]

pre_breeding_2022<-movebank_cleaned_data_2020_2024[[7]]
breeding_2023<-movebank_cleaned_data_2020_2024[[8]]
post_breeding_2023<-movebank_cleaned_data_2020_2024[[9]]

pre_breeding_2023<-movebank_cleaned_data_2020_2024[[10]]
breeding_2024<-movebank_cleaned_data_2020_2024[[11]]
post_breeding_2024<-movebank_cleaned_data_2020_2024[[12]]

#Save each dataset to an individual .Rda file
save(pre_breeding_2020, file = "~/Documents/GitHub/Ergm_manuscript/data/raw_data/pre_breeding_2020.Rda")
save(breeding_2021, file = "~/Documents/GitHub/Ergm_manuscript/data/raw_data/breeding_2021.Rda")
save(post_breeding_2021, file = "~/Documents/GitHub/Ergm_manuscript/data/raw_data/post_breeding_2021.Rda")

save(pre_breeding_2021, file = "~/Documents/GitHub/Ergm_manuscript/data/raw_data/pre_breeding_2021.Rda")
save(breeding_2022, file = "~/Documents/GitHub/Ergm_manuscript/data/raw_data/breeding_2022.Rda")
save(post_breeding_2022, file = "~/Documents/GitHub/Ergm_manuscript/data/raw_data/post_breeding_2022.Rda")

save(pre_breeding_2022, file = "~/Documents/GitHub/Ergm_manuscript/data/raw_data/pre_breeding_2022.Rda")
save(breeding_2023, file = "~/Documents/GitHub/Ergm_manuscript/data/raw_data/breeding_2023.Rda")
save(post_breeding_2023, file = "~/Documents/GitHub/Ergm_manuscript/data/raw_data/post_breeding_2023.Rda")

save(pre_breeding_2023, file = "~/Documents/GitHub/Ergm_manuscript/data/raw_data/pre_breeding_2023.Rda")
save(breeding_2024, file = "~/Documents/GitHub/Ergm_manuscript/data/raw_data/breeding_2024.Rda")
save(post_breeding_2024, file = "~/Documents/GitHub/Ergm_manuscript/data/raw_data/post_breeding_2024.Rda")





#Now we we need to do the same cuts for the roost data.

load("~/Documents/GitHub/Ergm_manuscript/data/raw_data/movebank_raw_data_cleaned_10.Rda")

#' Get roosts (data frame version)
#'
#' With several methods, get the roost locations of a vulture on each night. This is an adaptation of Marta Acácio's get_roosts function that takes a data frame as input instead of several vectors. The function consecutively calculates the night roost based on the following five methods:
#' 1.  It is the last GPS location of the day, and it is at night (and during the "night hours"), and the speed is equal or less than 4m/s (i.e., the bird was considered to not be flying);
#' 2.  It is the last GPS location of the day, and it is at night (and during the "night hours"), and the speed is NA;
#' 3.  It is the first GPS location of the day, and it is at night (and during the "morning hours"), and the speed is equal or less than 4m/s;
#' 4.  It is the first GPS location of the day, and it is at night (and during the "morning hours"), and the speed is NA;
#' 5.  The last GPS location of the day (independently of the light or the hours) is within the defined buffer (pre-defined, 1km) of the first GPS location of the following day.
#' The roost day is assigned in the following way:
#' 1.  If it is the last location of the day, it is that day's night roost;
#' 2.  If it was the first location of the day, it was the previous day's night roost;
#' 3.  If for a particular date, the roost was calculated using more than 1 method, the selected roost is the earliest calculated roost.
#' @param df data frame containing the `id`, `timestamp`, `x`, `y`, and `ground_speed` columns.
#' @param id column containing animal identifiers
#' @param timestamp column of class `as.POSIXct` (timestamps for GPS fixes)
#' @param x column containing longitude, in decimal degrees
#' @param y column containing latitude, in decimal degrees
#' @param ground_speed column containing ground speed of the animal
#' @param speed_units units of speed (either "m/s" or "km/h"). If speed is provided in "km/h" it is transformed to "m/s".
#' @param buffer optional, numerical value indicating the number of kms to consider the buffer for the roost calculation. The pre-defined value is 1km
#' @param twilight optional, numerical value indicating number of minutes to consider the twilight for calculating the day and night positions. If set to 0, the night period starts at sunset and the day period starts at sunrise. The pre-defined value is 61, so the night period starts 61 minutes before sunset and the day period starts 61 minutes after sunrise
#' @param morning_hours optional, vector indicating the range of hours (in UTC) that are considered the morning period. The pre-defined vector is `c(0:12)`
#' @param night_hours optional, vector indicating the range of hours (in UTC) that are considered the night period. The pre-defined vector is `c(13:23)`
#' @param quiet If F (default), prints time warning/progress message. If T, silences this message.
#' @return a data frame of the calculated roosts for every animal.
#' @export
get_roosts_df <- function(df, id = "local_identifier", timestamp = "timestamp", x = "location_long", y = "location_lat", ground_speed = "ground_speed", speed_units = "m/s", buffer = 1, twilight = 61, morning_hours = c(0:12), night_hours = c(13:23), quiet = F){
  # setup for time warning
  if(!quiet){
    cat("\nFinding roosts... this may take a while if your dataset is large.\n")
    start <- Sys.time()
  }
  
  # Argument checks
  checkmate::assertDataFrame(df)
  checkmate::assertCharacter(id, len = 1)
  checkmate::assertSubset(id, names(df))
  checkmate::assertCharacter(timestamp, len = 1)
  checkmate::assertSubset(timestamp, names(df))
  checkmate::assertCharacter(x, len = 1)
  checkmate::assertSubset(x, names(df))
  checkmate::assertCharacter(y, len = 1)
  checkmate::assertSubset(y, names(df))
  checkmate::assertCharacter(ground_speed, len = 1)
  checkmate::assertSubset(ground_speed, names(df))
  checkmate::assertCharacter(speed_units, len = 1)
  checkmate::assertSubset(speed_units, c("m/s", "km/h"))
  checkmate::assertNumeric(buffer, len = 1)
  checkmate::assertNumeric(twilight, len = 1)
  checkmate::assertNumeric(morning_hours, upper = 24, lower = 0)
  checkmate::assertNumeric(night_hours, upper = 24, lower = 0)
  checkmate::assertNumeric(df[[x]])
  checkmate::assertNumeric(df[[y]])
  checkmate::assertNumeric(df[[ground_speed]])
  
  # If the data is an sf object, remove the geometry to make it easier to work with. The computations in this function just depend on lat/long columns being present.
  if("sf" %in% class(df)){
    df <- df %>%
      sf::st_drop_geometry()
  }
  
  # Transform the twilight period into seconds
  twilight_secs <- twilight * 60
  
  # If the speed is in km/h transform into m/s
  if(speed_units == "km/h"){
    df <- df %>%
      dplyr::mutate({{ground_speed}} := round(.data[[ground_speed]] / 3.6, 3))
  }
  
  df[[timestamp]] <- as.POSIXct(df[[timestamp]],
                                format = "%Y-%m-%d %H:%M:%S",
                                tz = "UTC")
  
  if(sum(is.na(df[[timestamp]])) > 0){
    stop("Timestamp needs to be defined as.POSIXct (%Y-%m-%d %H:%M:%S)")
  }
  
  df$date <- as.Date(df[[timestamp]])
  
  # Separate into a list of individuals
  indivs <- df %>%
    dplyr::group_by(.data[[id]]) %>%
    dplyr::group_split(.keep = T)
  
  roosts <- purrr::map_dfr(indivs, ~{
    temp.id <- unique(.x[[id]])
    
    id.df <- .x %>%
      dplyr::group_by(date) %>%
      dplyr::arrange({{timestamp}}) %>%
      dplyr::mutate(
        row_id = dplyr::case_when(
          dplyr::row_number() == 1 ~ "first",
          dplyr::row_number() == max(dplyr::row_number()) ~ "last"),
        hour = lubridate::hour(.data[[timestamp]])) %>%
      dplyr::filter(row_id %in% c("first", "last")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(day_diff = round(difftime(dplyr::lead(date), date, units="days")))
    
    matrix <- as.matrix(id.df[,c(x, y)])
    leadMatrix <- as.matrix(cbind(dplyr::lead(id.df[[x]]),
                                  dplyr::lead(id.df[[y]])))
    distances <- geosphere::distGeo(p1 = matrix, p2 = leadMatrix)*0.001 %>%
      round(., 2)
    id.df$dist_km <- distances
    id.df$dist_km[id.df$day_diff != 1] <- NA
    
    # Ryan's Code: I think maptools::sunriset can be replaced with suncalc::getSunlightTimes since its used in other places
    # SEE: https://cran.r-project.org/web/packages/suncalc/ https://cran.r-project.org/web/packages/suncalc/suncalc.pdf
    
    data <- data.frame(date = as.Date(id.df[[timestamp]]), lat = id.df[[y]], lon = id.df[[x]])
    
    id.df$sunrise <- suncalc::getSunlightTimes(data = data, keep = c("sunrise"))$sunrise
    id.df$sunset <- suncalc::getSunlightTimes(data = data, keep = c("sunset"))$sunset
    
    # Set the twilight
    id.df$sunrise_twilight <- id.df$sunrise + twilight_secs
    id.df$sunset_twilight <- id.df$sunset - twilight_secs
    
    id.df <- id.df %>%
      dplyr::mutate(daylight = ifelse(.data[[timestamp]] >= sunrise_twilight &
                                        .data[[timestamp]] <= sunset_twilight,
                                      "day", "night"))
    
    # Identify the roosts
    id.df <- id.df %>%
      dplyr::mutate(
        is_roost = dplyr::case_when(
          row_id == "last" & daylight == "night" & hour %in% night_hours & ({{ground_speed}} <= 4 |
                                                                              is.na({{ground_speed}})) ~ 1,
          row_id == "first" & daylight == "night" & hour %in% morning_hours & ({{ground_speed}} <= 4 |
                                                                                 is.na({{ground_speed}})) ~ 1,
          dist_km <= buffer ~ 1
        ),
        roost_date = dplyr::case_when(
          is_roost == 1 & row_id == "last" ~ paste(as.character(date)),
          is_roost == 1 & row_id == "first" ~ paste(as.character(date-1))
        ),
        roost_date = as.Date(roost_date)
      )
    
    temp.id.roosts <- dplyr::filter(id.df, is_roost == 1)
    
    # If there is more than 1 roost per day, keep the earliest roost (night roost)
    temp.id.roosts <- temp.id.roosts %>%
      dplyr::group_by(roost_date) %>%
      dplyr::arrange({{timestamp}}) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c("row_id", "hour"))
    
    temp.id.roosts <- temp.id.roosts %>%
      dplyr::select({{id}}, date, roost_date, sunrise, sunset, sunrise_twilight, sunset_twilight, daylight, is_roost, location_lat, location_long)
    
    return(temp.id.roosts)
  })
  
  # complete the time message
  if(!quiet){
    end <- Sys.time()
    duration <- difftime(end, start, units = "secs")
    cat(paste0("Roost computation completed in ", duration, " seconds."))
  }
  
  roosts <- roosts %>%
    dplyr::select({{id}}, date, roost_date, sunrise, sunset, sunrise_twilight, sunset_twilight, daylight, is_roost, location_lat, location_long)
  
  # return
  return(roosts)
  
}

get_roosts <- function(dat){
roosts <- map(dat, get_roosts_df(df = .x, id = "Nili_id"), 
            .progress = T)
            roosts <- roosts %>%
          map(., ~st_as_sf(.x, crs = "WGS84", 
            coords = c("location_long", "location_lat"), 
            remove = F), .progress = T)
            return(roosts)
}

get_roosts_split_seasons_11 <- get_roosts(pre_breeding_2020)

save(get_roosts_movebank_cleaned_10, file = "get_roosts_movebank_cleaned_10.Rda")

load("~/Documents/GitHub/Ergm_manuscript/data/raw_data/get_roosts_movebank_cleaned_10.Rda")

# List of filenames corresponding to each dataset in data_cleaned_final
roost_season_names <- c("roost_fall_2020", "roost_breeding_2021", "roost_summer_2021",
                        "roost_fall_2021", "roost_breeding_2022", "roost_summer_2022", 
                        "roost_fall_2022", "roost_breeding_2023", "roost_summer_2023", 
                        "roost_fall_2023", "roost_breeding_2024", "roost_summer_2024")

# Loop through each index in data_cleaned_final and save the data with the respective season name
for (i in 1:12) {
  # Dynamically assign each dataset to its corresponding name
  assign(roost_season_names[i], get_roosts_vu[[i]])  # Assign the dataset with the correct name
  
  # Optionally print unique season info
  print(unique(get_roosts_vu[[i]]$seasonUnique))
  
  # Save the dataset with the respective season name
  save(list = roost_season_names[i], file = paste0(roost_season_names[i], ".Rda"))
  
  # Print a confirmation message
  print(paste("Saved roost season names -", roost_season_names[i]))
}











# Save each processed dataset to its own .Rda file
save(pre_breeding_2020_data, file = "pre_breeding_2020_data.Rda")
save(breeding_2021_data, file = "breeding_2021_data.Rda")
save(post_breeding_2021_data, file = "post_breeding_2021_data.Rda")

save(pre_breeding_2021_data, file = "pre_breeding_2021_data.Rda")
save(breeding_2022_data, file = "breeding_2022_data.Rda")
save(post_breeding_2022_data, file = "post_breeding_2022_data.Rda")

save(pre_breeding_2022_data, file = "pre_breeding_2022_data.Rda")
save(breeding_2023_data, file = "breeding_2023_data.Rda")
save(post_breeding_2023_data, file = "post_breeding_2023_data.Rda")

save(pre_breeding_2023_data, file = "pre_breeding_2023_data.Rda")
save(breeding_2024_data, file = "breeding_2024_data.Rda")
save(post_breeding_2024_data, file = "post_breeding_2024_data.Rda")



























seasons_list <- movebank_raw_data_all_seasons_10 %>%
  group_by(seasonUnique) %>%
  group_split(.keep = T)

season_names <- map_chr(seasons_list, ~ as.character(.x$seasonUnique[1]))

seasons_sf <- map(seasons_list, ~.x %>%
                    sf::st_as_sf(coords = c("location_long", "location_lat"), 
                                 remove = F) %>%
                    sf::st_set_crs("WGS84") %>%
                    sf::st_transform(32636))

remove_lfr <- function(data){
  Mode <- function(x) { 
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # Calculate daily modes and add them to the dataset (using the function defined above)
  with_modes <- map(data, ~.x %>%
                      group_by(dateOnly, Nili_id) %>%
                      mutate(diff = as.numeric(difftime(lead(timestamp), timestamp, units = "mins"))) %>%
                      group_by(dateOnly, Nili_id) %>%
                      mutate(mode = Mode(round(diff))) %>%
                      ungroup())
  
  # Identify individuals that never have a daily mode of 10 minutes
  low_fix_rate_indivs <- map(with_modes, ~.x %>%
                               sf::st_drop_geometry() %>%
                               group_by(Nili_id) %>%
                               summarize(minmode = min(mode, na.rm = T)) %>%
                               filter(minmode > 10) %>%
                               pull(Nili_id) %>%
                               unique())
  
  # Don't need to save with_modes because we've already used this step to identify low fix rate individuals.
  removed_lfr <- map2(data, low_fix_rate_indivs,                     
                      ~.x %>% filter(!(Nili_id %in% .y)))
  return(removed_lfr)
}


remove_lfr_vu_11<-remove_lfr(seasons_sf)

# Define the file path for saving the downloaded data
file_path <- "~/Documents/GitHub/Ergm_manuscript/data/raw_data/remove_lfr_vu_11.Rda"

# Save the data
#save(remove_lfr_vu_11, file = file_path)

load(file_path)

downsample_10min <- function(data){
    # Define function for downsampling 
    down <- function(df, mins){
      df <- dplyr::arrange(df, timestamp)
      howmuch <- paste(mins, "minute", sep = " ")
      rounded <- lubridate::round_date(df$timestamp, howmuch)
      keep <- !duplicated(rounded)
      return(df[keep,])
    }
    
    # Split by individual so we can downsample more efficiently
    lst <- map(data, ~.x %>% group_split(Nili_id))
    
    # Perform downsampling
    downsampled_10min <- map(lst, ~{
      purrr::list_rbind(map(.x, ~down(df = .x, mins = 10), .progress = T))
    }, .progress = T)
    
    # Add information on # of days tracked for each individual
    downsampled_10min <- map(downsampled_10min, ~.x %>% group_by(Nili_id) %>% 
                               mutate(daysTracked = length(unique(dateOnly))))
    return(downsampled_10min)
}

downsample_10min_movebank_data_12 <- downsample_10min(remove_lfr_vu_11)

# Define the file path for saving the downloaded data
file_path <- "~/Documents/GitHub/Ergm_manuscript/data/raw_data/downsample_10min_movebank_data_12.Rda"

# Save the data
#save(downsample_10min_movebank_data_12, file = file_path)

# Load the .Rda file
load(file_path)

# Assuming the loaded object is named `data` and contains the 12 lists
# Ensure the directory exists to save the seasons data
dir.create("~/Documents/GitHub/Ergm_manuscript/data/raw_data", recursive = TRUE, showWarnings = FALSE)

# For each list/season save the file .Rda
for (i in 1:12) {
  # Define the name of the season data
  assign(paste0("season_", i), downsample_10min_movebank_data_12[[i]])
  
  # Define the file path for saving the season data
  file_path <- paste0("~/Documents/GitHub/Ergm_manuscript/data/raw_data/season_", i, ".Rda")
  
  # Save the data with the season name
  save(list = paste0("season_", i), file = file_path)
}

#just load all the seasons
for (i in 1:12) {
  file_path <- paste0("~/Documents/GitHub/Ergm_manuscript/data/raw_data/season_", i, ".Rda")
  load(file_path)
}

dim(season_1)
dim(season_2)
dim(season_3)
dim(season_4)
dim(season_5)
dim(season_6)
dim(season_7)
dim(season_8)
dim(season_9)
dim(season_10)
dim(season_11)
dim(season_12)
dim(season_13)

#check the seasons data
unique(season_1$seasonUnique)
min(season_1$dateOnly)
max(season_1$dateOnly)

