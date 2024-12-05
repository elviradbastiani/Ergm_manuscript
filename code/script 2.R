#*****************************************************************************************
# Manuscript - What are the drivers of social interactions?
# Author: Elvira D'Bastiani
# Department of Ecology and Evolutionary Biology, University of California Los Angeles
# Corresponding Author: Elvira D'Bastiani, Email: elviradbastiani@gmail.com
#*****************************************************************************************

# Setup ---------------------------------------------------------------
#To remove all objects stored in the current workspace
rm(list=ls())

#To clean up the garbage collection in R, you can use the gc() function:
gc()

library(tidyverse) # for data wrangling
library(sf) # for spatial manipulation (necessary as a precursor to getting edgelists)
library(igraph) # for working with networks
library(tidygraph) # for working with networks
library(future) # for parallel processing
library(furrr) # for parallel processing
library(here) # for tidy file paths
library(purrr) # Provides functions for functional programming.
library(dplyr) 

#just load all the seasons

## Cut Summer 2023 into different time windows
### Define the time windows
#To test how the networks behave over different time intervals, we can prepare a subset of data (summer 2023) cut into different intervals. E.g. 1 day, 5 days, 10 days, 25 days, 50 days.

# What are the different time windows we want to test?
timewindows <- c(1)

# A function for cutting a vector of dates (`vec`) into `days`-day intervals
cutdates <- function(vec, days){
  # get min and max dates in the vector
  min <- min(vec)
  max <- max(vec)
  
  # determine how many cutpoints we'll need
  ncutpoints <- ceiling(as.numeric(max-min)/days) + 1
  
  # create the vector of cutpoints
  cutpoints <- seq(from = min, by = days, length.out = ncutpoints)
  
  # cut the dates according to the cutpoints. Intervals will have the format [low, high).
  out <- cut(vec, breaks = cutpoints, include.lowest = T, right = F)
  return(out)
}

## Load the seasons data subsets
load(file = "~/Documents/GitHub/Ergm_manuscript/data/raw_data/pre_breeding_2020.Rda")
load(file = "~/Documents/GitHub/Ergm_manuscript/data/raw_data/breeding_2021.Rda")
load(file = "~/Documents/GitHub/Ergm_manuscript/data/raw_data/post_breeding_2021.Rda")

load(file = "~/Documents/GitHub/Ergm_manuscript/data/raw_data/pre_breeding_2021.Rda")
load(file = "~/Documents/GitHub/Ergm_manuscript/data/raw_data/breeding_2022.Rda")
load(file = "~/Documents/GitHub/Ergm_manuscript/data/raw_data/post_breeding_2022.Rda")

load(file = "~/Documents/GitHub/Ergm_manuscript/data/raw_data/pre_breeding_2022.Rda")
load(file = "~/Documents/GitHub/Ergm_manuscript/data/raw_data/breeding_2023.Rda")
load(file = "~/Documents/GitHub/Ergm_manuscript/data/raw_data/post_breeding_2023.Rda")

load(file = "~/Documents/GitHub/Ergm_manuscript/data/raw_data/pre_breeding_2023.Rda")
load(file = "~/Documents/GitHub/Ergm_manuscript/data/raw_data/breeding_2024.Rda")
load(file = "~/Documents/GitHub/Ergm_manuscript/data/raw_data/post_breeding_2024.Rda")

# Apply the function to each season individually
data_cut_pre_breeding_2020 <- purrr::map(timewindows, ~{
  pre_breeding_2020 %>%
    mutate(int = cutdates(dateOnly, .x))
})

data_cut_breeding_2021 <- purrr::map(timewindows, ~{
  breeding_2021 %>%
    mutate(int = cutdates(dateOnly, .x))
})

data_cut_post_breeding_2021 <- purrr::map(timewindows, ~{
  post_breeding_2021 %>%
    mutate(int = cutdates(dateOnly, .x))
})

data_cut_pre_breeding_2021 <- purrr::map(timewindows, ~{
  pre_breeding_2021 %>%
    mutate(int = cutdates(dateOnly, .x))
})

data_cut_breeding_2022 <- purrr::map(timewindows, ~{
  breeding_2022 %>%
    mutate(int = cutdates(dateOnly, .x))
})

data_cut_post_breeding_2022 <- purrr::map(timewindows, ~{
  post_breeding_2022 %>%
    mutate(int = cutdates(dateOnly, .x))
})

data_cut_pre_breeding_2022 <- purrr::map(timewindows, ~{
  pre_breeding_2022 %>%
    mutate(int = cutdates(dateOnly, .x))
})

data_cut_breeding_2023 <- purrr::map(timewindows, ~{
  breeding_2023 %>%
    mutate(int = cutdates(dateOnly, .x))
})

data_cut_post_breeding_2023 <- purrr::map(timewindows, ~{
  post_breeding_2023 %>%
    mutate(int = cutdates(dateOnly, .x))
})

data_cut_pre_breeding_2023 <- purrr::map(timewindows, ~{
  pre_breeding_2023 %>%
    mutate(int = cutdates(dateOnly, .x))
})

data_cut_breeding_2024 <- purrr::map(timewindows, ~{
  breeding_2024 %>%
    mutate(int = cutdates(dateOnly, .x))
})

data_cut_post_breeding_2024 <- purrr::map(timewindows, ~{
  post_breeding_2024 %>%
    mutate(int = cutdates(dateOnly, .x))
})

# Okay, now we have the data classified into intervals, time to split each one into a list.
# There is an annoying thing here: when you have an sf object and you run group_by() %>% group_split() on it, the resulting sub-objects do not keep their sf status. They turn into regular data frames. Grrrrr! So I had to add a step to turn each of them back into an sf object.

# Pre-breeding 2020
dataCut_pre_breeding_2020 <- purrr::map(data_cut_pre_breeding_2020, ~.x %>%
                                           group_by(int) %>%
                                           group_split() %>%
                                           purrr::map(., ~sf::st_as_sf(.x, coords = c("location_long", "location_lat"),
                                                                       crs = "WGS84", remove = FALSE)))

# Breeding 2021
dataCut_breeding_2021 <- purrr::map(data_cut_breeding_2021, ~.x %>%
                                       group_by(int) %>%
                                       group_split() %>%
                                       purrr::map(., ~sf::st_as_sf(.x, coords = c("location_long", "location_lat"),
                                                                   crs = "WGS84", remove = FALSE)))

# Post-breeding 2021
dataCut_post_breeding_2021 <- purrr::map(data_cut_post_breeding_2021, ~.x %>%
                                            group_by(int) %>%
                                            group_split() %>%
                                            purrr::map(., ~sf::st_as_sf(.x, coords = c("location_long", "location_lat"),
                                                                        crs = "WGS84", remove = FALSE)))

# Pre-breeding 2021
dataCut_pre_breeding_2021 <- purrr::map(data_cut_pre_breeding_2021, ~.x %>%
                                           group_by(int) %>%
                                           group_split() %>%
                                           purrr::map(., ~sf::st_as_sf(.x, coords = c("location_long", "location_lat"),
                                                                       crs = "WGS84", remove = FALSE)))

# Breeding 2022
dataCut_breeding_2022 <- purrr::map(data_cut_breeding_2022, ~.x %>%
                                       group_by(int) %>%
                                       group_split() %>%
                                       purrr::map(., ~sf::st_as_sf(.x, coords = c("location_long", "location_lat"),
                                                                   crs = "WGS84", remove = FALSE)))

# Post-breeding 2022
dataCut_post_breeding_2022 <- purrr::map(data_cut_post_breeding_2022, ~.x %>%
                                            group_by(int) %>%
                                            group_split() %>%
                                            purrr::map(., ~sf::st_as_sf(.x, coords = c("location_long", "location_lat"),
                                                                        crs = "WGS84", remove = FALSE)))

# Pre-breeding 2022
dataCut_pre_breeding_2022 <- purrr::map(data_cut_pre_breeding_2022, ~.x %>%
                                           group_by(int) %>%
                                           group_split() %>%
                                           purrr::map(., ~sf::st_as_sf(.x, coords = c("location_long", "location_lat"),
                                                                       crs = "WGS84", remove = FALSE)))

# Breeding 2023
dataCut_breeding_2023 <- purrr::map(data_cut_breeding_2023, ~.x %>%
                                       group_by(int) %>%
                                       group_split() %>%
                                       purrr::map(., ~sf::st_as_sf(.x, coords = c("location_long", "location_lat"),
                                                                   crs = "WGS84", remove = FALSE)))

# Post-breeding 2023
dataCut_post_breeding_2023 <- purrr::map(data_cut_post_breeding_2023, ~.x %>%
                                            group_by(int) %>%
                                            group_split() %>%
                                            purrr::map(., ~sf::st_as_sf(.x, coords = c("location_long", "location_lat"),
                                                                        crs = "WGS84", remove = FALSE)))

# Pre-breeding 2023
dataCut_pre_breeding_2023 <- purrr::map(data_cut_pre_breeding_2023, ~.x %>%
                                           group_by(int) %>%
                                           group_split() %>%
                                           purrr::map(., ~sf::st_as_sf(.x, coords = c("location_long", "location_lat"),
                                                                       crs = "WGS84", remove = FALSE)))

# Breeding 2024
dataCut_breeding_2024 <- purrr::map(data_cut_breeding_2024, ~.x %>%
                                       group_by(int) %>%
                                       group_split() %>%
                                       purrr::map(., ~sf::st_as_sf(.x, coords = c("location_long", "location_lat"),
                                                                   crs = "WGS84", remove = FALSE)))

# Post-breeding 2024
dataCut_post_breeding_2024 <- purrr::map(data_cut_post_breeding_2024, ~.x %>%
                                            group_by(int) %>%
                                            group_split() %>%
                                            purrr::map(., ~sf::st_as_sf(.x, coords = c("location_long", "location_lat"),
                                                                        crs = "WGS84", remove = FALSE)))

# Process and save each season
purrr::map_dbl(dataCut_pre_breeding_2020, length) # each element has a different number of elements--122 for the 1-day intervals, 25 for the 5-day intervals, etc. etc.
save(dataCut_pre_breeding_2020, file = here("~/Documents/GitHub/Ergm_manuscript/data/raw_data/dataCut_pre_breeding_2020.Rda"))

purrr::map_dbl(dataCut_breeding_2021, length)
save(dataCut_breeding_2021, file = here("~/Documents/GitHub/Ergm_manuscript/data/raw_data/dataCut_breeding_2021.Rda"))

purrr::map_dbl(dataCut_post_breeding_2021, length)
save(dataCut_post_breeding_2021, file = here("~/Documents/GitHub/Ergm_manuscript/data/raw_data/dataCut_post_breeding_2021.Rda"))

purrr::map_dbl(dataCut_pre_breeding_2021, length)
save(dataCut_pre_breeding_2021, file = here("~/Documents/GitHub/Ergm_manuscript/data/raw_data/dataCut_pre_breeding_2021.Rda"))

purrr::map_dbl(dataCut_breeding_2022, length)
save(dataCut_breeding_2022, file = here("~/Documents/GitHub/Ergm_manuscript/data/raw_data/dataCut_breeding_2022.Rda"))

purrr::map_dbl(dataCut_post_breeding_2022, length)
save(dataCut_post_breeding_2022, file = here("~/Documents/GitHub/Ergm_manuscript/data/raw_data/dataCut_post_breeding_2022.Rda"))

purrr::map_dbl(dataCut_pre_breeding_2022, length)
save(dataCut_pre_breeding_2022, file = here("~/Documents/GitHub/Ergm_manuscript/data/raw_data/dataCut_pre_breeding_2022.Rda"))

purrr::map_dbl(dataCut_breeding_2023, length)
save(dataCut_breeding_2023, file = here("~/Documents/GitHub/Ergm_manuscript/data/raw_data/dataCut_breeding_2023.Rda"))

purrr::map_dbl(dataCut_post_breeding_2023, length)
save(dataCut_post_breeding_2023, file = here("~/Documents/GitHub/Ergm_manuscript/data/raw_data/dataCut_post_breeding_2023.Rda"))

purrr::map_dbl(dataCut_pre_breeding_2023, length)
save(dataCut_pre_breeding_2023, file = here("~/Documents/GitHub/Ergm_manuscript/data/raw_data/dataCut_pre_breeding_2023.Rda"))

purrr::map_dbl(dataCut_breeding_2024, length)
save(dataCut_breeding_2024, file = here("~/Documents/GitHub/Ergm_manuscript/data/raw_data/dataCut_breeding_2024.Rda"))

purrr::map_dbl(dataCut_post_breeding_2024, length)
save(dataCut_post_breeding_2024, file = here("~/Documents/GitHub/Ergm_manuscript/data/raw_data/dataCut_post_breeding_2024.Rda"))













































# Step 9. -----------------------------------------------------------------
#get roosts locations ----------------------------------------
## Load the seasons data subsets
load("~/Documents/GitHub/Ergm_manuscript/data/raw_data/season_1.Rda")


# Step 10. -----------------------------------------------------------------
#get Edges ----------------------------------------

















