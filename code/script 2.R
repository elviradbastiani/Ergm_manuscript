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

# Cut the data into the various intervals
data_cut_pre_breeding_2020 <- purrr::map(timewindows, ~{
  pre_breeding_2020 %>%mutate(int = cutdates(dateOnly, .x))})



































# Step 9. -----------------------------------------------------------------
#get roosts locations ----------------------------------------
## Load the seasons data subsets
load("~/Documents/GitHub/Ergm_manuscript/data/raw_data/season_1.Rda")


# Step 10. -----------------------------------------------------------------
#get Edges ----------------------------------------

















