# Reads MGX data

readAndLabel <- function(csvfile){
  # reads csv file and adds a column with filename
  sim <-  basename(dirname(dirname(dirname((csvfile)))))
  seed <-  as.numeric(gsub(".*evo([0-9]+)$", "\\1", sim))
  cat(seed)
  source('src/utility/convertEvoseedToPopulation.R')
  population <- convertEvoseedToPopulation(as.numeric(seed))
  temp <- read.csv(csvfile, header = TRUE, check.names = FALSE)
  temp <- cbind(seed = rep(seed, times = nrow(temp)), 
                temp)
  temp <- cbind(sim = rep(sim, times = nrow(temp)), 
                temp)
  temp <- cbind(population = rep(population, times = nrow(temp)), 
                temp)
  cat(paste('\r', 'reading reaction_counts.csv for', sim, ', seed = ', seed, 'population = ', population, '\n'))
  return(temp)
}

# Read data (all time points)
readMGX <- function(folders){
  require(tidyverse)
  
  mgx <- do.call(rbind, lapply(paste0(folders, '/data/population_dat/reaction_counts.csv'), readAndLabel))
  
  # Some data clean up
  source("src/utility/clean_reaction_names.R")
  mgx <- clean_reaction_names(mgx)
  mgx[is.na(mgx)] <- 0
  mgx$seed <- as.character(mgx$seed)
  
  mgx <- select(mgx, time_point, seed, population, sim, everything()) # reorder columns
  mgx <- arrange(mgx, sim, time_point)
  
  return(mgx)
}
