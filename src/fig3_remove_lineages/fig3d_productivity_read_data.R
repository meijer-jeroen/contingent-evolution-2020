library(tidyverse)

folders <- list.dirs('data/raw/remove_lineage',
                     recursive = FALSE,
                     full.names = TRUE)


readAndLabel <- function(csvfile){
  # reads csv file and adds a column with filename
  sim <-  basename(dirname(dirname(dirname((csvfile)))))
  seed = gsub('CF|fix|DI|freenon|_evo|nomut||\\_k.*|\\_', '', sim)
  temp <- read.csv(csvfile, header = TRUE, check.names = FALSE)
  temp <- cbind(sim = rep(sim, times = nrow(temp)), 
                temp)
  temp <- cbind(seed = rep(seed, times = nrow(temp)), 
                temp)
  
  cat(paste('\r', 'read file', sim, '\r'))
  return(temp)
}



prodValue <- do.call(rbind,
                     lapply(
                       paste0(folders, '/data/population_dat/production_values.csv'),
                       readAndLabel
                     ))

prodRate <-
  do.call(rbind, lapply(
    paste0(folders, '/data/population_dat/pos_production.csv'),
    readAndLabel
  ))

death <-
  do.call(rbind, lapply(
    paste0(folders, '/data/population_dat/death_rates.csv'),
    readAndLabel
  ))


genome_sizes <-
  do.call(rbind, lapply(
    paste0(folders, '/data/population_dat/genome_sizes.csv'),
    readAndLabel
  ))


pop_sizes <-
  do.call(rbind, lapply(
    paste0(folders, '/data/population_dat/population_size.csv'),
    readAndLabel
  ))

cell_sizes <-
  do.call(rbind, lapply(
    paste0(folders, '/data/population_dat/cell_sizes.csv'),
    readAndLabel
  ))



prodValue <- cbind(prodValue, cell_size = cell_sizes$avrg)
prodValue <- cbind(prodValue, prod_rate = prodRate$avrg)
prodValue <- cbind(prodValue, prod_rate_std = prodRate$std)
prodValue <- cbind(prodValue, genome_size = genome_sizes$avrg)
prodValue <- cbind(prodValue, death_rate = death$avrg)
prodValue <- cbind(prodValue, pop_size = pop_sizes$value)

saveRDS(prodValue,    
       file = 'data/temp/fig3_productivity.RDS'
)
