#reads individual (grid) production rates and metabolic phenotype based on expression of 4 core genes. 

sims <- list.dirs('~/alive/hosts/linuxhome/vacuole2/jeroen/emergent-ecologies/remove_lineage_CF_fixDI',
                  recursive = FALSE,
                  full.names = TRUE)
sims <- grep('CF_freenon_evo704', sims, value = T)

files <- paste0(sims, '/data/population_dat/population_size.csv')
sims <- sims[file.exists(files)] # not-so-elegant way to check for running simulations
files <- files[file.exists(files)]
tStep = 10# time steps to read
gridwidth = 45


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(cowplot)
library(digest)
library(data.table)
library(grid)  
library(gridExtra)
library(foreach)
library(doParallel)
library(parallel)
#source('/home/jeroen/Documents/vm-rscripts/utility/getGridsize.R') # shouldnt need this
#source('/home/jeroen/Documents/vm-rscripts/utility/computeCoordinates.R')
no.cores <- detectCores() - 1

source('../../../src/utility/readNumpyByTime.R')
source('../../../src/utility/readGridLineage.R')
source('../../../src/utility/  ')
source('../../../temp/vm-rscripts/utility/getNumpyTimes.R')



# Functions ---------------------------------------------------------------

getGridLineage <- function(folder, df_prot)  {
  # Reads grid lineage data
  
  #gridwidth = getGridsize(folder)
  # Check how grid lineage data is stored
  
  if (file.exists(file.path(folder, '/plots/grid views/lineage_0.csv'))) {
    stop('Lineage grid data in separate files in plots/grid_views/ (old version))\n')
    lintype <- 'gridview'
    
  } else if (file.exists(file.path(folder, 'data/ecology_dat/lineages.csv'))) {
    cat('Lineage grid data in Numpy-like filethingy in ecology_dat (recent version)\n')
    lintype <- 'numpy'
    
  } else {
    warning('No grid lineage data detected \n')
  }
  
  
  if (lintype == 'numpy') {
    pattern = paste(unique(df_prot$time), collapse = '|')
    
    lineage.file <-
      list.files(
        file.path(folder, 'data/ecology_dat'),
        pattern = 'lineage',
        full.names = TRUE
      )
    registerDoParallel(cores = nr_cores)
    lineages  <- readNumpyByTime(lineage.file, read_time)                       #why 1:2 of read.time?
    stopImplicitCluster()
    
    # Some clean up
    lineages[] <-mclapply(
      lineages,
      gsub,
      pattern = "array|\\(|\\)|\\[|\\]|\\s",
      replacement = "",
      perl = TRUE,
      mc.cores = nr_cores
    )
    
    lineages[] <- mclapply(lineages, as.numeric, mc.cores = nr_cores)
    colnames(lineages) <- c('time', 1:(ncol(lineages) - 1))
    lineages <- gather(lineages, gridpoint, lineage,-time)
    lineages$gridpoint <- as.numeric(lineages$gridpoint)
    lineages <- mutate(lineages, x = computeX(gridpoint, gridwidth))
    lineages <- mutate(lineages, y = computeY(gridpoint, gridwidth))
    lineages$lineage[is.na(lineages$lineage)] <- -1
    lineages <- lineages %>% arrange(time, y, x)
  }
    
    
  return(lineages)
}

read_data <- function(rem_lin_folder){
  # 1. read grid files
  grid.files <- list.files(
    file.path(rem_lin_folder, 'data/ecology_dat'),
    pattern = '^grid_conversion|^grid_ex|^grid_imp',
    full.names = TRUE
  )
  grid.files <- grep('import.*A|import.*E|1C_->_1A|1C_->_1E',
                     grid.files,
                     value = TRUE)
  cat('no. of grid files:', length(grid.files))
  options(scipen = 999)
  registerDoParallel(cores = no.cores)
  
  readTimes <- getNumpyTimes(rem_lin_folder, leftMargin = 200, tStep = tStep)
  
  cat('reading grid files', '\n')
  
  df <- foreach(i = grid.files, .combine = rbind) %dopar% readNumpyByTime(i, readTimes)
  
  # Data cleaning
  cat('cleaning data',  '\n')
  df[] <-mclapply(df, gsub, 
                  pattern = "array|\\(|\\)|\\[|\\]|\\s", 
                  replacement = "", 
                  perl = TRUE,
                  mc.cores = no.cores
  )
  df[] <- mclapply(df, as.numeric, mc.cores = no.cores)
  df <- cbind(reaction = rep(basename(grid.files), each = length(readTimes)), df)
  df <- cbind(sim = rep(basename(dirname(dirname(dirname(grid.files)))), 
                        each = length(readTimes)), 
              df)
  
  colnames(df) <- c('sim', 'reaction', 'time', 1:(ncol(df) - 3))
  df <- gather(df, gridpoint, concentration, -reaction, -time, -sim)
  df$gridpoint <- as.numeric(df$gridpoint)
  df <- spread(df, reaction, concentration)
  
  # Add metabolic marker based on 4 main metabolic genes
  metabolicType <- df %>% select(
    "grid_import_pump_1I*->1A.csv",
    "grid_import_pump_1I*->1E.csv",
    "grid_conversion_1C_->_1A_+_1I.csv",
    "grid_conversion_1C_->_1E_+_1I.csv"
  )
  metabolicType <- as.data.frame(metabolicType > 0)
  metabolicType[] <- lapply(metabolicType, as.numeric)
  metabolicType <- unite(metabolicType, metabolicType, everything(), sep = "")
  df <- cbind(metabolicType, df)
  
  # states <- c(0,1) # we need to globally set levels for metabolicType for consistent colouring accross different experiments
  # m.levels <- expand.grid(states, states, states, states)
  # m.levels <- apply(m.levels, 1, paste, collapse = '')
  # df$metabolicTypes <- factor(df$metabolicType, levels = m.levels)
  # rm(metabolicType, states, m.levels)
  
  # set clade_type factor level
  number2binary = function(number, noBits) {
    binary_vector = rev(as.numeric(intToBits(number)))
    if (missing(noBits)) {
      return(binary_vector)
    } else {
      binary_vector[-(1:(length(binary_vector) - noBits))]
    }
  }
  clade_levels <- lapply(seq(1:16), number2binary, 4)
  clade_levels <- sapply(clade_levels, paste0, collapse = '')
  cat(clade_levels, '\n')
  #mullerData_clean$clade_type <- gsub(',|\\[|\\]|\\s', '', mullerData_clean$clade_type)
  df$metabolicType <- factor(as.character(df$metabolicType), levels = clade_levels)
  
  
  # Compute x and y coordinates
  df$gridpoint <- as.numeric(df$gridpoint)
  df <- mutate(df, x = computeX(gridpoint, gridwidth))
  df <- mutate(df, y = computeY(gridpoint, gridwidth))
  df <- select(df, sim, time, metabolicType, x, y, gridpoint, everything())
  
  
  #2 read production rate data
  prodRate <- readNumpyByTime(paste0(rem_lin_folder, '/data/ecology_dat/grid_production_rates.csv'), 
                              readTimes)
  prodRate[] <- mclapply(prodRate, gsub, pattern = "array|\\(|\\)|\\[|\\]|\\s", replacement = "",
                         perl = TRUE,
                         mc.cores = no.cores
  )
  prodRate[] <- mclapply(prodRate, as.numeric, mc.cores = no.cores)
  colnames(prodRate) <- c('time', 1:(ncol(prodRate) - 1))
  prodRate <- gather(prodRate, gridpoint, prodRate, -time)
  prodRate$gridpoint <- as.numeric(prodRate$gridpoint)
  prodRate <- arrange(prodRate, time, gridpoint)
  
  df <- cbind(df, production_rate = prodRate$prodRate)
  df <-select(df, sim, time, x, y, gridpoint, production_rate, metabolicType, everything())
  
  # set relative time
  df <- mutate(df, 
               #mutant = as.numeric(gsub('^.*\\/mutant|\\/tmp.*$', '', csvfile)),
               #mutant <- as.numeric(gsub('^.*\\/mutant|\\/tmp.*$', '', sims)),
               seed = gsub('CF|fix|DI|freenon|_evo|nomut||\\_k.*|\\_', '', sim),
               killed = paste0(seed, '_', gsub('CF|fix|freenon|DI|evo[0-9]*|nomut|\\_t[0-9]*|k|\\_', '', sim)),
               test_time = round(as.numeric(gsub('.*\\_t|\\/data\\/.*', '', sim)), digits = -4),
               rel_time = time - test_time
               
  ) 
  
  # add lineage markers
  # read and add lineage markers
  cat('adding lineage markers', '\n')
  
  min_time <- unique(df$test_time)
  cat(min_time)
  
  read_time <<- paste0(filter(unique(df$time) >= unique(df$test_time)), ',array([[')
  cat(read_time)
  
  df_lin <- getGridLineage(rem_lin_folder, df)
  bla <-  filter(df_lin, time %in% df$time)
  df <- filter(df$time %in% bla$time)
  df$lineage <- bla$lineage
  df$lineage <- factor(df$lineage, levels = (-1:gridwidth^2))
  
  
  #3 read grid lineage files
  # problems: 
  # 1. the lineage file from the lineage removal experiment only contains data for the time points that were simulated.
  # 2. old data is only sparsely available if it is in the old format
  
  
  # gridlin <-
  #   readNumpyByTime(paste0(rem_lin_folder, '/data/ecology_dat/lineages.csv'),
  #                   readTimes[-c(1:4)])
  #
  # gridlin[] <-
  #   mclapply(
  #     gridlin,
  #     gsub,
  #     pattern = "array|\\(|\\)|\\[|\\]|\\s",
  #     replacement = "",
  #     perl = TRUE,
  #     mc.cores = no.cores
  #   )
  # gridlin[] <- mclapply(gridlin, as.numeric, mc.cores = no.cores)
  # colnames(gridlin) <- c('time', 1:(ncol(gridlin) - 1))
  # gridlin <- gather(gridlin, gridpoint, lineage, -time)
  # gridlin$gridpoint <- as.numeric(gridlin$gridpoint)
  # gridlin <- mutate(gridlin, x = computeX(gridpoint, gridwidth))
  # gridlin <- mutate(gridlin, y = computeY(gridpoint, gridwidth))
  # gridlin$lineage[is.na(gridlin$lineage)] <- -1
  #
  # df <- left_join(df, gridlin)


  # gridlin[] <-
  #   mclapply(
  #     gridlin,
  #     gsub,
  #     pattern = "array|\\(|\\)|\\[|\\]|\\s|\\.",
  #     replacement = "",
  #     perl = TRUE,
  #     mc.cores = no.cores
  #   )
  
  
  
}

for(sim in sims[1])  {
  df <- read_data(sim)
 # saveRDS(df, file = paste0('../data/temp/', basename(sim), 'grid_phenotype.RDS'))  
  saveRDS(df, file = paste0('../data/temp/test.RDS'))  
  
  
}
