# reads microbial production rates and protein expression grid data
# returns metabolic phenotype based on expression of all 59 metabolic genes

library(tidyverse)
library(cowplot)
library(doParallel)
library(data.table)

source('src/utility/readNumpyByTime.R')
source('src/utility/getNumpyTimes.R')
source('src/utility/computeCoordinates.R')
nr_cores <- detectCores() - 1
tStep = 5 # time step interval to read. Minimal value 5. 
gridwidth = 45

sims <- list.dirs('/hosts/linuxhome/vacuole2/jeroen/emergent-ecologies/remove_lineage_CF_fixDI',
                  recursive = FALSE,
                  full.names = TRUE)
sims <- grep('CF_freenon_evo704', sims, value = T)


read_data <- function(folder)  {
  
  getGridLineage <- function(folder, df_prot)  {
    # Reads grid lineage data
    # Check how grid lineage data is stored
    
    if (file.exists(file.path(folder, '/plots/grid views/lineage_0.csv'))) {
      stop('Lineage grid data in old format\n')
      lintype <- 'gridview'
      
    } else if (file.exists(file.path(folder, 'data/ecology_dat/lineages.csv'))) {
      cat('Lineage grid data in recent format\n')
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
      lineages  <- readNumpyByTime(lineage.file, read_time) 
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

  # 1. read grid files
  grid.files <- list.files(
    file.path(rem_lin_folder, 'data/ecology_dat'),
    pattern = '^grid_conversion|^grid_ex|^grid_imp|^grid_concentration_\\[',
    full.names = TRUE
  )
  
  cat('no. of grid files:', length(grid.files), '\n')
  options(scipen = 999)
  read_time <- getNumpyTimes(rem_lin_folder, leftMargin = 0, tStep = tStep) # was 200 previously
  cat('reading grid files', '\n')
  registerDoParallel(cores = nr_cores)
  df <- foreach(i = grid.files, .combine = rbind) %dopar% readNumpyByTime(i, read_time)
  stopImplicitCluster()
  
  # Data cleaning
  cat('cleaning data',  '\n')
  df[] <-mclapply(df, gsub, 
                  pattern = "array|\\(|\\)|\\[|\\]|\\s", 
                  replacement = "", 
                  perl = TRUE,
                  mc.cores = nr_cores
  )
  df[] <- mclapply(df, as.numeric, mc.cores = nr_cores)
  df <- cbind(reaction = rep(basename(grid.files), each = length(read_time)), df)
  df <- cbind(sim = rep(basename(dirname(dirname(dirname(grid.files)))), 
                        each = length(read_time)), 
              df)
  colnames(df) <- c('sim', 'reaction', 'time', 1:(ncol(df) - 3))
  df <- gather(df, gridpoint, concentration, -reaction, -time, -sim)
  df$gridpoint <- as.numeric(df$gridpoint)
  df <- spread(df, reaction, concentration)
 
  # add metabolic phenotype based on all reactions
  metabolicType <- df %>% select(
    c(starts_with('grid_conversion'), starts_with('grid_imp'), starts_with('grid_exp'))
  )
  metabolicType <- as.data.frame(metabolicType > 0)
  metabolicType[] <- lapply(metabolicType, as.numeric)
  metabolicType <- unite(metabolicType, metabolicType, everything(), sep = "")
  df <- cbind(metabolicType, df)
  
  
  # Compute x and y coordinates
  df$gridpoint <- as.numeric(df$gridpoint)
  df <- mutate(df, x = computeX(gridpoint, gridwidth))
  df <- mutate(df, y = computeY(gridpoint, gridwidth))
  df <- select(df, sim, time, metabolicType, x, y, gridpoint, everything())
  
  
  #2 read production rate data
  prodRate <- readNumpyByTime(paste0(rem_lin_folder, '/data/ecology_dat/grid_production_rates.csv'), 
                              read_time)
  prodRate[] <- mclapply(prodRate, gsub, pattern = "array|\\(|\\)|\\[|\\]|\\s", replacement = "",
                         perl = TRUE,
                         mc.cores = nr_cores
  )
  prodRate[] <- mclapply(prodRate, as.numeric, mc.cores = nr_cores)
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
  lineages <- getGridLineage(rem_lin_folder, df)
  lineages <- lineages %>% arrange(time, y, x)
  df$lineage <- lineages$lineage
  df$lineage <- factor(df$lineage, levels = (-1:gridwidth^2))
  
  return(df)
}


for(rem_lin_folder in sims)  {
  cat(rem_lin_folder, '\n')
  temp <- read_data(rem_lin_folder)
  if(!exists('alldata')) alldata <- temp
  else alldata <- rbind(alldata, temp)
}

saveRDS(alldata, file = 'data/temp/fig3_grid_data.RDS')

