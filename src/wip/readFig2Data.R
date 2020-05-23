#source('WIP_NLM_functions.R')

# set parameters ----------------------------------------------------------
folders <- list.dirs('../data/raw/', 
                     recursive = FALSE, 
                     full.names = TRUE)
folders <- grep('404|202|901', folders, value = TRUE)

gridwidth <- getGridsize(folder)
gridwidth <- 45
#lin_max <- gridwidth^2
tmax <- 1000000
stepsize <- 10000          # Read interval for grid lineage data. Every 5000 time steps is reasonable. Cannot go lower than lineage_X.csv save interval.

roundDown <- function(x, accuracy, f = floor){f(x / accuracy) * accuracy}
t_plot <- roundDown(seq((tmax / 10), tmax, length = 10), 
                    stepsize
)

#sq <- seq(0, tmax, by = 500)                                  # for subsampling lineage marker frequency plot
#tmax <- max(sq)


# functions ---------------------------------------------------------------
getGridProteome <- function(folder) {
  
  cat('\n', folder, '\n')
  
  # Read grid metabolites
  grid_files <- list.files(file.path(folder, 'data/ecology_dat'),
                           pattern = '^grid_conversion|^grid_ex|^grid_imp',
                           full.names = TRUE)
  cat('reading grid numpys', '\n')
  read_time <<- paste0(seq(0, tmax, by = stepsize), ',array([[')
  registerDoParallel(cores = nr_cores)
  df <- foreach(i = grid_files, .combine = rbind) %dopar% readNumpyByTime(i, read_time)
  stopImplicitCluster()
  
  # Some clean up
  cat('cleaning data', '\n')
  df[] <- mclapply(df, gsub, pattern = "array|\\(|\\)|\\[|\\]|\\s", replacement = "", perl = TRUE, mc.cores = nr_cores)
  df[] <- mclapply(df, as.numeric, mc.cores = nr_cores)
  df <- cbind(reaction = rep(basename(grid_files), each = length(read_time)), df)
  colnames(df) <- c('reaction', 'time', 1:(ncol(df) - 2))
  df <- gather(df, gridpoint, concentration, -reaction, -time)
  df$gridpoint <- as.numeric(df$gridpoint)
  df <- spread(df, reaction, concentration)
  
  df$gridpoint <- as.numeric(df$gridpoint)
  
  # add grid coordinates
  #gridwidth <<- getGridsize(folder)
  df <- mutate(df, x = computeX(gridpoint, gridwidth))
  df <- mutate(df, y = computeY(gridpoint, gridwidth))
  df <- select(df, time, x, y, gridpoint, everything())
  
  # read and add lineage markers
  cat('adding lineage markers', '\n')
  df_lin <- getGridLineage(folder, df)
  bla <-  filter(df_lin, time %in% df$time)
  df$lineage <- bla$lineage
  df$lineage <- factor(df$lineage, levels = (-1:gridwidth^2))
  
  
  return(df)
}   


getGridLineage <- function(folder, df_prot)  {
  # Reads grid lineage data
  
  #gridwidth = getGridsize(folder)
  # Check how grid lineage data is stored
  
  if (file.exists(file.path(folder, '/plots/grid views/lineage_0.csv'))) {
    cat('Lineage grid data in separate files in plots/grid_views/ (old version))\n')
    lintype <- 'gridview'
    
  } else if (file.exists(file.path(folder, 'data/ecology_dat/lineages.csv'))) {
    cat('Lineage grid data in Numpy-like filethingy in ecology_dat (recent version)\n')
    lintype <- 'numpy'
    
  } else {
    warning('No grid lineage data detected \n')
  }
  
  
  if (lintype == 'numpy') {
    pattern = paste(unique(df_prot$time), collapse = '|')
    
    lineage.files <-
      list.files(
        file.path(folder, 'data/ecology_dat'),
        pattern = 'lineage',
        full.names = TRUE
      )
    registerDoParallel(cores = nr_cores)
    lineages <- foreach(i = lineage.files, .combine = rbind) %dopar% readNumpyByTime(i, read_time[1:2])                       #why 1:2 of read.time?
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
    
    
  } else if (lintype == 'gridview') {
    lineage.files = paste0(folder,
                           '/plots/grid views/lineage_',
                           unique(df_prot$time),
                           '.csv')
    lineages <-
      do.call(rbind, lapply(lineage.files, readGridLineage))
    lineages <- lineages %>% arrange(time, y, x)
  }
  
  return(lineages)
} 


# main loop ---------------------------------------------------------------
for(folder in folders)  {
  
  
  # read grid data ----------------------------------------------------------
  df_prot <- getGridProteome(folder)
  saveRDS(df_prot, file = paste0('../data/temp/', basename(folder), 'df_prot.RDS'))
  
  # Read species_counts.csv -------------------------------------------------
  species_counts <- readSpeciesCounts(folder)
  species_counts$lineage <-factor(species_counts$lineage, levels = (-1:gridwidth^2))
  saveRDS(species_counts, file = paste0('../data/temp/', basename(folder), 'speciescounts.RDS'))

}
