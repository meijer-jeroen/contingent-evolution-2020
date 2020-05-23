getNumpyTimes <- function(remlinFolder, leftMargin, tStep, tMax = NA){
  # returns read time pattern for readNumpyByTime
  # left_margin = margin of data BEFORE start of remove lineage experiment
  # tStep = sampling time of data points to read (min == store-data-time)

  temp <- read.csv(paste0(remlinFolder, '/data/population_dat/cell_sizes.csv'))
  
  # final time point of simulation
  if(is.na(tMax)){tMax <- tail(temp, 1)$time_point}
     
  pattern <- '.*\\_t([0-9]+).*'

  # time point when lineage was removed (default = 10 time points after save file)
  tKill <- as.numeric(sub(pattern, "\\1", basename(remlinFolder)))

  # make sequence of time points to read
  tRead <- seq(tKill - 10, tMax, by = tStep) # simulation with more frequent store-data-time starts at t = tKill - 10

  tRead = c(seq(tKill-10 - leftMargin, tKill-10, by = 50), tRead[-1])
  tRead <- paste0(tRead, ',array([[')
  return(tRead)
}

