
readNumpyByTime <- function(numpyfile, timepoints, verbal = FALSE){
  # Searches numpy file for lines starting with provided time points 
  # Args:
  #   - nympy.file : a numpy array with grid data, 
  #     e.g. '/home/jeroen/jeroen2/prediction/data/uniform_seed03/data/ecology_dat/grid_conversion_1C_->_1A_+_1I.csv'
  #   - timepoints : list with characters that start the lines you want to read (e.g. c(0, 1000))
  #
  # Returns:
  # a data frame. Rows: time points, columns: concentration for each grid point
  
  result <- do.call(rbind, lapply(timepoints, function(x) return(fread(numpyfile, nrows = 1, skip = x))))
  if(verbal){cat('finished reading', basename(numpyfile), '\n')}
  return(result)
}
