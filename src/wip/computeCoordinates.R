computeX <- function(gridpoint, gridwidth)  
{
  # assumes square grid
  z <- ifelse(gridpoint %% gridwidth == 0, gridwidth, gridpoint %% gridwidth)
  return(z)
}

computeY <- function(gridpoint, gridwidth)
{
  # assumes square grid
  z <- ceiling(gridpoint/gridwidth)
  return(z)
}
