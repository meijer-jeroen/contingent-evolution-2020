# get grid dimensions -----------------------------------------------------
# get no. of columns from environment file

getGridsize <- function(folder){
  env_file <- list.files(folder, full.names = TRUE) %>% grep('env', ., value = TRUE)
  tempdat <- scan(env_file, what = 'character') %>%  grep('rows=', ., value =TRUE) %>% unlist(.)
  gridwidth <- as.numeric(gsub('rows=|,cols=[0-9]*', '', tempdat[1]))
  cat('grid width =', gridwidth, '\n')
  return(gridwidth)
}
