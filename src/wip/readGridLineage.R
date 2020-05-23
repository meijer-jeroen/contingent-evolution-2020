readGridLineage <- function(lineage_csv){
  # utility function
  # reads a single grid lineage file from /plots/grid views/lineage_X.csv
  # adds x and y coordinates
  time <- as.numeric(gsub('[a-z]|\\.|\\_', '', basename(lineage_csv)))
  g <- read.csv(lineage_csv, header = FALSE)
  g <- unlist(g)
  g <- data.frame(time = rep(time, times = length(g)), point = 1:length(g), lineage = g)
  g <- mutate(g, x = computeY(point, gridwidth)) # X and Y flipped in lineage files :(
  g <- mutate(g, y = computeX(point, gridwidth))
}