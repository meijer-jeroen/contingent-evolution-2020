
readSpeciesCounts <- function(folder)  {
  library(data.table)
  cat('reading species_counts.csv', '\n')
  #species_counts <- read.csv(file.path(folder, '/data/population_dat/species_counts.csv'))
  species_counts <- as.data.frame(fread(file.path(folder, '/data/population_dat/species_counts.csv')))  
  species_counts <- species_counts[, !apply (is.na(species_counts), 2, all)] # remove rows that are all NA
  species_counts[is.na(species_counts)] <- 0 
  colnames(species_counts) <- gsub("X", "", colnames(species_counts))
  species_counts <- gather(species_counts, lineage, fraction, -time_point)
  species_counts$sim = rep(basename(folder), time = nrow(species_counts))
  
  return(species_counts)  
}