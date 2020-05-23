# Reads MGX data

readAndLabel <- function(csvfile){
  # reads csv file and adds a column with filename
  sim <-  basename(dirname(dirname(dirname((csvfile)))))
  seed <-  as.numeric(gsub(".*evo([0-9]+)$", "\\1", sim))

  # number communities 1-60 -------------------------------------------------
  labels <-
    c(101,
      102,
      103,
      104,
      105,
      106,
      201,
      202,
      203,
      204,
      205,
      206,
      207,
      301,
      302,
      303,
      304,
      305,
      306,
      401,
      402,
      403,
      404,
      405,
      406,
      407,
      408,
      409,
      410,
      501,
      502,
      503,
      504,
      505,
      506,
      601,
      602,
      603,
      604,
      605,
      606,
      701,
      702,
      703,
      704,
      705,
      706,
      801,
      802,
      803,
      804,
      805,
      806,
      901,
      902,
      903,
      904,
      905,
      906,
      507
    )
  population <- which(labels== as.numeric(seed))

  temp <- read.csv(csvfile, header = TRUE, check.names = FALSE)
  temp <- cbind(seed = rep(seed, times = nrow(temp)), 
                temp)
  temp <- cbind(sim = rep(sim, times = nrow(temp)), 
                temp)
  temp <- cbind(population = rep(population, times = nrow(temp)), 
                temp)
  cat(paste('\r', 'reading reaction_counts.csv for', sim, ', seed = ', seed, 'population = ', population, '\n'))
  return(temp)
}

# Read data (all time points)
readMGX <- function(folders){
  require(tidyverse)
  
  mgx <- do.call(rbind, lapply(paste0(folders, '/data/population_dat/reaction_counts.csv'), readAndLabel))
  
  # Some data clean up
  source("/home/jeroen/Documents/scripts/clean_reaction_names.R")
  mgx <- cleanReactionNames(mgx)
  mgx[is.na(mgx)] <- 0
  mgx$seed <- as.character(mgx$seed)
  
  mgx <- select(mgx, time_point, seed, population, sim, everything()) # reorder columns
  mgx <- arrange(mgx, sim, time_point)
  
  
  # # Label most abundant energy reaction                    
  # energy_reactions <- mgx %>% 
  #   group_by(sim, time_point) %>% 
  #   select(sim, time_point, starts_with("A"), starts_with('C'), starts_with('E')) %>% 
  #   select(-c("C-->E+I", "C-->A+I")) %>% 
  #   gather(reaction, fraction, -sim, -time_point) %>% 
  #   slice(which.max(fraction))
  # 
  # mgx$ene_react <- energy_reactions$reaction
  # rm(energy_reactions)
  # 
  # label strategy as inferred from on importers
  # mgx <- mgx %>% 
  #   mutate(strategy = 
  #            case_when(`I*-->Aimport` > 0.33 & `I*-->Aimport` < 0.66 & `I*-->Eimport` > 0.33 & `I*-->Eimport` < 0.66 ~ 'CF',
  #                      `I*-->Aimport` > .95 & `I*-->Eimport` > .95 ~ '2 imp', 
  #                      `I*-->Aimport` > .95 | `I*-->Eimport` > .95 ~ '1 imp', 
  #                      TRUE ~ '0 imp'
  #            )
  #   )                     
  # 
  # mgx <- select(mgx, time_point, sim, seed, strategy, ene_react, everything()) # reorder columns
  # 
  # mgx <- mgx %>% 
  #   mutate(ene_sub = substr(ene_react, 1, 1))
  # 
  # mgx <- mgx %>% 
  #   mutate(ene_prod = gsub('[A-Z]-->|\\+.*', '', mgx$ene_react))
  # 
  # mgx %>% 
  #   group_by(sim) %>%
  #   slice(which.max(time_point)) %>% 
  #   select(sim, seed, time_point, ene_react, ene_sub, ene_prod, strategy, `I*-->Aimport`, `I*-->Eimport`)
  return(mgx)
}


#save(file = paste0(Sys.time(), '_mgx.rdata'), mgx)