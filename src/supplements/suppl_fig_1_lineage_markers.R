# Makes lineage marker figures for SI 1

library('tidyverse')
library(cowplot)    # for theme_minimal_hgrid()
library(ggthemes) # for geom_rangeframe

# set folders -----------------------------------------------------------------
folders <- list.dirs('../data/raw/', 
                     recursive = FALSE, 
                     full.names = TRUE)

output_folder <- '../results/figures/SI1/'
dir.create(output_folder, showWarnings = FALSE)
  
for(folder in folders[1:10]) {
  cat(folder, '\n')

  cat('reading species_counts.csv', '\n')
  species_counts <- read.csv(file.path(folder, '/data/population_dat/species_counts.csv'))
  species_counts <- species_counts[, !apply (is.na(species_counts), 2, all)] # remove rows that are all NA
  species_counts[is.na(species_counts)] <- 0 
  colnames(species_counts) <- gsub("X", "", colnames(species_counts))
  species_counts <- gather(species_counts, lineage, fraction, -time_point)
  species_counts$sim = rep(basename(folder), time = nrow(species_counts))
  
  tmax = 1000000

  # here we convert the evo seed (e.g. "101") to more reasonable numbers (e.g. "1")
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
  
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  
  seed <- substrRight(folder, 3)
  population <- which(labels== as.numeric(seed))
  figtitle <- paste0('Population ', population, ' (', basename(folder), ')') 
  

  cat('making species_counts.csv time plot', '\n')
  saving <-
    ggplot(data = species_counts %>% 
             filter(time_point %% 500 == 0),
           aes(x = time_point, y = fraction)) + 
    geom_line(aes(col = lineage), size = 1) + 
    geom_rangeframe(data = data.frame(x = c(0, tmax), 
                                      y = c(0.0001, 1.0)), 
                    aes(x, y),
                    size = 2) + 
    theme_tufte(base_size = 30) + 
    theme(legend.position = 'none')   +
    
    ylab('') + 
    xlab('') +
    scale_x_continuous(limits = c(0, tmax), 
                       expand = expand_scale(mult = 0.02), 
                       #breaks = scales::pretty_breaks(n = 10), 
                       breaks = seq(0, tmax, by = 100000),
                       labels = function(x) x/1000000) +
    scale_y_continuous(limits = c(0.0001, 1.0), 
                       expand = expand_scale(mult = 0.1), 
                       breaks = scales::pretty_breaks(n = 3)) +
    scale_colour_manual(values = rainbow(2025), drop = FALSE) + 
    ggtitle(figtitle) + 
    theme(plot.title = element_text(hjust = 0))
  
  width = 20 * max(species_counts$time_point)/1000000 # to scale width with simulation length
  ggsave(saving, 
         file = paste0(output_folder, '/population', population, '.png'),
         width = width, height = 5, 
         limitsize = FALSE
  )  
} 


