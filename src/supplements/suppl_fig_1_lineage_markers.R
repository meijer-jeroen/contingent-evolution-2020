# Makes lineage marker figures for SI 1

library('tidyverse')
library(cowplot)    # for theme_minimal_hgrid()
library(ggthemes) # for geom_rangeframe
source('src/utility/readSpeciesCounts.R')
source('src/utility/convertEvoseedToPopulation.R')

# set folders -----------------------------------------------------------------
folders <- list.dirs('data/raw/main_experiment', 
                     recursive = FALSE, 
                     full.names = TRUE)

output_folder <- 'figures/SI1/'
dir.create(output_folder, showWarnings = FALSE)
  
for(folder in folders) {

  cat(folder, '\n')
  species_counts <- readSpeciesCounts(folder)
  tmax = 1000000
  
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  seed <- substrRight(folder, 3)
  population <- convertEvoseedToPopulation(as.numeric(seed))
  
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


