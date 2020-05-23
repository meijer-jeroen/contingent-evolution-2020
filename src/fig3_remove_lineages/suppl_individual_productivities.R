library(tidyverse)
library(colorspace)
library(scales)
library(cowplot)
library(ggbeeswarm) 
alldata <- readRDS('data/temp/fig3_grid_data.RDS')
alldata <- select(alldata, -starts_with('grid')) 

library(tidyverse)
library(colorspace)
library(scales)
library(cowplot)
alldata <- readRDS('data/temp/fig3_grid_data.RDS')
alldata <- select(alldata, -starts_with('grid'))  # save memory

# We want to distinguish metabolic genotypes within each lineage. To colour metabolic 
# genotypes as different shades of their lineage colour, we need combine lineage with 
# metabolic phenotype

alldata <- mutate(alldata, 
                  shadetype = paste0(lineage, '_', metabolicType), 
                  test_time = test_time / 100000
)

levels <- alldata %>% select(shadetype) %>% 
  distinct(.) %>% 
  arrange(shadetype) %>% 
  unlist()


alldata$shadetype_n <-factor(alldata$shadetype, levels = levels)

#check how many colours we need to create for the shade types
lin_a <- as.character(sort(unique(alldata$lineage))[1])
lin_b <- as.character(sort(unique(alldata$lineage))[2])
no_shades_a <- filter(alldata, lineage == lin_a) %>% select(shadetype_n) %>% distinct(.) %>% nrow(.)
no_shades_b <- filter(alldata, lineage == lin_b) %>% select(shadetype_n) %>% distinct(.) %>% nrow(.)



set.seed(12)

cols <- c('black', 
          sample(sequential_hcl(no_shades_a + 2, 'Burg'))[-c((no_shades_a + 1) : (no_shades_a + 2))], 
          sample(sequential_hcl(no_shades_b + 2, 'Teal'))[-c((no_shades_b + 1): (no_shades_b + 2))]
)


for (rep in unique(alldata$sim)[3]) {
  cat(rep, '\n')
  fig <- alldata %>% 
    filter(sim == rep) %>% 
    filter(rel_time %in% c(0,2000)) %>% 
    #filter(sim == unique(alldata$sim)[2]) %>% 
    ggplot() +  
    geom_quasirandom(aes(x = paste0(rel_time, lineage), 
                      y = production_rate, 
                      col = shadetype_n
    ), 
    #priority = 'random',
    size = .01) + 
    theme_minimal_hgrid(12) + 
    facet_grid(killed~test_time) + 
    panel_border() +
    #scale_x_discrete(breaks = c("before\nremoval", "after\nremoval")) +
    theme(axis.text.x = element_text(hjust=-1),
          axis.ticks.x = element_blank()
    ) + 
    # facet_grid(#killed ~test_time, 
    #   test_time ~killed, 
    #   labeller = labeller(
    #     killed = c('704_701' = 'Major lineage removed', 
    #                '704_894' = 'Minor lineage removed'
    #     )
    #   )
    # )+
    ylab('Productivity') + 
    xlab('') +
    scale_colour_manual(values = cols, drop = FALSE) + 
    theme(legend.position = 'none')  
  
  
  save_plot(filename = paste0('figures/supp_individual_growth_rates_', rep, '.png'),
            fig,
            nrow = 1,
            base_asp = 2,
            base_height = NULL,
            base_width = 6
  )
  
}

foo <- function(plot){
  p <- ggplot_build(plot)
  p$data[[1]] <-   p$data[[1]] %>%
    mutate(diff = abs(x-round(x)),  # calculating the difference to the x axis position
           # update the new position depending if group is even (+diff) or odd (-diff)
           x = case_when(group %% 2 == 0 ~ round(x) + diff,
                         TRUE ~ round(x) - diff)) %>%
    select(-diff)
  plot(ggplot_gtable(p))
}
