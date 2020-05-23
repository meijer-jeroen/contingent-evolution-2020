library(tidyverse)
library(colorspace)
library(scales)
library(cowplot)

alldata <- readRDS('data/temp/fig3_grid_data.RDS')
t_plot <- c(0, 150, 200, 300, 400, 500, 600, 1500)
labels <- paste0("+", t_plot)
names(labels) <- t_plot


# figure in main text -----------------------------------------------------
lin <- alldata %>% 
  filter(sim == "CF_freenon_evo704_nomut_k701_t200010") %>% 
  filter(rel_time %in% t_plot) %>%
  ggplot() + 
  geom_tile(aes(x=x, y=y, fill = lineage)) + 
  theme_void() + 
  facet_wrap(~ rel_time, 
             labeller = labeller(rel_time = labels),
             nrow = 1
             
  ) + 
  scale_fill_manual(name = 'Lineage', 
                    values = c('black',  '#ff5c85ff', '#7ab1c7ff')
  ) + 
  coord_fixed()+ 
  theme(legend.position = 'none', 
        strip.background = element_blank(),
        strip.text.x = element_blank(), 
        panel.spacing = unit(0.01, "lines"))


a <- alldata %>% 
  filter(sim == "CF_freenon_evo704_nomut_k701_t200010") %>% 
  filter(rel_time %in% t_plot) %>%
  ggplot() + 
  geom_tile(aes(x=x, y=y, fill = `grid_concentration_[a.0].csv`)) +
  theme_void() + 
  facet_wrap(~ rel_time, nrow = 1) + 
  theme(legend.position = 'bottom') + 
  scale_fill_continuous_sequential(palette = 'Viridis', rev = FALSE, limits = c(0, 0.1), na.value = 'yellow') +
  guides(fill = guide_colourbar(title = 'Building block 1',  
                                barwidth = 10,
                                title.position = 'left', 
                                direction = 'horizontal')
  ) + 
  coord_fixed()+ 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(), 
        legend.position = 'none', 
        panel.spacing = unit(0.01, "lines"),
        plot.margin = margin(0, 0, 0, 0)
  ) 

e <- alldata %>% 
  filter(sim == "CF_freenon_evo704_nomut_k701_t200010") %>% 
  filter(rel_time %in% t_plot) %>%
  ggplot() + 
  geom_tile(aes(x=x, y=y, fill = `grid_concentration_[e.0].csv`)) +
  theme_void() + 
  coord_fixed() +
  theme(legend.position = 'bottom') + 
  facet_wrap(~ rel_time, nrow = 1) + 
  scale_fill_continuous_sequential(palette = 'Viridis', rev = FALSE, limits = c(0, 0.1), na.value = 'yellow') +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(), 
        legend.position = 'none', 
        panel.spacing = unit(0.01, "lines")
  )


legend_a <- get_legend(
  a + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)


save_plot(filename = 'figures/fig3b_grid_snapshots.png',
          plot_grid(lin, a, e, nrow = 3, rel_heights = c(1,1,1)),
          nrow = 3,
          base_asp = 9,
          base_height = NULL,
          base_width = 40
)




# plot all test for evo704 ------------------------------------------------

for(rep in unique(alldata$sim)) {
  filename <- paste0('figures/temp/fig3b_grid_snapshots', basename(rep), '.png')
  
  lin <- alldata %>% 
    filter(sim == rep) %>% 
    filter(rel_time %in% t_plot) %>%
    ggplot() + 
    geom_tile(aes(x=x, y=y, fill = lineage)) + 
    theme_void() + 
    facet_wrap(~ rel_time, 
               labeller = labeller(rel_time = labels),
               nrow = 1
               
    ) + 
    scale_fill_manual(name = 'Lineage', 
                      values = c('black',  '#ff5c85ff', '#7ab1c7ff')
    ) + 
    coord_fixed()+ 
    theme(legend.position = 'none', 
          strip.background = element_blank(),
          strip.text.x = element_blank(), 
          panel.spacing = unit(0.01, "lines"))
  
  
  a <- alldata %>% 
    filter(sim == rep) %>% 
    filter(rel_time %in% t_plot) %>%
    ggplot() + 
    geom_tile(aes(x=x, y=y, fill = `grid_concentration_[a.0].csv`)) +
    theme_void() + 
    facet_wrap(~ rel_time, nrow = 1) + 
    theme(legend.position = 'bottom') + 
    #scale_fill_continuous_sequential(palette = 'Viridis', rev = FALSE, limits = c(0, 0.1), na.value = 'yellow') +
    scale_fill_continuous_sequential(palette = 'Viridis', rev = FALSE) +
    guides(fill = guide_colourbar(title = 'Building block 1',  
                                  barwidth = 10,
                                  title.position = 'left', 
                                  direction = 'horizontal')
    ) + 
    coord_fixed()+ 
    theme(strip.background = element_blank(),
          strip.text.x = element_blank(), 
          #legend.position = 'none', 
          panel.spacing = unit(0.01, "lines"),
          plot.margin = margin(0, 0, 0, 0)
    ) 
  
  e <- alldata %>% 
    filter(sim == rep) %>% 
    filter(rel_time %in% t_plot) %>%
    ggplot() + 
    geom_tile(aes(x=x, y=y, fill = `grid_concentration_[e.0].csv`)) +
    theme_void() + 
    coord_fixed() +
    theme(legend.position = 'bottom') + 
    facet_wrap(~ rel_time, nrow = 1) + 
    #scale_fill_continuous_sequential(palette = 'Viridis', rev = FALSE, limits = c(0, 0.1), na.value = 'yellow') +
    scale_fill_continuous_sequential(palette = 'Viridis', rev = FALSE) +
    theme(strip.background = element_blank(),
          strip.text.x = element_blank(), 
          #  legend.position = 'none', 
          panel.spacing = unit(0.01, "lines")
    )
  
  
  legend_a <- get_legend(
    a + 
      guides(color = guide_legend(nrow = 1)) +
      theme(legend.position = "bottom")
  )
  
  
  save_plot(filename = filename, 
            plot_grid(lin, a, e, nrow = 3, rel_heights = c(1,1,1)),
            nrow = 3,
            base_asp = 9,
            base_height = NULL,
            base_width = 40
  )
}


