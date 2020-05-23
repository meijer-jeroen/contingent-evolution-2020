# simulations shown in figure 2: population 23, 8, 54 (evo seed 404 202 901)

library(magick)
library(cowplot)
source('src/fig2_evolutionary_trajectories/fig2_functions.R')
source('src/utility/linColRainbow.R')

# AU community ------------------------------------------------------------
au_plots <-plotSim('data/temp/CF_fixDI_freenon_evo404df_prot.RDS', 
                   'data/temp/CF_fixDI_freenon_evo404speciescounts.RDS'
)

set.seed(3) # Try different lineage marker colours until they look good
col <- c("#000000FF", sample(rainbow(gridwidth^2 + 2)))

au_plots$grid <- au_plots$grid + 
  scale_fill_manual(values = col , 
                    drop = FALSE)
au_plots$lineage <- au_plots$lineage + 
  scale_colour_manual(values = col , 
                      drop = FALSE)
au_plots$pca <- au_plots$pca + 
  scale_colour_manual(values = col , 
                      drop = FALSE)

au_plots$title <- ggdraw() + 
  draw_label(
    " Metabolically autonomous strategy (21/60 communities)",
    fontface = 'bold', 
    x = 0.5, 
    hjust = 0.5, 
    size = 24
  ) +
  theme(
    plot.margin = margin(0,0,0,30)
  )

au_plots$muller <- ggdraw() + draw_image(image_read('figures/muller_404.png')) +
  theme(plot.margin = margin(0, 1, 0, 0, unit = 'lines'))

plots <- align_plots(#au_plots$title,
                     au_plots$muller,
                     au_plots$lineage,
                     au_plots$grid,
                     au_plots$pca,
                     align = 'v',
                     axis = 'l')

comp <- plot_grid(plots[[1]],
                  plots[[2]],
                  plots[[3]],
                  plots[[4]],
                  #plots[[5]],
                  ncol = 1,
                  #labels = c("", "a", "b", "c", "d"),
                  rel_heights = c(#0.3, 
                                  2., 
                                  2.5, 
                                  2., 
                                  2.)

)

save_plot("figures/fig2_au.png", comp,
          nrow = length(plots),
          base_asp = 9,
          base_height = NULL,
          base_width = 20
)


# CF community ------------------------------------------------------------

rm(au_plots, comp, plots) # save memory
cf_plots <-plotSim('data/temp/CF_fixDI_freenon_evo202df_prot.RDS', 
                   'data/temp/CF_fixDI_freenon_evo202speciescounts.RDS', 
                   cols = c('#00CCFF', '#FF00a5')
                   #'#30C2FF', '#ff00a5ff'
)


cf_plots$title <- ggdraw() + 
  draw_label(
    " Cross-feeding strategy (24/60 communities)",
    fontface = 'bold', 
    x = 0.5, 
    hjust = 0.5, 
    size = 24
  ) +
  theme(
    plot.margin = margin(0,0,0,30)
  )

cf_plots$muller <- ggdraw() + draw_image(image_read('figures/muller_202.png'))+
  theme(plot.margin = margin(0, 1, 0, 0, unit = 'lines'))

plots <- align_plots(#cf_plots$title,
                     cf_plots$muller,
                     cf_plots$lineage,
                     cf_plots$grid,
                     cf_plots$pca,
                     align = 'v',
                     axis = 'l')

comp <- plot_grid(plots[[1]],
                  plots[[2]],
                  plots[[3]],
                  plots[[4]],
                  #plots[[5]],
                  ncol = 1,
                  #labels = c("", "e", "f", "g", "h"),
                  #rel_heights = c(0.3, 2., 2.2, 2., 2.)
                  rel_heights = c(
                    2., 
                    2.5, 
                    2., 
                    2.)
                  )

save_plot("figures/fig2_cf.png", comp,
          nrow = length(plots),
          base_asp = 9,
          base_height = NULL,
          base_width = 20
)


# SW community ------------------------------------------------------------
rm(cf_plots, comp, plots)
sw_plots <-plotSim('data/temp/CF_fixDI_freenon_evo901df_prot.RDS', 
                   'data/temp/CF_fixDI_freenon_evo901speciescounts.RDS'
)

set.seed(3) # other options: 15 16 23 28
col <- c("#000000FF", sample(rainbow(gridwidth^2 + 2)))

sw_plots$grid <- sw_plots$grid + 
  scale_fill_manual(values = col , 
                    drop = FALSE)

sw_plots$lineage <- sw_plots$lineage + 
  scale_colour_manual(values = col , 
                      drop = FALSE)
sw_plots$pca <- sw_plots$pca + 
  scale_colour_manual(values = col , 
                      drop = FALSE)

sw_plots$title <- ggdraw() + 
  draw_label(
    " Switching between strategies (15/60 communities)",
    fontface = 'bold', 
    x = 0.5, 
    hjust = 0.5, 
    size = 24
  ) +
  theme(
    plot.margin = margin(0,0,0,30)
  )

sw_plots$muller <- ggdraw() + draw_image(image_read('figures/muller_901.png'))+
  theme(plot.margin = margin(0, 1, 0, 0, unit = 'lines'))

plots <- align_plots(#sw_plots$title,
                     sw_plots$muller,
                     sw_plots$lineage,
                     sw_plots$grid,
                     sw_plots$pca,
                     align = 'v',
                     axis = 'l')


comp <- plot_grid(plots[[1]],
                  plots[[2]],
                  plots[[3]],
                  plots[[4]],
                 # plots[[5]],
                  ncol = 1,
                  rel_heights = c(
                   2., 
                   2.5, 
                   2., 
                   2.)
)

save_plot("figures/fig2_sw.png", comp,
          nrow = length(plots),
          base_asp = 9,
          base_height = NULL,
          base_width = 20
)
