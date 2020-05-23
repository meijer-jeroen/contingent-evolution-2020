# Simulations plotted in figure 2.
# 23 8 54 || evo 404 202 901
# in SI6 33 9 || 504 203
setwd('~/commsbio2020/src/wip')

set.seed(102)

library(magick)
library(cowplot)
source('WIP_NLM_functions.R')

# set parameters ----------------------------------------------------------
# folders <- list.dirs('../../data/raw/', 
#                      recursive = FALSE, 
#                      full.names = TRUE)
# folders <- grep('404|202|901', folders, value = TRUE)
#folder <- folders[2]
gridwidth <- 45
tmax <- 1000000
stepsize <- 10000                                             # Reduce read interval for grid lineage data. Every 5000 time steps is reasonable. Cannot go lower than lineage_X.csv save interval.

# Set time points to plot snapshots of grid  ----------------------------------
roundDown <- function(x, accuracy, f = floor){f(x / accuracy) * accuracy}
t_plot <- roundDown(seq((tmax / 10), tmax, length = 10), 
                    stepsize
                    )

sq <- seq(0, tmax, by = 500)                                  # for subsampling lineage marker frequency plot



au_plots <-plotSim('../../data/temp/CF_fixDI_freenon_evo404df_prot.RDS', 
                   '../../data/temp/CF_fixDI_freenon_evo404speciescounts.RDS'
)

# Play around with lineage markers until they look good
set.seed(3)
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



cf_plots <-plotSim('../../data/temp/CF_fixDI_freenon_evo202df_prot.RDS', 
                   '../../data/temp/CF_fixDI_freenon_evo202speciescounts.RDS', 
                   cols = c('#00CCFF', '#FF00a5')
                   #'#30C2FF', '#ff00a5ff'
)

sw_plots <-plotSim('../../data/temp/CF_fixDI_freenon_evo901df_prot.RDS', 
                   '../../data/temp/CF_fixDI_freenon_evo901speciescounts.RDS'
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



#tol21rainbow= c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")



au_plots$title <- ggdraw() + 
  draw_label(
    " Metabolically autonomous strategy (AU) 21/60",
    fontface = 'plain', 
    x = 0.5, 
    hjust = 0.5, 
    size = 20
  ) +
  theme(
    plot.margin = margin(0,0,30,30)
  )

cf_plots$title <- ggdraw() + 
  draw_label(
    " Crossfeeding strategy (CF) 24/60",
    fontface = 'plain', 
    x = 0.5, 
    hjust = 0.5, 
    size = 20
  ) +
  theme(
    plot.margin = margin(0,0,30,30)
  )

sw_plots$title <- ggdraw() + 
  draw_label(
    " Switching between autonomous/crossfeeding strategies (SW) 15/60",
    fontface = 'plain', 
    x = 0.5, 
    hjust = 0.5, 
    size = 20
  ) +
  theme(
    plot.margin = margin(0,0,30,30)
  )

muller_placeholder <- ggplot() + 
  ggtitle("Muller diagram of metabolic genotypes") + 
  theme_cowplot(12)   
#   theme_classic() +
#   theme(
#     legend.position = 'none',
#     axis.text.x = element_text(size = 15),
#     axis.text.y = element_text(size = 15),
#     axis.title.x = element_text(size = 12),
#     axis.title.y = element_text(size = 12)
#   ) +
#   ylab('Frequency') +
#   xlab(expression(paste('Time (', 10 ^ {
#     5
#   }, ' a.u.t.)'))) +
#   scale_x_continuous(
#     limits = c(0, tmax),
#     expand = c(0, 0),
#     breaks = c(0, t_plot),
#     labels = function(x)
#       x / 100000
#   ) +
#   scale_y_continuous(
#     limits = c(0.001, 1.0),
#     breaks = scales::pretty_breaks(n = 2), 
#     expand = c(0,0)
#   ) +
#   
#   theme(plot.margin = margin(0, 1, 0, 0, unit = 'lines'))
#align_test <- align_plots(muller_placeholder, au_muller, align = 'hv', axis = 'tblr')
#au_plots$muller <- ggdraw(align_test[[1]]) + draw_plot(align_test[[2]])

au_plots$muller <- ggdraw() + draw_image(image_read('404.png')) +
  draw_plot(muller_placeholder) +
  theme(plot.margin = margin(0, 1, 0, 0, unit = 'lines'))

au_plots$lineage <- au_plots$lineage +  theme(plot.margin = margin(1.4, 1, 0, 0, unit = 'lines'))

sw_plots$muller <- ggdraw() + draw_image(image_read('901.png'))
cf_plots$muller <- ggdraw() + draw_image(image_read('202.png'))

# combine subplots --------------------------------------------------------

muller <- ggdraw() + draw_image(image_read('404.png')) +
  theme(plot.margin = margin(0, 1, 0, 0, unit = 'lines'))

plots <- align_plots(au_plots$title,
                     muller_placeholder,
                     muller,
                     au_plots$lineage,
                     au_plots$grid,
                     au_plots$pca,
                     align = 'v', 
                     axis = 'tblr')

muller_aligned <- plot_grid(plots[[2]], plots[[3]], labels = 'A', label size = 12, ncol =1 )

comp <- plot_grid(plots[[1]],
                  muller_aligned, 
                  #plots[[2]],
                  #plots[[3]],
                  plots[[4]],
                  plots[[5]],
                  plots[[6]],
                  ncol = 1, 
                  rel_heights = c(0.25, 2., 2., 2., 2.), 
                  #label_x = c(0,0,0,0,0),
                  #hjust = c(-0.2), 
                  #vjust = c(3.50, 2.5, 2.5)
                  labels = c("", "A", "B", "C", "D"
                          )
                  
)


save_plot("muller_test.pdf", comp,
          nrow = length(plots),
          base_asp = 10,
          base_height = NULL,
          base_width = 20
)


plots <- align_plots(au_plots$title,
                     au_plots$muller,
                     au_plots$lineage,
                     au_plots$grid,
                     au_plots$pca,
                     cf_plots$title, 
                     cf_plots$muller,
                     cf_plots$lineage,
                     cf_plots$grid,
                     cf_plots$pca,
                     sw_plots$title, 
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
                  plots[[5]],
                  plots[[6]],
                  plots[[7]],
                  plots[[8]],
                  plots[[9]],
                  plots[[10]],
                  plots[[11]],
                  plots[[12]],
                  plots[[13]],
                  plots[[14]],
                  plots[[15]],
                  ncol = 1, 
                  rel_heights = c(0.25,1,1,1,1, 0.25,1,1,1,1, 0.25,1,1,1,1), 
                  #label_x = c(0,0,0,0,0),
                  #hjust = c(-0.2), 
                  #vjust = c(3.50, 2.5, 2.5)
                  labels = c("", "A", "B", "C", "D", 
                             "", "E", "F",  "G", "H",
                             "", "I", "J", "K", "L" )
                  
)




save_plot("fig2_combined2.pdf", comp,
          nrow = 15,
          base_asp = 8,
          base_height = NULL,
          base_width = 20
)
# 
# # plot external metabolite concentrations ---------------------------------
# metab_plot <- read_and_plot_external_metabolites(folder)
# metab_plot <- metab_plot +
#   scale_x_continuous(
#     limits = c(0, tmax),
#     expand = expand_scale(mult = 0.01),
#     breaks = t_plot,
#     #breaks = scales::pretty_breaks(n = 10),
#     labels = function(x)
#       x / 100000
#   )
# 
# 
