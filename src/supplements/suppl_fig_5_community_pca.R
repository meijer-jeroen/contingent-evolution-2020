
# make pca ----------------------------------------------------------------
library(colorspace)
library('tidyverse')
library('ggrepel')
library('cowplot')
load('data/temp/fig4.rdata') # first run fig4_heatmap/fig4_read_data.R

# remove two outlier populations. See SI5_PCA_all_communties.R for analysis including outliers.
#for_pca <- filter(mgx, seed != 906)
#for_pca <- filter(for_pca, seed != 706)
for_pca <- mgx

label_cols <-
  c('time_point',
    'sim',
    'seed',
    'population',
    'cur_commtype', # community type at current time point
    'final_commtype', 
    'ene_type', # topology of metabolic network, estimated by relative frequencies in population
    'ene_react') # most common energy reaction in community

labels <- select(for_pca, label_cols)
for_pca <- select(for_pca, -label_cols)

var0 <-
  unlist(lapply(for_pca, function(x)
    0 == var(if (is.factor(x))
      as.integer(x)
      else
        x)))
for_pca <- for_pca[, !var0]

pca <- prcomp(for_pca, center = TRUE, scale = TRUE)
PoV <- pca$sdev ^ 2 / sum(pca$sdev ^ 2)
pca <- cbind(labels, pca$x[, c(1:2)]) # only save PC1 and PC2

community_cols <- c('gold4', "#0072B2")
topology_cols <- c("#56B4E9","#F0E442", "#009E73","#999999")


# Plot Figure 5.a-d         -----------------------------------------------

selected <- c(104, 203, 701)

fig <- ggplot(pca) +
  # data coloured by sim
  geom_point(
    data =  pca %>%
      group_by(seed) %>%
      mutate(time_max = max(time_point)) %>%
      ungroup()  %>%
      filter(time_point == time_max),
    aes(x = PC1, y = PC2, shape = final_commtype, fill = ene_type, col = ene_type),
    size = 4,
    alpha = 0.55
  )  +
  scale_shape_manual(name = 'Community type',
                     values = c(24, 21, 22)) +

  scale_colour_manual(name = 'Network topology',
                    breaks = c('resource', 'building block', 'hybrid'),
                    values = darken(topology_cols, amount = 0.3, space = "HCL"), 
                    limits = c('resource', 'building block', 'hybrid'), # change order of legend items
                    labels = c('Resource', 'Building block', 'Hybrid') # capitals
  ) +
  scale_fill_manual(name = 'Network topology',
                    breaks = c('resource', 'building block', 'hybrid'),
                    values = (topology_cols),
                    limits = c('resource', 'building block', 'hybrid'), # change order of legend items
                    labels = c('Resource', 'Building block', 'Hybrid') # capitals
  ) + 
  
  # add start marker
  geom_text_repel(data = filter(pca,
                                time_point == 0 & seed == 101),
                  aes(x = PC1, y = PC2),
                  label = 'start',
                  nudge_x = -.7, 
                  nudge_y = .3)  +
  geom_point(
    data = filter(pca,
                  time_point == 0) %>% 
      filter(seed == unique(pca$seed[1])),
    aes(x = PC1, y = PC2),
    size = 4,
    shape = 18,
    colour = 'black'
  ) +
  
  #mark outliers
  geom_text_repel(data = filter(pca,
                                time_point == 1000000 & seed %in% c(906,706)
                                ),
                  aes(x = PC1, y = PC2, label = population), 
                  nudge_x = -0.5, 
                  nudge_y = -0.5
                 ) +
  
  theme_minimal_grid() + 
  ggtitle('') + 
  xlab(paste0('PC1 (', 100 * round(PoV[1], digits = 4), '%)')) +
  ylab(paste0('PC2 (', 100 * round(PoV[2], digits = 4), '%)')) +
  coord_fixed() +
  
  # Add invisible line to keep axis identical to figure bcd
  geom_path(data = pca %>%
              filter(seed %in% selected) %>%
              filter(time_point %in% seq(0, 1000000, by = 2500)) %>%
              select(-sim),
            aes(x = PC1,
                y = PC2,
                group = seed),
            alpha = 0
            ) + 
  panel_border(color = 'black') 


save_plot("figures/supplement_5_pca.pdf", 
          fig, 
          nrow = 1, 
          base_asp = 1, 
          base_height = NULL, 
          base_width = 8)



  

