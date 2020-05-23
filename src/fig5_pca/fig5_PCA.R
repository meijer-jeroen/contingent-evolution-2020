
# make pca ----------------------------------------------------------------
library(colorspace)
library('tidyverse')
library('ggrepel')
library('cowplot')
load('data/temp/mgx_commsbio2020.rdata') # first run read_save_mgx.R to collect data
colnames(mgx)[5] <- 'cur_commtype'

# remove two outlier populations. See SI5_PCA_all_communties.R for analysis including outliers.
for_pca <- filter(mgx, seed != 906)
for_pca <- filter(for_pca, seed != 706)

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

nooutlier <- ggplot(pca) +
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


# function to plot trace
plot_trace <- function(selected_sim){
  selected_sim <- enquo(selected_sim)
  
  trace <- ggplot() + 
    geom_point(
      data =  pca %>%
        group_by(seed) %>%
        mutate(time_max = max(time_point)) %>%
        ungroup()  %>%
        filter(time_point == time_max),
      aes(x = PC1, y = PC2),
      #col = 'white',
      size = 4,
      #shape = 21,
      alpha = 0
    )  +
    
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
    panel_border(color = 'black') +
  
    coord_fixed() +
    
    geom_point(data = pca %>%
                filter(seed == !!selected_sim) %>%
                filter(time_point %in% seq(0, 1000000, by = 2500)) %>%
                select(-sim),
              aes(x = PC1,
                  y = PC2,
                  group = seed,
                  col = time_point/100000),
              #col = 'grey',
              size = .4,
              alpha = 1.0) +
    
    geom_path(data = pca %>%
                filter(seed == !!selected_sim) %>%
                #filter(seed %in% sample(unique(pca$seed), 6)) %>%
                filter(time_point %in% seq(0, 1000000, by = 2500)) %>%
                select(-sim),
              aes(x = PC1,
                  y = PC2,
                  group = seed,
                  col = time_point/100000),
              #col = 'grey',
              size = 1,
              alpha = 1.0) +
    # add start marker
    geom_point(
      data = filter(pca,
                    seed == '101',
                    time_point == 0) %>%
        select(-seed),
      aes(x = PC1, y = PC2),
      size = 4,
      shape = 18,
      colour = 'black'
    ) +
    # endpoint of trace
    geom_point(
      data = pca %>%
        filter(seed ==!!selected_sim) %>%
        group_by(seed) %>%
        mutate(time_max = max(time_point)) %>%
        ungroup()  %>%
        filter(time_point == time_max)  %>%
        select(-sim),
      aes(x = PC1, y = PC2),
      size = 4,
      shape = 21,
      colour = 'black',
      fill = 'black'
    )  +
    scale_color_gradient2(
      low = 'blue',
      mid = 'red',
      high = 'yellow',
      midpoint = 5,
      breaks = seq(0, 10, by = 2)
    ) + 
    theme_map() + 
    xlab('') + 
    ylab('')

  aligned_plots <- align_plots(nooutlier + theme(legend.position = 'none'), trace+ theme(legend.position = 'none'), align = 'hv', axis = 'tblr')
  with_trace <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])
  return(with_trace)
}

# make plots

no <- nooutlier + theme(legend.position = 'none') + 
  theme(plot.title = element_text(hjust=0.5)) 

no <- plot_trace(NA)

cfa <- plot_trace(203) + 
  guides(fill = guide_legend(title = 'Community type', title.position = 'left', nrow = 1, order = 1), 
         colour = guide_colourbar(title = 'Time (x10e5 a.u.)', title.position = 'left', order =2))

cf <- cfa + 
  theme(legend.position = 'none') + 
  ylab(NULL) + 
  xlab(NULL) + 
  theme(plot.margin = margin(0, 0, 0, 0))


au <- plot_trace(104) +
  theme(legend.position = 'none') +
  ylab(NULL) + 
  xlab(NULL)+ 
  theme(plot.margin = margin(6, 0, 25, 0))

sw2 <- plot_trace(305) + 
  theme(legend.position = 'none') + 
  ylab(NULL) + 
  xlab(NULL)+ 
  theme(plot.margin = margin(6, 0, 6, 0))

# extract legend
legend <- get_legend(
  nooutlier + theme(legend.box.margin = margin(0, 0, 0, 40), 
              legend.position = 'bottom')
)

comp <- plot_grid(no, cf, au, sw2,
          ncol = 4, 
          align = 'vh', 
          axis = 'l', 
          labels = c("A", "B", "C", "D"),
          vjust = 4.5

)

for_save <- plot_grid(comp, legend, ncol = 1, rel_heights = c(1,.1), axis = 'l', align = 'v')

save_plot("figures/fig5.pdf", 
          for_save, 
          nrow = 1, 
          base_asp = 2.5, 
          base_height = NULL, 
          base_width = 16)



  

