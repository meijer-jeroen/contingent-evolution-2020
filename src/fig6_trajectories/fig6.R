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


# fig. 6.a ----------------------------------------------------------------
formatter1e5 <-  function(x){x/100000}

# selection of simulations
selected <- c(23, 8, 4,9, 18)
selected <- sort(c(selected,1, 2, 17, 20, 21, 35,  48, 50, 58, 60))



pca$population2 <- paste0('Community ', pca$population)
pca$population2 <- factor(pca$population2, levels = unique(pca$population2)) # for facet ordering

traject <- ggplot() +
  
  # grey trajectories of all simulations
  geom_line(data = pca %>%
              filter(time_point %in% seq(0, 1000000, by = 1000)) %>%
              select(-population2),
            aes(x = time_point,
                y = PC1,
                group = sim,
            ),
            col = 'grey',
            alpha = 0.0) +                                                                  # set to 0.5 for trajectory figure

  # add highlighted trajectories
  geom_line(data = pca %>%
              filter(population %in% selected) %>%
               select(-sim),
             aes(x = time_point,
                 y = PC1,
                 group = population2,
                 col = ene_type
             ),
             alpha = 0,                                                                      # set to 1.0 for trajectory figure
             size = 1
            ) +
  
  facet_wrap(~population2, 
             nrow = 5, 
             labeller = labeller(population = "labels")
             ) +
  theme_cowplot(16) +
  panel_border(colour  = 'black') +
  scale_colour_manual(name = 'Network topology',
                      breaks = c('resource', 'building block', 'hybrid'),
                      values = topology_cols, 
                      limits = c('resource', 'building block', 'hybrid'), # change order of legend items
                      labels = c('Resource', 'Building block', 'Hybrid'), # capitals
  ) + 
  theme(
    legend.position = "top",
    legend.justification = "right",
    legend.key.width = unit(1, 'cm'),
    strip.background = element_blank(), 
    strip.text = element_text(hjust = 1), 
    panel.spacing.x = unit(1.75, 'lines'), 
    axis.line = element_blank()
  ) + 
  xlab('time (x10e5 a.u.)') + 
  ylab(paste0('PC1 (', 100 * round(PoV[1], digits = 3), '%)')) + 
  scale_x_continuous(labels = formatter1e5,
                     breaks = seq(0, 1000000, by = 100000),
                     # expand = c(0,0),
                     limits = c(0, 1000000)
                     ) +
  guides(colour = guide_legend(
    override.aes = list(
      size=3
      )
    )
  )  

save_plot("figures/fig6_axis_and_text.pdf", 
          traject, 
          #nrow = 2, 
          base_asp = 1.35, 
          base_height = NULL, 
          base_width = 15)

# Journal needs text in separate layer. Set geom_line alpha to 0.5 and 1.0 to draw trajectories
traject_notext <- traject + 
  theme(axis.line = element_blank(), 
        strip.text = element_text(hjust = 1, colour = 'white'), 
        axis.text = element_text(colour = 'white'),
        axis.title = element_text(colour = 'white'),
        legend.text = element_text(colour = 'white'),
        legend.title = element_text(colour = 'white') 
        )


save_plot("figures/fig6_notext.png", 
          traject_notext, 
          #nrow = 2, 
          base_asp = 1.35, 
          base_height = NULL, 
          base_width = 15)
