library('tidyverse')
library('cowplot')
load('../data/temp/mgx_commsbio2020.rdata') # first run read_save_mgx.R to collect data

community_cols <- c('gold4', "#0072B2")
colnames(mgx)[5] <- 'cur_commtype'
label_cols <- c('time_point',
                'sim',
                'seed',
                'cur_commtype',
                'final_commtype',
                'population',
                'ene_type',
                'ene_react')

labels <- select(mgx, label_cols)
for_pca <- select(mgx, -label_cols)

var0 <- unlist(lapply(for_pca, function(x)
  0 == var(if (is.factor(x))
    as.integer(x)
    else
      x)))

for_pca <- for_pca[, !var0]

pca <- prcomp(for_pca, center = TRUE, scale = TRUE)
PoV <- pca$sdev ^ 2 / sum(pca$sdev ^ 2)

# re-attach labels
pca <- cbind(labels, pca$x[, c(1:3)])

# make the plot
all <- ggplot(pca) +
  geom_point(
    data =  pca %>%
      group_by(seed) %>%
      mutate(time_max = max(time_point)) %>% 
      ungroup()  %>%
      filter(time_point == time_max),
    aes(x = PC1, y = PC2, fill = final_commtype),
    col = 'white',
    size = 2.5,
    shape = 21,
    alpha = 0.8
  )  +
  scale_fill_manual(name = NULL,
                    values = community_cols)  +
  
  # add start marker
  geom_point(
    data = filter(pca,
                  time_point == 0) %>% 
      filter(seed == unique(mgx$seed[1])),
    aes(x = PC1, y = PC2),
    size = 2,
    shape = 21,
    colour = 'black',
    fill = 'white'
  ) +
  theme_bw() + 
  ggtitle('Supplementary Figure 5. \nPCA of metabolic networks, including \n2 outlier communties not shown in Fig. 5.a \nof the main text')  + 
  xlab(paste0('PCA (', 100 * round(PoV[1], digits = 4), '%)')) +
  ylab(paste0('PCA (', 100 * round(PoV[2], digits = 4), '%)')) +
  coord_fixed()


save_plot("../results/figures/supplementary_figure_5.pdf", 
          all, 
          #nrow = 2, 
          #base_asp = 3.5, 
          base_height = NULL, 
          base_width = 8)


