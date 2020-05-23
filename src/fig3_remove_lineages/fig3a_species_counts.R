library(tidyverse)
source('src/utility/readSpeciesCounts.R')
library(cowplot)

tmax = 1000000
sq = seq(0, tmax, by = 500)
t_plot <- seq(100000, 1000000, by = 100000) 


sims <- list.dirs('data/raw/main_experiment/',
                  recursive = FALSE,
                  full.names = TRUE)
sims <- grep('CF_freenon_evo704', sims, value = T)


sp <- readSpeciesCounts(sims)
sp$lineage <-factor(sp$lineage, levels = (-1:45^2))

# set lineage colours
lin_col <- c("#000000FF", sample(rainbow(45*45+2)))

surviving_lineages <- sp %>%
  filter(time_point == max(time_point)) %>%
  filter(fraction >0 ) %>% 
  select(lineage) %>%
  distinct(.) %>%
  unlist(.) %>%
  as.character(.) %>%
  sort(.)
surviving_lineages <- as.numeric(surviving_lineages) + 2
lin_col[surviving_lineages] <- c('#dc8290ff', '#76b5beff')


myplot <- sp %>%
  filter(time_point %in% sq) %>%
  ggplot(aes(x = time_point)) +
  geom_line(aes(y = fraction, col = lineage), size = 1) +
  theme_classic() +
  theme(
    legend.position = 'none',
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 12, hjust = 0),
    axis.title.y = element_text(size = 12)
  ) +
  ylab('Frequency') +
  xlab(expression(paste('Time (', 10 ^ {
    5
  }, ' a.u.t.)'))) +
  scale_x_continuous(
    limits = c(0, tmax),
    expand = c(0, 0),
    breaks = c(0, t_plot),
    labels = function(x)
      x / 100000
  ) +
  scale_y_continuous(
    limits = c(0.001, 1.0),
    breaks = scales::pretty_breaks(n = 2), 
    expand = c(0,0)
  ) +
  scale_colour_manual(values = lin_col, drop = FALSE) 

myplot <- myplot + geom_vline(xintercept = t_plot, linetype = 'dashed') +
  theme(
    plot.margin = margin(1,1,1,1, unit = 'lines')
  )

save_plot("figures/fig3a_speciescounts_wide.png", 
          myplot,
          nrow = .3,
          base_asp = 2.2,
          base_height = NULL,
          base_width = 15.4
)


