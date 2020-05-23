library(tidyverse)
library(cowplot)
library(colorspace)
library(ggrepel)

# read data ---------------------------------------------------------------
mydata <- readRDS('data/temp/fig3_productivity.RDS')
mydata <- select(mydata, -seed)
mydata <- mutate(mydata, 
                 seed = gsub('CF|fix|DI|freenon|_evo|nomut||\\_k.*|\\_', '', as.character(sim)),
                 killed = paste0(seed, '_', gsub('CF|fix|freenon|DI|evo[0-9]*|nomut|\\_t[0-9]*|k|\\_', '', sim)),   
                 test_time = round(as.numeric(gsub('.*\\_t|\\/data\\/.*', '', sim)), digits = -4),
                 rel_time = time_point - test_time
)
region_cols <- c('#dc8290ff', '#76b5beff', '#696969')

mydata <- mydata %>% 
  mutate(
    lin = (as.numeric(as.factor(killed))%%2)
  )

mydata <- mydata %>%
  mutate(
    regcol = ifelse(rel_time == 0, 'grey', lin)
  )

# community production rate -----------------------------------------------
com_labels <- c('704_701' = "Major lineage removed", "704_894" = "Minor lineage removed")

community_prod_rate <- mydata %>% 
  filter(seed == "704") %>% 
  group_by(sim) %>% 
  filter(rel_time %in% c(1500, 0)) %>%  
  ggplot(aes(x=as.factor(rel_time), y = pop_size * prod_rate , group=sim)) + 
  geom_line(col = 'grey', alpha = 0.7) +
  geom_point(aes(col = regcol, fill = regcol), size = 4, alpha = 0.8, shape = 21, col = 'white')  +
  theme_cowplot(12) + 
  scale_x_discrete(
    name = '', 
    labels = c('Cross-feeding\ncommunity', 'Surviving\nlineage'),
    expand= expand_scale(mult = c(0.4, .4))
  ) +
  scale_y_continuous(
    name = "Community production rate",
    limits = c(3, 7),
    breaks = c( 4,5,6, 7),
    expand = c(0, 0)
  ) + 
  scale_color_manual(
    name = NULL,
    values = darken(region_cols, 0.3)
  ) +
  scale_fill_manual(
    name = NULL,
    values = region_cols, 
    labels = c('Major lineage', 'Minor lineage', 'Community of both lineages')
  ) + 
  guides(
    color = guide_legend(
      override.aes = list(
        linetype = c(rep(0, 5), 1),
        shape = c(rep(21, 5), NA)
      )
    )
  ) +
  facet_wrap(~killed, ncol = 2, 
             labeller = labeller(killed = com_labels)
             ) +
  theme(
    legend.position = "none",
    strip.background = NULL,
    plot.title = element_text(hjust = 0.5),
    panel.spacing = unit(2, "lines"),
    axis.line = element_blank()
  ) +
  panel_border(color = 'black')


# differences in all communities ------------------------------------------

library(ggbeeswarm)
library(colorspace)
dif_cols <- c('black','#dc8290ff','#76b5beff')

all_change <- mydata %>% 
  filter(rel_time %in% c(2000, 0)) %>%
  select(sim, rel_time, killed, prod_rate, pop_size, seed) %>% 
  #mutate(comprod = pop_size * prod_rate) %>% 
  pivot_wider(names_from =rel_time, values_from = c(prod_rate, pop_size)) %>% 
  mutate(prod_change = prod_rate_2000 * pop_size_2000 - prod_rate_0 * pop_size_0) %>% 
  mutate(marked = case_when(seed == '704' & killed == '704_701' ~ 'minor survives', 
                            seed == '704' & killed == '704_894' ~ 'major survives', 
                            TRUE ~ 'a')
  ) %>% 
  arrange(marked) %>% 
  
  
  ggplot() + 
  geom_hline(yintercept = 0, col = 'grey', alpha = 0.7) +
  geom_beeswarm(aes(x = 0, 
                    y = prod_change, 
                    # col = marked, 
                    col = marked), 
                size = 1, alpha = 1
  ) +
  scale_colour_manual(values = lighten(dif_cols, 0.1, space = 'HLS')) +
  #scale_colour_manual(values = darken(region_cols, 0.4, space = 'HCL')) +
  xlab('') +
  ylab(expression(Delta*" Community production rate")) +
  theme_cowplot(12) + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.line = element_blank(),
        legend.position = 'none',
        plot.title = element_text(hjust = 0.5)
        
  ) + 
  panel_border(colour = 'black') + 
  scale_x_discrete(name = '', 
                   expand= expand_scale(mult = c(0.2, .2)
                   )
  )

comp <- plot_grid(community_prod_rate, 
                  all_change, 
                  labels = c('d', 'e'), 
                  align = 'h', 
                  axis = 'trb', 
                  rel_widths = c(3,1.7)
                  )


save_plot("figures/fig3d.pdf", 
          comp,
          nrow = 1, 
          base_asp = 2.2, 
          base_height = NULL, 
          base_width = 8
)



# no. of cross-feeding communities tested: 29
length(unique(mydata$seed))

# no. of total tests: 484
length(unique(mydata$sim))


# no. of surviving lineages: 412
mydata %>% 
  filter(rel_time == 2000) %>% 
  nrow(.)

# Survival rate: 85.12397 %
100* (1 - (484 - 412)  / 484)

# no. of INCREASED production rate surviving lineages: 5 (1.213582 %)
mydata %>% 
  filter(rel_time %in% c(2000, 0)) %>%
  select(sim, rel_time, killed, prod_rate, pop_size, seed) %>% 
  #mutate(comprod = pop_size * prod_rate) %>% 
  pivot_wider(names_from =rel_time, values_from = c(prod_rate, pop_size)) %>% 
  mutate(prod_change = prod_rate_2000 * pop_size_2000 - prod_rate_0 * pop_size_0) %>% 
  filter(prod_change > 0) %>% 
  nrow(.)

100 * 5/484


# Supplementary Figure 2 -------------------------------------------------

SM_prodrates <- mydata %>%
  group_by(sim) %>%
  filter(rel_time %in% c(2000, 0)) %>%
  ggplot(aes(x=as.factor(rel_time), y = pop_size * prod_rate , group=sim)) +
  geom_line(col = 'grey', alpha =0.3) +
  geom_point(aes(col = regcol, fill = regcol), size = 2, alpha = 0.8, shape = 21, col = 'white')  +
  scale_x_discrete(
    name = '',
    labels = c('crossfeeding\ncommunity\n(before\nremoval)', 'surviving\n lineage\n(after\nremoval)'),
    expand= expand_scale(mult = c(0.4, .4))
  ) +

  scale_y_continuous(
    name = "Community productivity",
    limits = c(0, 7),
    #breaks = c(3, 4,5,6, 7),
    expand = c(0, 0)
  ) +
  facet_wrap(~population, nrow = 6) +

  scale_color_manual(
    name = NULL,
    values = darken(region_cols, 0.3)
  ) +
  scale_fill_manual(
    name = NULL,
    values = region_cols,
    labels = c('Lineage 1', 'Lineage 2', 'Community of both lineages')
  ) +

 guides(
  fill = guide_legend(
    nrow = 1
   )
) +
  theme_cowplot(12) +
  theme(
    strip.text = element_text(size = 12, margin = margin(0, 0, 6, 0, "pt")),
    legend.position = "top",
    legend.justification = 'right',
    legend.text = element_text(size =12),
 
    panel.spacing.x = unit(1.0, 'lines'),
    panel.spacing.y = unit(1.5, 'lines'),
    strip.background = element_blank()
  ) +
  panel_border()



save_plot("figures/supp_fig_2_prodrates.pdf",
          SM_prodrates,
          nrow = 1,
          base_asp = 0.8,
          base_height = NULL,
          base_width = 10
)