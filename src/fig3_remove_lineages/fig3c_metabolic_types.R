# lineage shades
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

#alldata$shadetype_n <- as.numeric(factor(alldata$shadetype, levels = levels))
alldata$shadetype_n <-factor(alldata$shadetype, levels = levels)


# area plot ---------------------------------------------------------------
for_plot2 <- alldata %>% 
  filter(lineage != -1) %>% 
  group_by(sim, rel_time, shadetype_n, killed, test_time, lineage)%>% 
  summarize(count = n()) 
#  complete(rel_time, shadetype_n, fill = list(count = 0)) 

# extend t = 0 for visibility
padding <- filter(for_plot2, rel_time == 0)
padding <- padding %>% 
  ungroup() %>% 
  mutate(rel_time = -200)

for_plot2 <- rbind(as.data.frame(padding), as.data.frame(for_plot2))


#check how many colours we need to create for the shade types
lin_a <- as.character(sort(unique(for_plot2$lineage))[1])
lin_b <- as.character(sort(unique(for_plot2$lineage))[2])
no_shades_a <- filter(for_plot2, lineage == lin_a) %>% select(shadetype_n) %>% distinct(.) %>% nrow(.)
no_shades_b <- filter(for_plot2, lineage == lin_b) %>% select(shadetype_n) %>% distinct(.) %>% nrow(.)



set.seed(12)
  
cols <- c('black', 
          sample(sequential_hcl(no_shades_a + 2, 'Burg'))[-c((no_shades_a + 1) : (no_shades_a + 2))], 
          sample(sequential_hcl(no_shades_b + 2, 'Teal'))[-c((no_shades_b + 1): (no_shades_b + 2))]
          # sequential_hcl(no_shades_a + 2, 'Magenta')[-c((no_shades_a + 1) : (no_shades_a + 2))], 
          # sequential_hcl(no_shades_b + 2, 'Blues 3')[-c((no_shades_b + 1): (no_shades_b + 2))]
)

shadefig <- for_plot2 %>%
  ggplot() +
  geom_area(aes(x = rel_time, y = count, fill = shadetype_n),
  ) +
  scale_fill_manual(values = cols, drop = FALSE)  +
  facet_grid(#killed ~test_time,
    test_time ~killed,
    labeller = labeller(
      killed = c('704_701' = 'Major lineage removed',
                 '704_894' = 'Minor lineage removed'
      )
    )
  ) +
  theme_cowplot(10) +
  theme(
    strip.background = element_rect(colour = 'white', fill = 'white'),
    legend.box = 'vertical',
    legend.position = 'top',
    panel.spacing.x = unit(1.75, 'lines'),
    panel.spacing.y = unit(.7, 'lines'),
    strip.text = element_text(face = 'bold'),
    strip.text.y = element_text(angle = 0)
  ) +
  xlab('time relative to removing lineage') +
  ylab('population size') +
  guides(fill = guide_legend(title = 'metabolic genotype',
                             title.position = 'left', nrow = 1, order = 1)
  ) +

  #geom_vline(color = 'white', xintercept = 0) +
  scale_x_continuous(
    limits = c(-200, 2000),
    expand = c(0, 0),
    breaks = c(0,500,1000,2000)
  ) +
  scale_y_continuous(
    limits = c(0, 2000),
    breaks = scales::pretty_breaks(n = 2),
    expand = c(0,0)
  ) +
  theme(legend.position = 'none')

#saveRDS(shadefig, file = 'data/temp/fig3_shadefig.RDS') # for composition

save_plot(paste0("figures/fig3c_metabolic_types.png"),
          shadefig,
          nrow = 1,
          base_asp = .8,
          base_height = NULL,
          base_width = 7
)




