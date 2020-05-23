# fig3 cellular productivities violin plots
# first process data as in fig3_lineage_shades.R

# plot cellular productivity ----------------------------------------------
library(ggbeeswarm)
alldata %>% 
  filter(rel_time %in% c(0,1500)) %>% 
  filter(lineage != -1) %>% 
  ggplot(aes(x = lineage, y = production_rate)) + 
  geom_violin(aes(x = lineage, y = production_rate)) + 
  # geom_jitter(aes(x = lineage, y = production_rate), 
  #             shape =16, 
  #             size = 0.1, 
  #             position = position_jitter(0.4)) +
  geom_quasirandom(aes(x = lineage, 
                       y = production_rate, 
                       col = as.factor(shadetype_n)), 
                   shape =16, 
                   size = 0.5, 
                   #position = position_jitter(0.4)
  ) +
  scale_color_manual(values = cols[-1]) + 
  facet_wrap(~rel_time) + 
  theme_cowplot() + 
  stat_summary(
    fun=mean, geom="point", shape=23, size=2) + 
  stat_summary(fun=median, geom="point", size=2, color="red")


GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}


alldata %>% 
  filter(rel_time %in% c(0,1500)) %>% 
  filter(lineage != -1) %>% 
  ggplot(aes(x = as.factor(rel_time), y = production_rate, fill = lineage)) +
  geom_split_violin() + 
  theme_minimal_hgrid(12) + 
  geom_jitter(data = alldata %>% 
                filter(rel_time %in% c(0,1500)) %>% 
                filter(shadetype_n == surviving_shade), 
              aes(x = as.factor(rel_time), y = production_rate))


surviving_shade <- alldata %>% filter(rel_time ==1500) %>% 
  filter(lineage == 894) %>% 
  select(shadetype_n) %>% 
  distinct(.) %>% 
  unlist()