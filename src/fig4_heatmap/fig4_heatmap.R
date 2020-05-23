library(tidyverse)
library(gridtext)
source('src/utility/changeMetabNames.R')

load('data/temp/fig4.rdata') # first run fig4_read_data.R

labels <- mgx %>% 
  group_by(sim) %>% 
  slice(which.max(time_point)) %>% 
  ungroup() %>%
  select(colnames(mgx)[c(1:8)]) 

selected <- colnames(labels)

for_heatmap <- mgx %>% 
  group_by(sim) %>% 
  slice(which.max(time_point)) %>% 
  ungroup() %>%
  select(-selected) %>% 
  as.matrix()

colnames(for_heatmap) <-  unlist(lapply(colnames(for_heatmap), changeMetabNames))

rownames(for_heatmap) <- gsub(".*evo([0-9]+)$", "\\1", labels$sim) # label for evo seed
rownames(for_heatmap) <- labels$population # label for community number




# using ComplexHeatmap ----------------------------------------------------
library(devtools)
library(ComplexHeatmap)
library(circlize)
library(dendsort)
col_fun <- colorRamp2(c(0,1), c('white', 'black'))


dendrow = dendsort(hclust(dist(for_heatmap, method = 'euclidean')))
dendcol = dendsort(hclust(dist(t(for_heatmap), method = 'euclidean')), isReverse = TRUE)

# Annotate switching populations (messes up legend colours, adjust in Inkscape --------

pch <- rep(NA, 60)
swit <- c(207, 305, 402,405,409,410,504,506,601,701,702,802,806,901,905)
pch[which(labels$seed %in% swit)] <- 's'

ha = rowAnnotation(
  Community = anno_simple(
    labels$final_commtype,
    col = c('Autonomous' = "gold4", 'Cross-feeding' = "#0072B2"),
    pch = pch,
    pt_gp = gpar(col = "white")
  ),
  Topology = anno_simple(
    labels$ene_type,
    col = c(
      'building block' = "#F0E442",
      'resource' = "#56B4E9",
      'hybrid' = "#009E73"
    ),
    pch = rep(NA, 60), 
  ), 
  annotation_name_side = 'top'
)

# Ugly hack
ha@anno_list$Community@label = gt_render('**Community type**')
ha@anno_list$Community@legend_param = list(nrow =1)
ha@anno_list$Topology@label = gt_render('**Network Topology**')
ha@anno_list$Topology@legend_param = list(nrow =1)

lgd_com <- Legend(title = 'Community type', 
                  legend_gp = gpar(fill = 1:2),
                  labels=  c('Autonomous', 'Cross-feeding'),
                  nrow =1
)

lgd_top <- Legend(title = 'Network topology', 
                  legend_gp = gpar(fill = 1:3),
                  labels=  c('Resource', 'Building block', ' Hybrid'),
                  nrow =1
)


lgd_sw <- Legend(pch = 's', type = 'points', labels = 'Switched community type')

grDevices::cairo_pdf(
  file = 'figures/fig4_heatmap.pdf', # for special characters in reaction names
  height = 14, 
  width = 14
)

ht_list <- Heatmap(
  for_heatmap, 
  heatmap_legend_param = list(
    title = 'Fraction of microbes carrying gene\nat end of simulation', 
    direction = 'horizontal', 
    legend_width = unit(4, "cm")
  ),
  col = col_fun, 
  column_title = 'Metabolic genes', 
  row_title = '60 replicate simulations', 
  row_names_centered = TRUE, 
  column_split = 2, 
  left_annotation = ha, 
  cluster_rows = dendrow,
  cluster_columns = dendcol,
  row_dend_width = unit(6, 'cm'), 
  column_dend_height = unit(4, 'cm'), 
  row_names_side = 'left',
  column_names_side = 'top' 
  
)

draw(ht_list, heatmap_legend_side = "bottom", annotation_legend_side = 'bottom', annotation_legend_list = list(lgd_com, lgd_sw, lgd_top))
dev.off()
