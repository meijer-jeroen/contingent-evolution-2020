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
colnames(for_heatmap)

rownames(for_heatmap) <- gsub(".*evo([0-9]+)$", "\\1", labels$sim) # label for evo seed
rownames(for_heatmap) <- labels$population # label for community number




# using ComplexHeatmap ----------------------------------------------------
#To do

library(devtools)
library(ComplexHeatmap)
library(circlize)

col_fun <- colorRamp2(c(0,1), c('white', 'black'))

library(dendsort)
dendrow = dendsort(hclust(dist(for_heatmap, method = 'euclidean')))
#dendrow = dendsort(hclust(dist(for_heatmap, method = 'manhattan')))
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

ha@anno_list$Community@label = gt_render('**Community type**')
ha@anno_list$Community@legend_param = list(nrow =1)
ha@anno_list$Topology@label = gt_render('**Network Topology**')
ha@anno_list$Topology@legend_param = list(nrow =1)

lgd_com <- Legend(title = 'Community type', 
                  legend_gp = gpar(fill = 1:2),
                  labels=  c('Autonomous', 'Cross-feeding'),
                  nrow =1
)
# 
## Ugly hack, manually set these to correct index to fix colors in legend:
# Easier to do in Inkscape
# lgd_com@grob$children$GRID.legend_body.2194$children$GRID.rect.2191$gp$fill <- "gold4"
# lgd_com@grob$children$GRID.legend_body.2194$children$GRID.rect.2193$gp$fill <- "#0072B2"

lgd_top <- Legend(title = 'Network topology', 
                  legend_gp = gpar(fill = 1:3),
                  labels=  c('Resource', 'Building block', ' Hybrid'),
                  nrow =1
)

## Ugly hack, manually set these to correct index:
# Easier to do in Inkscape
# lgd_top@grob$children$GRID.legend_body.2207$children$GRID.rect.2202$gp$fill <- "#56B4E9"
# lgd_top@grob$children$GRID.legend_body.2207$children$GRID.rect.2204$gp$fill <- "#F0E442"
# lgd_top@grob$children$GRID.legend_body.2207$children$GRID.rect.2206$gp$fill <- "#009E73"

lgd_sw <- Legend(pch = 's', type = 'points', labels = 'Switched community type')

grDevices::cairo_pdf(
  file = '../results/figures/fig4a_complexheatmap_sw_popnames_dendsort.pdf', # for special characters in reaction names
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
  #rect_gp = gpar(col = 'white', lwd=.5),
  column_title = 'Metabolic genes', 
  row_title = '60 replicate simulations', 
  row_names_centered = TRUE, 
  column_split = 2, 
  left_annotation = ha, 
  #cluster_rows = dendrow,
  #cluster_columns = dendcol,
  row_dend_width = unit(6, 'cm'), 
  column_dend_height = unit(4, 'cm'), 
  row_names_side = 'left',
  column_names_side = 'top' 
  
)

draw(ht_list, heatmap_legend_side = "bottom", annotation_legend_side = 'bottom', annotation_legend_list = list(lgd_com, lgd_sw, lgd_top))
dev.off()


# # No annotation for switching communities (legend is fine) ----------------------
# grDevices::cairo_pdf(
#   file = '../results/figures/fig4a_complexheatmap.pdf', # for special characters in reaction names
#   height = 14, 
#   width = 14
# )
# 
# ha = rowAnnotation(
#   Community = labels$final_commtype,
#   Topology = labels$ene_type,
#   col = list(Community = c('Autonomous' = "gold4", 'Cross-feeding'="#0072B2"),
#              Topology =  c('building block' = "#F0E442", 'resource'= "#56B4E9", 'hybrid' = "#009E73")
#   ),
#   annotation_legend_param  = list(Community = list(nrow = 1),
#                                   Topology = list(nrow = 1)
#   ),
#   annotation_name_side = 'top'
# )
# 
# ha@anno_list$Community@label = gt_render('**Community type**')
# ha@anno_list$Community@legend_param = list(nrow =1)
# ha@anno_list$Topology@label = gt_render('**Network Topology**')
# ha@anno_list$Topology@legend_param = list(nrow =1)
# 
# ht_list <- Heatmap(
#   for_heatmap, 
#   heatmap_legend_param = list(
#     title = 'Fraction of microbes carrying gene\nat end of simulation', 
#     direction = 'horizontal', 
#     legend_width = unit(4, "cm")
#   ),
#   col = col_fun, 
#   #rect_gp = gpar(col = 'white', lwd=.5),
#   column_title = 'Metabolic genes', 
#   row_title = '60 replicate simulations', 
#   row_names_centered = TRUE, 
#   column_split = 2, 
#   left_annotation = ha, 
#   cluster_rows = dendrow,
#   cluster_columns = dendcol,
#   row_dend_width = unit(6, 'cm'), 
#   column_dend_height = unit(4, 'cm'), 
#   row_names_side = 'left',
#   column_names_side = 'top' 
#   
# )
# 
# draw(ht_list, heatmap_legend_side = "bottom")
# dev.off()


# # #Old version using heatmap.2 ---------------------------------------------
# library(gplots)
# library(heatmap.plus)
# library(RColorBrewer)
# # source("utility/heatmap.3.R")
# #community_cols <- c("#0072B2", "#E69F00","#009E73","#56B4E9",  "#F0E442", "#999999")
# community_cols <- c('gold4',"#0072B2")
# 
# enetype_cols <- c("#F0E442", "#009E73","#56B4E9","#999999")
# 
# commcol <- cbind(community_cols[as.numeric(as.factor((labels$final_commtype)))],
#                   enetype_cols[as.numeric(as.factor((labels$ene_type)))]
# )
# commcol <- cbind(commcol,
#                  scan('utility/cols20.txt', # energy reaction
#                       what = character())[as.numeric(as.factor((labels$ene_react)))])
# 
# #hmcol <- colorRampPalette(brewer.pal(9, 'GnBu'))(100)
# hmcol <- paste("gray", 99:1, sep="")
# 
# 
# # first figure: rows coloured for community type --------------------------
# grDevices::cairo_pdf(file = '../results/figures/fig4a_heatmap_commtype.pdf') # for special characters in reaction names
# heatmap.2(for_heatmap,
#           col = hmcol,
#           trace = 'none',
#           RowSideColors = commcol[,1],
#           Colv = TRUE,
#           cexCol = 1,
#           cexRow = 1
#           #ColSideColors = reaction_col
# )
# dev.off()



# heatmap.3 ---------------------------------------------------------------
main_title="Drug Response Predictions"
par(cex.main=1)
heatmap.3(prob_matrix, hclustfun=myclust, distfun=mydist, na.rm = TRUE, scale="none", dendrogram="both", margins=c(6,12),
          Rowv=TRUE, Colv=TRUE, ColSideColors=clab, RowSideColors=rlab, symbreaks=FALSE, key=TRUE, symkey=FALSE,
          density.info="none", trace="none", main=main_title, labCol=FALSE, labRow=drug_names, cexRow=1, col=rev(heat.colors(75)),
          ColSideColorsSize=7, RowSideColorsSize=2, KeyValueName="Prob. Response")
legend("topright",legend=c("Basal","LumA","LumB","Her2","Claudin","Normal","","Positive","Negative","NA","","Targeted","Chemo","","Approved","Experimental"),
       fill=c("red","blue","cyan","pink","yellow","green","white","black","white","grey","white","darkorchid","darkred","white","green","darkgreen"), border=FALSE, bty="n", y.intersp = 0.7, cex=0.7)
dev.off()

library(gplots)
library(heatmap.plus)
library(RColorBrewer)
source("utility/heatmap.3.R")
#community_cols <- c("#0072B2", "#E69F00","#009E73","#56B4E9",  "#F0E442", "#999999")
community_cols <- c('gold4',"#0072B2")

enetype_cols <- c("#F0E442", "#009E73","#56B4E9","#999999")

commcol <- cbind(community_cols[as.numeric(as.factor((labels$final_commtype)))],
                  enetype_cols[as.numeric(as.factor((labels$ene_type)))]
)
commcol <- cbind(commcol,
                 scan('utility/cols20.txt', # energy reaction
                      what = character())[as.numeric(as.factor((labels$ene_react)))])

#hmcol <- colorRampPalette(brewer.pal(9, 'GnBu'))(100)
hmcol <- paste("gray", 99:1, sep="")
rlab <- t(commcol[,1:2])


grDevices::cairo_pdf(file = '../results/figures/fig4a_heatmap_original.pdf',  # for special characters in reaction names
                     height = 18, 
                     width = 16) 

heatmap.3(for_heatmap,
          col = hmcol,
          trace = 'none',
          #distfun = function(x) dist(x, method = 'manhattan'),
          hclustfun = function(x) hclust(x, method = 'average'),
          RowSideColors = rlab,
          #Colv = TRUE,
          revC = TRUE,
          cexCol = 1.5,
          cexRow = 1.5,
          margins = c(15,3)
          #ColSideColors = reaction_col
)
legend(
  "topright", 
  legend = c('Autonomous', 'Crossfeeding', 'Building block', 'Hybrid', 'Resource'),
  fill = c(community_cols, enetype_cols), 
  border = FALSE, 
  bty = 'n'
)
graphics.off()
