# Plot external metabolite concentrations
read_and_plot_external_metabolites <- function(sim){
  cat('reading metabolite concentrations', '\n')
  metab_files <- list.files(file.path(sim, "data/ecology_dat"), full.names = TRUE)
  metab_files <- metab_files[grep("/concentration", metab_files)]
  
  get_conc <- function(metab_file){
    temp <- read.csv(metab_file)
    temp <- select(temp, time_point, avrg)
    names(temp)[2] <- gsub("concentration_|\\[|\\]|\\.0|\\.csv|\\{|\\}", '', basename(metab_file))
    #names(temp)[2] <- substring(basename(file), regexpr('\\.', basename(file)) -1, regexpr('\\.', basename(file)) -1)
    return(temp)
  }
  
  data <- do.call(cbind, lapply(metab_files, get_conc))
  data <- data[, !duplicated(colnames(data))]
  data <- gather(data, metabolite, concentration, -time_point)
  data <- arrange(data, metabolite)
  
  xmin <- min(data$time_point)
  xmax <- max(data$time_point)
  
  options(scipen=999)
  colours <- c("#e41a1c", "#56B4E9", "#009E73", "#F0E442", "#0072B2","#E69F00" , "#CC79A7", "#808080", "#000000", "#aa6e28")
  labels <- gsub("concentration_|\\[|\\]|\\.0|\\.csv|\\{|\\}", '', unique(data$metabolite))
  
  cat('Making GGplot of external metabolite concentrations', '\n')
  
  metabs <- 
    #filter(data, time_point > xmin && time_point < xmax) %>%
    ggplot(data, aes(x = time_point, y = concentration)) +
    geom_line(aes(col = metabolite), lwd = 1) + 
    
    
    theme_classic() + 
    theme(legend.position = 'bottom', 
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15), 
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12), 
          legend.text=element_text(size=15)
    ) +
    ylab('concentration') + 
    xlab(expression(paste('Time (', 10^{5}, ' a.u.t.)'))) +
    scale_x_continuous(limits = c(0, 1000000), 
                       expand = expand_scale(mult = 0.01), 
                       breaks = seq(0, 1000000, by = 100000),                       #breaks = scales::pretty_breaks(n = 10), 
                       labels = function(x) x/100000) +
    scale_y_continuous(#limits = c(0.001, 1.0), 
                       #expand = expand_scale(mult = 0.1), 
                       breaks = scales::pretty_breaks(n = 2)) +
    
    # 
    # 
    # theme_bw() + 
    # scale_fill_manual(values = colours) + 
    # theme(legend.position = 'bottom',
    #       # axis.line = element_blank(), 
    #       panel.border = element_blank(), 
    #       plot.margin = unit(c(.5, .5, .5, .5), 'cm')
    # ) + 
    scale_color_manual(values = colours) + 
    # scale_x_continuous(expand = c(0,0), 
    #                    labels = function(x) format(x/100000), 
    #                    
    # ) + 
    # #scale_y_continuous(expand = c(0,0)) + 
    # labs(x = bquote('time'~(x10^5~arbitrary~units)), y = 'metabolite concentration') +
    guides(colour = guide_legend(nrow = 1, override.aes = list(size = 8)))

  
}
