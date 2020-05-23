
linColRainbow <- function(max_lins, df_prot, cols)  {
  library(docstring)
  #' @title Makes a colour palette for lineage markers
  #' @description 
  #' @param max_lins maximum number of unique lineage markers (i.e. gridsize * gridwidth)
  #' @param df_prot data frame with lineage and time column. Necessary for determining which lineages survive. 
  #' @param cols a list of two optional hex colour for first and second of two surviving lineage markers
  #' @return a list of colours
  
  
  isColor <- function(X) {
    tryCatch(is.matrix(col2rgb(X)), 
             error = function(e) FALSE)
  }
  
  lin_col <- c("#000000FF", sample(rainbow(max_lins + 2)))
  
  if(isColor(cols[1]) && isColor(cols[2]))
  {
    cat('using provided colours\n')
    #set contrasting colours for final 2 lineages
    
    surviving_lineages <- df_prot %>%
      filter(time == max(time)) %>%
      select(lineage) %>%
      distinct(.) %>%
      unlist(.) %>%
      as.character(.) %>%
      sort(.)
    #cat('surviving lineages:', surviving_lineages, '\n')
    surviving_lineages <- as.numeric(surviving_lineages) + 2
    
    
    if (length(surviving_lineages) == 3) {
      lin_col[surviving_lineages] <- c('black', cols[1], cols[2])
    }
    
  } else{
    cat('using default colours\n')
  }
  
  
  return(lin_col)
}
