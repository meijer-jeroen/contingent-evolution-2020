# Changes generated Virtual Microbe metabolite names to something more sensible

my_dict <- c(LETTERS[1:9], '-->', '\\+')
names(my_dict) <- c('bb1 ', 'M1 ', 'R ', 'M2 ', 'bb2 ', 'M3 ', 'M4 ', 'M5 ', ' energy', '→ ', '+ ')
#names(my_dict) <- c('bb1', 'M1', 'R', 'M2', 'bb2', 'M3', 'M4', 'M5', 'energy', '→', '+')

# nothing goes wrong as long as we DON'T put capitals in my_dict names and 
# original metabolite names ARE capitalized...

changeMetabNames <- function(reaction){
  for(i in seq_along(my_dict)){
  reaction <- gsub(my_dict[i], names(my_dict)[i], reaction)
  }
  
  # check if transporter
  reaction <- gsub('^\\senergy\\*→\\s', '', reaction)
  reaction <- gsub('port', 'porter', reaction)
  
  
  # remove trailing whitespace
  reaction <- gsub('\\s$', '', reaction)
  
  # double whitespace
  reaction <- gsub('\\s\\s', '\\s', reaction)
  
  
  return(reaction)
}

# to use, do something like: 
# reactions <- colnames(mgx)[-c(1:7)]
# reactions_renamed <- unlist(lapply(reactions, change_name))
# reactions_renamed
