clean_reaction_names <- function(df){
  # cleans up transporter and enzyme names from VirtualMicrobes data
  # e.g.:
  # "1 A(8.0) --> 1 D(4.0) + 4 I*(1.0) False" 
  # becomes: 
  # "A->D+4I"
  
  # get pump, enzyme indices
  df <- df[,order(colnames(df), decreasing = FALSE)] # order by name
  pump_index <- grep('^.{3}\\*', names(df))
  enzyme_index <- grep('^.{3}\\*', names(df), invert = TRUE)
  
  # change false / true in import/export
  names(df) <- gsub("False", "import", names(df)) 
  names(df) <- gsub("True", "export", names(df)) 
  
  # remove energy values for readability
  pattern <- '\\([0-9]\\.[0-9]\\)'
  names(df) <- gsub(pattern, "", names(df)) 
  
  # remove ones for readability (i.e. "1D + 1E --> 2F" becomes "D + E --> 2F")
  names(df) <- gsub(1, "", names(df)) 
  
  # remove import from enzyme names  
  names(df)[enzyme_index]  <- gsub("import", "", names(df)[enzyme_index]) 
  names(df)[-enzyme_index] <- gsub("\\d.*-->\\s1\\s", "", names(df)[-enzyme_index])
  
  # remove whitespaces
  names(df)  <- gsub(" ", "", names(df)) 
  
  return(df)
}