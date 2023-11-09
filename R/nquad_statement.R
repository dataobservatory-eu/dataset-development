
nquad_statement <- function(subject, predicate, object) {
  
  paste(quote_statement(subject), 
        quote_statement(predicate), 
        object, ".", 
        collapse = " ")
  
}

quote_statement <- function(x) {
  
  tmp <- ifelse(substr(x, 1, 1)=="<", 
                yes = x, 
                no  = paste0("<", x))
  
  ifelse(substr(tmp, nchar(tmp), nchar(tmp))==">", 
                yes = tmp, 
                no  = paste0(tmp, ">"))

}
