

benelux <- data.frame ( 
  id = as.character(1:3), 
  refArea = c("BE", "NE", "LU"), 
  currency = rep("EUR", 3),
  obsValue = c(212, 214, 56)
)

attr(benelux, "prefix") <- c( "id" = "eg",
                              "refArea" = "sdmx-dimension",
                              "currency" = "sdmx-measure",
                              "obsValue" = "sdmx-measure")
attr(benelux, "prefix")

xs_class <- function(x){
  
  type <- switch(class(x)[[1]],
                 "numeric" = "xs:decimal",
                 "factor" = "xs:string",
                 "logical" = "xs:boolean",
                 "integer" = "xs:integer",
                 "Date" = "xs:date",
                 "POSIXct" = "xs:dateTime",
                 NULL
  )
  
  
  string <- gsub("^xs:", 
                 "http://www.w3.org/2001/XMLSchema#",
                 type)
  ## consistent return length, character(1)
  if (length(string) == 0) {
    string <- as.character(NA)
  }
  string
}

xs_convert <- function(x){
  
  type <- switch(class(x)[[1]],
                 "numeric" = "xs:decimal",
                 "factor"  = "xs:string",
                 "logical" = "xs:boolean",
                 "integer" = "xs:integer",
                 "Date"    = "xs:date",
                 "POSIXct" = "xs:dateTime",
                 NULL
  )
  
  
  string <- gsub("^xs:", 
                 "http://www.w3.org/2001/XMLSchema#",
                 type)

  if (length(string) == 0) {
    ifelse(is.na(x), "", paste0('\"', as.character(x) , '\"'))
    } else {
    paste0('\"', as.character(x),  '\"', "^^<", string, ">")
  }
}


df <- benelux

normalise_df <- function(df, id_col = "id", id_prefix = "eg:") {
  
  col_classes <- data.frame(datatype = 
                              vapply(df, 
                                     xs_class, 
                                     character(1)))
  
  col_classes$predicate <- rownames(col_classes)
  rownames(col_classes) <- NULL
  
  if (is.null(id_col)) {
    if(is.null(row.names(df))) {
      subject_df <- data.frame ( subject = as.character(1:dim(df)[[1]]))
    } else {
      subject_df <- data.frame ( subject = as.character(row.names(df)))
    } } else {
    subject_df <- data.frame ( subject = df[`id_col`])
    if ( length(unique(subject[, 1])) != dim(df)[[1]] ) {
      warning("The provided key column's values are not unique, using integer numbers instead.")
      subject_df <- data.frame ( subject = as.character(1:dim(df)[[1]]))
    }
    }

  i = 1
  observations_df <- function(df, i) {
    tmp <- df
    if(!is.null(attr(tmp, "prefix"))) {
      this_prefix <- attr(tmp, "prefix")[which(names(attr(tmp, "prefix"))==names(df)[i])]
      predicate <-  paste0("<",as.character(this_prefix), ":", 
                           names(this_predicate),  ">")
    } else {
      predicate <- names(df)[i]
    }
    
    tmp$predicate <- predicate
    
    tmp <- subset(tmp, select = c(id_col))
    names(tmp)[1] <- "subject"
    tmp$subject <- paste0("<", id_prefix, tmp$subject, ">")
    tmp$predicate <- "<rdf:type>"
    tmp$object <- "<qb:Observation>"
    tmp$graph <- "."
    tmp
  }
  
   subset_df <- function(df, i) {
    tmp <- df
    if(!is.null(attr(tmp, "prefix"))) {
      this_prefix <- attr(tmp, "prefix")[which(names(attr(tmp, "prefix"))==names(df)[i])]
      predicate <-  paste0("<",as.character(this_prefix), ":", 
                           names(this_predicate),  ">")
    } else {
      predicate <- names(df)[i]
    }
    
    tmp$predicate <- predicate
    
    tmp <- subset(tmp, select = c(id_col, "predicate", names(df)[i]))
    names(tmp)[1] <- "subject"
    tmp$subject <- paste0("<", id_prefix, tmp$subject, ">")
    names(tmp)[3] <- "object"
    tmp$object <- xs_convert(x  = tmp$object)
    tmp$graph <- "."
    tmp
  }
  
  
  
  ll <- lapply(which(names(df) != id_col), function(x) subset_df(df, x))
  observations <- observations_df(df, which(names(df) == id_col))
 
  rbind(observations, triplets)
  
 }

convert_to_nquad <- function(df, file, id_col, id_prefix="eg:") {
  
  poor_mans_nquad_table <- normalise_df(df, id_col, id_prefix)
  
  utils::write.table(poor_mans_nquad_table, 
                     file, 
                     col.names = FALSE, 
                     quote = FALSE,
                     row.names = FALSE)
  read_nquads(out)
  
  ### the literals are not coming out nice
  
}
out = tempfile()
testout = tempfile()
convert_to_nquad(df = benelux, out, "id")
rdf_serialize(rdf_parse(out, "nquads"), 
              testout,  
              format= "turtle", 
              namespace  = c(dc = "http://purl.org/dc/elements/1.1/",
                             foaf = "http://xmlns.com/foaf/0.1/", 
                             `sdmx-attribute` = "http://purl.org/linked-data/sdmx/2009/measure#",
                             `sdmx-dimension` = "http://purl.org/linked-data/sdmx/2009/dimension#")
)


readLines(testout)

read_nquads(out)
benelux_quad <- tempfile()
convert_to_nquad(df = benelux, benelux_quad, "id")
#write_nquads(x =benelux, key_column = "id", file = benelux_quad, prefix = c("eg", "sdmx-dimension"))
readLines(benelux_quad)

write_nquads2 <- function(x, file, ...){
  UseMethod("write_nquads2")
}

... <- NULL

write_nquads2.data.frame <- function(x, 
                                    file,
                                    ...){
  
  
  
  df <- normalize_table(x, ...)
  poor_mans_nquads(x=df, file, ...)
}

write_nquads(iris, out)
test <- read_nquads(out)

testout <- tempfile()
rdf_serialize(test, format="turtle")

?rdf_serialize



