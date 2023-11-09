irisref <- c(
  bibentry(
    bibtype = "Misc",
    title = "The iris Dataset",
    author = c(
      person(family ="Anderson", 
             given ="Edgar", 
             role = "aut")
    ),
    doi = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
    year = "1935",
    version = "1.0",
    description = "The famous dataset that is distributed with R.",
    url = "https://en.wikipedia.org/wiki/Iris_flower_data_set",
    resourceType = "Dataset"
  )
)


mysubjects <- subject( heading =  c("Data sets", "Irises (Plants)"), 
                 schemeURI = rep("https://id.loc.gov/authorities/subjects/", 2), 
                 valueURI = c("https://id.loc.gov/authorities/subjects/sh2018002256.html",
                              "https://id.loc.gov/authorities/subjects/sh85068079.html"))


initialise_dsd <- function(df, col) {
  r_class <- class(df[,col])
  col_class <- "xsd:string"
  col_class <- ifelse ("numeric" %in% r_class, "xsd:decimal", col_class) 
  col_class <- ifelse ("logical" %in% r_class, "xsd:boolean", col_class) 
  col_class <- ifelse ("integer" %in% r_class, "xsd:integer", col_class) 
  col_class <- ifelse ("factor" %in% r_class, "coded", col_class) 
  # find other labelled uses
  
  var <-  list ( var1 = list(name = names(df)[col],
                             label = list (""), 
                             type = "",
                             range = col_class, 
                             comment = "",
                             concept = list ( heading = "", 
                                              schemeURI = "", 
                                              valueURI = ""),
                             defintion = list ( schemeURI = "", 
                                                valueURI = ""))
  )
  
  names(var) <- names(df)[col]
  var
}
initialize_dsd <- initialise_dsd


new_dataset <- function (x, 
                         Identifier = NULL, 
                         DataBibentry,
                         Subject
) {
  
  validate_dataset(x = x, 
                   Identifier=Identifier, 
                   DataBibentry = DataBibentry, 
                   Subject=Subject)
  
  
  DataStructureNest <- lapply ( 1:ncol(x), function(col) initialise_dsd(df = x, col) )
  
  DataStructure <- DataStructureNest[[1]]
  if (ncol(x)>1) {
    for ( i in 2:ncol(x)) DataStructure <- c(DataStructure,  DataStructureNest[[i]])
  }
  
  attr(x, "DataBibentry") <- DataBibentry
  attr(x, "Subject") <- Subject
  attr(x, "Identifer") <- Identifier
  attr(x, "DataStructure") <- DataStructure
  class(x) <- c("dataset", class(x))
  x
}

validate_dataset <- function(x, 
                             Identifier, 
                             DataBibentry,
                             Subject) {
  
  if (! inherits(x, "data.frame"))   {
    wrong_class <- class(x)
    stop("dataset(x=...) must be inherited from a data.frame (like data.frame, tibble, data.table, tsibble ...), not ", wrong_class)
  }
  
  if (! inherits(DataBibentry, "bibentry"))   {
    wrong_class <- class(DataBibentry)
    stop("dataset(Bibentry=...) must be inherited from a bibentry, not ", wrong_class)
  }
  
  if (! inherits(Subject, "subject"))   {
    wrong_class <- class(Subject)
    stop("dataset(Subject=...) must be inherited from a subject [created by dataset::subject()], not ", wrong_class)
  }
  
  if (! is.null(Identifier)) {
    if (! inherits(Identifier, "character"))   {
      stop("dataset(Identifier=...) must be of class character.")
    }
  }
}

testiris <- iris
myiris <- new_dataset (x = testiris, 
                       DataBibentry = irisref, 
                       Subject = subject("Data sets"))

var_name = "Species"
var_label = c('"Species"@en', '"Fajok"@hu')

dsd_var_label <- function(x, var_name, var_label, labelType = "prefLabel") {
  
  old_DSD <- attr(myiris, "DataStructure")
  new_DSD <- old_DSD
  element <- old_DSD[[which(var_name == names(x))]]
  old_labels <- element$label
  new_labels <- list()
  if (length(old_labels) == 1 ) {
    if (nchar(old_labels)==0)
      new_labels <-list (prefLabel = var_label)
  } else {
    new_labels <- c(old_labels, list (prefLabel = var_label))
  }
  
  element$label <- new_labels
  
  new_DSD[[which(var_name == names(x))]] <- element
  
  attr(x, "DataStructure") <- new_DSD
  
  x
  
}

myiris <- dsd_var_label(myiris, "Species", c('"Species of iris"@en', '"Nőszirom fajok"@hu'))

attr(myiris, "DataStructure")



list ( name = "Sepal.Length",
       type = "measure", 
       definition = "", 
       label = '"sepal length"@en',
       comment = "Sepal length in centimeters",
       property = "Length", 
       range = "xsd:decimal", 
       concept = "Iris",
       unit = list ( schemeURI = "sdmx-concept:", 
                     valueURI = "unitMeasure")
)




