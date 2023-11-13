new_subject <- function(x, 
                        subjectScheme = NULL, 
                        schemeURI = NULL, 
                        valueURI = NULL, 
                        classificationCode = NULL) {
  
  if (is.null(subjectScheme)) subjectScheme <- ""
  if (is.null(schemeURI)) schemeURI <- ""
  if (is.null(valueURI)) valueURI <- ""
  
  if (!is.null(classificationCode)) {
    subject <- list ( heading = x, 
                      subjectScheme = subjectScheme, 
                      schemeURI = schemeURI, 
                      classificationCode = classificationCode)
  } else {
    subject <- list ( heading = x, 
                      subjectScheme = subjectScheme, 
                      schemeURI = schemeURI, 
                      valueURI = valueURI)
  }
  
  class(subject) <- c("subject", class(subject))
  
  subject
}

subject <- function(heading,
                    subjectScheme = NULL, 
                    context = NULL,
                    schemeURI = NULL, 
                    valueURI = NULL, 
                    classificationCode = NULL ) {
  
  #if (! all.equal(length(heading), length(subjectScheme))) {
  #  stop("You must provide exactly one subjectSchemes, URIs and Codes for each heading.")
  #}
  
  if ( length(heading)>1) {
    subject <-  lapply (1:length(heading), function(x) new_subject(heading[x], 
                                                                   subjectScheme = subjectScheme[x],
                                                                   schemeURI = schemeURI[x], 
                                                                   classificationCode = classificationCode[x])
    )
    # this is not nice
    class(subject) <- c("subject", class(subject))
  } else {
    subject <- new_subject(heading, subjectScheme, schemeURI, valueURI, classificationCode)
  }
  
  subject
}