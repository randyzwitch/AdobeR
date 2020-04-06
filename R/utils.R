#' @keywords internal
stringAfterSlash <- function(x) strsplit(x, "/")[[1]][2]

#Create an environment to hold credentials
AdobeRInternals <- new.env(parent = emptyenv())
