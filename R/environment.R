injectr.container <- new.env(parent = emptyenv())


#' Register an object to inject
#'
#' @param object the object to inject
#' @param name the name to use for resolution. Defaults to the passed in object
#'
#' @export
register <- function(object, name = NULL) {
    if (is.null(name)) {
        name <- as.character(substitute(object))
    }

    assign(name, object, envir = injectr.container)
}

#' Inject an object from the container into a variable
#'
#' @param name the name of the object to inject
#'
#' @return the defined object or null
#'
#' @export
inject <- function(name, target = NULL) {

    object <- get0(name, envir = injectr.container)

    if (is.null(object)) {
        return(invisible())
    }

    if (hasArg(target)) {
        name <- as.character(substitute(target))
    }

    assign(name, object, envir = parent.frame())
}

#' Clear the entire container
#'
#' @export
clear <- function() {
    rm(list = ls(envir = injectr.container), envir = injectr.container)
}