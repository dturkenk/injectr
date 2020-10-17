.injectr.container <- new.env(parent = emptyenv())

#' Register an object to inject
#'
#' @param object the object to inject
#' @param name the name to use for resolution. Defaults to the passed in object
#'
#' @export
register <- function(object, 
                    name = NULL,
                    generator = FALSE) {
    if (is.null(name)) {
        name <- as.character(substitute(object))
    }

    wrapper <- .object_wrapper(object, "generator" = generator)

    assign(name, wrapper, envir = .injectr.container)
}

#' Inject an object from the container into a variable
#'
#' @param name the name of the object to inject
#'
#' @return the defined object or null
#'
#' @export
inject <- function(name, target = NULL) {

    wrapper <- get0(name, envir = .injectr.container)

    if (is.null(wrapper)) {

        error_on_no_match <- getOption("injectr.error_on_no_match")

        if (is.null(error_on_no_match)) {
            error_on_no_match <- FALSE
        }

        if (error_on_no_match) {
            stop(paste0("No object matching ", name, " found."))
        }

        return(invisible())
    }

    if (hasArg(target)) {
        name <- as.character(substitute(target))
    }

    assign(name, wrapper$object, envir = parent.frame())
}

#' Clear the entire container
#'
#' @export
clear <- function() {
    rm(list = ls(envir = .injectr.container), envir = .injectr.container)
}


.object_wrapper <- function(object, ...) {
    if (is.null(object)) stop("object must be provided")
    structure(list("object" = object, ...), class = "object_wrapper")
}


