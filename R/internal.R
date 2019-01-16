#' Outputs errors, warnings and messages
#'
#' @param input character
#' @param type numeric, 1 = message/cat, 2 = warning, 3 = error and stop
#' @param msg logical. If \code{TRUE}, \code{message} is used instead of \code{cat}. Default is \code{FALSE}.
#' @param sign character. Defines the prefix string.
#'
#' @keywords internal
#' @noRd

out <- function(input, type = 1, ll = NULL, msg = FALSE, sign = "", verbose = getOption("temp.verbose")){
  if(is.null(ll)) if(isTRUE(verbose)) ll <- 1 else ll <- 2
  if(type == 2 & ll <= 2){warning(paste0(sign,input), call. = FALSE, immediate. = TRUE)}
  else{if(type == 3){stop(input,call. = FALSE)}else{if(ll == 1){
    if(msg == FALSE){ cat(paste0(sign,input),sep="\n")
    } else{message(paste0(sign,input))}}}}
}

#' Simplifies check of variables being FALSE
#'
#' @param evaluate variable or expression to be evaluated
#'
#' @keywords internal
#' @noRd
is.FALSE <- function(evaluate){if(evaluate == FALSE){return(TRUE)}else{return(FALSE)}}


#' Simplifies check of variables being TRUE
#'
#' @param evaluate variable or expression to be evaluated
#'
#' @keywords internal
#' @noRd
is.TRUE <- function(evaluate){if(evaluate == TRUE){return(TRUE)}else{return(FALSE)}}


#' Checks, if specific command is available
#'
#' @param cmd command
#' @importFrom devtools system_check
#' @keywords internal
#' @noRd
check.cmd <- function(cmd){
  sc <- try(system_check(cmd, quiet = TRUE),silent = TRUE)
  if(class(sc) == "try-error"){return(FALSE)}else{return(TRUE)}
}