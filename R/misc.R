#' Substitute file.path
#'
#' @param x a character
#' @param y a character
#'
#' @return
#' @export
#'
#' @examples None.
`%//%` <- function(x,y){
  if(!is.character(x) || !is.character(y)){
    stop("Either one of your inputs are/is not character.")
  }
  return(file.path(x,y))
}

