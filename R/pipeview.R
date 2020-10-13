#' Title
#'
#' @return
#' @export
#'
#' @examples none
get_selection <- function(){
  context <- rstudioapi::getActiveDocumentContext()
  selection <<- context$selection[[1]]$text
  selection
}
