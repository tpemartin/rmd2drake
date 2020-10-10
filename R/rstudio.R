#' Clean target at cursor
#'
#' @return
#' @export
#'
#' @examples
rs_addin_clean <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  target <- drake:::rs_get_symbol_at_cursor(context)
  cache <- getOption("rstudio_drake_cache")
  cache <- drake:::decorate_storr(cache)
  if (is.null(target)) {
    target=list()
  }
  drake::clean(
    list = target,
    cache = cache
  )
}
