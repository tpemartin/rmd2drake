
# helpers -----------------------------------------------------------------


get_RmdFrontmatterLines <- function(Rmdlines){
  Rmdlines %>%
    stringr::str_which("^---") -> whichHas3Dashes
  Rmdlines[(whichHas3Dashes[[1]]+1):(whichHas3Dashes[[2]]-1)]

}
get_frontmatterList <- function(Rmdlines){
  Rmdlines %>%
    get_RmdFrontmatterLines() -> RmdFrontmatter
  yaml::read_yaml(text=RmdFrontmatter)
}
get_paramsSetupString = function(RmdLines, filetitle){
  get_frontmatterList(RmdLines) -> paramsList
  knitr::knit_params(RmdLines) -> params
  # paramsList <- frontmatter
  if(
    ("params" %in% names(paramsList)) &&
    length(paramsList$params) !=0
  ){
    paramsList <- purrr::map(
      params,~purrr::pluck(.x, "value"))

    paramNames <-purrr::map_chr(
      params,~purrr::pluck(.x, "name"))

    names(paramsList) <- paramNames

    rdsName = glue::glue(
      "params_{filetitle}.rds")
    saveRDS(paramsList,
            file=rdsName)
    paramsSetupString <-
      glue::glue("params=readRDS(\"{rdsName}\")")
  } else {
    paramsSetupString="# no params in the frontmatter"
  }

  paramsSetupString
}
