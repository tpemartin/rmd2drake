cachePath = file.path(dirname(activeRmd),yml$drake_cache)
subplansPath = file.path(dirname(activeRmd),yml$drake_subplans)

saveParamRdsAndGet_paramsSetupString = function(planDetails){

  if(
    ("params" %in% names(planDetails$frontmatter)) &&
    (length(planDetails$frontmatter$params) !=0)
  ){

    filetitle <- planDetails$filetitle
    rdsName = glue::glue(
      "params_{filetitle}.rds")

    rdsfilepath <- file.path(planDetails$root, rdsName)
    saveRDS(planDetails$frontmatter$params,
            file=rdsfilepath)

    paramsSetupString <-
      glue::glue("params=readRDS(\"{rdsfilepath}\")")
  } else {
    paramsSetupString="# no params in the frontmatter"
  }

  paramsSetupString
}
