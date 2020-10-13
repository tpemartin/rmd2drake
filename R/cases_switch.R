#' Convert 2 drake plan script and make
#'
#' @return A list of planDetails
#' @export
#'
#' @examples
#' none
convert2drakeplanAndMake <- function() {
  require(dplyr)
  require(purrr)
  # rstudioapi::getSourceEditorContext() -> activeSource
  # activeSource$path -> activeRmd
  extract_activeEditorFilename()
  activeRmd <- .activeFile
  # glue::glue("options(rstudio_drake_cache = storr::storr_rds(\"")+cachePath+"\", hash_algorithm = \"xxhash64\"))",

  mainplanDetails = {
    activeRmd %>%
      rmd2drake:::extract_infoFromFilename() -> mainplanDetails0
    # make frontmatter path absolute and create params string
    #  for extra makecondition
    mainplanDetails0 %>%
      rmd2drake:::makeup_frontmatter() %>%
      ## build scripts of main and sub plans
      rmd2drake:::purl_drakeSubplanOnly2() %>%
      # augment makeconditions
      rmd2drake:::augment_makecondition() %>%
      rmd2drake:::augment_planScript_make_vis_components()
  }

  # get mainplan plan object in global env
  mainplanDetails$completePlanMkVisScript %>%
    {
      parse(text = .)
    } %>%
    eval(envir = .GlobalEnv)

  # subplans ----------------------------------------------------------------
  yml <- rmarkdown::yaml_front_matter(activeRmd)
  if ("drake_subplans" %in% names(yml)) {
    subplans <- vector("list", length(mainplanDetails$frontmatter$drake_subplans))
    for (.x in seq_along(mainplanDetails$frontmatter$drake_subplans)) {
        mainplanDetails$frontmatter$drake_subplans[[.x]] %>%
          extract_infoFromFilename() -> subplans[[.x]]

        subplans[[.x]] %>%
          # make frontmatter path absolute and create params string
          #  for extra makecondition
          makeup_frontmatter() -> subplans[[.x]]

        subplans[[.x]]$frontmatter$drake_cache <-
          mainplanDetails$frontmatter$drake_cache

        ## build scripts of main and sub plans
        subplans[[.x]] %>%
          purl_drakeSubplanOnly2() %>%
          # augment makeconditions
          augment_makecondition() %>%
          augment_planScript_make_vis_components() -> subplans[[.x]]

        # get subplan plan object to .GlobalEnv
        subplans[[.x]]$completePlanMkVisScript %>%
          {
            parse(text = .)
          } %>%
          eval(envir = .GlobalEnv)
      }

      # browser()
      # produce grandplan object (a combo plan) in global environment
      generate_grandplan(mainplanDetails, subplans)
      # browser()


      # rlang::expr(grandplan <- !!sym(a))
      grand_planScript = {
        grandplan %>%
          get_grandDrakePlanScript(mainplanname = mainplanDetails$planname)
      }

      # produce complete grandplan script
      grand_planScript %>%
        build_grandplanDetails(mainplanDetails, subplans) ->
      grandplanDetails

      grandplanDetails %>%
        augment_planScript_make_vis_components() ->
      grandplanDetails
    }

  if ("drake_subplans" %in% names(yml)) {
    destfile <- file.path(
      mainplanDetails$root,
      paste0("grandplan_", mainplanDetails$filetitle, ".R")
    )
    grandplanDetails$makefunctionText %>%
      {
        parse(text = .)
      } %>%
      eval(envir = .GlobalEnv)
    # browser()
    grandplanDetails$visfunctionText %>%
      {
        parse(text = .)
      } %>%
      eval(envir = .GlobalEnv)

    xfun::write_utf8(
      grandplanDetails$completePlanMkVisScript,
      con = destfile
    )
    assign(grandplanDetails$planname, grandplan,
           envir = .GlobalEnv)
    rm(list="grandplan", envir = .GlobalEnv)

    callMake <- rlang::call2(glue::glue("mk_{grandplanDetails$planname}"))
    rlang::eval_tidy(callMake, env = .GlobalEnv)

    callVis <- rlang::call2(glue::glue("vis_{grandplanDetails$planname}"))
    rlang::eval_tidy(callVis, env = .GlobalEnv)

    invisible(grandplanDetails)
  } else {

    destfile <- file.path(
      mainplanDetails$root,
      paste0("plan_", mainplanDetails$filetitle, ".R")
    )
    mainplanDetails$makefunctionText %>%
      {
        parse(text = .)
      } %>%
      eval(envir = .GlobalEnv)
    xfun::write_utf8(
      mainplanDetails$completePlanMkVisScript,
      con = destfile
    )


    callMake <- rlang::call2(glue::glue("mk_{mainplanDetails$planname}"))
    message(
      "Executing ", rlang::expr_text(callMake)
    )
    rlang::eval_tidy(callMake, env = .GlobalEnv)

    callVis <- rlang::call2(glue::glue("vis_{mainplanDetails$planname}"))
    message(
      "Executing ", rlang::expr_text(callVis)
    )
    rlang::eval_tidy(callVis, env = .GlobalEnv)

    rlang::exec(glue::glue("vis_{mainplanDetails$planname}"))
    invisible(mainplanDetails)
  }


}
