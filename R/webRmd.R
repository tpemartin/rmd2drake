#' Convert webRmd 2 drake plan script and make
#'
#' @return A list of planDetails
#' @export
#'
#' @examples none
convertWebRmd2drakeplanAndMake <- function() {
  require(dplyr)
  require(purrr)
  # rstudioapi::getSourceEditorContext() -> activeSource
  # activeSource$path -> activeRmd
  rmd2drake:::extract_activeEditorFilename()
  activeRmd <- .activeFile
  # glue::glue("options(rstudio_drake_cache = storr::storr_rds(\"")+cachePath+"\", hash_algorithm = \"xxhash64\"))",
  browser()
  mainplanDetails = {
    activeRmd %>%
      rmd2drake:::extract_infoFromFilename() -> mainplanDetails0
    # make frontmatter path absolute and create params string
    #  for extra makecondition
    mainplanDetails0 %>%
      rmd2drake:::makeup_frontmatter() %>%
      ## build scripts of main and sub plans
      rmd2drake:::purl_drakeSubplanOnly2() %>%
      augment_cssjs() %>%
      # augment makeconditions
      rmd2drake:::augment_makecondition() %>%
      rmd2drake:::augment_planScript_make_vis_loadcomponents()
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
        rmd2drake:::extract_infoFromFilename() -> subplans[[.x]]

      subplans[[.x]] %>%
        # make frontmatter path absolute and create params string
        #  for extra makecondition
        rmd2drake:::makeup_frontmatter() -> subplans[[.x]]

      subplans[[.x]]$frontmatter$drake_cache <-
        mainplanDetails$frontmatter$drake_cache

      ## build scripts of main and sub plans
      subplans[[.x]] %>%
        rmd2drake:::purl_drakeSubplanOnly2() %>%
        augment_cssjs() %>%
        # augment makeconditions
        rmd2drake:::augment_makecondition() %>%
        rmd2drake:::augment_planScript_make_vis_loadcomponents() -> subplans[[.x]]

      # get subplan plan object to .GlobalEnv
      subplans[[.x]]$completePlanMkVisScript %>%
        {
          parse(text = .)
        } %>%
        eval(envir = .GlobalEnv)
    }

    # browser()
    # produce grandplan object (a combo plan) in global environment
    rmd2drake:::generate_grandplan(mainplanDetails, subplans)
    # browser()


    # rlang::expr(grandplan <- !!sym(a))
    grand_planScript = {
      grandplan %>%
        rmd2drake:::get_grandDrakePlanScript(mainplanname = mainplanDetails$planname)
    }

    # produce complete grandplan script
    grand_planScript %>%
      rmd2drake:::build_grandplanDetails(mainplanDetails, subplans) ->
      grandplanDetails

    grandplanDetails %>%
      rmd2drake:::augment_planScript_make_vis_loadcomponents() ->
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


# helpers -----------------------------------------------------------------

# require(dplyr)
# planDetails <- mainplanDetails0
# "/Users/martinl/Github/webtemplate/drake_03_dateTime.Rmd" %>%
#   xfun::read_utf8() -> Rmdlines

augment_cssjs <- function(planDetails)
{
  Rmdlines <- planDetails$RmdLines
  Rmdlines %>%
    rmd2drake::get_listCodeChunksFromRmdlines("css") -> cssChunks
  Rmdlines %>%
    rmd2drake::get_listCodeChunksFromRmdlines("js") -> jsChunks

  cssTarget <- character()
  jsTarget <- character()
  if(length(cssChunks)!=0){
    cssTarget={
      require(dplyr)
      # Use the first css label to be target name
      cssTarget=names(cssChunks)[[1]]
      cssChunks %>%
        unlist() %>%
        c('<style>',.,'</style>') -> htmltext
      names(htmltext) <- NULL
      htmlExpr <- rlang::expr(htmltools::htmlTemplate(text_ = !!htmltext))
      htmlLines <- c(
        paste0(cssTarget," = {"),
        rlang::expr_text(htmlExpr)
      )
      htmlLines
    }
  }
  if(length(jsChunks)!=0){
    jsTarget={
      require(dplyr)
      # Use the first js label to be target name
      jsTarget=names(jsChunks)[[1]]
      jsChunks %>%
        unlist() %>%
        c('<script>',.,'</stript>') -> htmltext
      names(htmltext) <- NULL
      htmlExpr <- rlang::expr(htmltools::htmlTemplate(text_ = !!htmltext))
      htmlLines <- c(
        paste0(jsTarget," = {"),
        rlang::expr_text(htmlExpr),
        "}"
      )
      htmlLines
    }
  }
  cssTarget=if(length(jsTarget)==0){
    c(cssTarget, "}")
  } else {
    c(cssTarget, "},")
  }
  jscssStack <- c(cssTarget, jsTarget)
  jscssStack
  planDetails$drakePlanScript %>%
    stringr::str_which("# > plan ends ") -> loc_planEnd
  planDetails$drakePlanScript %>%
    {which(.!="")} -> possibleLastCommandLoc
  max(possibleLastCommandLoc[possibleLastCommandLoc < loc_planEnd]) -> loc_lastCommand
  planDetails$drakePlanScript -> planScript
  planScript[[loc_lastCommand]] <- paste0(planScript[[loc_lastCommand]], ",")
  planDetails$drakePlanScript <-
    c(planScript[1:loc_lastCommand],
      jscssStack,
      planScript[-c(1:loc_lastCommand)])
  planDetails$drakePlanScript
  planDetails
}


