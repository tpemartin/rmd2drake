#' Purl Activive Main Plan Rmd, with its subplan Rmds to a Grandplan
#' then make it
#'
#' @return
#' @export
#'
#' @examples none
purlActiveMainRmd_thenPlanMake <- function()
{
  grandplanDetails <- generateGrandplanScriptFromActiveRmd()

  source(
    file.path(
      grandplanDetails$root,
      paste0(grandplanDetails$planname,".R")
    )
  )
  eval(
    parse(
      text=paste0("mk_",
                  grandplanDetails$planname,
                  "()")
    ), envir = .GlobalEnv)


}

#' make function for current active grandplan
#'
#' @return
#' @export
#'
#' @examples none
mk_currentActiveGrandPlan <-   function(){
  require(dplyr)
  details <- rstudioapi::getActiveDocumentContext()
  details$path %>%
    stringr::str_extract(
      "[^/]+(?=\\.[Rr][mM][Dd]$)"
    ) %>%
    paste0("mk_grandplan_",.,"()") -> commandText

  eval(
    parse(text=commandText), envir = .GlobalEnv
  )
}


#' Visualize the current active grand plan
#'
#' @return
#' @export
#'
#' @examples none
vis_currentActiveMainPlan <-   function(){
  require(dplyr)
  details <- rstudioapi::getActiveDocumentContext()
  details$path %>%
    stringr::str_extract(
      "[^/]+(?=\\.[Rr][mM][Dd]$)"
    ) %>%
    paste0("vis_grandplan_",.,"()") -> commandText

  eval(
    parse(text=commandText), envir = .GlobalEnv
  )
}


#' Generate grandplan_XXX.R from active Rmd main plan
#'
#' @return A list of grandplanDetails
#' @export
#'
#' @examples none
generateGrandplanScriptFromActiveRmd <- function(){
  require(dplyr)
  require(purrr)
  # rstudioapi::getSourceEditorContext() -> activeSource
  # activeSource$path -> activeRmd
  extract_activeEditorFilename()
  activeRmd <- .activeFile

  require(dplyr)
  mainplanDetails <-
    {
      activeRmd %>%
        extract_infoFromFilename() -> mainplanDetails0
      # make frontmatter path absolute and create params string
      #  for extra makecondition
      mainplanDetails0 %>%
        makeup_frontmatter() %>%
        ## build scripts of main and sub plans
        purl_drakeSubplanOnly2() %>%
        # augment makeconditions
        augment_makecondition() %>%
        augment_planScript_make_vis_components()
    }

  # get mainplan plan object in global env
  mainplanDetails$completePlanMkVisScript %>%
    {parse(text=.)} %>%
    eval(envir = .GlobalEnv)

  # subplans ----------------------------------------------------------------

  subplans = vector("list", length(mainplanDetails$frontmatter$drake_subplans))
  for(.x in seq_along(mainplanDetails$frontmatter$drake_subplans)){
    subplans[[.x]] <-
      {
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
          {parse(text=.)} %>%
          eval(envir = .GlobalEnv)

        subplans[[.x]]
      }

  }

  # produce grandplan object (a combo plan) in global environment
  generate_grandplan(mainplanDetails, subplans)

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

  destfile = file.path(
    mainplanDetails$root,
    paste0("grandplan_",mainplanDetails$filetitle,".R")
  )
  xfun::write_utf8(
    grandplanDetails$completePlanMkVisScript,
    con=destfile
  )

  invisible(grandplanDetails)
}


# helpers -----------------------------------------------------------------


mkvis_functional <- function(prefix){
  function(){
    require(dplyr)
    details <- rstudioapi::getActiveDocumentContext()
    details$path %>%
      stringr::str_extract(
        "[^/]+(?=\\.[Rr][mM][Dd]$)"
      ) %>%
      paste0(prefix,"_grandplan_",.,"()") -> commandText

    eval(
      parse(text=commandText), envir = .GlobalEnv
    )
  }
}

