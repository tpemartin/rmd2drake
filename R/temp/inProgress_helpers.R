#' purl the main Rmd in multiple subplan Rmd cases
#'
#' @return
#' @export
#'
#' @examples none
purlActiveMainRmd_thenPlanMake <- function(){
  require(dplyr)
  # rstudioapi::getSourceEditorContext() -> activeSource
  # activeSource$path -> activeRmd
  extract_activeEditorFilename()
  activeRmd <- .activeFile
  yml <- rmarkdown::yaml_front_matter(activeRmd)
  if(!exists("yml") || !("drake_cache" %in% names(yml))){
    stop(
      "Frontmatter has no drake_cache assigned.")
  }
  if(!exists("yml") || !("drake_subplans" %in% names(yml))){
    stop(
      "Frontmatter has no drake child Rmds assigned.
      Maybe you should use purlActiveRmd_thenPlanMake")
  }
  generate_hiddenCacheNew(yml)
  # cacheNew = drake::new_cache(path=yml$drake_cache)

  # normalizePath(activeRmd) -> activeRmd
  # stringr::str_remove(activeRmd, rootPath) ->
  #   html2open

  webDirRoot <- dirname(activeRmd)
  activeRmdBase <- basename(activeRmd)
  xfun::read_utf8(
    activeRmd
  ) -> RmdLines
  mainPlanname <- get_planname(activeRmd)


  get_frontmatterList(RmdLines) -> frontmatter
  get_paramsSetupString(RmdLines,
                        get_filetitle(activeRmd)
  ) -> mainPlanParamsSetupString
  drakeMainPlanname <-
    paste0("plan_",
           stringr::str_remove(activeRmdBase,"\\.[rR][mM][dD]$"))

  ## build scripts of main and sub plans
  mainPlan=
    purl_drakeSubplanOnly(activeRmd, drakeMainPlanname)
  subplans = vector("list", length(frontmatter$drake_subplans))
  for(.x in seq_along(frontmatter$drake_subplans)){
    # .x <-1
    subplanfilename <- frontmatter$drake_subplans[[.x]]
    subplanfilename <- file.path(
      webDirRoot,
      frontmatter$drake_subplans[[.x]]
    )
    subplanfilename %>%
      xfun::read_utf8(con=.) -> subplanRmdLines#%>%
    subplanSingleton <- subplans[[.x]] <-
      purl_drakeSubplanOnly(subplanfilename, frontmatter$drake_cache)
    get_paramsSetupString(subplanRmdLines,
                          get_filetitle(subplanfilename)) -> subplans[[.x]]$paramsSetupString

    # obtain Regex for later rename subplan targets
    get_patternReplacementExp(subplanSingleton, .x) ->
      subplans[[.x]]$patternReplacementExp
    stringr::str_subset(subplans[[.x]]$targets, "makecondition",
                        negate=T) -> subplans[[.x]]$targets
  }

  append(
    list(mainPlan),
    subplans) -> allPlans

  # stack all makeconditions
  allPlans %>%
    purrr::map(~.x$makecondition) %>%
    purrr::flatten() %>%
    unlist() -> stackedMakecondition

  # rename all subplan targets and stack
  allPlans[-1] %>%
    purrr::map(
      ~{
        stringr::str_replace_all(
          .x$plan,
          .x$patternReplacementExp
        ) -> revisedSubplan
      }
    ) %>%
    unlist() -> subplanTargetsRenamed

  # rename main plan using subplan targets
  rename_planTarget <- function(planText, patterReplacementExp){
    stringr::str_replace_all(
      planText,
      patterReplacementExp
    )-> planText
    planText
  }
  # mainPlan$plan %>%
  #   rename_planTarget(
  #     subplans[[.x]]$patternReplacementExp
  #   ) -> mainPlan$plan
  require(purrr)
  subplans %>%
    map(pluck, "patternReplacementExp") -> list_patternReplacementExp
  append(
    list(mainPlan$plan),
    list_patternReplacementExp
  ) %>%
    purrr::reduce(
      .,
      rename_planTarget
    ) -> mainPlanTargetRenamed

  # stack all plans
  c(mainPlanTargetRenamed,
    subplanTargetsRenamed) -> stackedPlan

  # stack makeconditions and stackedPlan
  grandPlanscript <-
    c(
      stackedMakecondition,
      stackedPlan
    )

  # # backward plan evaluation
  # makeconditionExpr <- str2expression(stackedMakecondition)
  # eval(
  #   makeconditionExpr,
  #   env = .GlobalEnv
  # )
  # # eval subplans
  # eval_text <- function(subplan, envir=.GlobalEnv){
  #   subplanExpr <- str2expression(subplan)
  #   eval(
  #     subplanExpr,
  #     env = .GlobalEnv
  #   )
  # }

  tryCatch({
    # eval stacked makecondition and subplans
    c(
      stackedMakecondition,
      subplanTargetsRenamed
    ) %>%
      str2expression() %>%
      eval(envir=.GlobalEnv)

    # eval main plan
    mainPlanTargetRenamed %>%
      str2expression() %>%
      eval(envir=.GlobalEnv)

  }, error=function(e){
    stop(e)
  })

  # if no error, proceed to do text combined grand plan
  allPlannames <-
    purrr::map_chr(
      allPlans,
      ~.x$planname
    )
  # plannameSymbols <- rlang::syms(allPlannames)
  # bind_rowsExpr <-
  #   rlang::expr({
  #     bind_rows(!!!plannameSymbols)
  #   })
  # plan_grand <-
  #   eval(bind_rowsExpr, envir=.GlobalEnv)

  grandPlan <- c(
    mainPlanTargetRenamed,
    subplanTargetsRenamed
  )
  grandPlan %>%
    stringr::str_which(
      "# > plan begins -----------"
    ) -> planBeginsAtWhich

  grandPlan %>%
    stringr::str_which(
      "# > plan ends ------------"
    ) -> planEndsAtWhich

  planEndingBracketPosition = {
    grandPlan %>%
      stringr::str_trim(side='both') %>%
      stringr::str_which("\\}$") -> whichHasRightBracket
    planEndingBracketPosition <- vector("integer", length(planEndsAtWhich) )
    for(.x in seq_along(planEndsAtWhich)){
      pickNonexceeding <- (whichHasRightBracket <= planEndsAtWhich[[.x]])
      planEndingBracketPosition[[.x]] <-
        max(whichHasRightBracket[pickNonexceeding])
    }
    planEndingBracketPosition
  }

  grandPlanFinal = {
    lastEndingBracketPosition= max(planEndingBracketPosition)
    grandPlanText <- c()
    for(.x in seq_along(planEndsAtWhich)){
      endlinePosition <- planEndingBracketPosition[[.x]]
      grandPlan[[endlinePosition]] <-
        ifelse(
          endlinePosition!=lastEndingBracketPosition,
          paste0(grandPlan[[endlinePosition]],","),
          grandPlan[[endlinePosition]]
        )
      newText <- grandPlan[
        (planBeginsAtWhich[[.x]]+1):
          (endlinePosition)
      ]
      grandPlanText <- c(
        grandPlanText,
        newText
      )
    }
    grandPlanText
  }

  grandPlanScript = {
    # produce drake R script
    {
      prefix <- c(
        glue::glue("# grand{mainPlan$planname}------------"),
        glue::glue("grand{mainPlan$planname}=drake::drake_plan("),
        "# > grandplan begins -----------"
      )
      suffix <- c(
        "# > grandplan ends ------------",
        ")",
        ""
      )

      # assemble
      drakeScripts <-
        c(
          prefix,
          grandPlanText,
          suffix
        )
      drakeScripts
    }

  }

  stackedParamString = {
    mainPlanParamsSetupString
    subplans %>%
      map_chr(
        ~.x$paramsSetupString
      ) -> subplansParamSetupString
    c(
      mainPlanParamsSetupString,
      subplansParamSetupString
    )
  }
  cachePath = file.path(webDirRoot, frontmatter$drake_cache)
  make_vis_elements = {
    c(
      "# make plan -----------------",
      glue::glue("mk_grand")+mainPlan$planname+" = function(cachePath=\""+cachePath+"\")",
      "}",
      stackedParamString,
      "",
      stackedMakecondition,
      "",
      # "mkEnv=rlang::current_env()",
      "library(drake)",
      glue::glue("options(rstudio_drake_cache = storr::storr_rds(\"")+cachePath+"\", hash_algorithm = \"xxhash64\"))",
      glue::glue("make(grand")+mainPlan$planname+", cache=drake::drake_cache(path=cachePath))",
      # afterMakeCodes,
      "}",
      "",
      glue::glue("vis_grand")+mainPlan$planname+" <- function(cachePath=\""+cachePath+"\")",
      "{",
      stackedParamString,
      "",
      stackedMakecondition,
      glue::glue("drake::vis_drake_graph(grand")+mainPlan$planname+", cache=drake::drake_cache(path=cachePath))",
      "}",
      glue::glue("meta_grand")+mainPlan$planname+"=",
      "list(",
      glue::glue("cachePath=\"")+cachePath+"\",",
      "readd=function(t) {",
      glue::glue("drake::readd(t,cache=drake::drake_cache(path=\"")+cachePath+"\"))},",
      "clean=function(t=NULL) {",
      glue::glue("drake::clean(t,cache=drake::drake_cache(path=\"")+cachePath+"\"))})"
    )
  }

  wholeScripts = {
    c(
      grandPlanScript,
      make_vis_elements
    )
  }
  xfun::write_utf8(
    wholeScripts,
    con=paste0("grand",mainPlan$planname,".R")
  )
}
