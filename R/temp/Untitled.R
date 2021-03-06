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


# helps -------------------------------------------------------------------


purl_drakeSubplanOnly2 <- function(planDetails){

  # find drake information
  {
    require(dplyr)
    planDetails$RmdLines %>%
      stringr::str_which("(?<=```\\{r )[[:alnum:]_]+") -> whichHasRSetting
    planDetails$RmdLines[whichHasRSetting] %>%
      stringr::str_trim(side="both") %>%
      stringr::str_detect("(afterMake=T|drake=F|\\bsetup\\b)") -> pickDrakeF
    whichHasRSetting[!pickDrakeF] -> whichHasDrakeObjects
    planDetails$RmdLines[whichHasDrakeObjects] %>%
      stringr::str_extract("(?<=```\\{r )[[:alnum:]_]+") -> drakeObjects
  }


  {
    whichDrakeLineEnds <- vector("integer", length(whichHasDrakeObjects))
    for(.x in seq_along(whichHasDrakeObjects)){
      begin <- whichHasDrakeObjects[[.x]]+1
      end <- ifelse(.x!=length(whichHasDrakeObjects),
                    whichHasDrakeObjects[[.x+1]]-1,
                    length(planDetails$RmdLines))
      whichSeq <- begin:end
      planDetails$RmdLines[whichSeq] %>% stringr::str_which("^```") %>%
        whichSeq[.] %>%
        min() -> whichDrakeLineEnds[[.x]]
    }

    tidyr::tibble(
      object=drakeObjects,
      begin=whichHasDrakeObjects+1,
      end=whichDrakeLineEnds-1
    ) -> drakeLocations
  }



  # define drake body function
  nDrakeObjs <- nrow(drakeLocations)
  {
    require(dplyr)
    # drakeLocations %>%
    #   slice(.x) -> oneSlice
    # planDetails$RmdLines %>%
    #   get_drakeBody(oneSlice)
    drakeBody <- c()
    makecondition <- c()
    for(.x in 1:nDrakeObjs){
      oneSlice <- drakeLocations[.x,]
      planDetails$RmdLines %>%
        get_drakeBody(oneSlice) -> oneSliceBody
      oneSliceBody[[1]] %>%
        stringr::str_replace("<-","=") -> oneSliceBody[[1]]
      if(oneSlice$object=="makecondition"){
        makecondition <- oneSliceBody
        next
      }

      oneSliceBody %>%
        stringr::str_which("^#", negate=T) -> whichAreCommands # not comment
      oneSliceBody[whichAreCommands] -> oneSliceBody
      oneSliceBody %>%
        length() -> lastWhich
      oneSliceBody[[lastWhich]] =
        ifelse(
          .x!=nDrakeObjs,
          oneSliceBody[[lastWhich]] %>%
            paste0(.,","), #str_replace("\\}$","\\},"),
          oneSliceBody[[lastWhich]]
        )
      targetSlice <-
        c(
          glue::glue("# >> {oneSlice$object}--------------"),
          oneSliceBody,
          ""
        )
      drakeBody <- c(
        drakeBody,
        targetSlice
      )

    }

  }

  # produce drake R script
  {
    prefix <- c(
      "# {plan_name}------------",
      "{plan_name}=drake::drake_plan(",
      "# > plan begins -----------"
    )
    suffix <- c(
      "# > plan ends ------------",
      ")",
      ""
    )

    # assemble
    drakeScripts <-
      c(
        prefix,
        drakeBody,
        suffix
      )
  }

  plan_basename <- planDetails$filetitle

  # a patch to fix .cacheNew$path not exist
  .cacheNew <- list(
    path = planDetails$frontmatter$drake_cache
  )

  planfilepath= planDetails$root
  plan_name0=paste0("plan_",plan_basename)

  drakeScripts %>%
    stringr::str_replace_all("\\{plan_name\\}", plan_name0) %>%
    stringr::str_replace_all("\\{.cacheNew\\$path\\}", planDetails$frontmatter$drake_cache)->
    drakeScriptsFinal


  planEnvironmentSetup <- c(
    "# make plan -----------------",
    planDetails$frontmatter$paramsSetup,
    "",
    makecondition,
    "")

  planDetails$makecondition <- makecondition
  planDetails$drakePlanScript <- drakeScriptsFinal

  drakeScriptsAll <-
    c(
      planEnvironmentSetup,
      drakeScriptsFinal
    )

  xfun::write_utf8(
    drakeScriptsAll,
    con=
      file.path(
        planDetails$root,
        paste0(
          planDetails$planname,
          ".R"
        )
      )
  )

  invisible(
    planDetails
  )

}
