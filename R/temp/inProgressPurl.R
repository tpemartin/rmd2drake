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
