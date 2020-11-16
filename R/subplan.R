
# helpers -----------------------------------------------------------------

purl_drakePlanOnly <- function(filename, plan_name){
  readLines(filename) -> Rmdlines

  stringr::str_extract(
    basename(filename),"[:graph:]+(?=\\.)") -> filetitle

  frontmatterParams={
    knitr::knit_params(Rmdlines) -> params



    if(length(params)!=0){
      paramsList <- purrr::map(
        params,~purrr::pluck(.x, "value"))

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

  # find drake information
  {
    require(dplyr)
    Rmdlines %>%
      stringr::str_which("(?<=```\\{r )[[:alnum:]_]+") -> whichHasRSetting
    Rmdlines[whichHasRSetting] %>%
      stringr::str_trim(side="both") %>%
      stringr::str_detect("(afterMake=T|drake=F|\\bsetup\\b)") -> pickDrakeF
    whichHasRSetting[!pickDrakeF] -> whichHasDrakeObjects
    Rmdlines[whichHasDrakeObjects] %>%
      stringr::str_extract("(?<=```\\{r )[[:alnum:]_]+") -> drakeObjects
  }


  {
    whichDrakeLineEnds <- vector("integer", length(whichHasDrakeObjects))
    for(.x in seq_along(whichHasDrakeObjects)){
      begin <- whichHasDrakeObjects[[.x]]+1
      end <- ifelse(.x!=length(whichHasDrakeObjects),
                    whichHasDrakeObjects[[.x+1]]-1,
                    length(Rmdlines))
      whichSeq <- begin:end
      Rmdlines[whichSeq] %>% stringr::str_which("^```") %>%
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
    # Rmdlines %>%
    #   get_drakeBody(oneSlice)
    drakeBody <- c()
    makecondition <- c()
    for(.x in 1:nDrakeObjs){
      oneSlice <- drakeLocations[.x,]
      Rmdlines %>%
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


  plan_nameExtract=stringr::str_extract(plan_name,"(?<=/)[[:alnum:]_\\.]+$")
  plan_nameExtract=stringr::str_replace(plan_nameExtract,"\\.R","")
  plan_name0=ifelse(is.na(plan_nameExtract), plan_name, plan_nameExtract)
  plan_name0=stringr::str_replace(plan_name0,".R","")

  # a patch to fix .cacheNew$path not exist
  frontmatterList <- get_frontmatterList(Rmdlines)
  .cacheNew <- list(
    path = frontmatterList$drake_cache
  )

  subplanfilename <- frontmatter$drake_subplans[[.x]]
  subplanfilename <- file.path(
    webDirRoot,
    frontmatter$drake_subplans[[.x]]
  )
  subplanfilepath=
    stringr::str_extract(
      subplanfilename,
      "[[^/][:graph:]]+(?=\\.[Rr][Mm][Dd]")
    ifelse(
      is.na(plan_nameExtract),
      "",
      stringr::str_extract(plan_name,
                           glue::glue("[:graph:]+(?={plan_nameExtract})"))
    )

  drakeScripts %>%
    stringr::str_replace_all("\\{plan_name\\}", plan_name0) %>%
    stringr::str_replace_all("\\{.cacheNew\\$path\\}", .cacheNew$path)->
    drakeScriptsFinal
  xfun::write_utf8(
    drakeScriptsFinal,
    con=
      file.path(
        dirname(filename),
        paste0(
          stringr::str_replace(plan_name,"\\.R",""),
          ".R"
        )
      )
  )
  invisible(drakeScriptsFinal)

}

purl_drakeSubplanOnly <- function(filename, mainPlanCachePath){
  readLines(filename) -> Rmdlines

  stringr::str_extract(
    basename(filename),"[:graph:]+(?=\\.)") -> filetitle

  frontmatterParams={
    knitr::knit_params(Rmdlines) -> params



    if(length(params)!=0){
      paramsList <- purrr::map(
        params,~purrr::pluck(.x, "value"))
      rdsPath <- dirname(filename)
      activePath <- dirname(filename)

      rdsName <- glue::glue(
        "params_{filetitle}.rds")

      rdsfilename <- file.path(
        activePath, rdsName)
      saveRDS(paramsList,
              file=rdsfilename
      )

      paramsSetupString <-
        glue::glue("params=readRDS(\"")+rdsfilename+"\")"

    } else {
      paramsSetupString="# no params in the frontmatter"
    }

    paramsSetupString
  }

  # find drake information
  {
    require(dplyr)
    Rmdlines %>%
      stringr::str_which("(?<=```\\{r )[[:alnum:]_]+") -> whichHasRSetting
    Rmdlines[whichHasRSetting] %>%
      stringr::str_trim(side="both") %>%
      stringr::str_detect("(afterMake=T|drake=F|\\bsetup\\b)") -> pickDrakeF
    whichHasRSetting[!pickDrakeF] -> whichHasDrakeObjects
    Rmdlines[whichHasDrakeObjects] %>%
      stringr::str_extract("(?<=```\\{r )[[:alnum:]_]+") -> drakeObjects
  }


  {
    whichDrakeLineEnds <- vector("integer", length(whichHasDrakeObjects))
    for(.x in seq_along(whichHasDrakeObjects)){
      begin <- whichHasDrakeObjects[[.x]]+1
      end <- ifelse(.x!=length(whichHasDrakeObjects),
                    whichHasDrakeObjects[[.x+1]]-1,
                    length(Rmdlines))
      whichSeq <- begin:end
      Rmdlines[whichSeq] %>% stringr::str_which("^```") %>%
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
    # Rmdlines %>%
    #   get_drakeBody(oneSlice)
    drakeBody <- c()
    makecondition <- c()
    for(.x in 1:nDrakeObjs){
      oneSlice <- drakeLocations[.x,]
      Rmdlines %>%
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

  plan_basename <-
    stringr::str_extract(
      filename,
      "[^/]+(?=\\.[Rr][Mm][Dd]$)"
    )

  # a patch to fix .cacheNew$path not exist
  frontmatterList <- get_frontmatterList(Rmdlines)
  .cacheNew <- list(
    path = frontmatterList$drake_cache
  )

  planfilepath=dirname(filename)
  plan_name0=paste0("plan_",plan_basename)

  drakeScripts %>%
    stringr::str_replace_all("\\{plan_name\\}", plan_name0) %>%
    stringr::str_replace_all("\\{.cacheNew\\$path\\}", mainPlanCachePath)->
    drakeScriptsFinal


  planEnvironmentSetup <- c(
    "# make plan -----------------",
    frontmatterParams,
    "",
    makecondition,
    "")

  drakeScriptsAll <-
    c(
      planEnvironmentSetup,
      drakeScriptsFinal
    )

  xfun::write_utf8(
    drakeScriptsAll,
    con=
      file.path(
        dirname(filename),
        paste0(
          stringr::str_replace(plan_name0,"\\.R",""),
          ".R"
        )
      )
  )

  invisible(
    list(
      makecondition=planEnvironmentSetup,
      plan=drakeScriptsFinal,
      planname= plan_name0,
      targets=drakeObjects
    )
  )

}
get_patternReplacementExp <- function(subplanSingleton, subIndex){
  subplanSingleton$targets -> subplanTargets
  stringr::str_subset(subplanTargets,
                      "makecondition",
                      negate =T) -> subplanTargets
  patternReplacementExp = paste0("sub",subIndex,"_",subplanTargets)
  names(patternReplacementExp) <-
    paste0("\\b",subplanTargets,"\\b")
  patternReplacementExp
}
get_filetitle = function(activeRmd){
  activeRmd %>%
    basename() -> activeRmdBase
  activeRmdBase %>%
    getExtension() -> fileExtension
  stringr::str_remove(
    activeRmdBase,
    glue::glue("\\.{fileExtension}")) -> filetitle
  filetitle
}
# cachePath = file.path(dirname(activeRmd),yml$drake_cache)
# subplansPath = file.path(dirname(activeRmd),yml$drake_subplans)

extract_infoFromFilename <- function(activeRmd){
  frontmatter <- yml <- rmarkdown::yaml_front_matter(activeRmd)
  if(!exists("yml") || !("drake_cache" %in% names(yml))){
    stop(
      "Frontmatter has no drake_cache assigned.")
  }
  # if(!exists("yml") || !("drake_subplans" %in% names(yml))){
  #   stop(
  #     "Frontmatter has no drake child Rmds assigned.
  #     Maybe you should use purlActiveRmd_thenPlanMake")
  # }

  # get main Root, basename, planname, and readLines
  mainRoot <- dirname(activeRmd)
  mainBasename <- basename(activeRmd)
  if(
    stringr::str_detect(mainBasename,"\\.[Rr][Mm][Dd]", negate=T)
  ){
    stop('This is an Rmd File.')
  }
  mainBasenameNoExtension <-
    stringr::str_extract(mainBasename,"[^/]+(?=\\.[Rr][Mm][Dd]$)")

  mainPlanname <- get_planname(mainBasename)

  xfun::read_utf8(
    activeRmd
  ) -> RmdLines

  planDetails <- list(
    RmdLines=RmdLines,
    frontmatter=frontmatter,
    fullname=activeRmd,
    root=mainRoot,
    basename=mainBasename,
    filetitle=mainBasenameNoExtension, #basenameNoExtension
    planname=mainPlanname
  )

  planDetails
}

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

makeup_frontmatter <- function(planDetails){
  # get_frontmatterList(RmdLines) -> frontmatter
  # pad path to full path
  planDetails$frontmatter$drake_cache %>%
    file.path(planDetails$root, .) -> planDetails$frontmatter$drake_cache
  planDetails$frontmatter$drake_subplans %>%
    file.path(planDetails$root, .) -> planDetails$frontmatter$drake_subplans

  paramsWithPath <-
    stringr::str_subset(names(planDetails$frontmatter$params), "[Pp][Aa][Tt][Hh]$")

  for(.x in paramsWithPath){
    planDetails$frontmatter$params[[.x]] %>%
      file.path(planDetails$root,.) -> planDetails$frontmatter$params[[.x]]
  }

  # save ...params_XXX.rds
  saveParamRdsAndGet_paramsSetupString(planDetails
  ) -> planDetails$frontmatter$paramsSetup

  planDetails
}
augment_makecondition <- function(planDetails){
  c(
    planDetails$frontmatter$paramsSetup,
    planDetails$makecondition,
    glue::glue("options(rstudio_drake_cache = storr::storr_rds(\"")+planDetails$frontmatter$drake_cache+"\", hash_algorithm = \"xxhash64\"))") ->
    planDetails$augmentedMakeconditions
  planDetails
}
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
        makecondition <- c(makecondition, oneSliceBody)
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

augment_planScript_make_vis_loadcomponents <- function(planDetails){
  planDetails$makeText = glue::glue("drake::make({planDetails$planname},
                  cache=drake::drake_cache(
                    path=\"{planDetails$frontmatter$drake_cache}\"),...)")
  planDetails$makefunctionText =
    c(
      glue::glue("mk_")+planDetails$planname+
        "= function(...)",
      "{",
      planDetails$augmentedMakeconditions,
      planDetails$makeText,
      "}"
    )

  planDetails$visText=
    glue::glue("drake::vis_drake_graph({planDetails$planname},
                  cache=drake::drake_cache(
                    path=\"{planDetails$frontmatter$drake_cache}\"),...)")
  planDetails$visfunctionText=
    c(
      glue::glue("vis_")+planDetails$planname+
        "= function(...)",
      "{",
      planDetails$augmentedMakeconditions,
      planDetails$visText,
      "}"
    )
  planDetails$loaddText = glue::glue("drake::loadd(...,
                  cache=drake::drake_cache(
                    path=\"{planDetails$frontmatter$drake_cache}\"), envir = .GlobalEnv)")
  planDetails$loaddfunctionText =
    c(
      glue::glue("load_")+planDetails$planname+
        "= function(...)",
      "{",
      planDetails$loaddText,
      "}"
    )

  planDetails$completePlanMkVisScript =
    c(
      planDetails$augmentedMakeconditions,
      planDetails$drakePlanScript,
      planDetails$makefunctionText,
      planDetails$visfunctionText,
      planDetails$loaddfunctionText
    )
  planDetails
}

generate_grandplan <- function(mainplanDetails, subplans){
  require(purrr)
  require(dplyr)
  allPlannames <-
    c(mainplanDetails$planname,
      {
        subplans %>% map_chr(~.x$planname)
      }
    )

  allPlannames %>%
    map(as.symbol) -> allPlanSymbols

  bind_rowExpr <- rlang::expr(bind_rows(!!!allPlanSymbols))

  grandplanBind_rowText <- glue::glue("grandplan") + #mainplanDetails$filetitle+
    "<- "+rlang::expr_text(bind_rowExpr)

  eval(
    parse(text=as.character(grandplanBind_rowText)),
    envir = .GlobalEnv)

  invisible(grandplanBind_rowText)
}
get_grandDrakePlanScript <- function(grand_plan, mainplanname){
  maxrow <- nrow(grand_plan)
  grand_planContent <- vector("character", maxrow)
  for(.x in 1:maxrow){
    endtext = ifelse(.x==maxrow, "",",")
    grand_planContent[[.x]] <-
      paste0(
        grand_plan$target[[.x]]," = ",
        rlang::expr_text(grand_plan$command[[.x]]), endtext
      )
  }
  grand_planScript <- c(
    glue::glue("grand")+mainplanname+" = drake::drake_plan(",
    grand_planContent,
    ")"
  )
  grand_planScript
}
build_grandplanDetails <- function(grand_planScript, mainplanDetails, subplans) {
  list(
    planname = glue::glue("grandplan_") + mainplanDetails$filetitle,
    frontmatter = list(
      drake_cache = mainplanDetails$frontmatter$drake_cache
    ),
    augmentedMakeconditions = unique(c(
      mainplanDetails$augmentedMakeconditions,
      {
        subplans %>%
          map(~ .x$augmentedMakeconditions) %>%
          unlist()
      }
    )),
    drakePlanScript = grand_planScript,
    root=mainplanDetails$root
  )
}
