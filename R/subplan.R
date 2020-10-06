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
  c(planDetails$makecondition,
    planDetails$frontmatter$paramsSetup) ->
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
