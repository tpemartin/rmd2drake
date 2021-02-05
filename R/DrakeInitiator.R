Drake <- function(){
  drake <- new.env()
  drake$activeRmd <- list()
  rmd2drake:::extract_activeEditorFilename()
  drake$activeRmd$filenames <- .GlobalEnv$.activeFile
  drake$activeRmd$lines <-
    xfun::read_utf8(drake$activeRmd$filenames)

    drake$activeRmd$codeChunkTable <-
    drake_get_rmdlinesTable(
      drake$activeRmd$lines
    )

  drake_rmdlines_autopsy(drake)

  drake$process2get <- list(
    codes = list()
  )
  drake$process2get$codes <- {


    what2pick <- c("makecondition", "drake=F", "r", "[^r\\=]+")
    picks <- drake_generatePicks(drake$activeRmd$autopsy$head_info,
                                 what2pick)
    makeconditions = {
      targetContents <-
        drake$activeRmd$autopsy$content[picks$makecondition & !picks$`drake=F` & picks$r]
      drake_extractCodes(targetContents)
    }
    drakeTargetContents = drake_extraceTargetContents(drake, picks)

    list(
      makecondition=makeconditions,
      drakeTargetContent=drakeTargetContents
    )
  }
  drake_setup_drakeCacheOptions4storr_rds(drake)



  drake$process2get$codes$drakeTargetContent_standardized <-
    drake_standardizeTargetContents(
      drake$process2get$codes$drakeTargetContent
    )

  drake$process2get$planLegos <-
    drake_generatePlanLegos(drake)

  drake$source_plan <- function(){
    planEnvironment <- new.env(parent = drake)
    # evaluate makecondition
    if(!is.null(drake$process2get$codes$makecondition)){
      eval(parse(text=drake$process2get$codes$makecondition),
         envir = planEnvironment)
    }
    # evaluate storr_rds options
    eval(parse(text=drake$process2get$storr_rdsOptions),
    envir = planEnvironment)
    # evaluate drake plan
    eval(parse(text=drake$process2get$planLegos),
         envir = planEnvironment)
    drake$.planEnvironment <- planEnvironment

    drake$makePlan <- drake_generateMakeplanFunction(drake)
  }
  drake$source_planExport <- drake_generatePlanExportFunction(drake)


  drake$.updateCache <- updateCache(drake)
  # drake
  .GlobalEnv$drake <- drake
  invisible(drake)
}



# helpers -----------------------------------------------------------------


drake_get_rmdlinesTable <- function(rmdlines){
  stringr::str_which(
    rmdlines,
    "^```"
  ) -> whichStartsWith3Ticks
  oddIndices <- seq(from=1, to=length(whichStartsWith3Ticks), by=2)
  whichStartsWith3Ticks_odd <- whichStartsWith3Ticks[oddIndices]

  stringr::str_extract_all(
    rmdlines[whichStartsWith3Ticks_odd],
    "\\b[0-9a-zA-Z\\=_\\.]+\\b"
  ) -> whatAreChunkEngine_Labels_Options

  tibble::tibble(
    start=whichStartsWith3Ticks_odd,
    end=seq(2, length(whichStartsWith3Ticks), by=2),
    engine_label_option=whatAreChunkEngine_Labels_Options,
    content=rmdlines[whichStartsWith3Ticks_odd]
  )
}
drake_rmdlines_autopsy <- function(drake){
  seq_along(drake$activeRmd$lines) -> line_seq


  cut(
    line_seq,
    breaks=
      c(-Inf, drake$activeRmd$codeChunkTable$start, Inf),
    right = F
  ) -> line_groups

  purrr::map(
    seq_along(levels(line_groups)),
    ~{
      targetGroup <- levels(line_groups)[[.x]]
      subset(
        drake$activeRmd$lines, line_groups==targetGroup
      )
    }
  ) -> list_autopsy
  drake$activeRmd$autopsy$content <-
    setNames(
      list_autopsy, levels(line_groups)
    )

  drake$activeRmd$autopsy$head_info <-
    append(
      list(""),drake$activeRmd$codeChunkTable$engine_label_option
    )
}
drake_extractCodes <- function(targetContents){
  purrr::map(
    seq_along(targetContents),
    ~{
      XcodeInterval <- stringr::str_which(targetContents[[.x]], "^```")
      Xcodes <- targetContents[[.x]][XcodeInterval[[1]]:XcodeInterval[[2]]]
      subset(Xcodes, stringr::str_detect(Xcodes, "^```", negate = T))
    }
  ) -> list_codes
  unlist(list_codes)
}
drake_generatePicks <- function(list_chr, what2pick){

  picks <-
    setNames(
      vector("list", length(what2pick)),
      what2pick
    )
  for(eachWhat2pick in what2pick)
  {
    purrr::map_lgl(
      list_chr,
      ~(length(.x)!=0 && any(stringr::str_detect(.x, eachWhat2pick)))
    ) -> picks[[eachWhat2pick]]
  }
  return(picks)
}
drake_extraceTargetContents <- function(drake, picks){
  whichIsR_labelled_noDrakeF_notMakecondition <-
    which(picks[["[^r\\=]+"]] & picks$r & !picks$makecondition & !picks$`drake=F`)
  purrr::map(
    seq_along(whichIsR_labelled_noDrakeF_notMakecondition),
    ~{
      stringr::str_subset(
        drake$activeRmd$autopsy$head_info[whichIsR_labelled_noDrakeF_notMakecondition[[.x]]][[1]],
        "(=|\\br\\b)", negate=T
      )
    }
  ) -> list_drakeTargets

  whichHasDrakeTarges <- which(
    purrr::map_lgl(list_drakeTargets, ~{length(.x)!=0}))

  whichIsDrakeTarget <-
    whichIsR_labelled_noDrakeF_notMakecondition[whichHasDrakeTarges]
  whatIsDrakeTarget <-
    list_drakeTargets[whichHasDrakeTarges]
  drakeTargets <-
    purrr::map(
      seq_along(whichIsDrakeTarget),
      ~drake_extractCodes(drake$activeRmd$autopsy$content[whichIsDrakeTarget[[.x]]])
    )
  setNames(drakeTargets, whatIsDrakeTarget)
}
drake_setup_drakeCacheOptions4storr_rds <- function(drake){
  drake$activeRmd$frontmatter <-
    rmarkdown::yaml_front_matter(
      drake$activeRmd$filenames
    )

  if(is.null(drake$activeRmd$frontmatter$drake_cache)){
    drake$activeRmd$frontmatter$drake_cache <- "drake_temp"
    drake$activeRmd$frontmatter$drake_cache <-
      file.path(
        dirname(drake$activeRmd$filenames),
        drake_cache
      )
    message('No drake_cache in frontmatter. Use "drake_temp" as the cache folder name')
  } else {
    drake$activeRmd$frontmatter$drake_cache <-
      file.path(
        dirname(drake$activeRmd$filenames),
        drake$activeRmd$frontmatter$drake_cache
      )
  }

  options <-
    glue::glue('options(rstudio_drake_cache = storr::storr_rds(\"{drake$activeRmd$frontmatter$drake_cache}\", hash_algorithm = "xxhash64"))')

  drake$process2get$storr_rdsOptions <- options
}
drake_standardizeTargetContents <- function(targetContents){

  targets <- names(targetContents)
  purrr::map(
    seq_along(targetContents),
    ~{
      Xtarget <- targets[[.x]]
      XtargetContentS <- targetContents[[.x]]
      Xexpression <- parse(text=targetContents[[.x]])
      #
      if(length(Xexpression)==1L){
        stringr::str_remove_all(XtargetContentS[[1]],"\\s") -> XtargetContent
        # in XXX={ format?
        stringr::str_detect(
          XtargetContent, "^[0-9a-zA-Z\\._]+=\\{?"
        ) -> flag_hasTarget
        if(!flag_hasTarget){
          XtargetContentS <- c(
            paste0(Xtarget, "={"),
            XtargetContentS,
            "}"
          )
        }
      } else {
        # not as a programming block; hence, no target = {
        XtargetContentS <- c(
          paste0(Xtarget, "={"),
          XtargetContentS,
          "}"
        )
      }
      XtargetContentS
    }
  ) -> list_targetContents
}
drake_generatePlanLegos <- function(drake) {

  drakeTargetContents_std <- drake$process2get$codes$drakeTargetContent_standardized
  nTargets <- length(drakeTargetContents_std)
  purrr::map(
    seq_along(drakeTargetContents_std),
    ~{
      subset(
        drakeTargetContents_std[[.x]],
        drakeTargetContents_std[[.x]]!="") -> lines2keep
      if(.x!=nTargets){
        whichIsLast <- length(lines2keep)
        lines2keep[[whichIsLast]] <-
          paste0(
            lines2keep[[whichIsLast]], ","
          )
      }
      lines2keep
    }) -> drakePlanLegobody

  planBasename <-
    stringr::str_extract(basename(drake$activeRmd$filenames),"[^\\.]+")

  unlist(
    c(glue::glue("plan_{planBasename} <- "),
      "drake::drake_plan(",
      drakePlanLegobody,
      ")"
    )) -> drakePlanLegos
  drakePlanLegos
}
drake_generatePlanExportFunction <- function(drake){
  function(filename=NULL){
    c(
      drake$process2get$codes$makecondition,
      drake$process2get$storr_rdsOptions,
      drake$process2get$planLegos
    ) -> scriptLines

    if(is.null(filename)){
      planBasename <- stringr::str_extract(
        basename(drake$activeRmd$filenames),
        "[^\\.]+"
      )
      filename <- file.path(
        dirname(drake$activeRmd$filenames),
        paste0("drake_plan_",planBasename,".R"))
    }

    xfun::write_utf8(
      scriptLines,
      con=filename
    )
  }

}
drake_generateMakeplanFunction <- function(drake){
  function(){
    # # evaluate makecondition
    # eval(parse(text=drake$process2get$codes$makecondition),
    #      envir=drake$.planEnvironment)
    # # evaluate storr_rds options
    # eval(parse(text=drake$process2get$storr_rdsOptions),
    #      envir=drake$.planEnvironment)
    # evaluate drake plan
    drake$load <- drake_generateLoadFunction(drake)

    # browser()
    targets <- names(drake$process2get$codes$drakeTargetContent)
    # drake$loadTarget <-
    #   setNames(
    #     vector("list", length(targets)),
    #     targets
    #   )
    drake$loadTarget <-
      setNames(
        purrr::map(
          targets,
          ~drake_generateLoadCall(drake, .x)
        ), targets
      )

    planBasename <-
      stringr::str_extract(basename(drake$activeRmd$filenames),"[^\\.]+")
    planname <- rlang::sym(glue::glue("plan_{planBasename}"))
    rlang::expr(
      drake::make(
        !!planname,
        cache=drake::drake_cache(path=!!drake$activeRmd$frontmatter$drake_cache))
    ) -> exprMakeplan
    eval(exprMakeplan, envir=drake$.planEnvironment)


  }
  }
drake_generateLoadFunction <- function(drake){
  function(...){
    planBasename <-
      stringr::str_extract(basename(drake$activeRmd$filenames),"[^\\.]+")
    planname <- rlang::sym(glue::glue("plan_{planBasename}"))
    group_vars <- rlang::enquos(...)
    rlang::expr(
      drake::loadd(
        !!!group_vars,
        cache=drake::drake_cache(path=!!drake$activeRmd$frontmatter$drake_cache),
        envir = .GlobalEnv)
    ) -> exprLoadplan

    eval(exprLoadplan, envir=drake$.planEnvironment)

  }
}

drake_generateLoadCall <- function(drake, target){
  function(){
    rlang::expr(
      drake::loadd(
        !!target,
        cache=drake::drake_cache(path=!!drake$activeRmd$frontmatter$drake_cache),
        envir = .GlobalEnv)
    ) -> exprLoadplan

    eval(exprLoadplan, envir = rlang::caller_env())

  }

}

updateCache <- function(drake){
  function (newCache = ".temp")
  {
    # newCache = ".temp"
    newCachePath <- eval(parse(text=glue::glue("file.path(.root(),\"{newCache}\")")))
    storr_rdsOptions <- drake$process2get$storr_rdsOptions
    drake$process2get$storr_rdsOptions <-
      stringr::str_replace(storr_rdsOptions,
                           "(?<=(storr_rds\\(\"))[\"[:graph:]]+(?=\",)", glue::glue("{newCachePath}"))
    drake$activeRmd$frontmatter$drake_cache <- newCachePath

    if(newCache==".temp"){
      message(
      glue::glue("drake cache is set to {newCachePath}.\nIf you want other folder than \"/.temp\", reload the drake Rdata and run \ndrake$.updateCache(newCache=\"your_cache_folder_name\")")
    )}
  }
}


