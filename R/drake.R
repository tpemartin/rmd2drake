###
#' Purl active Rmd into a drake plan R script, named "plan_{activeRmdName}.R", then make the plan.
#'
#' @return
#' @export
#'
#' @examples purlActiveRmd_thenPlanMake()
purlActiveRmd_thenPlanMake <- function(){
  require(dplyr)
  # rstudioapi::getSourceEditorContext() -> activeSource
  # activeSource$path -> activeRmd
  extract_activeEditorFilename()
  activeRmd <- .activeFile
  yml <- rmarkdown::yaml_front_matter(activeRmd)
  if(!exists("yml") || !("drake_cache" %in% names(yml))){
    warning(
      "Frontmatter has no drake_cache assigned.")
    return()
  }
  generate_hiddenCacheNew(yml)
  # cacheNew = drake::new_cache(path=yml$drake_cache)

  # normalizePath(activeRmd) -> activeRmd
  # stringr::str_remove(activeRmd, rootPath) ->
  #   html2open
  webDirRoot <- dirname(activeRmd)
  activeRmdBase <- basename(activeRmd)
  drakePlanname <-
    paste0("plan_",
           stringr::str_remove(activeRmdBase,"\\.[rR][mM][dD]$"))
  purl_drakePlan(activeRmd, drakePlanname)
  drakefilename <-
    file.path(
      webDirRoot,paste0(drakePlanname,".R")
    )
  source(drakefilename)
  makeName <-
    paste0(
      "mk_",drakePlanname
    )
  do.call(makeName, list())

}

###
#' visualize active Rmd's "plan_{activeRmdName}"
#'
#' @return
#' @export
#'
#' @examples purlActiveRmd_thenPlanMake()
visActiveRmd_plan <- function(){
  require(dplyr)
  require(drake)
  # rstudioapi::getSourceEditorContext() -> activeSource
  # activeSource$path -> activeRmd
  extract_activeEditorFilename()
  activeRmd <- .activeFile
  activeRmd %>%
    basename() %>%
    stringr::str_remove(".[a-zA-Z]+$") %>%
    paste0("vis_plan_",.) -> visPlanName

  if(!exists(visPlanName)){
    purlActiveRmd_thenPlanMake()
  }

  visPlanName %>%
    call() %>%
    eval()

}

#' Purl Rmd to a drake plan R script
#'
#' @description All R chunks with chunk names without drake=F will be purled to
#' a Drake plan R script.
#'
#' @param filename A character defines the file path name of Rmd
#' @param plan_name A character defines the R script name to be plan_name.R
#'
#' @return invisible. A character of all R script lines
#' @export
#'
#' @examples none
purl_drakePlan <- function(filename, plan_name){
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

  # {
  #   Rmdlines[whichHasRSetting] %>%
  #     stringr::str_trim(side = "both") %>%
  #     stringr::str_detect("\\bsetup\\b") -> isSetup
  #   if(any(isSetup)){
  #     whichHasRSetting[isSetup] -> whichIsSetup
  #     setupLines <- c()
  #     count=0; max_count=100;
  #     isNotEnd=T
  #     while(isNotEnd && count < max_count){
  #       count=count+1
  #
  #       setupLines <-
  #         c(setupLines,
  #           Rmdlines[[whichIsSetup+count]])
  #       isNotEnd <-
  #         stringr::str_detect(
  #           Rmdlines[[whichIsSetup+count+1]],
  #           "^```",
  #           negate = T
  #         )
  #     }
  #     setupExpression <-
  #       parse(
  #         text=paste(setupLines, collapse = "\n")
  #       )
  #
  #     eval(setupExpression, envir = .GlobalEnv)
  #   }
  #
  # }
  # define drake block begins and ends
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
  # afterMake=t
  {
    Rmdlines %>% find_afterMake() ->
      whichHasAfterMake
    whichHasAfterMake %>%
      extract_codeChunksFromTheirStartingTicks(Rmdlines, .) -> afterMakeCodes
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
    makePlan <- c(
      "# make plan -----------------",
      "mk_{plan_name} = function(cachePath=\"{.cacheNew$path}\"){",
      frontmatterParams,
      "",
      makecondition,
      "",
      # "mkEnv=rlang::current_env()",
      "library(drake)",
      "options(rstudio_drake_cache = storr::storr_rds(\"{.cacheNew$path}\", hash_algorithm = \"xxhash64\"))",
      "make({plan_name}, cache=drake::drake_cache(path=cachePath))",
      afterMakeCodes,
      "}",
      "",
      "vis_{plan_name} <- function(cachePath=\"{.cacheNew$path}\"){",
      frontmatterParams,
      "",
      makecondition,
      "drake::vis_drake_graph({plan_name}, cache=drake::drake_cache(path=cachePath))",
      "}",
      "meta_{plan_name}=",
      "list(",
      "cachePath=\"{.cacheNew$path}\",",
      "readd=function(t) {
  drake::readd(t,cache=drake::drake_cache(path=\"{.cacheNew$path}\"))},",
      "clean=function(t=NULL) {
  drake::clean(t,cache=drake::drake_cache(path=\"{.cacheNew$path}\"))})",
      ""
    )


    # assemble
    drakeScripts <-
      c(
        prefix,
        drakeBody,
        suffix,
        makePlan
      )
  }


  plan_nameExtract=stringr::str_extract(plan_name,"(?<=/)[[:alnum:]_\\.]+$")
  plan_nameExtract=stringr::str_replace(plan_nameExtract,"\\.R","")
  plan_name0=ifelse(is.na(plan_nameExtract), plan_name, plan_nameExtract)
  plan_name0=stringr::str_replace(plan_name0,".R","")
  planfilepath=
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
  writeLines(
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


#' Create source_plan function source file under planPath
#'
#' @return A function that source an R script under planPath
#' @export
#'
#' @examples # # NOT RUN. produce error
#' # create_source_plan()
#'
#' planPath ="/Users/martin/Github/course-HS-society-and-citizen/plans"
#' Create Rmd template that can generate drake plan scripts
#'
#' @param planname A character.
#' @param title A character. (if null, default=planname)
#' @param root A chracter. (if null, default=getwd())
#'
#' @return
#' @export
#'
#' @examples none
create_planRmd <- function(planname, title=NULL, root=NULL){

  # readLines(
  #   "/Users/martin/Github/econDV/document/planRmdTemplate.Rmd"
  # ) -> planRmdTemplate
  #
  # usethis::use_data(planRmdTemplate,internal=T, overwrite=T)

  library(dplyr)
  planRmdTemplate %>%
    stringr::str_replace_all(
      c("\\{title\\}"=ifelse(is.null(title),planname,title),
        "\\{planname\\}"=planname,
        "\\{filename\\}"=
          ifelse(is.null(root),
                 file.path(getwd(),
                           paste0(planname,".Rmd")),
                 file.path(root,
                           paste0(planname,".Rmd")
                 )))
    ) -> myRmdLines

  writeLines(
    myRmdLines,
    paste0(planname,".Rmd")
  )
}

#' source_plan <- create_source_plan()
#'
create_source_plan <- function(){
  stopifnot(
    "Missing planPath. Please created the definition object planPath."=
      exists("planPath", envir = globalenv())
  )
  source_functional(planPath)
}

#' List makes (future) in your global environment
#'
#' @return character string of promises of make/mk_plan...
#' @export
#'
#' @examples none
list_makes <- function(){
  ls(envir = globalenv()) %>%
    str_subset("^(mk|make)")
}
#' List available plans in your global environment
#'
#' @return A character string of drake plans
#' @export
#'
#' @examples none
list_plans <- function(){
  ls(envir = globalenv()) %>%
    str_subset("^plan")
}

#' List all files under plans folder
#'
#' @return A character string of all files
#' @export
#'
#' @examples none
list_plan_files <- function(){
  rprojroot::is_rstudio_project-> pj
  pj$make_fix_file() -> myroot
  planFolder=file.path(myroot(),"plans")
  stopifnot(
    "Folder plans does not exist"=
      dir.exists(
        planFolder
      )
  )
  list.files(
    path = planFolder
  )
}

#' Send a drake plan wrapper to clipboard to paste
#'
#' @return
#' @export
#'
#' @examples none
clip_planWrapper <- function(){
  clipr::write_clip(econDV::drake_plan_wrapper)
}


# helpers -----------------------------------------------------------------

source_functional <- function(path){
  function(filename){
    source(
      file.path(path,filename)
    )
  }
}
extract_activeEditorFilename <- function(){
  activeSource <- rstudioapi::getSourceEditorContext()
  .activeFile <<- activeSource$path
  if(activeSource$path==''){
    warning("Target Rmd file hasn't been saved yet. No path to it will be found. Please save your Rmd and try again")
  }
}
get_drakeBody = function(Rmdlines, oneSlice){
  require(dplyr)
  oneSlice %>%
    {Rmdlines[(.$begin[[1]]:.$end[[1]])]} -> scriptBlock
  scriptBlock %>%
    stringr::str_remove_all("\\s") %>%
    {which(.!="")} %>%
    max() -> whichTargetEnds
  targetBody <- scriptBlock[1:whichTargetEnds]
  targetBody[[whichTargetEnds]] %>%
    stringr::str_remove_all("\\s") -> targetBody[[whichTargetEnds]]
  targetBody
}
generate_hiddenCacheNew = function(yml){
  ymlCachePath = yml$drake_cache
  # if(basename(.cacheNew$path) == ymlCachePath){
  #   # no need to create new cache
  # } else {
  #   .cacheNew <<-
  #     drake::new_cache(path=yml$drake_cache)
  # }
  if(
    !exists(".cacheNew") ||
    basename(.cacheNew$path) != ymlCachePath
  ) {
    .cacheNew <<-
      drake::new_cache(path=yml$drake_cache)
  }
}
find_afterMake <- function(Rmdlines){
  require(dplyr)
  Rmdlines %>%
    stringr::str_which("(?<=```\\{r )[[:alnum:]_]+") -> whichHasRSetting
  Rmdlines[whichHasRSetting] %>%
    stringr::str_trim(side="both") %>%
    stringr::str_detect("afterMake=T") -> pickAfterMake
  whichHasRSetting[pickAfterMake] -> whichHasAfterMakes
  whichHasAfterMakes
}

extract_codeChunksFromTheirStartingTicks <- function(RmdLines, startingTickPositions){
  allChunks <- c()
  for(.x in seq_along(startingTickPositions)){
    newChunk <- get_aChunk(RmdLines, startingTickPositions[[.x]])
    allChunks <- c(
      allChunks,
      newChunk
    )

  }
  allChunks
}

get_aChunk <- function(RmdLines, start){
  require(dplyr)
  RmdLines[[start]] %>%
    stringr::str_replace("^```","###") ->
    chunk
  count=1; max_count=100; isNotEnd=T;
  while(isNotEnd && count <= max_count){
    newLine <-
      RmdLines[[start+count]]
    isNotEnd <-
      stringr::str_detect(newLine,
                          "^```", negate = T)
    newLine = ifelse(isNotEnd,
           newLine,
           newLine %>%
             stringr::str_replace("^```","###"))
    chunk <- c(
      chunk,
      newLine
    )
    count=count+1
  }
  chunk
}
