


  planSingletonExpr <- str2expression(grandPlanscript)
  eval(
    planSingletonExpr,
    env = .GlobalEnv
  )




  ###### Make separate plans

  planSingleton <- unlist(purrr::flatten(allPlans[[2]]))
  planSingletonExpr <- str2expression(planSingleton)
  planCombo <-

  planSingleton <- unlist(purrr::flatten(allPlans[[2]]))
  planSingletonExpr <- str2expression(planSingleton)
  eval(
    planSingletonExpr,
    env = .GlobalEnv
  )
  ######
  make(plan_login, cache=drake::drake_cache(path=cachePath))


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

## extract main's child yaml
{

}

## purl child Rmd's plan only

{

  childDocument = purl_drakePlanOnly(childfilename)
  ## NOT GOOD. makeup_childRmd(childfilename) # prefix-target as well
  ## should supply a readd function for our purpose
}

## purl main Rmd's plan
{

  mainDocument = purl_
}

## combine all plans

## produce mk and vis



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

  addMakeVis = {
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
  }


  plan_nameExtract=stringr::str_extract(plan_name,"(?<=/)[[:alnum:]_\\.]+$")
  plan_nameExtract=stringr::str_replace(plan_nameExtract,"\\.R","")
  plan_name0=ifelse(is.na(plan_nameExtract), plan_name, plan_nameExtract)
  plan_name0=stringr::str_replace(plan_name0,".R","")

  browser()
  # a patch to fix .cacheNew$path not exist
  frontmatterList <- get_frontmatterList(Rmdlines)
  .cacheNew <- list(
    path = frontmatterList$drake_cache
  )

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

prefix="ch1";
'plan_z=drake::drake_plan(
  a={},
  b={a
    c},
  d={
    f(a)
  }
)'-> planLines
planfilename <- "R/plan_z.R"
prefix_childPlan <- function(planfilename, prefix){
  require(dplyr)
  readLines(planfilename) -> planLines
  planname = basename(planfilename) %>%
    stringr::str_remove("\\.R$")

  eval(parse(text=planLines), envir = .GlobalEnv)

}
