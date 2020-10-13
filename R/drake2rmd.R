#' Generate drake plan Rmd from active drake plan R script
#'
#' @param removePlanPrefix A logical. default=T will remove 'plan_' from R script filename
#'
#' @return
#' @export
#'
#' @examples none
produceRmdFromActiveRscript <- function(removePlanPrefix=T)
{
  rmd2drake:::extract_activeEditorFilename()
  activeR <- .activeFile
  if(!stringr::str_detect(activeR, "\\.[Rr]$")){
    stop("This is not a R script file.")
  }

  require(purrr)
  require(dplyr)
  # run drake R script
  e <- rlang::env(
    rlang::caller_env()
  )
  source(activeR, local=e)
  ls(envir=e) -> objectnames
  objectnames %>%
    map_lgl(
      ~("drake_plan" %in% class(e[[.x]]))
    ) -> pickDrakeplanObjects
  loc_drakeplans <- which(pickDrakeplanObjects)

  .x= loc_drakeplans[[1]]
  planname = objectnames[[.x]]
  drakeplan = e[[planname]]


  context <- c()
  for(.x in seq_along(drakeplan$target)){
    targetRow <- drakeplan[.x,]
    context <- c(
      context,
      generate_RmdCodeChunkFromOneTarget(targetRow)
    )
  }

  if(any(stringr::str_detect(objectnames, "^mk_"))){

    mkfunctionName <- objectnames[[stringr::str_which(objectnames, "^mk_")]]
    makeFunction <- e[[mkfunctionName]]
    makeBody <- body(makeFunction)

    makeBody %>%
      extract_makeconditionFromMakefunctionBody() %>%
      {c(
        .,
        context
      )} -> context
  }

  destfile <-
    stringr::str_replace(
      activeR,
      "\\.[Rr]$",
      "\\.Rmd"
    )

  if(removePlanPrefix){
    destfile %>%
      stringr::str_remove(
        "(grand)?plan_"
      ) -> destfile
  }

  destfile %>%
    cookFrontmatterFromRmdfilename() -> frontmatter

  context %>%
    {
      c(
        frontmatter,
        "",
        .
      )
    } -> context

  xfun::write_utf8(
    context, con=destfile
  )

  invisible(context)
}

# helpers -----------------------------------------------------------------


cookFrontmatterFromRmdfilename <- function(destfile)
{
  basename(destfile) %>%
    stringr::str_remove("\\.Rmd") -> title
  frontmatter <-
    c(
      "---",
      paste0("title: \"",title,"\""),
      paste0("drake_cache: \".",title,"\""),
      "---"
    )
  frontmatter
}


extract_makeconditionFromMakefunctionBody <-
  function(makeBody){
    makeBody %>%
      convert_expr2string() %>%
      {.[2:(length(.)-1)]} -> contentLines
    loc_drakeMake <- stringr::str_which(contentLines,"drake::make")

    c("# makecondition",
      "",
      "```{r makecondition}",
      contentLines[1:(loc_drakeMake-1)],
      "```",
      ""
    ) %>%
      stringr::str_trim(side="left")
  }

convert_expr2string <- function(expr_){
  expr_ %>%
    rlang::expr_text() %>%
    stringr::str_split("\\n") %>%
    unlist()
}

generate_RmdCodeChunkFromOneTarget <- function(targetRow){
  require(dplyr)
  c(
    paste0("## ", targetRow$target),
    "",
    paste0("```{r ",targetRow$target,"}"),
    paste0(targetRow$target, " = {"),
    {
      targetRow$command %>%
        rlang::expr_text() %>%
        stringr::str_split("\\n") %>%
        unlist() -> code_text
      code_text[2:(length(code_text)-1)]
    },

    paste0("}"),
    "```",
    ""
  )

}

