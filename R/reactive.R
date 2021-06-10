#' Create Reactive script to clipboard
#'
#' @return
#' @export
#'
#' @examples None
create_reactiveClip <- function(){
  assertthat::assert_that(
    exists("drake"),
    "There is no drake."
  )
  assertthat::assert_that(
    !is.null(drake$.planEnvironment),
    "Please run drake$source_plan() first."
  )
  clipr::write_clip(
    create_reactiveScript()
  )
}

#' Create Reactive script from drake
#'
#' @return
#' @export
#'
#' @examples None
create_reactiveScript <- function(){
  filename = basename(.GlobalEnv$drake$activeRmd$filenames)
  planname =
    paste0("plan_", stringr::str_extract(filename, ".+(?=\\.)"))
  assertthat::assert_that(!is.null(.GlobalEnv$drake$.planEnvironment[[planname]]),
    msg="Drake plan does not exist. Please source_plan and makePlan.")

  drakePlan = .GlobalEnv$drake$.planEnvironment[[planname]]

  generate_reactExpressionsFromDrakePlan(drakePlan) -> reactExprs
  purrr::map_chr(
    reactExprs,
    ~{
      paste0(deparse(.x), collapse="\n")
    }
  ) -> reactScripts
  return(reactScripts)
}

generate_reactExpressionsFromDrakePlan <- function(drakePlan){
  targets <- drakePlan$target
  list_exprs <- vector("list", nrow(drakePlan))
  for(.i in 1:nrow(drakePlan)){
    list_exprs[[.i]] <- convert2reactExpression(targets, drakePlan[.i,])
  }
  return(list_exprs)
}

convert2reactExpression <- function(targets, drakePlanX){
  # drakePlanX = drakePlan[4,]
  commandX <- drakePlanX$command[[1]]
  targetX <- drakePlanX$target[[1]]
  commandX <-
    remove_duplicateTargetAssignment(targetX, commandX)
  commandX_react <-
    addReactParenthesis(targets, commandX)
  reactExprX <- rlang::parse_expr(paste0(targetX," <- reactive(",commandX_react,")"))

  return(reactExprX)
}

remove_duplicateTargetAssignment <- function(targetX, commandX){
  commandX_deparse <- deparse(commandX)
  if(length(commandX_deparse)==1) return(commandX)
  secondExpr <- stringr::str_remove_all(commandX_deparse[[2]],"\\s")
  pattern = paste0(targetX,"(<-|=)\\{")
  if(stringr::str_detect(secondExpr, pattern)) commandX_deparse <-
    commandX_deparse[ - c(2, length(commandX_deparse)-1)]

  commandX <- rlang::parse_expr(paste0(commandX_deparse, collapse="\n"))
  return(commandX)
}
addReactParenthesis <- function(targets, commandX){
  commandX_deparse <- paste0(deparse(commandX), collapse="\n")
  for(.x in targets){
    pattern = .x
    replacement = paste0(.x,"()")
    stringr::str_replace_all(commandX_deparse, pattern, replacement) -> commandX_deparse
  }
  return(commandX_deparse)
}
