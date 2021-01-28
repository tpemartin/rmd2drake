
#' Generate drake Rmd from active Rmd (for bookdown writing)
#'
#' @param needMakeconditionHolder A logical default=F meaning no needs for a makecondition code chunk prepared for you.
#' @param removeLabelSuffix A character to represent "{removeLabelSuffix}$" regex pattern, default = "_[0-9]{2}". If not intend for removal, set it to "".
#'
#' @return
#' @export
#'
#' @examples none
generate_drakeRmdFromActiveRmd <- function(needMakeconditionHolder=F, removeLabelSuffix="_[0-9]{2}"){
  require(dplyr)
  require(stringr)
  rmd2drake:::extract_activeEditorFilename()
  .activeFile %>%
    xfun::read_utf8() -> rmdLines

  rmdLines %>%
    get_chunksTable() -> chunkTable

  drakeRmdlines = {
    purrr::map(
      1:nrow(chunkTable),
      ~{
        seq(chunkTable$begin[[.x]]-1, chunkTable$end[[.x]]+1)
      }
    ) -> whichAreCodeChunks
    unlist(whichAreCodeChunks) -> whichAreCodeChunks

    rmdLines %>%
      str_which("^#") -> whichMightBeOutlines
    pick_outlines <- !(whichMightBeOutlines %in% whichAreCodeChunks)
    whichAreOutlines <- whichMightBeOutlines[pick_outlines]

    # browser()
    chunkTable %>%
      filter(!is.na(object) & !str_detect(object, "^(eval|setup)")) -> chunkTableWithLabels
    seq_along(chunkTableWithLabels$object) %>%
      map(
        ~{
          seq(chunkTableWithLabels$begin[[.x]]-1, chunkTableWithLabels$end[[.x]]+1)
        }
      ) -> whichCouldBeDrakePlanTargets
    unlist(whichCouldBeDrakePlanTargets) -> whichCouldBeDrakePlanTargets

    whichAreDrakePlanContents <-
      sort(c(whichAreOutlines, whichCouldBeDrakePlanTargets))
    drakeRmdlines <-
      rmdLines[whichAreDrakePlanContents]

    title = str_remove(basename(.activeFile), ".Rmd")
    # browser()
    makeconditionHolder <- c()
    if(needMakeconditionHolder){
      makeconditionHolder <-
        c("```{r makecondition}\n",
          "```\n\n")
    } else {
      drakeRmdlines %>%
        stringr::str_remove(paste0("(?<=makecondition)",removeLabelSuffix)) -> drakeRmdlines
    }
    drakeRmdlines <- c(
      "---",
      glue::glue('title: "{title}"'),
      glue::glue('drake_cache: ".{title}"'),
      "---\n\n",
      "```{r setup, include=FALSE, drake=F}",
      "knitr::opts_chunk$set(echo = TRUE)",
      "```\n\n",
      makeconditionHolder,
      drakeRmdlines
    )

    # browser()
    # remove bookdown suffix _XX like _01 _02 for different chapters
    if(removeLabelSuffix != ""){
      drakeRmdlines %>%
        stringr::str_remove(
          paste0(removeLabelSuffix,"(?=\\}$)")) %>%
        stringr::str_remove(
          "(?<=makecondition)_[^,]+"
          )-> drakeRmdlines
    }

    drakePlanFolderPath <-
      file.path(
        dirname(.activeFile), "drake_plans"
      )
    if(!dir.exists(drakePlanFolderPath)) dir.create(drakePlanFolderPath)
    xfun::write_utf8(
      drakeRmdlines,
      con=file.path(
        drakePlanFolderPath,
        paste0("drake_", basename(.activeFile))
      )
    )

    drakeRmdlines
  }
}


