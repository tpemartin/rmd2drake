#' Setup .gitignore to fit R users, also purge those tracked-but-should-be-ignored files
#'
#' @return
#' @export
#'
#' @examples none
set_gitignore <- function(){
  library(gitignore); library(dplyr); library(stringr)
  gi_fetch_templates("R") -> Rtemplate


  # remove existing should-not-be-tracked
  {
    stringr::str_split(Rtemplate, "\n") -> newLines

    newLines[[1]] %>%
      stringr::str_detect("^#", negate = T) -> pickNonComment
    (newLines[[1]]!="") -> pickNonEmpty
    newLines[[1]][pickNonComment & pickNonEmpty] -> ignorePattern

    # mac .DS_Store
    ignorePattern <- c(ignorePattern,".DS_Store","**/.DS_Store")
    # newLines %>% View()

    command2RemoveFile <- 'git rm --cached {file}'
    command2RemoveFolder <- 'git rm -r --cached {folder}'

    for(.x in seq_along(ignorePattern)){
      #.x=1
      if(stringr::str_detect(ignorePattern[[.x]],"/$")){
        folder=ignorePattern[[.x]]
        system(glue::glue(command2RemoveFolder))
      } else {
        file=ignorePattern[[.x]]
        system(glue::glue(command2RemoveFile))
      }
    }
    }

  paste0(Rtemplate, c(".DS_Store","**/.DS_Store"), collapse = "\n")  %>%
  gi_write_gitignore()


}
