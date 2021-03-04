saveDrake2Dropbox <- function(){
  assertthat::assert_that(
    exists("droppath"),
    msg="There is no droppath in .Globalenv which defined where the Rdata of drake will be saved at."
  )
  filename <- rstudioapi::getSourceEditorContext()
  savename <- stringr::str_replace(
    filename$path, "\\.[:alpha:]+$","_drake.Rdata"
  )
  savename <-
    file.path(
      droppath,
      basename(savename)
    )

  save(drake, file=savename)
  message(
    glue::glue("drake in .Globalenv is saved in \n{savename}")
  )
}

openFinderAtDropPath <- function(){
  assertthat::assert_that(
    exists("droppath"),
    msg="There is no droppath in .Globalenv which defined where the Rdata of drake will be saved at."
  )
  system(glue::glue("open {.GlobalEnv$droppath}"))
}

