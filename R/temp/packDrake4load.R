drake_pack4load <- function(path=""){
  drake$source_plan()
  drake$makePlan()
  filename <- rstudioapi::getSourceEditorContext()
  savename <- stringr::str_replace(
    filename$path, "\\.[:alpha:]+$","_drake.Rdata"
  )  
  if(path!=""){
    savename <- file.path(path, basename(savename))
  } else
  if(!is.null(.GlobalEnv$droppath)){
    savename <- file.path(.GlobalEnv$droppath, basename(savename))
  }
  save(drake, file = savename)
    glue::glue('xfun::download_file()
  load("{basename(savename)}")
  ') -> clip4paste
    clipr::write_clip(clip4paste)
    
}

