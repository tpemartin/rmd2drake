#
# file.edit("/Users/martinl/Github/rmd2drake/drake_03_dateTime.Rmd"
# )
# Rmdlines = xfun::read_utf8(
#   "/Users/martinl/Github/rmd2drake/drake_03_dateTime.Rmd"
# )
# engine = "r"
# requireLabel=F
# exclude = "(afterMake=T|drake=F|\\bsetup\\b)" # regex for chunk exclusion

#' Get chunk labels start end position in Rmdlines
#'
#' @param Rmdlines character. lines extracted from readLines
#' @param engine character. default="r"
#' @param requireLabel logical. default=T. extracted only those chunks with a labels.
#' @param exclude character. Regex represents pattern in starting ticks. Match will be ignored.
#'
#' @return A data frame table
#' @export
#'
#' @examples None.
get_chunksTable <- function(Rmdlines, engine = "r",
                            requireLabel=F,
                            exclude = "(afterMake=T|drake=F|\\bsetup\\b)" # regex for chunk exclusion
){
  require(dplyr)
  # define engine target pattern
  {
    if(requireLabel){
      pattern <- glue::glue("(?<=```\\{<<engine>> )[[:alnum:]_]+", .open="<<", .close=">>")
    } else {
      pattern <- glue::glue("^```\\{<<engine>>", .open="<<", .close=">>")
    }
  }
  # find drake information
  {
    require(dplyr)
    Rmdlines %>%
      stringr::str_which(pattern) -> whichHasRSetting
    Rmdlines[whichHasRSetting] %>%
      stringr::str_trim(side="both") %>%
      stringr::str_detect(exclude) -> pickDrakeF
    whichHasRSetting[!pickDrakeF] -> whichHasDrakeObjects
    Rmdlines[whichHasDrakeObjects] %>%
      stringr::str_extract("(?<=```\\{r )[[:alnum:]_]+") -> drakeObjects
  }
  # generate chunk table
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
  drakeLocations
}


#' Get a list of code chunks from Rmdlines
#'
#' @param Rmdlines A character string from readLines.
#' @param engine A character specifies which language code chunk to target
#' @param stackNAchunks A logical. default=T the unlabelled code chunks will be stack together
#' @param requireLabel A logical. default=F extract unlabelled chunks as well
#' @param exclude A character of regex setting labels pattern whose chunks will be ignored from extraction.
#'
#' @return
#' @export
#'
#' @examples None.
get_listCodeChunksFromRmdlines <- function(
  Rmdlines, engine = "r",
  stackNAchunks = T,
  requireLabel=F,
  exclude = "(afterMake=T|drake=F|\\bsetup\\b)" # regex for chunk exclusion
){
  require(dplyr)

  Rmdlines %>%
    get_chunksTable(engine = engine,
                    requireLabel=requireLabel,
                    exclude = exclude # regex for chunk exclusion
    ) -> chunkTable

  targets <- chunkTable$object %>% unique()

  chunkTable %>%
    get_listOfCodeChunksFromChunkTable(Rmdlines, targets, stackNAchunks = stackNAchunks)
}


# helpers -----------------------------------------------------------------

get_listOfCodeChunksFromChunkTable <- function(chunkTable, Rmdlines, targets, stackNAchunks=T)
{
  # get list of non-NA-chunks
  {
    require(dplyr)
    nonNATargets = targets[which(!is.na(targets))]
    nonNAchunks = vector("list", length(nonNATargets))
    names(nonNAchunks) <- nonNATargets
    for(.x in seq_along(nonNATargets)){
      nonNAchunks[[.x]] <-
        get_drakeBody(
          Rmdlines,
          chunkTable %>%
            filter(
              object==nonNATargets[[.x]]
            )
        )
    }
  }
  NaChunks <- list()
  if(any(is.na(targets)))
  # get list of NA-chunks
  {
    chunkTable %>%
      filter(
        is.na(object)
      ) -> NAtable
    if(nrow(NAtable)==0) stop("there is no NA code chunk.")
    NaChunks <- vector("list", length = nrow(NAtable))
    for(.x in seq_along(NAtable$object)){
      NaChunks[[.x]] <-
        get_drakeBody(
          Rmdlines,
          NAtable[.x,]
        )
    }
    if(stackNAchunks==T){
      NaChunks <- unlist(NaChunks)
    }
  }
  ## stack nonNaChunks and NaChunks
  append(nonNAchunks, NaChunks)
}


# debug -------------------------------------------------------------------

require(dplyr)
"/Users/martinl/Github/webtemplate/drake_03_dateTime.Rmd" %>%
  xfun::read_utf8() -> Rmdlines

Rmdlines %>%
  rmd2drake::get_listCodeChunksFromRmdlines() -> rChunks
Rmdlines %>%
  rmd2drake::get_listCodeChunksFromRmdlines("css") -> cssChunks
Rmdlines %>%
  rmd2drake::get_listCodeChunksFromRmdlines("js") -> jsChunks
Rmdlines %>%
  rmd2drake::get_listCodeChunksFromRmdlines("html") -> htmlChunks
