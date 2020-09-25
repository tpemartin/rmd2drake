# setRepositories(
#   addURLs=c('https://cran.csie.ntu.edu.tw/')
# )
install.packages("devtools")
rprojroot::is_r_package -> pj
pj$make_fix_file() -> root
readLines(
  file.path(
    root(),
    "Untitled/Untitled1.Rmd")) -> Rmdlines
knitr::knit_params(Rmdlines) -> params
params$memberGmails$value

elementNames <- purrr::map(
  params,~purrr::pluck(.x, "name"))
params <- purrr::map(
  params,~purrr::pluck(.x, "value"))
