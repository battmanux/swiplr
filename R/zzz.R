.onLoad <- function(libname, pkgname) {
  knitr::knit_engines$set(swiplr=knit_prolog_engine)
  knitr::knit_engines$set(prolog=knit_prolog_engine)
}
